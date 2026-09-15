use std::{
    error::Error,
    fs,
    time::{Duration, Instant},
};

use pallas_addresses::Address;
use pallas_crypto::{hash::Hash, key::ed25519::SecretKey};
use pallas_txbuilder::{BuildConway, BuiltTransaction, Input, Output, StagingTransaction};
use serde::Deserialize;
use tonic::transport::{Channel, Endpoint};
use utxorpc_spec::utxorpc::v1beta::{
    cardano::{big_int::BigInt as BigIntValue, AddressPattern, BigInt, TxOutputPattern},
    query::{
        any_chain_params::Params as AnyChainParamsVariant, any_utxo_data, any_utxo_pattern,
        query_service_client::QueryServiceClient, AnyUtxoPattern, ReadParamsRequest,
        SearchUtxosRequest, UtxoPredicate,
    },
    submit::{
        any_chain_tx::Type as SubmitTxType, submit_service_client::SubmitServiceClient,
        AnyChainTx as SubmitAnyChainTx, SubmitTxRequest,
    },
};

const RPC_URL: &str = "http://localhost:50051";
const CLUSTER_DIR: &str = "/tmp/demo-cluster";
const LOVELACE_TO_SEND: u64 = 5_000_000; // 5 ADA (1 ADA = 1,000,000 lovelace)

#[derive(Deserialize)]
struct TextEnvelope {
    #[serde(rename = "cborHex")]
    cbor_hex: String,
}

// cardano-cli's normal (non-extended) signing keys wrap a 32-byte seed as a
// CBOR bytestring: 0x58 0x20 <32 bytes>. Skip the two-byte header.
fn read_signing_key(path: &str) -> Result<SecretKey, Box<dyn Error>> {
    let envelope: TextEnvelope = serde_json::from_str(&fs::read_to_string(path)?)?;
    let seed: [u8; 32] = hex::decode(envelope.cbor_hex)?[2..].try_into()?;
    Ok(SecretKey::from(seed))
}

fn read_address(path: &str) -> Result<Address, Box<dyn Error>> {
    Ok(Address::from_bech32(fs::read_to_string(path)?.trim())?)
}

// Testnet-scale values always fit in the `int` arm of the BigInt oneof.
fn big_int_to_u64(value: &Option<BigInt>) -> u64 {
    match value.as_ref().and_then(|b| b.big_int.as_ref()) {
        Some(BigIntValue::Int(v)) => *v as u64,
        _ => 0,
    }
}

async fn fetch_utxos(
    query_client: &mut QueryServiceClient<Channel>,
    address: &Address,
) -> Result<Vec<(Input, u64)>, Box<dyn Error>> {
    let response = query_client
        .search_utxos(SearchUtxosRequest {
            predicate: Some(UtxoPredicate {
                r#match: Some(AnyUtxoPattern {
                    utxo_pattern: Some(any_utxo_pattern::UtxoPattern::Cardano(TxOutputPattern {
                        address: Some(AddressPattern {
                            exact_address: Some(address.to_vec().into()),
                            ..Default::default()
                        }),
                        ..Default::default()
                    })),
                }),
                ..Default::default()
            }),
            ..Default::default()
        })
        .await?
        .into_inner();

    response
        .items
        .into_iter()
        .map(|item| {
            let txo_ref = item.txo_ref.ok_or("UTxO is missing its txo_ref")?;
            let tx_hash: [u8; 32] = txo_ref.hash.as_ref().try_into()?;
            let lovelace = match item.parsed_state {
                Some(any_utxo_data::ParsedState::Cardano(output)) => big_int_to_u64(&output.coin),
                None => 0,
            };
            let input = Input::new(Hash::new(tx_hash), txo_ref.index as u64);
            Ok((input, lovelace))
        })
        .collect()
}

#[allow(clippy::too_many_arguments)]
fn build_and_sign(
    utxos: &[(Input, u64)],
    signing_key: &SecretKey,
    recipient_address: &Address,
    sender_address: &Address,
    lovelace_to_send: u64,
    fee: u64,
    change: u64,
) -> Result<BuiltTransaction, Box<dyn Error>> {
    let mut tx = StagingTransaction::new().fee(fee);
    for (input, _) in utxos {
        tx = tx.input(input.clone());
    }
    tx = tx
        .output(Output::new(recipient_address.clone(), lovelace_to_send))
        .output(Output::new(sender_address.clone(), change));
    Ok(tx.build_conway_raw()?.sign(signing_key)?)
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let channel = Endpoint::try_from(RPC_URL)?.connect().await?;
    let mut query_client = QueryServiceClient::new(channel.clone());
    let mut submit_client = SubmitServiceClient::new(channel);

    let signing_key = read_signing_key(&format!("{CLUSTER_DIR}/utxo-keys/utxo1/utxo.skey"))?;
    let sender_address = read_address(&format!("{CLUSTER_DIR}/utxo-keys/utxo1/utxo.addr"))?;
    let recipient_address = read_address(&format!("{CLUSTER_DIR}/utxo-keys/utxo2/utxo.addr"))?;

    let utxos = fetch_utxos(&mut query_client, &sender_address).await?;
    let total_lovelace: u64 = utxos.iter().map(|(_, lovelace)| lovelace).sum();

    println!("Sender address:    {}", sender_address.to_bech32()?);
    println!("Recipient address: {}", recipient_address.to_bech32()?);
    println!("Spendable UTxOs:   {}", utxos.len());

    if total_lovelace < LOVELACE_TO_SEND {
        return Err("sender does not hold enough lovelace to cover the transfer".into());
    }

    let AnyChainParamsVariant::Cardano(pparams) = query_client
        .read_params(ReadParamsRequest { field_mask: None })
        .await?
        .into_inner()
        .values
        .and_then(|v| v.params)
        .expect("cardano-rpc always returns Cardano parameters");
    let min_fee_coefficient = big_int_to_u64(&pparams.min_fee_coefficient);
    let min_fee_constant = big_int_to_u64(&pparams.min_fee_constant);

    // pallas-txbuilder's `build_conway_raw` does no automatic fee or change
    // balancing: the fee and every output must already be exact. Build once
    // with a guessed fee to measure the real encoded size, then rebuild with
    // the fee computed from that size (both guess and real fee land in the
    // same CBOR integer width for a plain payment, so one refinement suffices).
    let change_before_fee = total_lovelace - LOVELACE_TO_SEND;
    let fee_guess = min_fee_constant + min_fee_coefficient * 300;
    let draft = build_and_sign(
        &utxos,
        &signing_key,
        &recipient_address,
        &sender_address,
        LOVELACE_TO_SEND,
        fee_guess,
        change_before_fee - fee_guess,
    )?;

    let fee = min_fee_constant + min_fee_coefficient * draft.tx_bytes.0.len() as u64;
    let change = change_before_fee - fee;
    let signed = build_and_sign(
        &utxos,
        &signing_key,
        &recipient_address,
        &sender_address,
        LOVELACE_TO_SEND,
        fee,
        change,
    )?;

    let submit_response = submit_client
        .submit_tx(SubmitTxRequest {
            tx: Some(SubmitAnyChainTx {
                r#type: Some(SubmitTxType::Raw(signed.tx_bytes.0.clone().into())),
            }),
        })
        .await?
        .into_inner();
    let submitted_hash: [u8; 32] = submit_response.r#ref.as_ref().try_into()?;
    println!("Submitted tx: {}", hex::encode(submitted_hash));

    // Confirm by polling the recipient's UTxOs for the new output.
    let deadline = Instant::now() + Duration::from_secs(60);
    loop {
        let recipient_utxos = fetch_utxos(&mut query_client, &recipient_address).await?;
        if let Some((input, lovelace)) = recipient_utxos
            .iter()
            .find(|(input, _)| input.tx_hash.0 == submitted_hash)
        {
            println!(
                "Confirmed: {lovelace} lovelace landed at {} ({}#{})",
                recipient_address.to_bech32()?,
                hex::encode(submitted_hash),
                input.txo_index
            );
            return Ok(());
        }
        if Instant::now() >= deadline {
            return Err(
                "Timed out waiting for the new UTxO to appear at the recipient address".into(),
            );
        }
        tokio::time::sleep(Duration::from_secs(3)).await;
    }
}
