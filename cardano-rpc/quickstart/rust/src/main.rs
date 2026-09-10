use tonic::transport::Endpoint;

use utxorpc_spec::utxorpc::v1beta::query::{
    any_chain_params::Params as AnyChainParamsVariant, query_service_client::QueryServiceClient,
    ReadParamsRequest,
};
use utxorpc_spec::utxorpc::v1beta::sync::{sync_service_client::SyncServiceClient, ReadTipRequest};

const RPC_URL: &str = "http://localhost:50051";

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let channel = Endpoint::try_from(RPC_URL)?.connect().await?;

    let mut sync_client = SyncServiceClient::new(channel.clone());
    let mut query_client = QueryServiceClient::new(channel);

    let tip = sync_client
        .read_tip(ReadTipRequest {})
        .await?
        .into_inner()
        .tip
        .expect("ReadTip response always carries a tip");
    println!(
        "Tip: slot {} height {} hash {}",
        tip.slot,
        tip.height,
        hex::encode(&tip.hash)
    );

    let params = query_client
        .read_params(ReadParamsRequest { field_mask: None })
        .await?
        .into_inner()
        .values
        .and_then(|v| v.params)
        .expect("cardano-rpc always returns Cardano parameters");
    match params {
        AnyChainParamsVariant::Cardano(pparams) => println!(
            "Protocol parameters: max_tx_size {} max_block_body_size {}",
            pparams.max_tx_size, pparams.max_block_body_size
        ),
    }

    Ok(())
}
