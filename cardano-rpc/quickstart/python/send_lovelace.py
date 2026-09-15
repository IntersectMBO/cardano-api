import time
from fractions import Fraction
from pathlib import Path
from typing import List

import grpc
from pycardano import (
    Address,
    ChainContext,
    Network,
    PaymentSigningKey,
    ProtocolParameters,
    TransactionBuilder,
    TransactionInput,
    TransactionOutput,
    UTxO,
    Value,
)
from pycardano.hash import TransactionId
from utxorpc_spec.utxorpc.v1beta.cardano.cardano_pb2 import (
    AddressPattern,
    TxOutputPattern,
)
from utxorpc_spec.utxorpc.v1beta.query.query_pb2 import (
    AnyUtxoPattern,
    ReadParamsRequest,
    SearchUtxosRequest,
    UtxoPredicate,
)
from utxorpc_spec.utxorpc.v1beta.query.query_pb2_grpc import QueryServiceStub
from utxorpc_spec.utxorpc.v1beta.submit.submit_pb2 import AnyChainTx, SubmitTxRequest
from utxorpc_spec.utxorpc.v1beta.submit.submit_pb2_grpc import SubmitServiceStub

RPC_URL = "localhost:50051"
CLUSTER_DIR = "/tmp/demo-cluster"
LOVELACE_TO_SEND = 5_000_000  # 5 ADA (1 ADA = 1,000,000 lovelace)


class CardanoRpcChainContext(ChainContext):
    """A PyCardano ChainContext backed directly by cardano-rpc's v1beta gRPC services.

    Covers only what building, signing, and submitting a plain payment needs:
    UTxO lookup, protocol parameters, and submission.
    It is demo scaffolding, not a general-purpose provider: it does not surface
    datums, inline datums, reference scripts, or script cost models, and every
    BigInt field is read through its `int` branch, which testnet-scale values
    always fit.
    """

    def __init__(self, target: str):
        channel = grpc.insecure_channel(target)
        self._query = QueryServiceStub(channel)
        self._submit = SubmitServiceStub(channel)

    @property
    def network(self) -> Network:
        return Network.TESTNET

    @property
    def protocol_param(self) -> ProtocolParameters:
        pparams = self._query.ReadParams(ReadParamsRequest(), timeout=5).values.cardano

        def fraction(r) -> Fraction:
            return Fraction(r.numerator, r.denominator)

        return ProtocolParameters(
            min_fee_constant=pparams.min_fee_constant.int,
            min_fee_coefficient=pparams.min_fee_coefficient.int,
            max_block_size=pparams.max_block_body_size,
            max_tx_size=pparams.max_tx_size,
            max_block_header_size=pparams.max_block_header_size,
            key_deposit=pparams.stake_key_deposit.int,
            pool_deposit=pparams.pool_deposit.int,
            pool_influence=fraction(pparams.pool_influence),
            monetary_expansion=fraction(pparams.monetary_expansion),
            treasury_expansion=fraction(pparams.treasury_expansion),
            # Byron-era fields with no cardano-rpc equivalent; unused by a
            # plain Conway-era payment.
            decentralization_param=Fraction(0),
            extra_entropy="",
            min_utxo=0,
            coins_per_utxo_word=0,
            protocol_major_version=pparams.protocol_version.major,
            protocol_minor_version=pparams.protocol_version.minor,
            min_pool_cost=pparams.min_pool_cost.int,
            price_mem=fraction(pparams.prices.memory),
            price_step=fraction(pparams.prices.steps),
            max_tx_ex_mem=pparams.max_execution_units_per_transaction.memory,
            max_tx_ex_steps=pparams.max_execution_units_per_transaction.steps,
            max_block_ex_mem=pparams.max_execution_units_per_block.memory,
            max_block_ex_steps=pparams.max_execution_units_per_block.steps,
            max_val_size=pparams.max_value_size,
            collateral_percent=pparams.collateral_percentage,
            max_collateral_inputs=pparams.max_collateral_inputs,
            coins_per_utxo_byte=pparams.coins_per_utxo_byte.int,
            cost_models={},  # not needed to build a plain (non-script) payment
        )

    def _utxos(self, address: str) -> List[UTxO]:
        address_bytes = bytes(Address.decode(address).to_primitive())
        predicate = UtxoPredicate(
            match=AnyUtxoPattern(
                cardano=TxOutputPattern(
                    address=AddressPattern(exact_address=address_bytes)
                )
            )
        )
        response = self._query.SearchUtxos(
            SearchUtxosRequest(predicate=predicate), timeout=5
        )
        utxos = []
        for item in response.items:
            cardano = item.cardano
            tx_in = TransactionInput(
                TransactionId(item.txo_ref.hash), item.txo_ref.index
            )
            tx_out = TransactionOutput(
                Address.from_primitive(cardano.address), Value(cardano.coin.int)
            )
            utxos.append(UTxO(tx_in, tx_out))
        return utxos

    def submit_tx_cbor(self, cbor):
        if isinstance(cbor, str):
            cbor = bytes.fromhex(cbor)
        response = self._submit.SubmitTx(
            SubmitTxRequest(tx=AnyChainTx(raw=cbor)), timeout=5
        )
        return response.ref.hex()


def main() -> None:
    cluster_dir = Path(CLUSTER_DIR)
    context = CardanoRpcChainContext(RPC_URL)

    # cardano-testnet's genesis UTxO signing key is a TextEnvelope with type
    # "GenesisUTxOSigningKey_ed25519"; PaymentSigningKey.from_json() only
    # validates the type when asked to, so it loads this key as-is.
    signing_key = PaymentSigningKey.from_json(
        (cluster_dir / "utxo-keys/utxo1/utxo.skey").read_text()
    )
    sender_address = Address.load(str(cluster_dir / "utxo-keys/utxo1/utxo.addr"))
    recipient_address = Address.load(str(cluster_dir / "utxo-keys/utxo2/utxo.addr"))

    utxos = context.utxos(sender_address)
    print(f"Sender address:    {sender_address}")
    print(f"Recipient address: {recipient_address}")
    print(f"Spendable UTxOs:   {len(utxos)}")

    builder = TransactionBuilder(context)
    for utxo in utxos:
        builder.add_input(utxo)
    builder.add_output(TransactionOutput(recipient_address, Value(LOVELACE_TO_SEND)))

    signed_tx = builder.build_and_sign([signing_key], change_address=sender_address)
    tx_hash = context.submit_tx(signed_tx)
    print(f"Submitted tx: {tx_hash}")

    # Confirm by polling the recipient's UTxOs for the new output.
    deadline = time.monotonic() + 60
    while time.monotonic() < deadline:
        for utxo in context.utxos(recipient_address):
            if str(utxo.input.transaction_id) == tx_hash:
                print(
                    f"Confirmed: {utxo.output.amount.coin} lovelace landed at "
                    f"{recipient_address} ({tx_hash}#{utxo.input.index})"
                )
                return
        time.sleep(3)
    raise TimeoutError(
        "Timed out waiting for the new UTxO to appear at the recipient address"
    )


if __name__ == "__main__":
    main()
