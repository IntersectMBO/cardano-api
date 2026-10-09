import grpc

from utxorpc_spec.utxorpc.v1beta.query.query_pb2 import ReadParamsRequest
from utxorpc_spec.utxorpc.v1beta.query.query_pb2_grpc import QueryServiceStub
from utxorpc_spec.utxorpc.v1beta.sync.sync_pb2 import ReadTipRequest
from utxorpc_spec.utxorpc.v1beta.sync.sync_pb2_grpc import SyncServiceStub

RPC_URL = "localhost:50051"


def main() -> None:
    channel = grpc.insecure_channel(RPC_URL)

    sync_client = SyncServiceStub(channel)
    tip = sync_client.ReadTip(ReadTipRequest(), timeout=5).tip
    print(f"Tip: slot {tip.slot} height {tip.height} hash {tip.hash.hex()}")

    query_client = QueryServiceStub(channel)
    pparams = query_client.ReadParams(ReadParamsRequest(), timeout=5).values.cardano
    print(
        f"Protocol parameters: max_tx_size {pparams.max_tx_size} "
        f"max_block_body_size {pparams.max_block_body_size}"
    )


if __name__ == "__main__":
    main()
