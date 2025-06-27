import cardano_api from "./cardano-api.js";

let promise = cardano_api();
async function get_protocol_params() {
    const response = await fetch("./preview_pparams.json");
    return (await response.json());
}
let protocolParams = await get_protocol_params();
async function do_async_work() {
    let api = await promise;
    console.log("Api object:");
    console.log(api);

    let emptyTx = await api.newConwayTx();
    console.log("UnsignedTx object:");
    console.log(emptyTx);

    let tx = await emptyTx.addTxInput("be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd978", 0)
        .addSimpleTxOut("addr_test1vzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpcue4v2v", 10_000_000n)
        .addSigningKey("addr_sk1648253w4tf6fv5fk28dc7crsjsaw7d9ymhztd4favg3cwkhz7x8sl5u3ms");
    let feeEstimate = await tx.estimateMinFee(protocolParams, 0, 0, 0);
    console.log("Estimated fee:");
    console.log(feeEstimate);

    let signedTx = await tx.setFee(feeEstimate).signTx();
    console.log("SignedTx object:");
    console.log(signedTx);

    let txCbor = await signedTx.txToCbor();
    console.log("Tx CBOR:");
    console.log(txCbor);
}
do_async_work().then(() => { });
