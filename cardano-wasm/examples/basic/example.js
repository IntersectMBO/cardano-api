//@ts-check
import cardano_api from "./cardano-api.js";

let promise = cardano_api();

async function get_protocol_params() {
    const response = await fetch("./preview_pparams.json");
    return (await response.json());
}

let protocolParams = await get_protocol_params();

const output = document.createElement("code");
output.innerText = "";
output.id = "test-output";
document.body.appendChild(output);

function log(out) {
    console.log(out);
    if (typeof (out) == "object") {
        output.innerText += "> [object] {\n";
        for (let [key, val] of Object.entries(out)) {
            let text = val.toString();
            if (typeof (val) == "function") {
                text = text.split("{")[0];
            }
            output.innerText += "    " + key + ": " + text + "\n";
        }
        output.innerText += "  }\n";
    } else if (typeof (out) == "bigint") {
        output.innerText += "> " + out.toString() + "n\n";
    } else {
        output.innerText += "> " + JSON.stringify(out) + "\n";
    }
}

function finish_test() {
    let finishTag = document.createElement("p");
    finishTag.innerText = "Finished test!";
    finishTag.id = "finish-tag";
    document.body.appendChild(finishTag);
}

async function do_async_work() {
    let api = await promise;
    log("Api object:");
    log(api);

    let PREVIEW_MAGIC_NUMBER = 2;
    let secretKey = "addr_sk1648253w4tf6fv5fk28dc7crsjsaw7d9ymhztd4favg3cwkhz7x8sl5u3ms";
    let wallet = await api.restoreTestnetPaymentWalletFromSigningKeyBech32(PREVIEW_MAGIC_NUMBER, secretKey);
    let bech32Address = await wallet.getAddressBech32();

    log("Bech32 of address:");
    log(bech32Address);

    let emptyTx = await api.newConwayTx();
    log("UnsignedTx object:");
    log(emptyTx);

    let tx = await emptyTx.addTxInput("be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd978", 0)
        .addSimpleTxOut("addr_test1vzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpcue4v2v", 10_000_000n)

    let feeEstimate = await tx.estimateMinFee(protocolParams, 1, 0, 0);
    log("Estimated fee:");
    log(feeEstimate);

    let signedTx = await tx.setFee(feeEstimate)
        .signWithPaymentKey(secretKey);
    log("SignedTx object:");
    log(signedTx);

    let txCbor = await signedTx.txToCbor();
    log("Tx CBOR:");
    log(txCbor);

    finish_test();
}

do_async_work().then(() => { });
