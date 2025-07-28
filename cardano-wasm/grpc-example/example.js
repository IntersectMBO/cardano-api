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

    let grpcApi = await api.newGrpcConnection("http://localhost:8080")

    log("GRPC connection object:");
    log(grpcApi);

    let PREVIEW_MAGIC_NUMBER = 2;
    // let secretKey = "addr_sk18dt3c2tctvnj7eytclrhq02xtgq4wk77wflk2hgydl4kkujmtdpqrnlfe8";
    let secretKey = "addr_sk1c5878e87y5vapvrx0eejqnlky2djp99zj2agpvx92mvch5f7ck0srvaf4l";
    let address = await api.restoreTestnetPaymentAddressFromSigningKeyBech32(PREVIEW_MAGIC_NUMBER, secretKey);
    // let address = await api.generateTestnetPaymentAddress(PREVIEW_MAGIC_NUMBER);
    // let secretKey = await address.getBech32ForSigningKey();
    log("Bech32 of secret key:");
    log(secretKey);

    let bech32Address = await address.getAddressBech32();
    log("Bech32 of address:")
    log(bech32Address);

    let emptyTx = await api.newConwayTx();
    let tx = await emptyTx.addTxInput("3efd469bdcbdd82cf120220e52298cabd955ae2289e4b4903203deab209e3dc5", 0)
        .addSimpleTxOut("addr_test1vqeux7xwusdju9dvsj8h7mca9aup2k439kfmwy773xxc2hcu7zy99", 9_999_835_819n)

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

    let submitResult = await grpcApi.submitTx(txCbor);
    log("Transaction submitted. Result:");
    log(submitResult);

    finish_test();
}

do_async_work().then(() => { });
