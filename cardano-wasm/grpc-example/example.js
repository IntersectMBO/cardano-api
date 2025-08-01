//@ts-check
import cardanoApi from "./cardano-api.js";

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
  let api = await cardanoApi();
  log("Api object:");
  log(api);

  let grpcApi = globalThis.grpcApi = await api.newGrpcConnection("http://localhost:8080")

  log("GRPC connection object:");
  log(grpcApi);

  let eraNum = await grpcApi.getEra();
  log("Era number:");
  log(eraNum);

  let pparams = await grpcApi.getProtocolParams();
  log("Protocol Parameters:");
  log(pparams);

  let utxos = await grpcApi.getAllUtxos();
  log("Utxos:");
  log(utxos);

  let addr = utxos[0].address;
  log(`Utxos for first address: ${addr}`)
  log((await grpcApi.getUtxosForAddress(addr)));

  finish_test();
}

do_async_work().then(() => { });
