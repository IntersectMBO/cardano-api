//@ts-check
import cardano_api from "./cardano-api.js";

let promise = cardano_api();

async function do_async_work() {
  let api = await promise;
  let grpcApi = await api.newGrpcConnection("http://localhost:8080")

  let showPrivateKey = true;
  let wallet = await api.generateTestnetPaymentWallet(2);
  let refresh = async function () { };

  async function generateAddress() {
    wallet = await api.generateTestnetPaymentWallet(2);
    await refresh();
  }

  // @ts-ignore
  document.getElementById("regenerate-address-button").addEventListener("click", generateAddress);

  async function togglePrivateKeyDisplay() {
    showPrivateKey = !showPrivateKey;
    await refresh();
  }

  // @ts-ignore
  document.getElementById("show-private-key-button").addEventListener("click", togglePrivateKeyDisplay);

  // @ts-ignore
  async function loadPrivateKey() {
    let pki = document.getElementById("private-key-input");
    // @ts-ignore
    wallet = await api.restoreTestnetPaymentWalletFromSigningKeyBech32(2, pki.value);
    await refresh();
  };

  refresh = async function () {
    // @ts-ignore

    // Wallet rendering
    document.getElementById("address-input").value = await wallet.getAddressBech32();
    let pki = document.getElementById("private-key-input")
    if (showPrivateKey) {
      // @ts-ignore
      pki.value = await wallet.getBech32ForSigningKey();
      // @ts-ignore
      pki.disabled = false;
      // @ts-ignore
      document.getElementById("show-private-key-button").innerText = "Hide";
    } else {
      // @ts-ignore
      pki.value = "<<< Private key hidden >>>";
      // @ts-ignore
      pki.disabled = true;
      // @ts-ignore
      document.getElementById("show-private-key-button").innerText = "Show";
    }
    let utxos = await grpcApi.getUtxosForAddress(await wallet.getAddressBech32());
    let utxoTable = document.getElementById("utxo-table");

    // UTxO rendering
    // @ts-ignore
    while (utxoTable.rows.length > 1) {
      // @ts-ignore
      utxoTable.deleteRow(1);
    }
    // Add new rows for each UTxO
    for (let utxo of utxos) {
      let row = document.createElement("tr");
      let txHashCell = document.createElement("td");
      txHashCell.innerText = utxo.txHash;
      row.appendChild(txHashCell);
      let txIndexCell = document.createElement("td");
      txIndexCell.innerText = utxo.txIndex.toString();
      row.appendChild(txIndexCell);
      let valueCell = document.createElement("td");
      valueCell.innerText = utxo.value.toString();
      row.appendChild(valueCell);
      utxoTable.appendChild(row);
    }


    // @ts-ignore
    document.getElementById("load-private-key-button").addEventListener("click", loadPrivateKey);


    generateAddress()
  }

  do_async_work().then(() => { });
