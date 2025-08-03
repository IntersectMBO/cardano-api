//@ts-check
import cardano_api from "./cardano-api.js";

const TESTNET_MAGIC = 42;
let promise = cardano_api();

async function do_async_work() {
  let api = await promise;
  let grpcApi = await api.newGrpcConnection("http://localhost:8080");
  let protocolParams = await grpcApi.getProtocolParams();

  // State
  let showPrivateKey = false;
  let wallet = await api.generateTestnetPaymentWallet(42);
  let transactionInputs = [];
  let transactionOutputs = [];

  // Helper function for table cell creation using insertCell
  function insertCell(tableRow, className, textContent) {
    const cell = tableRow.insertCell();
    cell.className = className;
    cell.textContent = textContent;
    return cell;
  }

  // Helper function to add a button cell to a table row
  function addButtonCell(tableRow, className, caption, onClick) {
    const cell = insertCell(tableRow, className, "");
    const button = document.createElement("button");
    button.innerText = caption;
    button.addEventListener("click", onClick);
    cell.appendChild(button);
    return cell;
  }

  async function makeTransaction() {
    let tx = await api.newConwayTx();
    for (let input of transactionInputs) {
      tx = tx.addTxInput(input.txId, input.txIndex);
    }
    for (let output of transactionOutputs) {
      tx = tx.addSimpleTxOut(output.address, output.lovelace);
    }
    let txFee = await tx.estimateMinFee(protocolParams, 1, 0, 0);
    tx.setFee(txFee);
    return tx;
  }

  // Refresh function
  async function refresh() {
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
    for (let utxo of utxos) {
      // @ts-ignore
      let row = utxoTable.insertRow();
      insertCell(row, "utxo-txid long", utxo.txId);
      insertCell(row, "utxo-txix", utxo.txIndex.toString());
      insertCell(row, "utxo-txada", utxo.lovelace.toString());
      addButtonCell(row, "utxo-addtotx", "Add to tx", () => { addInputToTx(utxo.txId, utxo.txIndex, utxo.lovelace); });
    }

    // Transaction Inputs
    let txinTable = document.getElementById("txin-table");
    // @ts-ignore
    while (txinTable.rows.length > 1) {
      // @ts-ignore
      txinTable.deleteRow(1);
    }
    let total = 0;
    let index = 0;
    for (let input of transactionInputs) {
      // @ts-ignore
      let row = txinTable.insertRow();
      insertCell(row, "txin-txid long", input.txId);
      insertCell(row, "txin-txix", input.txIndex.toString());
      insertCell(row, "txin-txada", input.lovelace.toString());
      total += input.lovelace;
      let thisIndex = index;
      addButtonCell(row, "txin-remove", "Remove", async () => {
        transactionInputs.splice(thisIndex, 1);
        await refresh();
      });
      index++;
    }
    // @ts-ignore
    let totalRow = txinTable.insertRow();
    insertCell(totalRow, "", "");
    insertCell(totalRow, "", "");
    insertCell(totalRow, "total", total.toString());
    insertCell(totalRow, "total-label", "Total");

    // Transaction Outputs
    const txOutTable = document.getElementById('txout-table');
    // @ts-ignore
    while (txOutTable.rows.length > 1) {
      // @ts-ignore
      txOutTable.deleteRow(1);
    }
    let transactionOutputIx = 0;
    let outputsTotal = 0n;
    for (let output of transactionOutputs) {
      // @ts-ignore
      const newRow = txOutTable.insertRow();
      insertCell(newRow, 'txout-txid long', output.address);
      insertCell(newRow, 'txout-txada', output.lovelace);
      // Remove the row of the clicked button
      let ix = transactionOutputIx;
      addButtonCell(newRow, 'txout-remove', 'Remove', async () => {
        transactionOutputs.splice(ix, 1);
        await refresh();
      });
      outputsTotal += output.lovelace;
      transactionOutputIx += 1;
    }
    let tx = await makeTransaction();
    let fees = await tx.estimateMinFee(protocolParams, 1, 0, 0);
    // @ts-ignore
    const feesRow = txOutTable.insertRow();
    insertCell(feesRow, 'txout-txid long', '');
    insertCell(feesRow, '', fees.toString());
    insertCell(feesRow, 'total-label', "Fees");

    let totalSpent = outputsTotal + fees;
    // @ts-ignore
    const totalOutRow = txOutTable.insertRow();
    insertCell(totalOutRow, 'txout-txid long', '');
    insertCell(totalOutRow, 'total', totalSpent.toString());
    insertCell(totalOutRow, 'total-label', "Total");
  }

  // Callbacks
  async function generateAddress() {
    wallet = await api.generateTestnetPaymentWallet(TESTNET_MAGIC);
    await refresh();
  }

  document.getElementById("regenerate-address-button")?.addEventListener("click", generateAddress);

  async function togglePrivateKeyDisplay() {
    showPrivateKey = !showPrivateKey;
    await refresh();
  }

  document.getElementById("show-private-key-button")?.addEventListener("click", togglePrivateKeyDisplay);

  // @ts-ignore
  async function loadPrivateKey() {
    let pki = document.getElementById("private-key-input");
    // @ts-ignore
    wallet = await api.restoreTestnetPaymentWalletFromSigningKeyBech32(TESTNET_MAGIC, pki.value);
    await refresh();
  };

  async function addInputToTx(txId, txIndex, lovelace) {
    // @ts-ignore
    transactionInputs.push({
      txId: txId,
      txIndex: txIndex,
      lovelace: lovelace
    });
    await refresh();
  }

  async function addOutputFromForm() {
    const address =
      // @ts-ignore
      document.getElementById('add-output-address').value.trim();
    const lovelace =
      // @ts-ignore
      BigInt(document.getElementById('add-output-lovelace').value.trim());
    transactionOutputs.push({
      address: address,
      lovelace: lovelace
    });
    // @ts-ignore
    document.getElementById('add-output-address').value = '';
    // @ts-ignore
    document.getElementById('add-output-lovelace').value = '';
    await refresh();
  }

  async function submitTransaction() {
    let tx = await makeTransaction();
    let signingKey = await wallet.getBech32ForSigningKey();
    let signedTx = await tx.signWithPaymentKey(signingKey);

    await grpcApi.submitTx(await signedTx.txToCbor()).then((txId) => {
      alert("Transaction submitted successfully with ID: " + txId);
      transactionInputs = [];
      transactionOutputs = [];
    }).catch((err) => {
      alert("Error submitting transaction: " + err);
    });
    await refresh();
  }

  document.getElementById('load-private-key-button')?.addEventListener('click', loadPrivateKey);

  document.getElementById('utxo-reload-button')?.addEventListener('click', refresh);

  document.getElementById('add-output-button')?.addEventListener('click', addOutputFromForm);

  document.getElementById('submit-button')?.addEventListener('click', submitTransaction);

  generateAddress();
}
do_async_work().then(() => { });
