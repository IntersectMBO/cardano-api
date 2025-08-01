//@ts-check
import cardano_api from "./cardano-api.js";

let promise = cardano_api();

async function do_async_work() {
  let api = await promise;
  let grpcApi = await api.newGrpcConnection("http://localhost:8080");
  let protocolParams = await grpcApi.getProtocolParams();

  // State
  let showPrivateKey = true;
  let wallet = await api.generateTestnetPaymentWallet(2);
  let transactionInputs = [];
  let transactionOutputs = [];

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
      let row = document.createElement("tr");
      let txId = document.createElement("td");
      txId.className = "utxo-txid long";
      txId.innerText = utxo.txId;
      row.appendChild(txId);
      let txIx = document.createElement("td");
      txIx.className = "utxo-txix";
      txIx.innerText = utxo.txIndex.toString();
      row.appendChild(txIx);
      let txAda = document.createElement("td");
      txAda.className = "utxo-txada";
      txAda.innerText = utxo.lovelace.toString();
      row.appendChild(txAda);
      let addToTx = document.createElement("td");
      addToTx.className = "utxo-addtotx";
      let button = document.createElement("button");
      button.innerText = "Add to tx";
      button.addEventListener("click", () => { addInputToTx(utxo.txId, utxo.txIndex, utxo.lovelace); });
      // @ts-ignore
      addToTx.appendChild(button);
      row.appendChild(addToTx);
      // @ts-ignore
      utxoTable.appendChild(row);
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
      let row = document.createElement("tr");
      let txId = document.createElement("td");
      txId.className = "txin-txid long";
      txId.innerText = input.txId;
      row.appendChild(txId);
      let txIx = document.createElement("td");
      txIx.className = "txin-txix";
      txIx.innerText = input.txIndex.toString();
      row.appendChild(txIx);
      let txAda = document.createElement("td");
      txAda.className = "txin-txada";
      txAda.innerText = input.lovelace.toString();
      total += input.lovelace;
      row.appendChild(txAda);
      let removeButton =
        document.createElement("td");
      removeButton.className = "txin-remove";
      let button = document.createElement("button");
      button.innerText = "Remove";
      let thisIndex = index;
      button.addEventListener("click", async () => {
        transactionInputs.splice(thisIndex, 1);
        await refresh();
      });
      // @ts-ignore
      removeButton.appendChild(button);
      row.appendChild(removeButton);
      // @ts-ignore
      txinTable.appendChild(row);
      index++;
    }
    let totalRow = document.createElement("tr");
    let emptyCell = document.createElement("td");
    emptyCell.innerText = "";
    totalRow.appendChild(emptyCell);
    emptyCell = document.createElement("td");
    emptyCell.innerText = "";
    totalRow.appendChild(emptyCell);
    let totalCell = document.createElement("td");
    totalCell.className = "total";
    totalCell.innerText = total.toString();
    totalRow.appendChild(totalCell);
    let labelCell = document.createElement("td");
    labelCell.className = "total-label";
    labelCell.innerText = "Total";
    totalRow.appendChild(labelCell);
    // @ts-ignore
    txinTable.appendChild(totalRow);

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

      // Address cell
      const cellAddress = newRow.insertCell();
      cellAddress.className = 'txout-txid long';
      cellAddress.textContent = output.address;

      // Lovelaces cell
      const cellLovelaces = newRow.insertCell();
      cellLovelaces.className = 'txout-txada';
      cellLovelaces.textContent = output.lovelace;

      // Remove the row of the clicked button
      async function removeRow(index) {
        transactionOutputs.splice(index, 1);
        await refresh();
      }

      // Remove button cell
      const cellRemove = newRow.insertCell();
      cellRemove.className = 'txout-remove';
      const btn = document.createElement('button');
      btn.textContent = 'Remove';
      let ix = transactionOutputIx;
      btn.onclick = removeRow.bind(null, ix);
      cellRemove.appendChild(btn);
      outputsTotal += output.lovelace;
      transactionOutputIx += 1;
    }
    let tx = await makeTransaction();
    let fees = await tx.estimateMinFee(protocolParams, 1, 0, 0);
    // @ts-ignore
    const feesRow = txOutTable.insertRow();

    // Empty cell
    const cellEmpty = feesRow.insertCell();
    cellEmpty.className = 'txout-txid long';
    cellEmpty.textContent = '';

    // Lovelaces cell
    const cellLovelaces = feesRow.insertCell();
    cellLovelaces.textContent = fees.toString();

    // Label cell
    const cellAddress = feesRow.insertCell();
    cellAddress.className = 'total-label';
    cellAddress.textContent = "Fees";

    let totalSpent = outputsTotal + fees;
    // @ts-ignore
    const totalOutRow = txOutTable.insertRow();
    // Empty cell
    const cellEmpty2 = totalOutRow.insertCell();
    cellEmpty2.className = 'txout-txid long';
    cellEmpty2.textContent = '';

    // Total spent cell
    const cellTotalSpent = totalOutRow.insertCell();
    cellTotalSpent.className = 'total';
    cellTotalSpent.textContent = totalSpent.toString();

    // Label cell for total spent
    const cellTotalLabel = totalOutRow.insertCell();
    cellTotalLabel.className = 'total-label';
    cellTotalLabel.textContent = "Total";
  }

  // Callbacks
  async function generateAddress() {
    wallet = await api.generateTestnetPaymentWallet(2);
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
    wallet = await api.restoreTestnetPaymentWalletFromSigningKeyBech32(2, pki.value);
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

  function addOutput(address, lovelace) {
    transactionOutputs.push({
      address: address,
      lovelace: lovelace
    });
  }

  document.getElementById('load-private-key-button')?.addEventListener('click', loadPrivateKey);

  document.getElementById('utxo-reload-button')?.addEventListener('click', refresh);

  document.getElementById('add-output-button')?.addEventListener('click', async () => {
    addOutput(
      // @ts-ignore
      document.getElementById('add-output-address').value.trim(),
      // @ts-ignore
      BigInt(document.getElementById('add-output-lovelace').value.trim()),
    );

    // @ts-ignore
    document.getElementById('add-output-address').value = '';
    // @ts-ignore
    document.getElementById('add-output-lovelace').value = '';

    await refresh();
  });

  document.getElementById('submit-button')?.addEventListener('click', async () => {
    let tx = await makeTransaction();
    let signingKey = await wallet.getBech32ForSigningKey();
    let signedTx = await tx.signWithPaymentKey(signingKey);

    await grpcApi.submitTx(await signedTx.txToCbor()).then((txId) => {
      alert("Transaction submitted successfully with ID: " + txId);
    }).catch((err) => {
      alert("Error submitting transaction: " + err);
    });

    transactionInputs = [];
    transactionOutputs = [];
    await refresh();
  });

  generateAddress();
}
do_async_work().then(() => { });
