import { readFileSync } from "node:fs";
import { MeshWallet, MeshTxBuilder } from "@meshsdk/core";
import { CardanoRpcProvider } from "./cardano-rpc-provider.mjs";

const RPC_URL = "localhost:50051";
const CLUSTER_DIR = "/tmp/demo-cluster";
const LOVELACE_TO_SEND = "5000000"; // 5 ADA (1 ADA = 1,000,000 lovelace)

function readCborHex(path) {
  const envelope = JSON.parse(readFileSync(path, "utf8"));
  return envelope.cborHex;
}

async function main() {
  const provider = new CardanoRpcProvider(RPC_URL);

  const paymentSkeyHex = readCborHex(`${CLUSTER_DIR}/utxo-keys/utxo1/utxo.skey`);
  const recipientAddress = readFileSync(`${CLUSTER_DIR}/utxo-keys/utxo2/utxo.addr`, "utf8").trim();

  const wallet = new MeshWallet({
    networkId: 0, // 0 = testnet
    fetcher: provider,
    submitter: provider,
    key: { type: "cli", payment: paymentSkeyHex },
  });
  await wallet.init();

  // cardano-testnet's genesis UTxO keys fund an ENTERPRISE address (payment
  // credential only). MeshWallet also derives a base address by pairing the
  // payment key with a placeholder stake key, and its address-lookup methods
  // default to that (unfunded) base address. Pass "enterprise" explicitly.
  const changeAddress = await wallet.getChangeAddress("enterprise");

  // Fetch UTxOs from the provider rather than wallet.getUnspentOutputs():
  // the wallet converts them to CSL-style objects that
  // MeshTxBuilder.selectUtxosFrom() does not accept.
  const utxos = await provider.fetchAddressUTxOs(changeAddress);

  console.log(`Sender address:    ${changeAddress}`);
  console.log(`Recipient address: ${recipientAddress}`);
  console.log(`Spendable UTxOs:   ${utxos.length}`);

  const protocolParams = await provider.fetchProtocolParameters();
  const txBuilder = new MeshTxBuilder({ params: protocolParams });

  const unsignedTx = await txBuilder
    .txOut(recipientAddress, [{ unit: "lovelace", quantity: LOVELACE_TO_SEND }])
    .changeAddress(changeAddress)
    .selectUtxosFrom(utxos)
    .complete();

  const signedTx = await wallet.signTx(unsignedTx);
  const txHash = await wallet.submitTx(signedTx);
  console.log(`Submitted tx: ${txHash}`);

  // Confirm by polling the recipient's UTxOs for the new output.
  const deadlineMs = Date.now() + 60_000;
  while (Date.now() < deadlineMs) {
    const recipientUtxos = await provider.fetchAddressUTxOs(recipientAddress);
    const newOutput = recipientUtxos.find((u) => u.input.txHash === txHash);
    if (newOutput) {
      const lovelace = newOutput.output.amount.find((a) => a.unit === "lovelace")?.quantity;
      console.log(
        `Confirmed: ${lovelace} lovelace landed at ${recipientAddress} (${txHash}#${newOutput.input.outputIndex})`,
      );
      return;
    }
    await new Promise((resolve) => setTimeout(resolve, 3000));
  }
  throw new Error("Timed out waiting for the new UTxO to appear at the recipient address");
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
