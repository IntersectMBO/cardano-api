// Implements the three MeshWallet/MeshTxBuilder hooks needed for building,
// signing, and submitting a plain payment transaction:
// fetchAddressUTxOs, fetchProtocolParameters, submitTx.

import * as grpc from "@grpc/grpc-js";
import * as protoLoader from "@grpc/proto-loader";
import { Address } from "@meshsdk/core-cst";
import { castProtocol } from "@meshsdk/core";

// Path to the UTxO RPC v1beta .proto sources vendored in this package;
// override with CARDANO_RPC_PROTO when running from elsewhere.
const PROTO_ROOT = process.env.CARDANO_RPC_PROTO ?? "../../proto";

function loadService(protoFile, serviceLookup) {
  const packageDefinition = protoLoader.loadSync(protoFile, {
    keepCase: false,
    longs: String,
    enums: String,
    defaults: true,
    oneofs: true,
    includeDirs: [PROTO_ROOT],
  });
  const proto = grpc.loadPackageDefinition(packageDefinition);
  return serviceLookup(proto);
}

function promisify(client, method) {
  return (request) =>
    new Promise((resolve, reject) => {
      client[method](request, (err, response) => {
        if (err) reject(err);
        else resolve(response);
      });
    });
}

// BigInt fields are a oneof of {int, bigUInt, bigNInt}; testnet-scale values
// always fit in `int`.
function bigIntField(field) {
  return field?.int ?? "0";
}

function rationalToNumber(rational) {
  if (!rational || Number(rational.denominator) === 0) return 0;
  return Number(rational.numerator) / Number(rational.denominator);
}

export class CardanoRpcProvider {
  constructor(target) {
    const credentials = grpc.credentials.createInsecure();

    const QueryService = loadService(
      "utxorpc/v1beta/query/query.proto",
      (proto) => proto.utxorpc.v1beta.query.QueryService,
    );
    const SubmitService = loadService(
      "utxorpc/v1beta/submit/submit.proto",
      (proto) => proto.utxorpc.v1beta.submit.SubmitService,
    );

    this._query = new QueryService(target, credentials);
    this._submit = new SubmitService(target, credentials);
  }

  async fetchAddressUTxOs(bech32Address) {
    const addressBytes = Buffer.from(Address.fromBech32(bech32Address).toBytes(), "hex");
    const searchUtxos = promisify(this._query, "SearchUtxos");
    const response = await searchUtxos({
      predicate: { match: { cardano: { address: { exactAddress: addressBytes } } } },
    });
    return (response.items ?? []).map((item) => {
      const cardano = item.cardano;
      const amount = [{ unit: "lovelace", quantity: bigIntField(cardano.coin) }];
      for (const bundle of cardano.assets ?? []) {
        // @grpc/grpc-js hands back `bytes` fields as Buffers already; do not
        // re-decode them as base64: that corrupts the data.
        const policyId = Buffer.from(bundle.policyId).toString("hex");
        for (const asset of bundle.assets ?? []) {
          const assetName = Buffer.from(asset.name).toString("hex");
          amount.push({ unit: policyId + assetName, quantity: bigIntField(asset.outputCoin) });
        }
      }
      return {
        input: {
          txHash: Buffer.from(item.txoRef.hash).toString("hex"),
          outputIndex: Number(item.txoRef.index),
        },
        output: {
          address: bech32Address,
          amount,
        },
      };
    });
  }

  async fetchProtocolParameters() {
    const readParams = promisify(this._query, "ReadParams");
    const response = await readParams({});
    const p = response.values.cardano;
    return castProtocol({
      coinsPerUtxoSize: Number(bigIntField(p.coinsPerUtxoByte)),
      collateralPercent: Number(p.collateralPercentage),
      decentralisation: 0,
      keyDeposit: Number(bigIntField(p.stakeKeyDeposit)),
      maxBlockExMem: Number(p.maxExecutionUnitsPerBlock?.memory),
      maxBlockExSteps: Number(p.maxExecutionUnitsPerBlock?.steps),
      maxBlockHeaderSize: Number(p.maxBlockHeaderSize),
      maxBlockSize: Number(p.maxBlockBodySize),
      maxCollateralInputs: Number(p.maxCollateralInputs),
      maxTxExMem: Number(p.maxExecutionUnitsPerTransaction?.memory),
      maxTxExSteps: Number(p.maxExecutionUnitsPerTransaction?.steps),
      maxTxSize: Number(p.maxTxSize),
      maxValSize: Number(p.maxValueSize),
      minFeeA: Number(bigIntField(p.minFeeCoefficient)),
      minFeeB: Number(bigIntField(p.minFeeConstant)),
      minPoolCost: bigIntField(p.minPoolCost),
      poolDeposit: bigIntField(p.poolDeposit),
      priceMem: rationalToNumber(p.prices?.memory),
      priceStep: rationalToNumber(p.prices?.steps),
      minFeeRefScriptCostPerByte: rationalToNumber(p.minFeeScriptRefCostPerByte),
    });
  }

  async submitTx(txHex) {
    const submitTx = promisify(this._submit, "SubmitTx");
    const response = await submitTx({ tx: { raw: Buffer.from(txHex, "hex") } });
    return Buffer.from(response.ref).toString("hex");
  }
}
