import { test, expect } from '@playwright/test';

test('test output matches', async ({ page }) => {
  // Navigate to the test page
  await page.goto('http://localhost:8080');
  // Wait for the page to load and the window title to be set (it should be "cardano-wasm test")
  await expect(page).toHaveTitle(/cardano-wasm test/);
  // Wait for the test to finish running (we signal this by creating a tag with id "finish-tag" and text "Finished test!")
  await expect(page.locator('#finish-tag')).toHaveText("Finished test!");
  // Check the output of the test (from the example folder), which is displayed in the code element with id "test-output". The output contains information about the various objects and results of trying some of the functions.
  await expect(page.locator('#test-output')).toHaveText('> \"Api object:\"> [object] {    objectType: cardano-api    newConwayTx: async function (...args)    newGrpcConnection: async function (...args)    generateMainnetPaymentAddress: async function (...args)    restoreMainnetPaymentAddressFromSigningKeyBech32: async function (...args)    generateTestnetPaymentAddress: async function (...args)    restoreTestnetPaymentAddressFromSigningKeyBech32: async function (...args)  }> \"UnsignedTx object:\"> [object] {    objectType: UnsignedTx    addTxInput: function(txId,txIx)    addSimpleTxOut: function(destAddr,lovelaceAmount)    setFee: function(lovelaceAmount)    estimateMinFee: function(protocolParams,numKeyWitnesses,numByronKeyWitnesses,totalRefScriptSize)    signWithPaymentKey: function(signingKey)  }> \"Estimated fee:\"> 164005> \"SignedTx object:\"> [object] {    objectType: SignedTx    alsoSignWithPaymentKey: function(signingKey)    txToCbor: function()  }> \"Tx CBOR:\"> \"84a300d9010281825820be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd97800018182581d6082935e44937e8b530f32ce672b5d600d0a286b4e8a52c6555f659b871a00989680021a000280a5a100d9010281825820adfc1c30385916da87db1ba3328f0690a57ebb2a6ac9f6f86b2d97f943adae005840a49259b5977aea523b46f01261fbff93e0899e8700319e11f5ab96b67eb628fca1a233ce2d50ee3227b591b84f27237d920d63974d65728362382f751c4d9400f5f6\"');
});
