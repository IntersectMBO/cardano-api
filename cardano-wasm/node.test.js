// Test snippet from NPM's package README.md

(async () => {
  try {
    // 1. Dynamically import the package.
    const cardanoModule = await import('cardano-wasm');

    // 2. The module's default export is an async function that initializes the WASM instance.
    //    Await this to get the usable API object.
    const api = await cardanoModule.default();
    console.log("Cardano API loaded successfully!");

    // 3. Now you can use the API to perform Cardano operations.
    //    For example, let's create a new transaction body.
    const tx = await api.tx.newTx();
    console.log("New Transaction Body Created:", tx);

    // You can now build upon the 'tx' object to add inputs, outputs, etc.

    console.log("Test Passed.");
    process.exit(0);

  } catch (error) {
    console.error("Failed to load or use the Cardano API:", error);

    console.log("Test Failed.");
    process.exit(1);
  }
})();
