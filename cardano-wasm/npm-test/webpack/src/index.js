function log(out) {
    var output = "";
    if (typeof (out) == "object") {
        output += "> [object] {\n";
        for (let [key, val] of Object.entries(out)) {
            let text = val.toString();
            if (typeof (val) == "function") {
                text = text.split("{")[0];
            }
            output += "      " + key + ": " + text + "\n";
        }
        output += "  }\n";
    } else if (typeof (out) == "bigint") {
        output += "> " + out.toString() + "n\n";
    } else {
        output += "> " + JSON.stringify(out) + "\n";
    }
    console.log(output);
}

(async () => {
    const cardanoModule = await import('cardano-wasm');
    const api = await cardanoModule.default();

    log("Cardano API initialised:");
    log(cardanoModule);


    log("Cardano API instance created:");
    log(api);

    const tx = await api.tx.newTx();
    log("New Transaction Body Created:", tx);
})();
