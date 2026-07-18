// Boot the cardano-wasm engine, then start the Elm application.
// The Elm app is only started once `initialise()` has succeeded, so the page
// rendering at all is proof that the wasm engine loaded.
import initialise from "./cardano-api.js";

async function boot() {
  await initialise();
  window.Elm.Main.init({ node: document.getElementById("app") });
}

boot().catch((e) => {
  document.getElementById("app").innerHTML =
    '<div style="color:#e6edff;font-family:sans-serif;padding:30px">Failed to load cardano-wasm:<br><pre>' +
    String(e) + "</pre>Make sure the page is served over http(s), not opened as a file://</div>";
});
