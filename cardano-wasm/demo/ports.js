// Boot the cardano-wasm engine, then start the Elm application.
// The Elm app is only started once `initialise()` has succeeded, so the page
// rendering at all is proof that the wasm engine loaded.
import initialise from "./cardano-api.js";

async function boot() {
  await initialise();
  window.Elm.Main.init({ node: document.getElementById("app") });
}

boot().catch((e) => {
  const app = document.getElementById("app");
  if (!app) return;

  const msg = document.createElement("div");
  msg.style.cssText = "color:#e6edff;font-family:sans-serif;padding:30px";
  msg.append("Failed to load cardano-wasm:");

  const pre = document.createElement("pre");
  pre.textContent = String(e);
  msg.appendChild(document.createElement("br"));
  msg.appendChild(pre);
  msg.append("Make sure the page is served over http(s), not opened as a file://");

  app.replaceChildren(msg);
});
