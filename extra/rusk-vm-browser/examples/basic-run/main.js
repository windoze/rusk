import init, { Vm } from "../../pkg/rusk_vm_browser.js";
import { runVm } from "../../js/driver.js";

async function loadRbcBytes() {
  const url = new URL("./program.rbc", import.meta.url);
  const resp = await fetch(url);
  if (!resp.ok) {
    throw new Error(`failed to fetch ${url}: ${resp.status} ${resp.statusText}`);
  }
  return new Uint8Array(await resp.arrayBuffer());
}

async function main() {
  await init();

  const out = document.getElementById("out");
  const btn = document.getElementById("run");

  btn.addEventListener("click", async () => {
    btn.disabled = true;
    try {
      out.textContent = "running...";
      const rbc = await loadRbcBytes();
      const vm = new Vm(rbc);
      const v = await runVm(vm);
      out.textContent = `done: ${String(v)}`;
    } catch (e) {
      out.textContent = `error: ${String(e?.message ?? e)}`;
    } finally {
      btn.disabled = false;
    }
  });
}

main().catch((e) => {
  const out = document.getElementById("out");
  out.textContent = `init error: ${String(e?.message ?? e)}`;
});

