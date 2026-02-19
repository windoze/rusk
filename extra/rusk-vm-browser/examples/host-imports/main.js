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

function findHostImportId(vm, name) {
  for (const imp of vm.listHostImports()) {
    if (imp.name === name) return imp.id;
  }
  throw new Error(`missing host import: ${name}`);
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

      const logId = findHostImportId(vm, "js::log");
      vm.registerHostImport(logId, (args) => {
        console.log("[rusk/js::log]", String(args[0]));
      });

      const addId = findHostImportId(vm, "js::add_int");
      vm.registerHostImport(addId, (args) => {
        const a = BigInt(args[0]);
        const b = BigInt(args[1]);
        return a + b;
      });

      const v = await runVm(vm);
      out.textContent = `done: ${String(v)} (type=${typeof v})`;
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

