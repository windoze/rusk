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
  const runBtn = document.getElementById("run");
  const clickBtn = document.getElementById("click");

  /** @type {import("../../pkg/rusk_vm_browser.js").Vm | null} */
  let vm = null;

  /** @type {{ index: number, generation: number } | null} */
  let stored = null;

  runBtn.addEventListener("click", async () => {
    runBtn.disabled = true;
    clickBtn.disabled = true;
    out.textContent = "running…\n";

    try {
      // If the previous run left a pinned continuation un-resumed, release it so it doesn't keep
      // the captured VM state alive indefinitely.
      if (vm && stored) {
        vm.dropPinnedContinuation(stored.index, stored.generation);
        stored = null;
      }

      const rbc = await loadRbcBytes();
      vm = new Vm(rbc);

      // Host imports:
      // - `js::log(string) -> unit`
      // - `js::store_cont(cont(int) -> int) -> unit`
      const logId = findHostImportId(vm, "js::log");
      vm.registerHostImport(logId, (args) => {
        out.textContent += `[log] ${String(args[0])}\n`;
      });

      const storeId = findHostImportId(vm, "js::store_cont");
      vm.registerHostImport(storeId, (args) => {
        stored = args[0];
      });

      const v = await runVm(vm);
      out.textContent += `\n[done] ${String(v)}\n`;

      if (stored) {
        out.textContent += "\n[host] continuation stored; click to tail-resume it with 42…\n";
        clickBtn.disabled = false;
      }
    } catch (e) {
      out.textContent += `\n[error] ${String(e?.message ?? e)}\n`;
    } finally {
      runBtn.disabled = false;
    }
  });

  clickBtn.addEventListener("click", async () => {
    if (!vm || !stored) return;
    clickBtn.disabled = true;

    try {
      const k = stored;
      stored = null;

      out.textContent += "\n[host] tail-resuming pinned continuation…\n";
      vm.resumePinnedContinuationTail(k.index, k.generation, 42n);

      const v = await runVm(vm);
      out.textContent += `\n[done after resume] ${String(v)}\n`;
    } catch (e) {
      out.textContent += `\n[error] ${String(e?.message ?? e)}\n`;
    }
  });
}

main().catch((e) => {
  const out = document.getElementById("out");
  out.textContent = `init error: ${String(e?.message ?? e)}`;
});
