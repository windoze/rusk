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

function findExternalEffectId(vm, iface, method) {
  for (const eff of vm.listExternalEffects()) {
    if (eff.interface === iface && eff.method === method) return eff.id;
  }
  throw new Error(`missing external effect: ${iface}.${method}`);
}

function createDomEventWaiter() {
  const buttonToEvent = new Map([
    ["inc", "inc"],
    ["dec", "dec"],
    ["reset", "reset"],
  ]);

  /** @type {((ev: string) => void) | null} */
  let pendingResolve = null;

  for (const [buttonId, ev] of buttonToEvent.entries()) {
    const btn = document.getElementById(buttonId);
    btn.addEventListener("click", () => {
      if (!pendingResolve) return;
      const resolve = pendingResolve;
      pendingResolve = null;
      resolve(ev);
    });
  }

  return function waitEvent() {
    if (pendingResolve) {
      throw new Error("Dom.wait_event called while another wait is pending");
    }
    return new Promise((resolve) => {
      pendingResolve = resolve;
    });
  };
}

async function main() {
  const status = document.getElementById("status");
  status.textContent = "initializing wasm…";

  await init();

  status.textContent = "loading bytecode…";
  const rbc = await loadRbcBytes();
  const vm = new Vm(rbc);

  // Host import: `dom::set_text(id: string, text: string) -> unit`
  const setTextId = findHostImportId(vm, "dom::set_text");
  vm.registerHostImport(setTextId, (args) => {
    const [id, text] = args;
    const el = document.getElementById(String(id));
    if (!el) return;
    el.textContent = String(text);
  });

  // External effect: `Dom.wait_event() -> string`
  const waitEventId = findExternalEffectId(vm, "Dom", "wait_event");
  const waitEvent = createDomEventWaiter();
  const effects = new Map();
  effects.set(waitEventId, () => waitEvent());

  status.textContent = "running (click buttons)…";

  // This program is an infinite loop; `runVm` will keep awaiting `Dom.wait_event()`.
  await runVm(vm, { effects });
}

main().catch((e) => {
  const status = document.getElementById("status");
  status.textContent = `error: ${String(e?.message ?? e)}`;
});

