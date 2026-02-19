# `rusk-vm-browser`

Browser/JavaScript bindings for the **Rusk bytecode VM**, built as a `wasm32-unknown-unknown`
WebAssembly module.

This lives under `extra/` on purpose:

- it is **WASM-targeted** and should stay **standalone** (not a workspace member),
- it provides packaging + distribution glue that doesn’t belong in the core Rust crates.

## API (JS-facing)

The exported API is intentionally small and step-driven:

- `new Vm(rbcBytes)` creates a VM from `.rbc` bytes
- `vm.step(fuel?) -> StepResult` runs until a boundary
- `vm.registerHostImport(id, fn)` installs a synchronous host import implementation
- `vm.resume(kIndex, kGeneration, value)` resumes a suspended external effect
- `vm.dropContinuation(kIndex, kGeneration)` cancels a suspended external effect (traps)
- `vm.listHostImports()` and `vm.listExternalEffects()` help wire up IDs by name

`AbiValue` mapping at the JS boundary:

- `unit` → `undefined`
- `bool` → `boolean`
- `int(i64)` → `bigint`
- `float(f64)` → `number`
- `string` → `string`
- `bytes` → `Uint8Array`

## Build

Prereqs:

- `rustup target add wasm32-unknown-unknown`
- `cargo install wasm-pack`

Build:

```sh
cd extra/rusk-vm-browser
wasm-pack build --target web --release --out-dir pkg
```

## Test

These are `wasm-bindgen-test` tests intended to run under Node:

```sh
cd extra/rusk-vm-browser
wasm-pack test --node
```

## Usage (example)

Pseudo-code driver loop (matches the step/request/resume contract):

```js
import init, { Vm } from "./pkg/rusk_vm_browser.js";

await init();

const vm = new Vm(rbcBytes);

// Host imports (synchronous).
for (const imp of vm.listHostImports()) {
  if (imp.name === "test::add_int") {
    vm.registerHostImport(imp.id, ([a, b]) => a + b);
  }
}

while (true) {
  const r = vm.step(50_000);
  switch (r.tag) {
    case "done":
      console.log("done:", r.value);
      return;
    case "trap":
      throw new Error(r.message);
    case "yield":
      await new Promise(requestAnimationFrame);
      break;
    case "request": {
      // Resolve effectId → handler in JS, then resume/cancel.
      const v = await handleEffect(r.effectId, r.args);
      vm.resume(r.k.index, r.k.generation, v);
      break;
    }
  }
}
```

There is also a small reference implementation under `js/driver.js` which you can adapt for your
embedding.

## Notes

- This is currently a **bindings layer** on top of the existing `rusk-vm` implementation.
- The long-term direction (per the browser VM proposal) is to use **host-provided GC** (WASM GC / GC-managed refs)
  for VM heap objects. That work intentionally lives outside this initial standalone package.
