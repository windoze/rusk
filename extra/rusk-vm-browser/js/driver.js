/**
 * Reference host-side driver loop for `rusk-vm-browser`.
 *
 * The VM is fully cooperative: it runs until it hits a boundary (`done`/`trap`/`request`/`yield`),
 * and the host decides how to schedule further execution.
 */

/**
 * @typedef {undefined | boolean | bigint | number | string | Uint8Array | ContinuationHandle} AbiValue
 *
 * @typedef {{ index: number, generation: number }} ContinuationHandle
 *
 * @typedef {{ tag: "done", value: AbiValue }
 *   | { tag: "trap", message: string }
 *   | { tag: "yield", remainingFuel: number | bigint }
 *   | { tag: "request", effectId: number, args: AbiValue[], k: ContinuationHandle }
 * } StepResult
 */

/**
 * @param {import("../pkg/rusk_vm_browser.js").Vm} vm
 * @param {{
 *   fuelPerTick?: number,
 *   effects?: Map<number, (...args: AbiValue[]) => Promise<AbiValue> | AbiValue>,
 *   onYield?: () => Promise<void>
 * }} [opts]
 */
export async function runVm(vm, opts = {}) {
  const fuelPerTick = opts.fuelPerTick ?? 50_000;
  const effects = opts.effects ?? new Map();
  const onYield =
    opts.onYield ??
    (() =>
      new Promise((resolve) => {
        if (typeof globalThis.requestAnimationFrame === "function") {
          globalThis.requestAnimationFrame(() => resolve());
          return;
        }
        setTimeout(resolve, 0);
      }));

  while (true) {
    /** @type {StepResult} */
    const r = vm.step(fuelPerTick);
    switch (r.tag) {
      case "done":
        return r.value;

      case "trap":
        throw new Error(r.message);

      case "yield":
        await onYield();
        break;

      case "request": {
        const handler = effects.get(r.effectId);
        if (!handler) {
          vm.dropContinuation(r.k.index, r.k.generation);
          throw new Error(`unhandled external effect id ${r.effectId}`);
        }

        try {
          const v = await handler(...r.args);
          vm.resume(r.k.index, r.k.generation, v);
        } catch (e) {
          vm.dropContinuation(r.k.index, r.k.generation);
          throw e;
        }
        break;
      }
    }
  }
}
