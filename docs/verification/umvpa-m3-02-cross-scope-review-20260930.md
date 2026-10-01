# UMVPA M3.02 cross-scope source review

Date: 2026-09-30

This is a read-only source review. No compiler or test result is asserted.

| Scope | Source SHA-256 | Suite SHA-256 | Finding |
| --- | --- | --- | --- |
| Alder Swift validation lifecycle | `38d1577050dae6f9fbe7afc8d5280bb3119f530710d3e05dfa3af35f1b3a5a36` | `34f84ce87bb567b598e6bf135344e55b91dbed2b76e7a8bf0aca5da094d24ca9` | Evaluation binds `ValidationDesign[..., Coverage.ExactOnce]`, verifies the native mapping/root population, uses real exhaustive holdouts, and retains the validation receipt plus materialization/read receipts. The suite checks that a same-seed preparation receipt differs. |
| Classification access seam | `023199aadcceae183a42e9bb399ede069ec9805e474e98adb438c33374d298da` | Swift suite above | The probability reordering helper is scoped `private[mvpa]` for the dataset predictive adapter; no public classifier claim is added. |
| Alder predictive admission | `40e5b50fe72597f7378366bdd5f218c67ec9c6ea64754a7859af2ee63ff9a12b` | Swift suite above | Native measurement authorizes its budget before the read callback, validates source/axis identity, and preserves explicit replay/read-policy checks. |
| Measurement leg | `3d0d1998869712738c75d036066c3fab5ff1b7f44e92ec828bd5b08ec49a1bab` | `f8028ebc9c3ffd0d273b6834ac28aa4e313ce17e6f97d95cc8a2650f5141a3f8` | Source-axis checks, finite maps, and selection/basis-map orientation are explicit. |
| Ordered pairing | `4fdca33864f64d23a180154a50186aedad5e52976ba21e89c372d42be1c99d0c` | `f28f9fe85681e4ceb13db06128881b0af1f3fd00cdeb73716e18dbadd29bbfd4` | Ordered reduction, endpoint-bound conditional evidence, support overlap checks, and finite-weight protection are explicit. |

The pairing claim remains a declared conditional scientific claim, not independent inference qualification. The Swift output is predictive and does not qualify a pattern/inference result. Build and JVM/JS test gates remain pending.
