# PHRF-CMP S6: fixture output-hash drift investigation

Mote: `bd-01M3VSW06P4G26MA7DEC9BZ4WH`. Date: 2026-10-10. Branch
`work/phrf-cmp-reconcile`. Follow-up to the blocking finding in
`s6-glmsingle-bridge.md`. **The pinned hash was not changed and no tolerance was
loosened. S6 stays open.**

## Symptom

`GlmSingleBridgeSuite` pins the frozen converter's output for the committed
fixture `T-TX-fast__d0000.npz` (`3b5cb36b...cd0f`) at SHA-256
`575a74822329ea91f32acc00d2e5ca979d8424bd5bdf2c34a1ee532384aa72e8`. On this
host the output is always `398d45a249590ef9d41eca2e659eb2fcc8c3a5d9aca8d3eb749b9f826d98e27f`.
The source, lock freeze, upstream commit and fixture are identical to 10-05.

Reproduced today with a fresh runtime built from `requirements.lock` (uv
Python 3.12.10, the same interpreter version as the 10-05 environment, and
GLMsingle at `1ab54a65`). `testOnly GlmSingleBridgeSuite` with
`PHRF_GLMSINGLE_PYTHON` pointing at it gave **35 total, 33 passed, 2 failed**.
Both failures are the hash pin (`logs/s6-hash-drift-bridge-suite.log.gz`). The
bridge's npz hash equals the direct CLI hash (`398d45a2...`), so the bridge
returns the converter bytes unchanged on this host as well.

## Root cause: the pin was made on a different machine

The pin was computed, and last passed, on the **pre-migration workstation**.
Development moved to this machine on 2026-10-05
(`docs/verification/machine-migration-20261005/manifest.json`: "move
development to another machine").

| Evidence | Old host (pin made, 10-01 and 10-05 pass) | This host (10-10 fail) |
| --- | --- | --- |
| Per-user Darwin temp dir | `/private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T`. It appears in the 10-05 passing log (`consolidation-20261005/final-consumer-gates-v3.log.gz`, scratch paths and the leak-scan roots) and in the 10-01 GLMsingle `tools/phrf-comparison/glmsingle/install.log` | `/var/folders/f8/k46hqpm95mn7ns9f7wxw88640000gp/T`. The `9h/...` directory does not exist here |
| sbt-warm base used | `~/.cache/scalafim-sbt/bases/6a3bc7e1a11cca00` | absent. `~/.cache/scalafim-sbt` was created here 2026-10-05 19:09 EDT, about 4 h **after** the 10-05 run (19:03-19:17 UTC = 15:03 EDT) |
| Checkout | `/private/tmp/scalafim-consolidation-20261005/cascade` | `~/code/scala/scalafim` was created here 2026-10-05 17:52 EDT |
| Hardware / OS | the same `9h/...` temp dir is the workspace in `docs/plans/unified-mvpa-foundation-admission.md`. The repository's recorded workstation before the migration is Apple M3 Max, `Mac15,11`, macOS 14.3 `23D56` (e.g. `docs/plans/unified-mvpa-resource-comparative-protocol.md`). This tie is circumstantial: no S6 receipt recorded the model | Apple M2 Pro, `Mac14,12`, macOS 15.1.1 `24B91` (up 37 days, so no OS update since 10-05) |

The numpy 2.5.3 (`macosx_14_0_arm64`) and scipy 1.18.1 (`macosx_14_0_arm64`)
wheels in the lock link the **system Accelerate framework** for BLAS and LAPACK
(`numpy.show_config()`: `blas`/`lapack` `accelerate`, `detection method:
system`). Accelerate ships with macOS and picks its kernels for the CPU. It is
not in the lock and no environment variable pins it. Same lock, different
macOS version and chip, so different Accelerate code and different
low-order float results. The 10-01 to 10-05 pass and the 10-10 failure differ
only in host. The pin therefore covers bytes that depend on the host's
Accelerate build.

The "unchanged environment" reasoning in `s6-glmsingle-bridge.md` checked the
install history of *this* host. That history cannot show a change, because the
change was the host itself.

## Excluded causes

- **BLAS thread count.** On this host the output hash is `398d45a2...` with the
  bridge's six thread pins, with no thread variables at all (`env -i`), and with
  `VECLIB_MAXIMUM_THREADS=8`. The wrapper also calls GLMsingle with
  `threads=1`. Adding another thread pin to the scrub would therefore not
  restore the pin (`logs/s6-hash-drift-probe.txt`).
- **Python, packages, upstream, source, fixture.** These were already shown
  identical, and today's freeze matches. The 10-05 run used Python 3.12.10, and
  so did today's runtime.
- **Run-to-run nondeterminism.** Repeated runs are byte-identical on a fixed
  host and BLAS.

## Classification: float32 working-precision noise, no semantic change

The 10-01/10-05 output bytes are not retained: only their hash is. The old
host is not available, so the old arrays cannot be rebuilt. Instead, the effect
of a BLAS-implementation change alone was measured on this host. The same lock
was used except that numpy/scipy were swapped for their own OpenBLAS wheels
(`numpy-2.5.3-...-macosx_11_0_arm64`, `scipy-1.18.1-...-macosx_12_0_arm64`,
`scipy-openblas`).

| Variant | Output SHA-256 |
| --- | --- |
| Accelerate (lock), thread-pinned / unpinned / VECLIB 8 | `398d45a2...` |
| OpenBLAS, default core type (x2), `OPENBLAS_CORETYPE=NEOVERSEN1` | `c170f924...` |
| OpenBLAS, `OPENBLAS_CORETYPE=ARMV8` | `8d9ff972...` |

None reproduces `575a7482...`, as expected. Element-wise, Accelerate against
OpenBLAS:

- **Identical:** `d_HRFindex` (19, 14, 14), `d_FRACvalue` (0.65, 0.15, 0.15),
  `realized_pool_size` (3), `pcnum` (0), `noisepool`, `meanvol`,
  `hrf_library`, `trial_*`. So there is no change in HRF selection, fraction,
  noise pool, PC count or trial order.
- **Low-order differences:** `d_beta_data`/`d_beta_psc`, 418 of 432 elements
  differ. Per-voxel relative L2 is 3.3e-6, 9.5e-7 and 1.0e-6. Max abs is 1.4e-5
  (max abs over RMS beta is at most 7.9e-6), and 1 - r is at most 7.7e-12.
  `d_R2` has a relative difference of at most 9e-7.

These are float64 values carried through GLMsingle's float32 pipeline. The
differences are about 10-30 float32 eps, the same order as the v0b
float64-reference gap (L2 max 9.1e-6). The `precision_parity.py` rerun in
`v0b-glmsingle-gate.md` also agreed with the 10-01 old-host JSON only to about
1e-11 relative, with HRF indices exact. **Classification: a host or BLAS
implementation change, giving float32-level noise. No numerical or semantic
change was found.** The size of the old-host versus this-host difference is
inferred from these two measurements, not observed directly.

## Remedy

No fix is clearly correct within S6's scope, so none was applied.

- An environment pin cannot fix it: thread count has no effect, and Accelerate
  takes no other selector.
- Updating the expected hash is forbidden. It would also only move the
  host-dependence to this machine.

Proposals for owner approval (not applied):

1. **Host-profile-qualified pin.** Keep `575a7482...` bound to the recorded
   qualifying profile (BLAS vendor, macOS build, `hw.model`). Add a reviewed
   second pin for `Accelerate / macOS 15.1.1 24B91 / Mac14,12` = `398d45a2...`,
   with this report as its justification. The test reads the profile, asserts
   the matching pin, and fails closed on an unknown profile. It should report a
   typed "unqualified host" outcome, not pass. This keeps an exact-bytes
   regression pin on each qualified host. It needs one new reviewed constant
   per host.
2. **Make the bytes host-independent.** Lock OpenBLAS wheels (hash-pinned) and
   set `OPENBLAS_CORETYPE` in `GlmSingleEnv.ThreadPins` and in the generator and
   receipt runtimes. Both are required: the core type alone changes the bytes
   above. This changes the v0b/S6 runtime, so v0b and S6 must be requalified
   and the pin re-derived once. It does not recover `575a7482...`.
3. **Canonicalize the cross-host assertion.** Keep the host-independent
   property exact: bridge E-trial equals the converter output bit for bit, and
   repeat runs are byte-identical on one host. Replace the cross-host absolute
   hash with exact discrete outputs (HRF index, FRAC, pool, pcnum, trial
   arrays) plus betas within the frozen v0b float32 bound. This is a loosening
   and needs explicit owner approval.

Recommendation: option 1 now, since it is the smallest change and keeps exact
pins. Consider option 2 before campaign custody spans more than one host.

## Status

The cause is identified and evidenced. The two pin tests still fail with the
original hash on this host, so **S6 is not closed**. Every other S6 obligation
stays evidenced as recorded in `s6-glmsingle-bridge.md`. No source, lock, test
or pin was modified. The runtimes used here live in the session scratchpad and
are not committed.

Evidence: `logs/s6-hash-drift-probe.txt` (hashes and the element-wise
comparison) and `logs/s6-hash-drift-bridge-suite.log.gz` (suite run, scratch
paths redacted).
