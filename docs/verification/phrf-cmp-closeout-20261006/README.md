# PHRF-CMP S1, S2 and S11 closeout

Date: 2026-10-06. Base: `08547ded484483d131e8e605eb8eda71159e9ed5`.
Worktree branch: `work/phrf-cmp-closeout-20261006`.
This receipt reconciles the original slice acceptance in
[`phrf-pilot-runner-design.md`](../../plans/phrf-pilot-runner-design.md#6-slices)
with the recovered implementation and fresh, source-bound execution.

## Changes

- S1 (`bd-01M3VSVVE466AR8BBX1SK8FC2J`): an explicitly configured
  `PHRF_GENERATOR_PYTHON` now fails fixture regeneration on launch/execution
  errors. The optional default interpreter is probed for NumPy/SciPy before
  regeneration; a subsequent generator error fails. Temporary outputs are
  cleaned on every outcome.
- S1: a committed adapter-output golden is parsed on JVM and JS. The unchanged
  frozen `from_generator.py` produces its 12 arrays from the committed trial
  HARNESS fixture through an injected deterministic wrapper double. The golden
  uses 3 scored voxels, 2 pool voxels and 144 trials. Python independently
  computes the SHA-256 of the archive and of the canonical decoded shape/value
  fingerprint. This is a serialization contract, not native GLMsingle admission.
- S2 (`bd-01M3VZTHVNNK5TNS5KHX76PM0E`): `RhoBiasHeavySuite` now honors the
  same explicit generator interpreter as S1. Dataset count, cell designs,
  correction policy, seeds and criterion are unchanged.
- S11 (`bd-01M3VSVXW0WM9N39CNPV9GD7QS`): the committed R fixtures and their
  original conventions and tolerances are retained. Their manifest verifies
  all 101 listed files.

No production numerical source, provider pin or module dependency changed.

## Acceptance mapping

| Slice | Obligation | Executable evidence |
| --- | --- | --- |
| S1 | Strict STORED ZIP/NPY parsing; tamper refusals; exact seed/denylist binding | `NpzSuite`, `BindingSuite`, `SeedsSuite` |
| S1 | Committed trial/condition generator files; regeneration byte equality | `RealGeneratorSuite`, using the pinned interpreter explicitly |
| S1 | Adapter-output bytes parsed identically on both platforms | `NpzSuite`: frozen adapter output; full decoded fingerprint from Python |
| S1 | Fitter inputs cannot access or forge truth | `FitInputsSuite`: field labels and compile-error checks |
| S1 | Verified input and per-array hashes exposed for persistence | `BindingSuite`: hashes recorded in `BoundDataset`; ledger ownership remains S7 as declared in the original S1 receipt |
| S2 | Per-kind AR design, corrected estimate, rank/clamp/status refusals, sigma2 and heterogeneity | `CommonPreparationSuite`, `RealGeneratorPrepSuite` |
| S2 | Bit-identical phi in fit config and whitening; one whitened source for native arms; nuisance span and determinism | `CommonPreparationSuite`, `RealGeneratorPrepSuite` |
| S2 | Absolute mean rho bias <= 0.02 over 50 datasets of each kind | Explicit opt-in `RhoBiasHeavySuite`, both cell kinds |
| S11 | CAN/INF3/FIR betas and E-resp <= 1e-8 relative | `S11ConditionParitySuite`, both cells and whitening conventions |
| S11 | LSA/LSS <= 1e-8; rLSS amplitudes/deviations <= 1e-10 | `S11TrialParitySuite`, both trial cells and whitening conventions; shared `RidgeLssSuite` |
| S11 | Kernel/design and whitening conventions; fixture integrity | `S11KernelDesignParitySuite`, `S11WhiteningParitySuite` |

## Execution

| Gate | Result |
| --- | --- |
| Selected S1/S2/S11 JVM suites | 137 passed, no failures/errors/skips; includes all 61 S11 fixture parity checks |
| S2 opt-in heavy criterion | Both kinds pass on all 50 datasets per kind (included in the 137 above) |
| `arJVM/test`, `arJS/test` | 155 JVM and 153 JS passed |
| Shared `RidgeLssSuite` | 10 JVM and 10 JS passed |
| Python generator suite | 45 passed |
| Full shared comparison JS suite | 243 passed, no failures/errors/skips; includes the adapter golden |
| Explicit configured-generator failure probe | Expected exit 1: exactly the generator test fails (9 pass, 1 fails), with no skip |
| Restored configured-generator run | All 10 fixture tests pass after resetting the temporary fork/environment settings |
| `scalafimCompileAll` | Exit 0 for every module on both platforms; no warnings or errors |

| Heavy gate | Mean rho | Bias from 0.3 | Criterion |
| --- | --- | --- | --- |
| `T-TX-fast` (50 trial datasets) | 0.29983 | -0.00017 | absolute bias <= 0.02: pass |
| `C-TX-.5` (50 condition datasets) | 0.30043 | +0.00043 | absolute bias <= 0.02: pass |

Runtime: macOS arm64, Temurin JDK 21.0.12.1, Node.js 24.21.0,
Scala 3.7.4 and sbt 1.11.7. The task-owned Python
environment uses Python 3.12 with NumPy 2.4.3, SciPy 1.17.1 and pytest 7.4.4 from
the generator lock. The historical lock's Python comment is 3.12.10; this host
uses 3.12.13. Regeneration byte equality is an explicit gate. The runtime freeze,
commands, compressed logs and [`source manifest`](source-manifest.json) retain
the exact tested boundary.

`jvm-final.log.gz` is the successful JVM gate. `jvm.log.gz` is the initial
bootstrap stopped after passing a lone `-D` option without a task; it supplies
no passing test evidence. The final invocation sets the heavy-test property
with an explicit sbt `eval` command. An initial root-level Python test invocation
failed collection because `phrf_gen` was absent from its import path; the final
45-test command explicitly supplies `PYTHONPATH=tools/phrf-comparison/generator`.
The deliberate nonzero generator test is retained separately and must not be
read as a passing run.

## Scientific and integration boundary

The S2 heavy rerun reuses historical verdict root `80dd06a7d4666a7b` and indices
0–49 for `T-TX-fast` and `C-TX-.5`, with 40 scored voxels per dataset. It is
current-source reproducibility evidence, not a fresh seed draw or new
independent calibration. Per-kind AR design and all refusal criteria remain
frozen; passing the gate does not qualify PHRF release, approximation or a pilot
campaign.

The R references retain exact-first AR(1) whitening with fixed phi 5/16 as the
primary convention and identity-first as secondary, a 0.1 s event grid,
declared kernel units and joint condition fitting. Fixtures are verified and
consumed, not regenerated as a new R experiment. Their source/session evidence
remains in the original S11 receipt and fixture metadata.

S6 native GLMsingle qualification, S7 scheduling/custody acceptance, S10
production truth-to-scorer and owner-reader wiring, rehearsal, launch and
scientific-release gates remain separate. This batch does not close those
motes or authorize a campaign. Numerical shared code is checked on JVM and JS;
file-backed fixture consumers and the Python-driven heavy gate are JVM-only.
