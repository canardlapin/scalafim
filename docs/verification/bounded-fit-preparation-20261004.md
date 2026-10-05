# Bounded, replayable first-level preparation

Mote: `bd-01KX6G9B1ZT10A3214DPA096RZ`.

## Qualified revision and scope

Code revision: `752c323ea3e17050779e51c874f6e03595cd2271`, based on
`06ecaac9a6158e1a04133cba05bb6dbacb9550b4`, in the task-owned worktree
`/private/tmp/scalafim-mote-queue-20261004` on
`work/mote-queue-20261004`. The existing landed descriptor, bounded OLS/GLS/LSS,
missing-pattern, DVARS, pooled-AR and saved-GLS slices are reused.

This change completes bounded response preparation for robust and learned
reduced-rank GLS engines. Immutable work descriptors remain data-only replayable
recipes, interpreted through explicit resolver/reader capabilities. Preparation
topology declares global reductions before execution; typed runtime contexts
carry learned state without captured preparation callbacks. Run time series,
structural coefficient axes, inference and covariance scopes, contrast geometry,
missing-data exclusions and provenance remain part of the execution contract.

Robust preparation computes exact whole-population row medians using eight
byte-radix replay passes, including the dense even-population averaging rule.
Every pass validates its complete population. IRLS weights and convergence are
synchronized across spatial blocks; scale reconstruction uses the correct
pre-final-update state. Estimated-AR passes use the existing raw-statistic
finalizer and original-coordinate residuals. Structural rank errors retain the
plan's coefficient identities.

Reduced-rank preparation assembles the same target-score matrix from bounded
response blocks and calls the existing Gale full SVD. This preserves tied, tiny,
rank-deficient and null-spectrum basis behavior. Conditional covariance and
bootstrap retain the existing scientific policies. Bootstrap uses the same
replicate sample indices across blocks, including partial terminal blocks;
nuisance coefficients are refitted against each reduced target fit.

Replay checks retained membership, geometry and a finite-response IEEE checksum
on each preparation and final-fitting pass. The checksum detects accidental
mutation; immutable source revisions and storage remain responsible for content
integrity. It is not an authentication mechanism.

## Verification

Author-run gates used macOS arm64, Temurin 17.0.20.1 and the installed Scala.js
Node runner. This is local JVM/JS evidence, not a new hosted Linux qualification.

| Gate | Result |
| --- | --- |
| Focused robust/reduced-rank/descriptor suites | 107 JVM and 107 JS passed |
| `fitJVM/test`, final code revision | 645 passed, zero failures/errors/skips |
| `fitJS/test`, final code revision | 590 passed, zero failures/errors/skips |
| `scalafimCompileAll`, final code revision | JVM and JS compilation passed |

The focused run preceded the final nuisance-score and explicit phase-count
tests; the complete fit gates include those tests. Coverage includes:

- Existing independent fmrireg coefficient, conditional variance and bootstrap
  covariance fixtures; dense/block coefficient, standard-error, residual-variance,
  covariance, T/F contrast and diagnostic parity with explicit tolerances.
- Exact base and bootstrap task-score identity at block widths 1, 7, 8 and 9,
  reversed selection order, uneven final blocks, nuisance projection and
  tied/null/tiny/rank-deficient spectra.
- Odd/even exact medians, signed zeros, subnormals, infinities and canonical NaNs;
  adversarial chunk medians and too-small/too-large replay populations.
- Robust Huber/Bisquare and global/run/voxel scales, pooled estimated AR,
  censors/gaps, complete exclusions, parallel final fitting and rank refusals.
- Configured spatial read caps, explicit global phase counts, decoded descriptor
  execution through a fresh resolver, and finite-response mutation refusals.
- Preserved typed refusals for disabled robust psi, non-OLS volume weighting and
  unsupported compressed/bootstrap voxelwise whitening.

Raw logs and actual process exit metadata are retained under
`/private/tmp/scalafim-mote-queue-evidence`:

| Artifact | Bytes | SHA-256 |
| --- | ---: | --- |
| `fit-preparation-reductions-focused-v1.log` | 19,044 | `074c93e495bb780362eb45d89e3cefd30d1c7deb3e1b55c1664c6dc5610b012e` |
| `fit-preparation-full-module-segments.txt` | 122,014 | `e756861ad01cffb4a4ff4749dfda8552adf5fd6c88e0957fd3ef77c0cca0696a` |
| `fit-preparation-full-gates-java17-v1.log` | 123,346 | `aa3482bf54aa6522ecb13f0a7cc57247959bd3b60de46501945a6d0943632de5` |

The full-gate sidecar records exit zero, 737.067452 seconds, and completion at
2026-10-05T01:09:19.197089Z. The complete log has no compiler warning or error.
It contains transient shared Ivy-lock waits and one sbt GC notice from the
task-owned three-GB server. No foreign lock or process was removed or stopped.
The temporary runner configuration reserved 512 MB of JIT code cache; project
configuration was unchanged.

Independent read-only review approved code revision `752c323e`, verified all
15 committed and working-file hashes against the candidate manifest, and
inspected the complete terminal JVM/JS logs. The reviewer did not independently
rerun the tests. Final acknowledgment verified the full-gate log hash and exit
metadata, cleared the compilation condition and approved ticket closure.
No numerical tolerance, scientific admission policy or solver
budget was weakened. Only this receipt is added after the tested code revision.

## Memory and persistence boundaries

Response reads are bounded by the configured spatial block. Retained scientific
state is not constant in population size. With T rows, P coefficients, B block
voxels, V retained voxels and learned rank r:

- Robust scratch is O(TB + PB + TP + P²B + 256T), including existing weighted
  least-squares covariance workspaces. No full T-by-V response is retained.
- Reduced-rank score/SVD work is O(Ptarget V), the spatial basis O(Vr), latent
  state O(Tr), and response scratch O(TB). Bootstrap retains replicate bases
  O(V sum(rreplicate)) and small factors; final covariance scratch is
  O(B Ptarget²). Savings diminish when Ptarget approaches T.
- Existing result assembly retains requested outputs; selected sinks retain
  their own narrower contracts. This change does not qualify arbitrary-volume
  constant-memory execution or performance.

Descriptors serialize the preparation recipe, not a finalized robust or
reduced-rank solver. Additional saved-artifact families, a concrete immutable
registry and distributed reductions remain separate work. Existing saved-GLS
artifact limits are unchanged. These boundaries do not prevent the ticket's
descriptor, bounded-read, dense-parity and shared-platform acceptance.

The candidate remains in the task-owned branch. Canonical local main is
`06ecaac9`; this ticket's code has not been landed there or pushed.
