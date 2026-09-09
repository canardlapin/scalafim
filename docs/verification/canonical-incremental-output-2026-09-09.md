# Canonical incremental NIfTI output qualification

The ScalaFIM image bridge works against the existing canonical image architecture. This is an isolated, uncommitted candidate with passing image, dataset and fit suites on JVM and Scala.js. It is not yet adopted by PLS Neuro or the active legacy ScalaFIM branch.

## Correction to the previous checkpoint

The earlier report tested the active legacy branch and an unsuccessful seven-file compatibility experiment. Its 84 compile errors and 35 JVM / 33 JS test failures are valid evidence for that experiment. The conclusion that a canonical migration still needed to be implemented was incorrect: `a3c232055b2514865ec286e35d99ff113c611e1c` already implemented it, and `main` at `10d14666edd06813cd909727f6b9a04b4ed41ba2` includes the subsequent provider unification. This candidate reuses that implementation. No legacy image aliases, global owner merging or affine-equality workaround were introduced.

## Exact candidate

| Repository | Base | Candidate location |
| --- | --- | --- |
| image4s | `18ffdce67fd05f5bcd28336650edcae891da6265` | `/private/tmp/image4s-output-canonical-1` |
| ScalaFIM | `10d14666edd06813cd909727f6b9a04b4ed41ba2` | `/private/tmp/scalafim-output-canonical-1` |
| reframe4s | `53a5c3c0fc3e72a2cbb7a1889ae72eee0cce32a2` | exact local archive `/private/tmp/reframe4s-output-canonical-1` |

The image4s candidate forward-ports the previously verified incremental writer and checked sample-space refinement in 12 source/test paths. The ScalaFIM candidate changes four paths:

- `SampleSpaces.scala` delegates dynamic D3 admission to image4s, preserving the exact space/frame/grid/axes and ScalaFIM's existing dimensionality error.
- `Nifti.scala` exposes `openScalarWriter` and `withScalarWriter` from a typed `SampleSpace[F, D3]`, delegating encoding and physical resource ownership to image4s.
- `ProviderDimensionRefinementSuite.scala` checks exact owners, physical declarations, FIR axes and D2/volume refusal.
- `NiftiIncrementalOutputSuite.scala` checks asymmetric spatial/FIR coordinates through independent bytes and ScalaFIM readback, sparse selected maps with a caller-supplied extension, existing-target preservation and scoped exception closure.

The bundle embeds both complete source patches and their resulting file contents/hashes. Both patches were applied to fresh files from their stated base commits and reconstructed every candidate path byte-for-byte. Native active-branch ScalaFIM sources, build pins, application sources and peer-owned patches were not modified. The previous image4s implementation remains in its original native checkout; this supplement records its forward-port to the canonical provider base.

## Qualification

| Suite | Passing tests |
| --- | ---: |
| image4s core JVM / Scala.js | 96 / 92 |
| image4s NIfTI JVM / Node | 55 / 37 |
| ScalaFIM image JVM / Scala.js | 305 / 284 |
| ScalaFIM dataset JVM / Scala.js | 76 / 62 |
| ScalaFIM fit JVM / Scala.js | 235 / 226 |

All six ScalaFIM suites were rerun on the final four-path candidate after incidental formatter changes elsewhere were restored to the exact base. `git diff --check` passes. These are the ordinary canonical modules, not the earlier reduced legacy harness. The fit suites cover the canonical baseline; they do not cover the recent selected-estimate and response-basis code absent from that baseline.

A fresh public-API heap probe wrote and independently checked all 40,000,000 values in a 160,000,352-byte `.nii` file with `-Xmx64m` (67,108,864 heap bytes). This run took 1.071 seconds in the JVM. A separate Python `struct` stream then checked every value and an analytic checksum of 19,919,995,270. Whole-file SHA-256: `9585f6bdbbc9ff3f32567c33cd2df9c9f573b2216eb76ac3d3d2198e35700aac`. This is one local bounded-memory measurement, not a comparative benchmark.

## Reproduction and limits

The bundle contains exact commands, complete final logs, failed-load diagnostics, source/config manifests, dependency revisions, the read-only Git worktree harness and both patches. Its source manifests include repository inputs beyond the modules exercised; they do not imply a full repository test run.

The initial worktree load hit sbt-git's JGit worktree limitation. The image4s worktree uses the console read-only Git backend through a temporary `worktree-git.sbt`; this does not change source dependency pins. A tag-description query still prints `fatal: No names found` during load, while the actual test commands exit successfully.

A fresh remote checkout could not resolve the exact reframe4s pin `53a5c3c`. The local archive was checked against every recorded source/config input from that same commit. This supports local qualification only; remote dependency availability must be resolved before pin admission. No pins or commits were published. Evidence is local macOS/JDK 25/Node 26, not the hosted JDK/platform matrix.

Incremental output remains exclusive `.nii` staging in RAS. Spatial block indices are `x + nx * (y + ny * z)`, not Ravel linear offsets. Non-spatial header sampling and time units follow explicit write options. Axis labels, scientific identity and nonzero FIR origins require a manifest/extension; closing the writer does not certify coverage, cancellation success or scientific completeness. The sparse test checks stored-code zero for unwritten samples, which decodes to the declared intercept.

## Next integration step

Reconcile recent selected-estimate and response-basis changes from the active ScalaFIM branch onto this canonical architecture, preserving their numerical evidence and current peer changes. A read-only patch check of the selected-fit slice from `702ecd9` found one textual conflict in `MatrixAdapters.scala`; this is a lead for a deliberate forward-port, not qualification of that port. Do not replace the current application pin wholesale: the application still consumes newer scientific features on the older image API.

After that reconciliation, implement the selected-output scientific binding and transactional sink/catalog, then the application's saved-estimate handoff. These remain open as ScalaFIM `bd-01M23Q9Q92SBD5PHZZJYVY9EC3`, adoption `bd-01M21H6P1908Q5PGSZYWQZHWRN`, catalog `bd-01KX6G9B8R86MRBZ9S8K8F5G7V`, and PLS Neuro W5 `bd-01M1YVQ52TAVZB2SN37DT96EAD`.

Evidence: [canonical-incremental-output-2026-09-09.json.gz](canonical-incremental-output-2026-09-09.json.gz), SHA-256 `5a6a21108e2dfe9a6b92d28ff884744f263620127b6edf3003cab6856d27166c`.
