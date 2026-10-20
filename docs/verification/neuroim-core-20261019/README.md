# Complete neuroim4s core extraction

The prepared candidate removes ScalaFIM's local `image` module. Its neuroimaging values,
selected-image storage, masks, sampling/orientation/reslicing policy, statistics,
searchlights and retained parity tests move to independent `neuroim4s-core`.
Optional JVM anatomical IO moves to `neuroim4s-io`, around image4s codecs.
There is no ScalaFIM, surface, atlas, MVPA or rendering dependency upstream.

Generic sample spaces, axes and grids remain image4s-owned; frames remain
spatial4s-owned; domains and selections remain locus4s-owned; arrays remain
Ravel-owned; map/resampling capabilities remain reframe4s-owned. Core values
retain the exact admitted provider object and its storage. No compatibility
module, package aliases or header wrapper remain. Two dead facade files,
`PrimitiveBuffers.scala` and `GridDomainOps.scala`, are removed rather than moved.
The migration inventory accounts for 104 original source/test paths, with only
those two intentional deletions; all five IO resources are byte-identical.

## Provider candidates

These commits are local and unpublished:

| Repository | Candidate | Published parent observed before review |
| --- | --- | --- |
| image4s | `7885f67798242412800a448eda68acfbfff4f751` | `ba7ca3dae7181b4b3267b2d3220f0c50f06b93df` |
| reframe4s | `e4df73969e1fccb7aa0153a9203927c6dd8b0e3d` | `a9134f2cd92e884fa3c9ceafcbcfe52cf60f8295` |
| neuroim4s | `0ca4d07fc021d21a7802929069396d4df5bf4cc3` | `9780bcab6c061284a3c8864d9599b0534c71a97f` |

image4s makes its existing header affine selector public. Its algorithm body is
unchanged; four new public API tests join the existing NIfTI suite (67 JVM and
46 JS tests passed). reframe4s only aligns that image4s source pin; its resampling
suites passed 13 JVM and 11 JS tests. neuroim4s clean standalone qualification
passed 753 tests under `-Werror`: 26 world-space tests per platform, 336 JVM and
335 JS core tests, and 30 JVM IO tests, plus full compilation. Source publication
and artifact release are separate; neither is claimed for these candidates.

## Correctness boundaries

- D3/rank and semantic-role admission, exactly one Time axis for series, exact
  provider owners, ordered selections, strided views and canonical x/y/z/time
  order retain their existing contracts. The independent public consumer tests
  both constructor/selection/dense-output composition and canonical reads on a
  flipped view, including bounds and a series sequence.
- Public canonical ordinal reads keep their original checked bodies. Provider
  property forwarding helpers remain private. Callers use provider APIs directly.
- Persisted world/grid identity strings and parity fixtures retain their original
  spellings. No numerical tolerances or allocation budgets are widened.
- Nearest-voxel and atlas admission preserve spatial-terms-first summation rather
  than changing to the provider's translation-first evaluation. The adversarial
  cancellation fixture distinguishes x=0 from x=1 at a half-voxel tie and passed
  with the complete sampling suite on JVM and JS (24 tests per platform).
- NIfTI evidence and header admission delegate affine selection to image4s.
  Evidence-only calls now reject invalid agreement tolerances with the existing
  typed provider error, matching full decode. This is an intentional stricter
  invalid-input boundary; valid selection/scaling/world semantics are retained.
- Dataset, estimate and spatial readers use actual provider headers, datatypes
  and byte-order ADTs. Their bounded read plans and scalar decoding are retained.

The clean standalone allocation gates passed unchanged. The prepared exact
searchlight allocated 2,050,632 bytes against its 2,244,608-byte limit. Native
NIfTI scaled reads retained one Ravel destination; streaming writes retained
bounded buffers for both plain and gzip formats. These are computational and
allocation checks, not new scientific or performance qualification claims.

## Consumer qualification

The bounded headless CI recipe is locally qualified against explicit candidate
checkouts: **10,236 unique cases passed**, including 753 neuroim4s and 9,483
ScalaFIM cases. Thirteen existing optional or platform-specific cases/suites were
skipped; their exact names and projects are in [receipt.json](receipt.json).
There are no unresolved failures in the latest test reports. All modules and
examples compile on JVM and JS with no Scala source warnings. Repaired gates
were rerun at their affected scope; original failed logs are retained separately.
The earlier successful cases were not counted again merely because a repair
needed a focused rerun.

The consumer source candidate is `7ec3d6f26c39e904b41f71384d99cb328acc8fe5` on
`eco/neuroim-core-ready-20261019`. Its 2,509 qualified source/build/resource inputs match
the frozen manifest byte for byte. A verified Git bundle and binary cutover
patch are retained in the raw evidence directory. The active eco worktree was
returned to its working published-pin baseline `a01ed0a21a17461bce493fa93d54ba18b593c464`;
no unpublished GitHub pin is left active. ScalaFIM remains local and unpushed.

Provider publication still needs approval. After the three exact provider
fast-forwards, apply the qualified consumer commit and run a fresh default-pin
gate without local overrides before closing `eco-neuroim-core`. No hosted CI,
artifact release, scientific-study acceptance or new performance qualification
is claimed here.

## Raw evidence and recovery

Raw logs, exit-status sidecars, source SHA-256 manifests, migration inventory and
candidate recovery material live in the shared Git directory at
`verification/neuroim-core-20261019/`. The consumer runner derives its bounded
batches from `tools/ci/full-repository-tests.sh`, starting a fresh sbt process for
each batch; it never runs all Scala.js suites in one JVM. JavaFX display tests
are outside that existing headless recipe; their sources compile.

Retained adverse evidence includes the initial compile-driven migration repairs,
a 1 GiB standalone JS-link heap saturation (resumed with 4 GiB), obsolete local
header class products found by independent qualification, and a missing cached
root build-helper class. Owned generated build products were cleaned/regenerated;
no check was weakened. The clean standalone pass is `producer-final-clean-03`.
The comparative JVM court also caught an over-broad package substitution in one
configuration-key test fixture. `StampBuilderSuite` was restored byte-for-byte to
baseline; its focused rerun passed all seven tests. The original full run retains
543 passing cases, four optional skips (three GLMsingle bridge cases and one heavy rho-bias control) and the failed case; the
focused rerun qualifies that case after repair. A literal audit distinguished
configuration keys from compile snippets and diagnostic package names.

The motion JVM benchmark tests caught a separate invalid constructor migration:
a Time axis had been supplied to a 3D-only constructor. The client now uses
image4s `appendNonSpatial`, exactly preserving the original append operation and
its live grid owner. Admission checks were retained; all 105 motion JVM tests
passed after repair, followed by the remaining surface/view/raster JVM gates.

The full-suite first-level fixtures retain their non-gated scientific reports,
including low-SNR Gaussian admission and LWU admission below its 95% target.
These observations remain in the raw logs; this source extraction does not
resolve or close those scientific acceptance gaps.
