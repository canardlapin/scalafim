# Eco five-mote tranche, 2026-10-21 UTC

The tranche covers `eco-u3-kernels`, `eco-u3-consumers`,
`eco-neuroatlas-volume`, `eco-neuroim-display` and
`eco-neuroim-integrations`. Published-pin and ScalaFIM functional workflow gates pass. The mesh/kernel/atlas-volume cutovers are qualified. I1 display and integration remain open while existing focused-layout capabilities and the actual PLS Neuro consumer are recovered; provider-only tests do not establish external compatibility.

## Source and publication

ScalaFIM production source remains `17aee3f9b096f8b9d8c3f024a221e9ef61fde51c`, with the descendant provider pins below; [pin gates](layout-descendant-pin-gates.json) bind the exact build-file hash and default-pin checks.
Reviewed and published provider revisions:

| Provider | Exact main revision |
|---|---|
| mesh4s | `35db08f864c86f211a14155fcaf6ef2fd811bcca` |
| neuroim4s | `21df6416e4e28e97cfbe39e20d7303349be7e4cf` |
| neuroatlas4s | `fe1e8f29942e7f0bc3b570e63b8ec4b4b5d8cd98` |

[Publication receipts](provider-publication.json) record non-force pushes and
exact live remote-main parity. Graph4s remains `613371f3bd0b12ce99352db0c637645eb62c6283`;
Gale remains the deliberate `779ed3d8169cdd9f4eb9ab10b1ca9defadc27715`.
These are source publications, not Maven artifact releases or hosted CI results.

## Verified changes

The optional volume scene, interaction and Canvas/Java2D/JavaFX implementations
moved to neuroim4s. All eight production files match the old implementation
apart from package names and direct Intaglio imports. ScalaFIM's 22 old tracked
view files/assets were removed; the alias-only Colorizer file was deleted.
Portable core, IO and transforms have no dependency edge to display projects.
The CI inventory includes all five optional view targets in bounded JVM/JS batches.

Mesh4s owns spherical angular arithmetic, exact-order topology edge lengths and
3-D face area. ScalaFIM retains mean-radius/endpoint policy, arbitrary chord
lengths, and domain normal-generation conventions. The checked area boundary
preserves refusal of nonfinite differences/cross components while retaining the
existing finite-cross squared-norm overflow result.

Two bounded numerical corrections have explicit analytic witnesses: thin
triangles retain positive area, and admitted small directions no longer collapse
to a zero angle. Generic minimum-subnormal directions are scaled before
normalization. [Arithmetic witnesses](kernel-adverse-arithmetic.json) are
independent IEEE-double replays, not executions of the old Scala implementation.

## Evidence

- [Mesh review](mesh-review.json) preserves the initial P2 and final source approval.
- [View review](view-review.json) records source fidelity and browser acceptance.
- [Executed gates](executed-gates.json) bind full raw-log hashes and actual command metadata.
- Source-override integration at the exact candidate passed surface 338 JVM/295 JS
  (two existing JS skips), surface-view 256 JVM/256 JS, and `scalafimCompileAll`.
- Provider geometry passed 43 JVM/39 JS; 19/19 law passes are earlier evidence,
  before the final angular normalization repair.
- The moved view corpus passed 42 portable cases on each platform, Canvas 8,
  Java2D 2 and JavaFX 2. The strengthened Canvas suite passed again after its
  browser assertion repair.
- The native Canvas probe drew three panels and checked 12 exact gray pixels.
  Its startup callback shutdown race was repaired; raw failed runs remain.
- [Browser receipt 02](volume-view-browser-02.json) passes all ten contracts with
  zero page errors and artifact hash `7305bca67292a57d9eabf11f821d317df61e4c6490e04b7e5cb62f1e9e98e775`.
  [The adverse receipt](volume-view-browser-adverse01.json) preserves the stale
  reference-equality failure. Both have checksum `5bec4f18`; the repair changes
  the assertion, not rendering. The [screenshot](volume-view-browser-02.png)
  was inspected. The runner closes its page/context/browser/server and the
  cleanup audit found no automated top-level browser processes.

The constant-volume native probe establishes basic drawing, not comprehensive
native visual qualification. Historical browser timings remain historical.
No performance improvement, hosted CI pass or complete eco-epic acceptance is claimed.

## Published-pin and functional acceptance

All six [final bounded batches](published-pin-and-functional-gates.json) exited
zero with no source overrides. They cover surface/spatial/atlas/MVPA support,
all optional view contracts, standalone atlas core/volume/IO, and dataset,
fit, group, archive, motion, threshold and first-level workflow suites on JVM
and JS. The atlas-to-MVPA executable workflow also passed. The public-pin
native JavaFX probe completed successfully with three panels and 12 gray pixels.
The default buildDependencies diagnostic lists the published neuroim view refs;
its object toString is not a complete dependency graph dump.

## Remaining I1 requirement

Independent consumer audit found stale `scalafim.image.view` callers and real
missing contracts in PLS Neuro: focused single-plane layouts, logical-pixel
insets, visible panel/readout enumeration, and typed volume/cursor adoption.
The prior source review approves the extraction delta only; it does not waive
known-consumer acceptance. The narrow provider recovery is the three-file delta
from ScalaFIM `b77c946ce2d4ee2962cf843610462fbdb66bb336`, preserving the default
layout and all frame-owner checks. The recovered provider delta is now qualified and published at `21df6416e4e28e97cfbe39e20d7303349be7e4cf`; [layout/IO qualification](layout-io-qualification.json) records 106 portable/backend test executions, 31 IO tests, and native sampled drawing of all five arrangements (36 gray pixels). Standalone unresolved inspection was faithfully recovered from `56d0cbcbfa55c7edb8651d19153b9e8c7897d88d`; each read owns a fresh unresolved world and strict evidence-required reads are unchanged. ScalaFIM/atlas descendant pins and actual PLS acceptance remain pending.

The existing consumer recovery source `656bd6095b94d34a6677eebb96b92b54829fd69b`
is retained in the external PLS Git store. It already preserves reversible
legacy/canonical ordinal conversion and is the donor for an isolated migration;
old native successes coexist with cold-open/layout failures and are not new
acceptance. `bd-01M52AQ9JQS51XFMB17X4BDQJX` tracks the existing recovery.
Current canonical PLS and ScalaFIM dirty worktrees remain untouched.


## Imaging closeout progress

The descendant pins pass 318 affected test executions on JVM/JS (atlas,
first-level workflow, portable view and applicable hosts), without source
overrides. Provider volume atlas suites pass 24 JVM/24 JS and IO 26. The first
atlas command passed both portable suites before an invalid IO target; the
corrected `volumeIoJVM/test` command passed separately. Raw adverse commands
remain in the common Git verification directory.

Actual PLS source checkpoint `c6ae1db3729a72e8a134c30090cb803f69e1bab6`
compiled 22 complete production files, including `ResultVolumeView` and
`SliceFit`, under strict warnings. Its synthetic native run passed 12 physical
layout plans, four focus modes and 24 map pixels, cursor ownership, saved focus
and foreign-world anatomy refusal. These are development component results.
A reviewer identified failure-path cleanup gaps in the new probe. The repair
and two fault-injection modes are reviewed at
`c9cd664cc3ea0f597e763d0edec04a5b0e3d35f6`; fresh committed qualification with
private provider outputs is in progress. No full PLS qualification is claimed.

Remaining I1 boundaries are the real application's provider/build/preparation
wiring, standalone `VolumePane` loading, `ResultGeometry`/ordered scatter and
legacy ordinal conversion, and `SpatialResponsePane` linked cursor interaction.
Historical application/native/cold-open/performance issues remain separately
open. Neither I1 task is closed by the 22-file component alone.
