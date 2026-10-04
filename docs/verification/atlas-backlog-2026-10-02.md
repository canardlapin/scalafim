# Atlas backlog verification — 2026-10-02

This record covers the seven requested Atlas/provenance tickets. Changes remain
local and uncommitted in the shared checkout. Existing unrelated changes have
been preserved. Mote remains authoritative for ticket acceptance and ownership.

## Implemented behavior

- Composition, selection, hemisphere/network grouping and image dilation use
  exact parcel assignments. Grouping chooses parcel or spatial-support weights
  and handles incomplete annotations explicitly. Dilation chooses grid/world
  distance, radius, destination mask and tie policy; its provenance retains the
  parent assignment and mask digest. Independent all-pairs tests cover shear,
  ties and extreme finite grid scales.
- Pinned volume loaders cover Harvard–Oxford cortical/subcortical, Jülich,
  HCPex, Olsen MTL/hippocampus and anatomical visual-cortex selection. Executed
  source artifact identities agree with loaded/parsed-label derivations.
- Native subcortical descriptors preserve upstream CIT168, HCP thalamic,
  MDTB10 and HCP hippocampus/amygdala grids. User direction: “Use original
  upstream atlases in their native spaces, with explicit provenance.” CIT168
  remains unsplit. Unknown/scanner/aligned header coordinates retain artifact
  identities and uncertain confidence; no implicit standard-template route is
  claimed.
- Schaefer 100/7 fsaverage6 annotation and white-surface requests pin CBIG
  bytes. Glasser uses Kathryn Mills’ published Figshare fsaverage derivative,
  including canonical anatomical keys and its declared unknown background.
  It does not claim native fsLR geometry. Shared surface field workflows check
  exact bilateral assignment owners.
- The outer `atlas-workflows` module provides named batch extraction, timed
  canonical connectivity inputs and affine nearest-label transport through the
  existing provider. Plans and completed receipts are distinct. A reproduced
  affine pullback direction error was corrected; translation and noncommuting
  translate/scale tests check independently expected coordinates.
- Kernel/condition provenance encodes floating-point values using IEEE bits
  with framed fields and explicit schema versions, avoiding JVM/JS decimal
  rendering drift.

## Source boundary

MDTB10’s original file is NIfTI datatype 256 (signed INT8), unsupported by the
currently pinned image4s codec. The inspected historical official version is
byte-identical. Its original header and voxel bytes have been retained. The
upstream codec patch passes image4s NIfTI JVM (56) and JS (37) tests, including
signed-byte boundaries, affine scaling and preservation of UInt8 255. It is a
local candidate against image4s revision
`26a74ad99b9ee49a9555344e19b82d69a2ba50e4`; it has not been committed or published.
Descriptor availability is not evidence of successful native loading.

The other actor's `build.sbt` reservation cleared, and the parent acquired
reservation `rv-01M3YB760WBJ9KA3CJ35CNPBA1` before editing. Canonical outer module
definition, root aggregate and compile/test aliases are now applied, as recorded
in [atlas-workflows-build-integration.patch](atlas-workflows-build-integration.patch).
The temporary `atlas-workflows.sbt` was removed. The canonical image4s pin has
not been changed.

## Verification

The full actual-source JVM Atlas run completed: 214 tests, 212 passed, two
failed. The failures were MDTB10’s unsupported codec and Glasser’s unknown
label; the latter was repaired and its source suite subsequently passed all
four tests, including the real CBIG annotation/white-surface pair and published
Mills annotations. The retained run also passed the
existing native TemplateFlow inverse test (493 seconds), rather than silently
skipping installed assets.

The final focused repair run passed all 155 Atlas JS tests and all three outer
workflow tests on each platform. The image dilation suite passed three tests
on each platform. Provenance gates passed HRF 262 and design 255 on each
platform, plus the focused condition-profile provenance test on each.

The isolated native-source check against the upstream codec candidate passed
all four families: CIT168, HCP thalamic, MDTB10 and HCP ROI. It explicitly enabled
`scalafim.subcortical.fixtureRoot` and returned exit 0. The same run passed outer
workflow JVM/JS tests (three each). Only `scalafim.image4s.build` was overridden,
so the run also checks the mixed transitive closure that a ScalaFIM image4s pin
update would create. An earlier candidate attempt skipped all four native tests
because its fixture property was omitted; it supplies no native-loading evidence.

The canonical `scalafimCompileAll` gate and subsequent outer module JVM/JS
tests returned exit 0. Its complete log contains no compiler warnings or errors.
The first compile-all attempt was stopped after sustained heap pressure; the
successful run explicitly used a 6 GB heap and the same dependency cache.

Upstream publication and the main pin update await explicit user authorization;
the main loading ticket remains open.
Sealed logs, source inventory and hashes are retained in
[the evidence directory](atlas-backlog-2026-10-02/).

## Ticket disposition

| Ticket | Disposition |
| --- | --- |
| `bd-01M3RY81BSEPB2F8Y43GK8XZF8` | Closed: composition, selection, grouping and dilation; all three children closed. |
| `bd-01M3RY82QRX5J6G04V7S4Y4TVF` | Open: other loading children closed; native subcortical child awaits upstream publication and immutable pin update. |
| `bd-01M3RY846W0RP9G6S5B2RCTQQV` | Closed: surface acquisition and field workflows. |
| `bd-01M3RY85HRGZA151F2Y73S6PPZ` | Closed: canonical outer module, connectivity/batch/transport workflows and direction regression repair. |
| `bd-01M3VT7SD4EZX9MG356225G0C7` | Closed: typed hemisphere and partial-network grouping. |
| `bd-01M3VT7XJPZ9HNG6FYQ86YQQ2Y` | Closed: assignment-based dilation adapter. |
| `bd-01M3W1XYAHHM2RM8SB6HQCAQSY` | Closed: cross-platform canonical numeric provenance. |

The candidate native-source log records one pre-existing sbt build-definition
warning in the pinned linops4s project; it contains no compiler warning in the
changed ScalaFIM or image4s code. The independently retained image4s candidate
test log passes 56 JVM and 37 JS tests. Original atlas sources were never
converted or resampled to work around their file encodings.
