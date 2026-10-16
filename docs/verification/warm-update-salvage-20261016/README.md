# September branch salvage assessment — 2026-10-16

The branch is older than current `main`, but it is **not wholly superseded**.
PLS Neuro's committed provider contract still pins its exact tip and its source
calls APIs absent from current main. The right destination is a set of reviewed
behavioral ports onto main, followed by a verified consumer repin. A whole-branch
merge would restore obsolete architecture alongside useful missing behavior.

Implementation is in progress through [PR #62](https://github.com/canardlapin/scalafim/pull/62). The surface/camera/scene source `f87fecf7a` passed affected JVM/Scala.js/native checks and full compilation. Consumer acceptance then exposed missing explicit event roles; `5d2a86db6598a9358a65189e674607e5d3939000` restores `categorical(...)` and `continuous(...)`, with seven passing tests on each platform and an independent exact-commit review. Those tests include missing-value and degenerate-modulator policies. Required hosted checks must run on the new tip. PLSNeuro fresh-checkout `sbt check` passed at `0eb0b828`, but its headless lane failed and remains unqualified. Catalog ownership, fixture spatial metadata, duplicate Gale roots, and a historical HRF fixture are being repaired. See [migration-progress.json](migration-progress.json) and [continuation.md](continuation.md); source comparisons below remain tied to the original pinned baseline.

## Examined revisions and scope

| Input | Exact revision |
|---|---|
| ScalaFIM main | `ac2ec8b706fce08596a943d79f71a140aad05948` |
| `surface/warm-update-20260921` | `7c3ff0a0edc92be0e2fe35c431d5b9e458c92ab4` |
| Common ancestor | `fd992a0c85001eb50ba2e78d29c497693ca494dc` |
| October retained-atlas snapshot | `cb0712b4a93636372228f9e0dc0c64270f882a36` |
| October snapshot parent | `9b29fcd303879c31d0e4cf0d2984c95730f19411` |
| PLS Neuro local committed consumer | `2e0aac8e90300fd5a87d5b0d79d3bb9b78ecd1bf` |

The September branch has 33 unique commits; main has 733 since their common
ancestor. The earlier merge assessment found 157 conflicted files. The 33 commits
touched 996 distinct paths, including files later deleted within the branch.
The 80 old `src/main` paths absent at exactly the same path on main are **not 80
missing features**: this includes moved classes, benchmarks, examples and retired
architecture. For example, `NiftiStagingCache` still exists on main inside another
file, but with weaker behavior.

All 33 commits have a [disposition](commit-dispositions.md); the full mechanical
inventory is [machine-readable](commit-inventory.json). Large consolidation
commits received feature-family analysis, not a new scientific audit of every
changed line. Source inspection, exact comparisons and synthetic cache execution
are current evidence. Native screenshots, cortical timings, numerical admission
and older test counts cited below remain historical evidence.

Concurrent modularization commits `1412c16c2` and `c2a6b3bf1` appeared in the
working checkout during analysis. Comparisons use the frozen main above rather
than a moving worktree. Production ports are being prepared in an isolated clone.

## Consumer compatibility is the primary recovery gate

PLS Neuro's `docs/provider-contract.json` explicitly pins `7c3ff0a0…` and describes
the transaction, off-thread preparation and atlas-query contracts it expects.
`ResultSurfaceView.scala` prepares `SurfaceLayerTransaction`, obtains
`JavaFxColorPreparationBasis`, prepares colours on a worker, then calls
`applyLayerTransaction` with the prepared result. Its runtime provenance probe
also expects that exact revision. These inspected consumer files were clean and
identical to their committed versions; hashes are in
[consumer-evidence.json](consumer-evidence.json). This is evidence of the local
committed consumer, not of a deployed or released application.

The dependencies extend beyond rendering. Production consumer code references
`FirstLevelCompanionReview[Jvm]`, `FirstLevelUnitDataset`, `SamplingReference`,
`StudySpatialSupport[Jvm]`, `FreeSurferMorphometryReader` and the staging cache.
`ResultVolumeView` uses single-plane logical-pixel layouts, and `AtlasCursorQuery`
calls `AtlasBoundaryQuery`. Event-support policy also appears throughout first-level
model editing. `ReachableResponseBasis` appears in an adoption probe. The
[symbol-use inventory](consumer-absent-symbol-uses.json) is a discovery aid:
generic names such as `Binding`, `Measurement` and `AnalysisResult` can be false
positives and are not proof of ScalaFIM API dependence.

The consumer's Gale, Image4s, Locus4s, Multivar, Intaglio and BIDS4s inputs also
differ from current ScalaFIM pins. Some are declared patched candidates. Updating
only the ScalaFIM SHA would neither produce a coherent composite build nor prove
behavioral compatibility. The final gate must compile and exercise the actual
consumer with one exact provider graph, remove patches already integrated
upstream, retain unresolved patch provenance, and update the provider contract
and runtime expectation together.

## Voxel ordering is a consumer migration boundary

The old branch uses first-axis-fastest voxel ordinals (`x + dx*y + dx*dy*z`).
Main uses canonical last-axis-fastest ordinals (`(x*dy + y)*dz + z`), introduced
by `50f3f14eb` on August 23. NIfTI storage remains first-axis-fastest; current
`NiftiResponseBlockSource` translates through `NiftiReadPlan.fromCanonicalVoxels`.
For a 2 by 3 by 4 image, ordinal 1 denotes `(1,0,0)` in the old API and `(0,0,1)`
in the current API. These are different physical voxels.

PLS Neuro `FirstLevelFeatures` currently emits `grid-v1` identities containing
space, dimensions and affine, without an ordinal-order discriminator. Its saved
result and participant-inspection readers reconstruct geometry from those fields.
Replacing old image class names while retaining these ordinals could silently
move values. Consumer adoption therefore needs an explicit index/format migration,
asymmetric multidimensional fixtures, and reopening of existing saved results.
No such migration has yet been admitted.

This predates the October 16 ecosystem-unification branch. Current unification
changes eventual module ownership; it did not cause the pinned-main failures.

## Recommended recovery sequence

| Priority | Recoverable behavior | Destination under the modularization plan | Gate before adoption |
|---|---|---|---|
| P0 | PLS-used first-level review, table/support and surface transaction contracts | ScalaFIM workflow/design; optional neurosurf4s viewer modules | Real consumer compilation and workflow/interaction regressions against current types |
| P0 | Scene persistence compatibility and variant provenance | neurosurf4s persistence/composition | Explicit old-format migration; fixed/morph and digest mismatch tests on JVM and JS |
| P1 | Verified, bounded NIfTI staging | ScalaFIM dataset IO, using upstream format primitives | Content replacement, cache corruption, concurrency, cancellation and resource-bound tests |
| P0 | Automatic orthogonal layout and single-plane focus | neuroim4s optional display | Physical aspect/area, tiny viewport, hidden-panel read and pick tests on JVM and JS |
| P1 | Stable-layout CPU colour preparation and warm transaction application | neurosurf4s JavaFX provider | Current-plan binding, stale-owner/geometry refusal, atomicity, native pixel/pick equivalence |
| P0 | FreeSurfer scalar morphometry import | neurosurf4s IO | Modern/legacy independent fixtures, invalid input, exact mesh/vertex association |
| P0 | Voxel-cell nearest-boundary query | neuroatlas4s volume query policy | Preserve all final-series numerical refusals; independent affine/uncertainty oracles and bounded access |
| P2 | Reachable response-family basis | ScalaFIM HRF/design policy over Gale decomposition | Repair overflow/nonfinite results; independent rank/projection/residual tests on JVM and JS |
| P2 | Sign-flip policy and group contrast diagnostics | ScalaFIM group over shared numerical/resampling providers | Compare current engine and declared assumptions; independent scientific qualification |
| Blocked separately | Dense-colour atlas chunk refinement; stock native affine sampler admission | Optional JavaFX backend | Reproduce and resolve retained native failures before enabling |

P0 means necessary to remove the consumer's old-branch dependency. It does not
authorize bypassing correctness gates or promise that every historical API shape
will remain. Current public domain types should remain authoritative; adapt
consumer code where newer APIs already supply the correct behavior.

### Staging cache: two reproduced correctness gaps

At main `ac2ec8b7`, `NiftiResponseBlockSource.scala` keys staged files using source
path, size and modification time, and accepts an existing cache file if it is at
least 352 bytes. The September `NiftiStagingCache.scala` instead keys compressed
content, validates an expanded-content digest receipt, coordinates writers with
OS and bounded JVM locks, publishes atomically, and supports expansion/cache/
free-space limits and cancellation.

The [reproducible probe](probes/run-cache-probe.py) compiled the actual two cache
implementations with Scala 3.7.4 and a minimal `DatasetError` ADT. It used synthetic
512-byte gzip payloads; this tests staging, not NIfTI image interpretation.

| Experiment | Current main | September cache |
|---|---|---|
| Replace compressed content while retaining path, compressed size and mtime | Returns stale content | Returns new content under a new key |
| Overwrite the staged file with different same-length bytes, then request source again | Reuses corrupted content | Detects corruption and repairs the entry |

Both compile and execution commands exited zero; observations are in
[cache-results.json](probes/cache-results.json). The old implementation is a
strong recovery candidate, but this probe does not qualify its concurrency or
resource policies. Its default limits are effectively unbounded; applications
must select an explicit product envelope. Hashing every source and reused output
has an IO cost that must remain visible in performance measurements.

### Surface updates: recover the contract, adapt the implementation

The useful contract is: prepare a compatible scalar/packed-RGBA layer replacement
off-thread, retain stable geometry, rebase current camera and selection, validate
the complete change before mutation, apply the backend successfully, then commit
controller state. Reuse prepared selection readout/chrome only if selection is
still unchanged. This is directly exercised by PLS Neuro's source.

The October snapshot is a better implementation starting point than the September
tip. Its eight-file delta adds a CPU-only preparation basis, snapshots chunk
descriptors/capacities, binds preparation to an opaque owner token, compares mesh
buffer identity as well as resource keys, and refuses stale/foreign work even on
no-op/rebuild paths. The September basis retains a live probe owner and invokes
its preparation methods from the worker, leaving more dependence on mutable
owner state.

The October source and hashes survive in
[october-source-manifest.json](october-source-manifest.json). Its
[exact patch](october-eight-file.patch) applies syntactically to six of eight
current paths; `JavaFxSurfaceProbe.scala` and `JavaFxSurfaceProgram.scala` conflict.
That is a source-recovery fact, not a compilation result. Whole-file replacement
would discard newer behavior.

Current render plans have evolved to include sample normals, scalar/coverage
data, colour pipeline, surface base/opacity, fragment mappings and richer view
composition. The old `sameContent` binding cannot be assumed to cover these new
semantics. A port must preserve current capability refusals and complete
preflight validation, and bind every input relevant to pixels, geometry and
readout. Preserve negative tests for same-key/different-buffer geometry,
camera/selection interleaving, stale A→B→A preparation, foreign owners, late
budget refusal and failure without partial controller/backend mutation.

Stable-layout recolouring and dense-colour refinement must be separate changes.
The October draft's `RefineAtlases` replaces affected chunks while retaining
unaffected ones, accounts for mesh/UV bytes and stages allocations first, but a
historical dense framebuffer failure remains unresolved. Do not treat the entire
eight-file snapshot as an accepted optimization.

Current `JavaFxRasterSurfaceHost` supplies worker rendering, coalescing and coherent
pixel/pick generations for the software-raster provider. This does not replace
Scene3D retained atlas behavior or qualify its native sampling/performance.

### Persistence: the two V8 formats are incompatible

September's `SurfaceSceneDocument`/`SurfaceSceneCodec` V8 requires
`geometryStates`: family variants with asset identities/URIs/digests and fixed or
morph presentation. Current main also calls its scene format V8, but uses that
revision for `colorPipeline`, `surfaceOpacities` and `surfaceBase`. Its default
reader rejects fields outside its current root-field set.

Current `SurfaceCompositionDocument` revision 2 already preserves fixed/morph
geometry and multi-view pose/framing. It does not supply the same per-variant
asset digest binding. Recover that provenance deliberately, with an explicit
legacy reader and an unambiguous new format/version. Do not relabel either V8
definition or silently discard unknown geometry fields. Test old saved consumer
scenes as well as current scenes and mismatched family assets.

### Image layout and morphometry: bounded missing features

September's `OrthogonalArrangement.Automatic` selects among L/row/column layouts
using fitted physical image area; `SinglePlane` renders, samples and picks only
the focused plane. It uses logical-pixel margins/gaps and scales insets for tiny
viewports. Current main exposes L/row/column with normalized spacing and lacks
these modes. Recover the five layout tests and adapt current typed grids and
pane ownership. This is independent of native surface-sampler admission.

The retained FreeSurfer morphometry codec handles modern big-endian float32 and
legacy signed-int16/100 scalar files. Current FreeSurfer surface geometry loading
is not a replacement for `.curv`, `.sulc` or `.thickness` loading. The retained
codec explicitly checks counts rather than anatomical correspondence: its caller
must bind the correct mesh and vertex ordering. PLS Neuro uses its JVM reader.
Keep that limitation explicit and check upstream ownership before adding a
second generic decoder.

### Atlas query: useful, but preserve the entire repair series

`AtlasBoundaryQuery` computes distance to exposed voxel-cell faces at ±0.5 voxel,
in world millimetres under a full affine/shear metric, retaining ties across
regions. Main's point/voxel-centre query and surface region contacts are different
contracts. The final seven-commit series adds outward discovery bounds,
inverse-residual uncertainty, cancellation, budgets, ill-conditioned geometry
refusal and globally ambiguous nearest-witness refusal. Recover the final
`535977a86` state, not the initial draft.

The retained 15 tests include cube, edge/corner, anisotropy, rotation, shear,
budget/cancellation and floating-point counterexamples. They were inspected, not
rerun as independent numerical qualification here. Adapt to exact current frame
and realization identity rather than string-only space comparison. In current
`VolumeAtlas`, `labelVolume` materializes a dense volume: an old query visit budget
does not bound that allocation. The port needs bounded label access or an
explicitly admitted materialization envelope.

### First-level and group recovery needs scientific judgment

`ContrastReprojection.scala` and its suite are byte-identical to main;
`BasisExpandedRetention.scala` is also identical while its suite has evolved.
Cutoff DCT, event-column roles and Gale delegation are already represented.
Do not reintroduce old implementations simply because their commits are absent
from ancestry.

The missing first-level helpers are more substantive. `ConvolutionSupport` and
`EventResponseSupport` carry observed-response windows and retain/drop decisions;
`FirstLevelTables` binds events/confounds to captured units;
`StudySpatialSupport` validates common grids and computes strict finite-nonzero
mask intersection with bounded read width and coverage receipts. It holds
grid-sized arrays, so bounded reads are not a total-heap bound. These contracts
must be ported or replaced by equivalent current APIs and exercised through the
actual PLS review/estimation workflow.

`ReachableResponseBasis` is absent. Its projection accumulates `value * value`
and checks input finiteness but not finiteness of the accumulated squared norm.
Finite values around `1e308` can therefore overflow to infinity and lead to a
successful receipt containing a nonfinite norm/residual. This is a source-level
counterexample, not an executed full HRF/Gale integration test. Recover the
domain idea only after using overflow-safe norm/decomposition contracts, checking
rank/residual results and delegating generic linear algebra to Gale.

`GroupSignFlip` and `GroupContrastDiagnostics` are candidate policy/diagnostic
salvage, not permission to replace the current group numerical engine. Preserve
symmetry/weighting assumptions, leverage/working-variance meaning and historical
adverse evidence. New independent fixtures and scientific admission are required.

## Native evidence and limits

The September notes report 151 shared tests on each of JVM and JS and 49 JavaFX
tests. Their full consumer FX p95 was **6.904 ms against a 4 ms target**, despite a
backend p95 of 2.849 ms; only five stable samples were recorded. Those results
do not establish a current performance pass.

Current committed native records distinguish patched ES2 diagnostics from stock
sampler admission. The stock Metal experiment recorded 39/48 passing cases and
9/32 vertex-permutation failures on each of JavaFX 25.0.4 and 26.0.2; the archived
patched ES2 control passed 48/48. These are runtime-specific historical results,
not a claim about every JavaFX version. Production affine creation remains gated
by `SamplerUnqualified` on the recorded baseline.

The stock Metal evidence bundle is present and its SHA-256 matches
`969933c2ebf547841191b2ab429ac6f1dc41041fc9a126cefe579b2b7380c209`.
The October queue's raw `/private/tmp/scalafim-mote-queue-evidence` directory is
gone. Its source snapshot and selected tracker notes remain available. See
[evidence-availability.json](evidence-availability.json) and
[historical-tracker-evidence.json](historical-tracker-evidence.json). No new native
GUI, cortical benchmark, full repository suite or scientific admission was run
for this assessment.

## Reproduction and completion boundary

From a clone containing the retained refs and locally cached Scala 3.7.4 jars:

```sh
python3 docs/verification/warm-update-salvage-20261016/probes/run-cache-probe.py
```

The runner accepts `--repo`, `--maven-cache` and `--output`. Compiler/runtime logs
and exact exit codes accompany the observations. Other comparisons use `git
show` at the exact revisions above; the October patch can be regenerated with
`git diff 9b29fcd303879c31d0e4cf0d2984c95730f19411 cb0712b4a93636372228f9e0dc0c64270f882a36 -- modules/surface-view-javafx`.

The user subsequently made the implementation objective explicit: correct
behavior required by PLS Neuro belongs on main, and PLS Neuro should depend on
that qualified main-line behavior. This assessment supplies the recovery map;
it does **not** claim that those ports, a main merge, or the consumer repin are
already complete. Track implementation and native acceptance separately from
this source-analysis receipt under `bd-01M24EQFARZKYMZ71BJP91C9XS` and the
appropriate workflow/modularization tasks.
