# Image module unification and interface hardening

- Status: active
- Reviewed baseline: `a3c232055b2514865ec286e35d99ff113c611e1c`
- Governing decision: [`../decisions/native-image-data-model.md`](../decisions/native-image-data-model.md)
- Scope: `image` public API plus the dataset, fit, group, atlas, image-view, motion,
  and spatial consumers that must preserve its ownership and geometry contracts

## Outcome

ScalaFIM will expose one provider-faithful image model rather than a ScalaFIM
model beside the provider model. The completed interface has one authoritative
type and one authoritative set of invariants for each concept. ScalaFIM may add
neuroimaging refinements, policies, algorithms, and format adapters, but it may
not add a second generic representation.

This plan is intentionally breaking. Deleted compatibility types are not kept
as deprecated aliases because an alias would preserve the parallel public
vocabulary and make the intended API ambiguous.

## Authority ledger

| Concept | Sole authority | ScalaFIM surface |
| --- | --- | --- |
| Dense values, rank, shape, strides, views, builders | Ravel | Retain `NDArray` directly |
| Generic matrices and linear algebra | Gale | Use `gale.linalg.DMat`; do not wrap it |
| Spatial affine and grid geometry | image4s geometry | Use `image4s.geometry.Affine[D3]` and `Grid` directly |
| Sample spaces, non-spatial axes, metadata, value semantics | image4s | Use `SampleSpace`, `Axis`, `ImageMetadata`, and semantic tags directly |
| Exact indices, regions, ordered selections, finite-domain maps | locus4s and image4s-locus | Use `GridDomain`, `Index`, `Region`, `Selection`, `SelectedSampled`, and `PartialSurjection` directly |
| Coordinate transforms and resampling | reframe4s | Add only neuroimaging policy and adapters |
| Anatomical orientation | ScalaFIM image | Retain only `AnatomicalAxis` and `Orientation3D`; these are not sampling axes |
| Dense neuroimaging refinements | ScalaFIM image | `NeuroVolume[S, A, Sem]` and `NeuroSeries[S, A, Sem]` only |
| Image-boundary failure | ScalaFIM image | One `NeuroImageError` that retains provider errors as typed causes |
| Statistical map bundles | ScalaFIM fit | A neuro-specific record over provider `SelectedSampled`, never a time series |

## Rules for every phase

1. A convenience method must expand into the canonical provider operation and
   preserve values, exact owner, axes, metadata, order, errors, and allocation
   behavior. If it cannot, it needs an explicit policy parameter or must be
   removed.
2. A ScalaFIM type is admitted only when it enforces a neuroimaging invariant
   not expressible by the provider type. It must not copy provider state.
3. Static APIs preserve the concrete owner `S`. Existential APIs perform one
   checked alignment and return a typed error; they do not throw.
4. Raw ordinals and mutable arrays are kernel or format-boundary views, not
   public scientific identity.
5. Every phase passes on JVM and Scala.js before the next phase builds on it.

## Execution receipts (2026-08-24)

- Phase 0.1 is implemented in an uncommitted image4s worktree: provider axis
  selection/concatenation and laws pass on JVM and Scala.js. ScalaFIM consumes
  that checkout through the explicit local-build override; the immutable pin
  cannot move until the provider change is committed and published.
- Phase 0.2 is implemented: the unpublished `provider-spike` compiles on JVM
  and Scala.js with no ScalaFIM imports and no missing provider capability.
- Phase 1.1 is implemented: adversarial volume admission and exact-owner tests
  pass 11/11 on each platform.
- Phase 1.3 is implemented: `NativeImageError` is deleted, the exhaustive
  `NeuroImageError` fixture is fatal on stale matches, and its six tests pass
  on each platform; NIfTI's typed JVM read boundary passes 7/7.
- Phase 1.2 is implemented: all eight forgeable space/dimension refinements
  are absent from production, the external compile boundary passes 7/7 on
  each platform, and the affected image, image-view, dataset, atlas,
  MVPA-spatial, and surface-view JVM/Scala.js suites are green.
- Phase 2.1 is implemented locally: ScalaFIM `Axis`/`AxisSet` and the `slash`
  dependency are deleted; the orientation/provider-axis gate passes 15/15 on
  each platform and affected downstream test sources compile.
- Phase 2.2 is implemented locally: ScalaFIM `DMat`, `Affine3D`, and its generic
  affine implementation are deleted. Spatial geometry is image4s
  `Affine[D3]`, generic matrix work is Gale, and the provider affine contract,
  nibabel/neuroim2 coordinate oracles, motion, and atlas suites pass on JVM
  and Scala.js.
- Phase 2.3 is implemented locally: the image transform hierarchy and its
  execution plans are deleted. Spatial routing retains one
  `CoordinateMap.Geometric(ProviderMapBinding)` payload whose executable value,
  composition, inverse, dense-field interpolation, and differential are owned
  by reframe4s. Directional fingerprints are structural, order-sensitive, and
  stable under double inversion. External compile tests reject every deleted
  image transform and spatial dense/composite variant; full spatial suites pass
  140/140 on JVM and 116/116 on Scala.js, including independent SimpleITK HDF5
  composition and inverse oracles.
- Phase 2.4's public contraction is implemented locally: the package-wide
  `SampleSpaces.*` export is removed and provider-state shorthand extensions
  are package-private implementation details. External consumers construct a
  volume with `SampleSpaces(...)` and append axes with image4s
  `appendNonSpatial`; compile-negative tests prove that `dims`, `addDim`, and
  the old package export are not public. The mechanical compile gate rejects a
  returned public alias or package export. Removing the remaining private
  shorthands is cleanup, not an interface prerequisite.
- Phase 6.1 is implemented locally: ScalaFIM's grid-comparison and certificate
  algebra is deleted in favor of image4s `SamplingAlignment` and
  `GridCongruence`; exact endpoint binding and typed provider-cause courts pass
  on JVM and Scala.js. Focused affected suites pass 1,885 tests and
  `scalafimCompileAll` is warning-clean against the local provider worktree.
- Phase 3.1 and Phases 4.1-4.3 are partially implemented for volume/series:
  exact-owner operation contracts and provider-faithful selection,
  concatenation, and sequence-ordering suites pass 52/52 on each platform.
- The full ScalaFIM `scalafimTestAll` gate passes 4,348 tests across every
  listed JVM and Scala.js module against the two local provider worktrees.
  Affected image4s geometry/core/law suites pass 39/36, 94/90, and 23/21 on
  JVM/Scala.js; affected reframe4s field/resample/law suites pass 7/7, 1/1,
  and 42/40, and both provider repositories pass their complete cross-platform
  compile aggregates. Their monolithic test aggregates exceed the long-lived
  sbt JVM's classloader-cache memory, so the provider test receipt is the split
  affected-module execution rather than a false aggregate-green claim.
- Later ownership and semantic phases remain pending; no interface-freeze,
  provider publication, or release claim is made by these local receipts.

## Phase 0: close provider gaps first

### 0.1 Provider-owned axis selection and concatenation

Add generic axis operations to image4s if the admitted revision still lacks
them. `Axis.select(indices)` must preserve name, kind, unit, requested order,
and exact coordinates. `Axis.concatenate` must require an explicit coordinate
policy and reject incompatible names, kinds, units, or continuity. ScalaFIM
must not implement its own axis record or coordinate algorithm while waiting
for this capability.

Right tests:

- An image4s shared property suite over ordinal, regular, explicit, and
  categorical coordinate records. For arbitrary valid index sequences,
  including reverse order and duplicates, every output coordinate must equal
  the corresponding input coordinate.
- A table suite for concatenation: continuous regular seconds succeeds;
  milliseconds versus seconds, differing kinds, discontinuities, overlaps,
  and incompatible categorical axes fail with the exact typed error.
- JVM and Scala.js tests at the immutable image4s revision, followed by a
  ScalaFIM pin update. A ScalaFIM copy of these algorithms is a failing review
  condition.

### 0.2 Confirm existing provider capabilities

Before adding any ScalaFIM replacement, prove that these admitted capabilities
cover the migration:

- `image4s.geometry.Affine[D3]` for validated spatial affines;
- Gale `DMat` for genuinely generic matrices;
- image4s-locus `SelectedSampled` for arbitrary selected non-spatial axes; and
- reframe4s for coordinate-map composition and resampling.

Right test:

- A tiny provider-spike cross-project, outside `package scalafim`, compiles one
  representative affine, selected statistical-map stack, exact selection, and
  resampling plan on JVM and Scala.js. A missing operation becomes provider
  work; it must not trigger a local parallel abstraction.

## Phase 1: seal the unsafe admission boundaries

### 1.1 Reject non-volume spaces at volume admission

Change dynamic `SomeNeuroVolume` constructors to require exactly three spatial
dimensions and no non-spatial axes. Remove implicit use of `spatialOnly`.
Callers that deliberately project a series or channel image must do so through
an explicitly named provider operation before constructing a volume.

Right tests:

- Shared adversarial tests construct equal-shape D3 spaces carrying Time,
  Channel, Echo, and a custom axis. Every safe volume constructor must return
  the precise `UnexpectedNonSpatialAxes` error.
- A D3-only space succeeds and retains the exact sample-space object.
- An explicit spatial projection followed by construction succeeds, proving
  that the intended operation remains available without being implicit.

### 1.2 Remove forgeable space refinements

Delete `ImageDim`, `ImageDimEvidence`, `ImageSpace`, `VolumeSpace`, and
`SeriesSpace`. The neuro image value itself is the refinement; a second wrapper
around `SampleSpace` does not add another invariant. Internal validators may
return provider sample spaces, but no public `unsafe` space constructor remains.

Right tests:

- An external compile suite must fail to resolve every deleted type and every
  former `.unsafe` constructor.
- Positive compile probes show that `NeuroVolume[S, ...].sampleSpace` has type
  `S`, while dynamic `SomeNeuroVolume` exposes only `SomeSampleSpace`.
- Mechanical source gate: no definitions or aliases named `ImageSpace`,
  `VolumeSpace`, `SeriesSpace`, or `ImageDimEvidence`.

### 1.3 Use one checked error algebra

Replace `NativeImageError`, the older `NeuroImageError`, and string/exception
translations at public checked boundaries with one exhaustive
`NeuroImageError`. Provider failures remain typed causes such as
`Image(ImageError)`, `Geometry(GeometryError)`, and `Selection(SelectionError)`;
neuro-specific cases cover only neuro-specific invariants such as
`ExpectedSingleTimeAxis`, `UnexpectedNonSpatialAxes`, `GridMismatch`, and
`LayoutCapabilityRequired`. Throwing entry points, if retained at all, are
explicitly named `unsafe*` and are not the implementation used by checked APIs.

Right tests:

- Constructor error table asserting the exact ADT case for wrong rank, Channel
  in place of Time, multiple Time axes, shape mismatch, foreign grid, and
  noncanonical layout. Tests must not match error strings.
- A compile-time exhaustive match fixture over `NeuroImageError`; adding a case
  must force the fixture and public translations to be updated. Compile this
  fixture with `-Werror` so a non-exhaustive match cannot remain a warning.
- A diagnostic test verifies that a rank-4 Channel image reports
  `ExpectedSingleTimeAxis`, never `InvalidRank(expected = 4, actual = 4)`.

## Phase 2: remove parallel axis, space, affine, and transform vocabularies

### 2.1 Separate anatomical orientation from sampled axes

Delete the hybrid ScalaFIM `Axis` and `AxisSet`. Keep the closed
`AnatomicalAxis` enum and make `Orientation3D` its only aggregate. Remove
`Time`, `NoneAxis`, and generic labels from the anatomical model. All
non-spatial sampling uses image4s `Axis` directly, including Time, Channel,
Echo, Direction, and custom axes. `SampleSpaces` must not translate between
the two concepts.

If a construction convenience is still warranted, it must return an image4s
`SampleSpace` and accept an image4s `Axis`; it must not expose a ScalaFIM space
record.

Right tests:

- Compile gate: production source outside orientation code cannot resolve
  `scalafim.image.Axis` or `AxisSet`; Time-axis construction imports
  `image4s.Axis`.
- Exhaustive orientation tests cover every valid signed anatomical permutation
  and prove orientation-to-matrix-to-orientation round trips.
- Axis provenance tests construct regular, explicit, and categorical image4s
  axes and prove sample-space construction returns the same axis records
  without ordinal reconstruction.
- Existing asymmetric NIfTI orientation fixtures remain coordinate-exact on
  JVM; shared orientation behavior passes on JVM and Scala.js.

### 2.2 Remove the local matrix and affine algebra

Delete `scalafim.image.DMat`, `Affine3D`, and the generic operations in
`scalafim.image.Affine`. Spatial geometry accepts
`image4s.geometry.Affine[D3]`. Non-affine generic matrix calculations use Gale
directly. Coordinate-map composition and resampling migrate to reframe4s.
Small neuroimaging calculations such as voxel size or obliquity may remain as
extensions over the provider affine, but they may neither store a second
matrix nor implement inversion/composition.

Migrate image first, then motion, atlas, and spatial. Do not introduce a
temporary public adapter type; use private conversion functions only at
unmigrated file-format boundaries and delete them in the same phase.

Right tests:

- Provider affine construction copies untrusted input: mutating the input
  collection after construction cannot alter the affine or its inverse.
- Affine law tests: identity, inverse round trip, composition associativity
  within tolerance, homogeneous-row validation, singular rejection, and
  inverse-residual rejection on JVM and Scala.js.
- Fixture parity against the existing nibabel/neuroim2 voxel-to-world and
  world-to-voxel coordinates, including an oblique affine.
- Motion and atlas golden tests compare the migrated provider/Gale results to
  their existing numerical fixtures with explicit tolerances.
- Mechanical gate: no `scalafim.image.DMat`, `Affine3D`, `DMat.invert`, or local
  generic matrix implementation remains in production source.

### 2.3 Use reframe4s as the sole transform algebra

Before thinning the space façade, remove the generic local transform
vocabulary. Map every case of `scalafim.image.Morphism`, spatial
`CoordinateMap`, and generic resampling-plan logic to the admitted reframe4s
types. A remaining ScalaFIM plan may contain neuroimaging policy, but its value
must be a provider plan and its execution must delegate to that plan; it may
not implement a second composition or interpolation interpreter. A missing
generic transform case is provider work under Phase 0.

Right tests:

- A canonical-expansion suite compares identity, affine, dense-field, and
  composed paths through the ScalaFIM policy entrance and direct reframe4s
  construction. Values, coordinate direction, errors, and interpolation
  policy must match.
- Composition-law and inverse round-trip tests run against the provider value,
  including an asymmetric affine-plus-warp fixture that detects reversed
  composition order.
- Mechanical gate: no public generic ScalaFIM coordinate-map ADT or independent
  transform interpreter remains. Any retained neuro policy type must expose
  its provider plan.

### 2.4 Thin or remove `SampleSpaces`

Remove the package-wide `export SampleSpaces.*` façade and every extension that
renames or reconstructs provider state (`dims`, `axes`, `trans`, `addDim`,
`dropDim`, and similar operations). Retain only narrowly named neuroimaging
smart constructors or policies that return provider values and have an exact
canonical expansion. Prefer placing fixture-only conveniences in test code.

Right tests:

- For every retained convenience, an equivalence test compares its complete
  `SampleSpace`/`AxisRecord`/grid record to construction through the provider
  API.
- External compile tests demonstrate the shortest supported volume and series
  construction paths without importing `SampleSpaces.*`.
- Mechanical API gate enumerates the intentionally retained methods; new
  public extensions on `SomeSampleSpace` fail the gate until reviewed against
  the authority ledger.

## Phase 3: make exact ownership survive ordinary operations

### 3.1 Preserve `S` through value-preserving operations

Define static operations on `NeuroVolume[S, ...]` and
`NeuroSeries[S, ...]` that return the same `S` whenever geometry is unchanged.
Binary operations with the same static owner need no runtime geometry check.
Existential binary operations use a separately named checked operation and
return `Either[NeuroImageError, SomeNeuro*]` after one explicit alignment.

Right tests:

- Compile contracts assign the result of `mapValues`, comparison, arithmetic,
  and semantic conversion to a value with the original `S`. Operations such as
  time selection that change the sample space must return a newly owned space
  rather than claiming to preserve `S`.
- Compile-negative probes reject binary operations between distinct static
  owners unless a checked alignment is supplied.
- Runtime adversarial tests use equal shapes with translated, reflected, and
  reordered grids; existential operations reject each one.
- Value properties compare static and checked existential operations on an
  aligned pair and require identical values, metadata, axes, and output owner.

### 3.2 Replace ownerless mask and voxel APIs

The static mask extraction API accepts `MaskVolume[S]`, `Region[S]`, or
`Selection[S]`, never a bare array of ordinals. The dynamic mask overload
returns `Either` after exact grid/domain compatibility. Raw ordinal adapters,
where required for I/O or kernels, are package-private and say `unsafe` or
`canonicalOrdinals` in their names.

Right tests:

- Same-owner mask extraction succeeds and matches a coordinate-loop oracle.
- Same-size masks on translated, reflected, and independently registered grids
  all fail before any values are read.
- Selection tests prove declared order, uniqueness rejection, empty-policy
  behavior, and exact source-domain ownership.
- External compile tests reject public `timeSeries(Int)`, `Array[Int]`, and
  ownerless `Mask.fromIndices` entry points.

### 3.3 Preserve the owner through dataset, fit, and group

Dataset request ADTs may continue to accept user intent as coordinates or
integers, but resolution must return and carry the actual provider
`GridDomain` and `Selection`. `FmriSeries`, fitted maps, and group inputs retain
that selection and its position domain. Integer ordinals are derived only
inside kernels and encoders. No consumer registers a fresh domain for data
that already has an owner.

Right tests:

- An integration fixture resolves one asymmetric dataset selection, carries it
  through fit and group, and asserts `sameRuntimeOwnerAs` at every boundary.
- A same-size independently registered domain is rejected at dataset-to-fit
  and fit-to-group composition.
- A non-sorted selection such as `[5, 1, 3]` remains in that exact order through
  extraction, model fitting, map construction, and serialization round trip.
- Property tests compare selected values against direct coordinate access for
  generated unique selections.

## Phase 4: preserve non-spatial semantics through transformations

### 4.1 Make series selection axis-preserving

Implement `NeuroSeries.selectTimes` by selecting both the Ravel data and the
existing image4s Time axis. It must preserve name, kind, unit, coordinate
values, requested order, and duplicates. The result retains `S` only if the
sample-space type can express the changed axis; otherwise it returns a precise
new provider-owned sample space without fabricating a static owner.

Right tests:

- Table tests for regular seconds, explicit milliseconds, and categorical
  Time axes; each checks values and complete `AxisRecord` after forward,
  reverse, sparse, and duplicate selections.
- A data/coordinate coupling property verifies that output position `j`
  contains both the value and coordinate from requested input position
  `indices(j)`.
- Out-of-bounds and empty selections return typed errors, not `require`
  failures.

### 4.2 Make concatenation policy explicit

`NeuroSeries.concatenate` delegates to image4s axis concatenation and requires
an explicit policy when continuity cannot be inferred. It checks spatial
identity plus axis name, kind, unit, coordinate compatibility, and metadata
policy. It never invents an ordinal Time axis.

Right tests:

- Continuous regular series concatenate and preserve the expected regular
  axis.
- Unit mismatch, discontinuity, overlap, different axis kinds, and different
  grids each return their exact error case.
- An explicit rebase/output-axis policy is tested separately and must produce
  the declared coordinates; it may not be an ambient default.
- Associativity is checked for compatible blocks under the same policy,
  comparing both values and full axis records.

### 4.3 Correct `NeuroSeriesSeq` ordering

Selection over a sequence must emit frames in caller-requested order, not
source-block order, and must preserve duplicates.

Right tests:

- Focused cases `[3, 0]`, `[0, 3, 1, 2]`, and `[3, 0, 3]` across block
  boundaries.
- A generated property compares sequence selection with concatenating the
  blocks once and applying single-series selection to the same index vector.

### 4.4 Represent statistical maps as statistical maps

`FitImageMaps` stores an image4s-locus `SelectedSampled` with a provider custom
axis kind such as `statistical-map`; it is not `SelectedSeries` and never uses
`AxisKind.Time`. Map names are the categorical coordinates or an explicitly
validated one-to-one metadata field, not a parallel unsynchronized vector.

Right tests:

- Construction asserts custom axis kind, categorical labels equal map names,
  and exact inherited voxel owner.
- Refinement as `NeuroSeries`/`SelectedSeries` fails with
  `ExpectedSingleTimeAxis`.
- Reordering maps reorders both values and labels; duplicate/blank map names
  are rejected according to the chosen naming policy.
- Dense realization preserves the same grid domain and map-axis record.

### 4.5 Keep NIfTI admission provider-owned

The ScalaFIM NIfTI boundary retains the sample space, axes, selected affine,
and selection provenance produced by image4s-nifti. It does not reconstruct a
new space from dimensions and a matrix. sform/qform choice is an explicit
provider policy, not `sform.orElse(qform)` hidden in a convenience method.

Right tests:

- A fixture whose valid sform and qform intentionally disagree verifies each
  explicit selection policy and records which transform was selected.
- A 4D fixture with non-unit temporal spacing and declared time units verifies
  the complete decoded Time `AxisRecord`, not only its extent.
- Read-write-read parity compares grid record, affine-selection provenance,
  non-spatial axes, metadata, dtype/scaling policy, and coordinate-addressed
  values.
- Existing independent nibabel and neuroim2 asymmetric fixtures remain green;
  a dimensions-only reconstructed space must fail the provenance assertion.

## Phase 5: make views and copies truthful

`asMatrix` and reshape conveniences become view-only capability refinements.
For a series, the canonical view is `(voxel, time)`. A noncanonical input
returns `LayoutCapabilityRequired`. Copying alternatives are explicitly named
`materializedVoxelTimeMatrix` or `materializedCanonicalCopy`. No convenience
silently copies.

Right tests:

- Whole-canonical inputs return a zero-copy view with the expected shape,
  strides, and backing identity on JVM and Scala.js.
- Transposed, sliced, reversed, and offset views fail the view-only operation
  with the precise capability error.
- The named materialization succeeds for every valid layout and matches a
  coordinate-loop oracle.
- JVM allocation tests prove zero allocation for the view path after warmup
  and one bounded destination allocation for materialization; JS tests prove
  backing identity versus distinct materialized storage.

## Phase 6: converge downstream consumers and dependencies

### 6.1 Use one grid-compatibility path in image-view

Ordinary and mapped slice plans must both call the same
`GridCompatibility`/alignment-certificate operation. Direct provider-object
equality is not a second compatibility policy.

Right tests:

- Both plan types accept the same exact grid and the same certified persistent
  equivalent grid.
- Both reject the same translated, reflected, reordered, and uncertified
  separately decoded grids with the same error ADT.
- A shared parameterized suite runs the identical compatibility court against
  both implementations.

### 6.2 Remove unused and duplicate dependency edges

Remove `image -> locusData` if compilation confirms no ScalaFIM locus API is
used, and remove production `cats-effect` if it is test-only. Audit the local
`locus-data` parcellation vocabulary separately: generic finite-domain
machinery migrates to locus4s; only documented neuroimaging policy remains in
ScalaFIM.

Right tests:

- JVM and Scala.js image compilation with each suspect dependency removed.
- Dependency-tree assertion that the published image artifact contains no
  `scalafim-locus-data` and no unintended cats-effect runtime dependency.
- Mechanical import gate: image production source imports provider locus
  packages directly and contains no `scalafim.locus` generic aliases.

### 6.3 Add a real external consumer contract

Add an unpublished JVM/Scala.js cross-project in `package external.consumer`
that depends only on the image artifact. This is the public-interface court;
tests inside `package scalafim.*` are insufficient for visibility guarantees.

Right tests:

- Positive compile contracts: provider `Sampled` admission, precise
  `NeuroVolume[S]` construction, dynamic widening, owner-preserving mapping,
  exact selection, axis-preserving time selection, and provider interop.
- Negative compile contracts: arbitrary `Sampled` cannot masquerade as a neuro
  refinement; former unsafe space constructors and ownerless ordinal methods
  are unavailable; categorical/mask images cannot use continuous
  interpolation; statistical maps cannot masquerade as Time series.
- The project compiles and tests on both JVM and Scala.js from the pinned
  provider graph, without depending on any other ScalaFIM module.

## Phase 7: final interface-freeze gate

The interface is ready to freeze only when all of the following are true:

1. Mechanical searches find no parallel public image/space/axis/affine/domain
   algebra and no deprecated aliases for removed types.
2. All focused shared suites pass on JVM and Scala.js.
3. Provider axis/affine/locus suites pass at the exact immutable revisions.
4. The external consumer contract passes on JVM and Scala.js.
5. Atlas, dataset, fit, group, image-view, motion, spatial, MVPA, threshold,
   backends, and examples pass their focused JVM/Scala.js courts.
6. `scalafimCompileAll`, `scalafimTestAll`, `examplesCompile`, and
   `examplesTest` pass warning-clean from a clean worktree.
7. NIfTI nibabel/neuroim2 parity, exact-domain adversarial tests, and current
   allocation receipts pass at the reviewed SHA.
8. A clean packaged-artifact consumer proves the dependency tree and public
   visibility boundary rather than relying only on composite-build source
   access.

## Recommended implementation order

1. Land any missing image4s axis operations and update the immutable pin.
2. Fix volume admission, mask compatibility, and the unified error channel.
3. Delete forgeable space refinements and thin the `SampleSpaces` façade.
4. Remove the hybrid axis layer and migrate temporal operations to image4s
   axes.
5. Replace local matrix/affine/transform code with Gale, image4s geometry, and
   reframe4s, migrating downstream consumers in the same slice.
6. Preserve static owner `S`, then carry exact selections through dataset,
   fit, and group.
7. Correct statistical-map and series-sequence semantics.
8. Make view/materialization behavior explicit.
9. Converge image-view compatibility, remove dependency edges, and land the
   external consumer project.
10. Run the final clean-worktree interface-freeze court.

Each numbered item is independently reviewable, but an item is not complete
until its specified negative and adversarial tests pass on both platforms. A
green happy-path suite is not evidence that a parallel or unsafe entrance has
been removed.
