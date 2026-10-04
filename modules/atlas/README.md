# scalafim-atlas

Typed standard-atlas metadata and parcel operations for `scalafim`.

The atlas module is a Scala 3 rewrite of the useful computational core from
`neuroatlas`: standard atlas identity, provenance, region metadata, lookup,
parcel reduction, overlap, and graph relationships. It deliberately leaves
plotting, Shiny-style interaction, and hidden TemplateFlow downloads out of the
core.

`RegionGraph.contactCounts` keeps its efficient implicit voxel-neighborhood
scan. `RegionGraph.relation` is the semantic reference
`p.converse ; voxelAdjacency ; p`, with self-edges removed.
`RegionGraph.topology` lowers deterministic contact counts to a canonical
graph4s `WeightedGraph[RegionId, Int]` for traversal and optional
`graph4s-gale` numerical operators without materializing a voxel graph.

See [docs/plans/atlas.md](../../docs/plans/atlas.md) for the fuller design and
workflow guide.

## Imports

Shared, cross-platform atlas API:

```scala
import scalafim.atlas.*
import scalafim.atlas.syntax.*
```

JVM-specific atlas IO and download-backed loaders:

```scala
import scalafim.atlas.io.*
```

## Shared Model

- `AtlasRef.Volume`, `AtlasRef.Surface`, and `AtlasRef.Derived` fix the
  representation and space kinds at construction. Checked space factories
  reject known names of the wrong kind; `withDetails` updates descriptive
  metadata while preserving the representation and spaces.
- `AtlasProvenance` is the typed audit trail behind `AtlasRef`: identity,
  spatial support, label schema, source artifacts, derivation steps, citations,
  confidence, and validation issues.
- `RegionId`, `Hemisphere`, `NetworkId`, `AtlasRegionMetadata`, and
  `RegionIndex` describe typed metadata. Labels and attributes are stored as
  `RegionLabel` and `RegionAttributes`; `AtlasRegionMetadata.checked` admits
  raw strings with typed errors, and `fromStrings` delegates to that check.
  Extensional regions are `locus4s.Region`.
- `VolumeAtlas` owns one exact `VolumeAtlasRealization` and its spatial-to-parcel
  assignment. Dense labels are derived materializations; source region IDs
  must match the non-zero payload IDs.
- `subsetEither` rejects an empty selection, composes a partial parcel map
  with the existing assignment, and preserves the exact grid owner. Its
  `SelectedParcels` derivation records the parent parcel/support identities,
  assignment digests, and canonical kept/dropped keys without changing the
  source release identity.
- `SurfaceAtlas` owns one bilateral `SurfaceAtlasRealization`, with retained
  geometry and label-table metadata. Vertex labels derive from its assignment.
  Single-side access requires `scalafim.surface.CorticalHemisphere`.
- Every atlas `realization` carries parcel-indexed metadata, an explicit display
  `Selection`, and an optional validated parcel-to-network `Surjection`.
  Network regions are derived from quotient composition. Realization types
  are sealed; checked volume and surface admission own their construction.
- `AtlasRegistry` and `AtlasSpec` provide immutable discovery for known atlas
  families and aliases.
- `Schaefer2018`, `GlasserHcpMmp1`, `Schaefer2018Surface`, and
  `GlasserHcpMmp1Surface` provide typed standard-atlas descriptors.
- `SpaceTransforms` is a manifest of known template-space steps. It populates
  a `scalafim.spatial.SpatialGraph` (`SpaceTransformGraph`) over unsampled
  template domains from `TemplateCatalog`, and routes with the spatial graph's
  forward-first, inverse-fallback search. Steps holding an internal affine or a
  provider `TransformAsset` (a `scalafim.transform.WorldTransform`) carry
  points and lower to grid pullbacks; planned nonlinear and surface steps stay
  typed non-executable edges and are never silently executed.
- `MniTemplateBridge` executes `MNI152NLin6Asym -> MNI152NLin2009cAsym` with
  TemplateFlow's exact ANTs composite (`MniTemplateBridgeFiles.loadCached` on
  the JVM, which hashes the bytes; the file is admitted only by its pinned
  SHA-256 and its 2009c res-01 displacement lattice; the ITK parity tests need
  the file locally and are skipped, not failed, without it). It pulls 2009c points to 6Asym and resamples 6Asym data
  onto 2009c grids, matching ITK `TransformPoint` to 1e-8 mm; `install` puts its
  steps in a manifest, where the route is pullback-executable
  (`TransformPlan.pullPoints`). The forward map exists only after
  `withNumericalInverse` passes its gates. TemplateFlow's
  `tpl-MNI152NLin6Asym_from-MNI152NLin2009cAsym` file pulls the same way as the
  forward file (measured, see `TemplateFlowXfm`), so it is refused rather than
  used as the inverse. `TemplateGrids` holds the stock MNI res-01/res-02 grids.

The direct query functions and extension methods interpret points in
`atlas.ref.coordSpace` unless the caller supplies `fromSpace`. Use
`ParcelFields.fromKeys(realization)` for detached values: duplicates always
fail, and unknown/missing entries require explicit policies. Fields use canonical
domain order; `ParcelFields.records(realization)(field)` derives display rows
from the realization metadata.

```scala
val ref = Schaefer2018.default.atlasRef()
val annotated: VolumeAtlasRef =
  ref.withDetails(_.copy(notes = Some("analysis input")))
val metadata = AtlasRegionMetadata.checked(
  RegionId(1), "V1", labelFull = Some("L_V1_ROI")
)
val typedMetadata = AtlasRegionMetadata(
  RegionId(1), RegionLabel.unsafe("V1")
)
```

## Standard Atlas Descriptors

```scala
val schaefer =
  Schaefer2018(
    parcels = SchaeferParcels.P400,
    networks = YeoNetworks.Seventeen,
    resolution = VoxelResolution.TwoMm
  )

val glasser =
  GlasserHcpMmp1(GlasserSource.Mni2009c)

val schaeferSurface =
  Schaefer2018Surface(
    parcels = SchaeferParcels.P400,
    networks = YeoNetworks.Seventeen,
    surface = StandardSurface.FsAverage6
  )

val glasserSurface =
  GlasserHcpMmp1Surface(StandardSurface.FsLR32k)

val registry = AtlasRegistry.default
val hcpMmp = registry("hcp-mmp")
val hcpMmpSurface = registry("hcp-mmp-surface")
```

Descriptors are pure values. They do not download or parse atlas payloads until
you call a JVM loader or supply already-loaded surface labels.

## Parcel Identity

`RegionId` is a source integer label. `realization.parcelKeys` identifies parcels
on the exact persistent domain. The reference's `parcelIdentity` declares how
the source labels acquire that identity:

- `SourceLabels` is the default for custom atlases. Keys include representation,
  source, integer label, full label and hemisphere within the atlas release and
  variant. Unknown volume and surface encodings therefore stay separate.
- `SharedRegionIds` explicitly asserts a common integer encoding across
  representations. Schaefer descriptors use this policy.
- `GlasserHcpMmp1` uses checked full names such as `L_V1_ROI` and `R_V1_ROI`.
  The domain sorts those canonical names, independently of source numbers and
  table order. Metadata, assignments, networks and display order use the same
  canonical owner. Missing, malformed, duplicate or conflicting keys fail with
  `ParcelIdentityError`; surface support also checks the key's hemisphere.

Glasser volume ID 1 and surface ID 1 can name opposite hemispheres. The JVM
loader normalizes xcpEngine names such as `Right_V1` to `R_V1_ROI`, retaining the
original name in `source_label`. Each publication assignment retains the exact
source-label-to-parcel-key mapping. Field transport uses
`source.parcelDomain.align(target.parcelDomain)` and `field.rebind(alignment)`;
display order remains a separate selection.

All policies use one current key format inside the finite-indexed publication
descriptor. The fixed foreign-producer byte fixture tests that format. There is
no compatibility mode for the former numeric key scheme. Different or unknown
domains require an explicit checked locus4s `Bijection` for conversion. Choose
`SharedRegionIds` only when the source's common numeric encoding is established.

## Runnable Examples

Compiled examples live in [`../../examples/atlas-jvm`](../../examples/atlas-jvm).
They demonstrate descriptor inspection, explicit local-path loading, coordinate
lookup, parcel reduction, and atlas-to-MVPA regional feature plans.

```sh
sbt atlasExamplesJVM/test
sbt "atlasExamplesJVM/runMain scalafim.examples.atlas.describeStandardAtlases"
```

## Provenance

Every `Atlas` exposes both a compact `ref` and a richer `provenance` value:

```scala
val atlas = SchaeferLoader.loadFromPaths(spec, volumePath, labelPath)

val support = atlas.provenance.support
val sources = atlas.provenance.sourceArtifacts
val labels = atlas.provenance.labels
val derivation = atlas.provenance.derivation
val report = atlas.provenance.summary
```

The typed layer replaces stringly provenance conventions with ADTs:

- `ArtifactRole` distinguishes parcellation volumes, surface annotations,
  label tables, network tables, transforms, geometry, documentation, and
  descriptor-only sources.
- `SpatialSupport` distinguishes volume, surface, and derived atlas support,
  including separate template and coordinate spaces, voxel size, surface density, and
  hemisphere coverage.
- `LabelSchema` records integer label encoding, background value, region IDs,
  and the label-table artifact when known.
- `LicenseInfo` distinguishes known licenses, restricted terms, explicitly
  unspecified source terms, and truly missing license accounting.
- `DerivationStep` records loaded artifacts, legacy load history, validated
  labels, filtered/dropped labels, resampling, and volume/surface projection
  plans.

Use validation to surface audit gaps without making exploratory descriptors
unusable:

```scala
val issues = atlas.provenance.validate(strict = true)
```

Strict validation reports missing digests and truly missing license accounting.
Non-strict validation still reports structural uncertainty such as
descriptor-only provenance, unknown spaces, or uncertain confidence. JVM
loaders now populate structured provenance from their existing source metadata,
attach local paths and SHA-256 digests for files they read, account for known,
restricted, or unspecified source terms, and record whether label tables were
validated as-is or filtered to labels present in the payload.

`summaryLines` and `summary` provide human-readable audit output including
standard release/version/commit accounting, spatial support, label shape,
source paths, SHA-256 values, and license status:

```scala
atlas.provenance.summaryLines.foreach(println)
```

## Surface Atlases

Surface atlas payloads are shared, cross-platform values built from the surface
module:

```scala
import scalafim.surface.{CorticalHemisphere, VertexId}

val surfaceAtlas =
  SurfaceAtlas.fromLabeledSurfaces(
    ref = Schaefer2018Surface.default.atlasRef(),
    regions = regionIndex,
    left = leftLabels,
    right = rightLabels
  )

val region =
  surfaceAtlas.regionAt(CorticalHemisphere.Left, VertexId(1024))

val contacts =
  surfaceAtlas.boundaryContacts(CorticalHemisphere.Right)
```

`SurfaceAtlas` requires a left `LabeledSurface` and a right `LabeledSurface`.
Zero labels are treated as background. All non-zero labels must be present in
the `RegionIndex`, and all regions must be present in the payload. This mirrors
the strict `VolumeAtlas` contract.

On the JVM, GIFTI label payloads can be lifted into a `SurfaceAtlas` while
preserving label-table metadata and local-file provenance:

```scala
import java.nio.file.Path

val atlas =
  SurfaceGiftiAtlasLoader.loadFromPaths(
    ref = Schaefer2018Surface.default.atlasRef(),
    geometry = SurfaceGiftiAtlasLoader.Geometry(leftGeometry, rightGeometry),
    paths = SurfaceGiftiAtlasLoader.Paths(
      left = Path.of("lh.Schaefer2018.label.gii"),
      right = Path.of("rh.Schaefer2018.label.gii")
    )
  )
```

The loader is strict: non-background payload labels must be positive, every
payload label must appear in the GIFTI `LabelTable`, conflicting bilateral
label-table entries are rejected, and local paths plus SHA-256 digests are
recorded in `AtlasProvenance`.

Volume/surface bridge plans are explicit values:

```scala
val bridge =
  VolumeSurfaceTransformPlan.volumeToSurface(
    fromVolumeSpace = SpaceId.MNI152NLin6Asym,
    toSurfaceSpace = SpaceId.FsAverage
  )
```

The route can include planned nonlinear, sphere-resampling, and volume/surface
steps. `scalafim-atlas` does not execute Workbench or projection backends from
the shared core; the TemplateFlow 6Asym/2009c composite runs through
`MniTemplateBridge` once the JVM loader has read it.

## JVM Loading

The JVM loader surface is intentionally explicit:

- `FileAtlasStore`: cache-backed asset resolver with optional download,
  min-size validation, and SHA-256 validation.
- `AtlasLabelMaps`: NIfTI/double-labelmap to integer label volume conversion.
- `SchaeferLoader`: Schaefer2018 volume/LUT assets, LUT parsing, and
  `VolumeAtlas` loading from cached or explicit paths.
- `GlasserLoader`: HCP-MMP1.0 source-specific assets, node-label parsing, and
  `VolumeAtlas` loading from cached or explicit paths.
- `BrainnetomeLoader`: Brainnetome 246-region volume/LUT/network assets, label
  and Yeo-network parsing, and `VolumeAtlas` loading from cached or explicit
  paths.
- `AsegLoader`: FreeSurfer ASEG 17-region subcortical atlas metadata, bundled
  neuroatlas volume asset, color-LUT parsing, and `VolumeAtlas` loading from
  cached or explicit paths.
- `SurfaceGiftiAtlasLoader`: explicit left/right GIFTI label payload loading
  into `SurfaceAtlas` using caller-supplied surface geometry.

Use `CacheOnly` when a reproducible pipeline should fail instead of reaching
the network:

```scala
import java.nio.file.Path
import scalafim.atlas.*
import scalafim.atlas.io.*

val store = FileAtlasStore(Path.of("atlas-cache"))
val atlas = SchaeferLoader.load(
  Schaefer2018.default,
  store = store,
  policy = AssetPolicy.CacheOnly
)
```

Use `loadFromPaths` when files are already managed by another workflow:

```scala
val atlas = GlasserLoader.loadFromPaths(
  GlasserHcpMmp1(GlasserSource.Mni2009c),
  volumePath = Path.of("MMP_in_MNI_corr.nii.gz"),
  labelPath = Path.of("glasser360NodeNames.txt")
)
```

Brainnetome loading follows the same pattern and can include the optional Yeo
network table for richer region metadata:

```scala
val atlas = BrainnetomeLoader.loadFromPaths(
  Brainnetome246.default,
  volumePath = Path.of("BN_Atlas_246_1mm.nii.gz"),
  lutPath = Path.of("BN_Atlas_246_LUT.txt"),
  networkPath = Some(Path.of("subregion_func_network_Yeo_updated.csv"))
)
```

ASEG uses built-in region metadata that mirrors the neuroatlas
`get_aseg_atlas()` table:

```scala
val atlas = AsegLoader.loadFromPaths(
  FreeSurferAseg.default,
  volumePath = Path.of("atlas_aparc_aseg_prob33.nii.gz")
)
```

## Queries

Coordinate lookup returns typed region metadata. The default input coordinate
space is the atlas coordinate space; pass `fromSpace` when the point needs an
available affine transform first. `Point3D` is a source-compatible alias for
`scalafim.image.SpatialPoint`, so atlas queries use the same finite 3D
coordinate value as image and surface code.

```scala
val hits =
  atlas.query(
    Point3D(10.0, -20.0, 35.0),
    radiusMm = 2.0,
    fromSpace = SpaceId.MNI152
  )

val labels =
  hits.map(hit => (hit.id, hit.label, hit.distanceMm))
```

## Parcel Fields And Reduction

A parcel value is a locus4s `Field[P,A]` on the realization's exact parcel owner.
Scalar reduction preserves that owner in its return type; time-series reduction
preserves the realization's frame, spatial and parcel types. Generic field
mapping, zipping, restriction and transport remain locus4s operations.

```scala
import locus4s.data.Field
import scalafim.atlas.syntax.*

val values: Field[atlas.realization.P, Double] = atlas.reduce(statMap)
val series = atlas.reduceSeries(boldSeries, mask = Some(brainMask))
val atFirstTime: Field[atlas.realization.P, Double] =
  series.fieldAt(0).toOption.get
val displayRows = ParcelFields.records(atlas.realization)(values)
val summed = atlas.reduce(statMap, ParcelReducer.Sum)
```

`ParcelReducer.Mean` and `Sum` aggregate in one pass. `Custom` accepts a function
over a fresh array for each parcel/sample; callbacks may retain or mutate it.
Results are fully evaluated before returning. Callback exceptions become typed
`AtlasReductionError.CallbackFailed` errors in checked entry points.

`ParcelReductionPolicy` applies equally to scalar maps and series. `SkipNaN`
(default) excludes NaNs: an all-missing mean is NaN, a sum is zero, and a custom
reducer receives an empty array. `PropagateNaN` returns NaN without invoking a
custom callback. `RejectNaN` identifies the parcel and sample. NaNs outside the
assignment or mask are ignored; infinities remain values. No assigned samples
after masking uses `EmptyParcelPolicy.Fill` (default NaN) or `Reject`, separately
from all-missing data.

```scala
val checked = atlas.reduceEither(
  statMap,
  policy = ParcelReductionPolicy(
    MissingValuePolicy.RejectNaN,
    scalafim.image.EmptyParcelPolicy.Reject
  )
)
val portable = AtlasReduce.reduceField(surfaceAtlas.realization)(vertexValues)
```

For detached data, `ParcelFields.fromKeys(target)(entries)` admits full checked
`AtlasParcelKey` values and stores canonical order. `fromGlasserKeys` admits
checked anatomical `GlasserParcelKey` values. Normalize raw source aliases with
`GlasserParcelKey.fromSourceLabel` before admission. Duplicate detection precedes
unknown-row dropping; missing keys either fail or use `MissingParcelPolicy.Fill`.
Unknown keys either fail (default) or use `UnknownParcelPolicy.Drop`.

`fromSourceIds(target)(source, entries)` resolves numeric IDs through the declared
source realization's actual ID-to-key mapping before target alignment. Glasser
volume and surface IDs can encode opposite hemispheres, even with the same
canonical domain. The source mapping also handles arbitrary renumbering.
`alignTo(target)(field)` explicitly transports a field through exact persistent
ordered domain identity. Equal size or reordered keys do not establish identity.
`records` requires the same live parcel owner and derives metadata/display order;
input values never replace canonical atlas metadata.

## Expansion And Metric Persistence

`AtlasExpand.volume(atlas)(values, background)` (or `atlas.expand`) scatters a
`Field[atlas.realization.P, Double]` through the authoritative assignment. The
result is continuous and retains the exact grid, frame and stable scalar sample
owner. Background is explicit; a missing parcel value and an unassigned voxel
can therefore remain distinct. Categorical atlas labels are unchanged. Parcel
series expand through the existing `series.toDense(background)` operation.

```scala
val expanded = atlas.expand(values, background = Double.NaN)
val saved = for
  schema <- ParcelMetricSchema.from("parcel mean", Some("percent signal"))
  document <- ParcelMetricJson.encode(atlas.realization)(values, schema)
yield document

val restored = saved.flatMap(document =>
  ParcelMetricJson.decode(target.realization)(document))
// In Right(metric), metric.values is Field[target.realization.P, Double].
val savedAgain = restored.flatMap(metric =>
  ParcelMetricJson.encode(target.realization)(metric))
```

Metric V1 stores one Float64 scalar per canonical parcel with a checked measure
schema and attributes, ordered parcel identity, original atlas provenance and
metadata, and compact support/assignment fingerprint references. Decoding
validates the original key namespace and identity policy before matching the
target's exact canonical ordered domain. Glasser volume/surface ID conventions
and display order can differ; an equal-sized foreign domain is rejected. The
restored origin describes where the saved measure was computed. Restoring onto
another support does not recompute it there. The overload accepting a restored
metric preserves that origin; encoding a field and schema captures a new origin.

The format is `org.scalafim.atlas/parcel-metric/v1`. The JSON envelope contains
`format`, a JSON `payload` string, and its lowercase SHA-256 digest over exact
UTF-8 bytes. Payloads declare version 1 and `float64-ieee754-hex-v1`; each value
is exactly 16 lowercase IEEE bit digits. Signed zero, infinities, subnormals and
finite extremes survive; all NaNs use `7ff8000000000000`. Generic image expansion
preserves these values; the R expansion parity fixture covers finite and missing
values only. Unsupported formats, invalid origin, keys, value counts, encodings
or payload digests return typed errors.

Use `decode(target)(document, expectedSha256 = Some(retainedDigest))` when a
trusted external digest is available. The embedded digest detects corruption;
it does not authenticate authorship. Support and assignment references identify
originating assets without embedding geometry or verifying absent asset bytes.
The JVM `scalafim.atlas.io.ParcelMetricFiles` adapter reads UTF-8 and writes new
files with `CREATE_NEW`, refusing replacement. Shared codecs run on JVM and JS.

## Exact-grid Composition

Compose two volume realizations with required overlap and occlusion policies:

```scala
val first = atlas1.realization
val second = atlas2.realization
val composed = AtlasCompose.volume(
  first,
  second,
  AtlasCompositionOverlap.PreferSecond,
  AtlasCompositionOccluded.Drop
)
```

`Reject`, `PreferFirst` and `PreferSecond` decide overlapping membership.
`AtlasCompositionOccluded.Reject` fails if any parent parcel loses all support;
`Drop` removes it and records an undefined parent remap. Inputs must have exactly
congruent grids and identical declared template/coordinate spaces. Composition
does not resample.

The result retains the first realization's exact frame and voxel owner and owns
a fresh parcel domain. Its `firstRemap` and `secondRemap` are checked locus4s
`PartialMap`s from their respective parent parcel owners to the result. They
describe parcel correspondence; partially occluded fibers change. `result.atlas`
provides the usual facade. New positive IDs follow canonical parent order, while
display order follows each parent's presentation. Source IDs can collide or be
`Int.MaxValue` without offset arithmetic.

Canonical parent keys, retained ancestry, parent order and policies determine
the new namespace. Glasser renumbering/presentation changes preserve canonical
anatomy; conservative source-local identities remain source-local. Equal network
names are scoped by parent (`first:<length>:<name>`, `second:<length>:<name>`).
Cross-atlas network equivalence requires a separate declared grouping.

Both complete parent provenance records retain original metadata, display order,
support evidence and assignment digests. Publication and metric persistence
validate that evidence recursively. Compact assignment references identify
omitted spatial payloads; they do not independently prove their bytes or the
overlap computation.

## Overlap And Adjacency

```scala
val overlap =
  atlas.overlap(otherAtlas, AtlasAlignment.Exact)

val explicitlyAligned =
  atlas.overlap(
    otherAtlas,
    AtlasAlignment.NearestNeighborToFirst
  )

val edges =
  atlas.adjacency(VoxelConnectivity.Connect6)

val semanticRelation =
  RegionGraph.relation(atlas, VoxelConnectivity.Connect6).toOption.get

val weightedContacts =
  RegionGraph.contactCounts(atlas, VoxelConnectivity.Connect6)
```

Overlap rows include Dice, Jaccard, overlap counts, and both source region
sizes. Exact grid agreement is the default; resampling requires an explicit
alignment value. Parcel adjacency is an unweighted relation returned in an
`Either[RelationError, ParcelAdjacencyRelation]`, while boundary contact counts
are a separate weighted result.

## Parity Fixtures

The default Scala test suite includes typed provenance conversion/validation,
standard release summaries, local path/SHA-256 source completeness,
label-filtering derivation, non-contiguous region IDs, ROI metadata, query,
reduction, overlap, invalid metadata, and JVM loader `loadFromPaths` coverage
with synthetic NIfTI images.

An optional local R parity check compares the same fixture semantics against
the local `neuroatlas` checkout:

```sh
Rscript tools/r-parity/check_neuroatlas_atlas_parity.R
```

The script expects `pkgload`, `neuroim2`, and `~/code/neuroatlas` by default.
Set `NEUROATLAS_R=/path/to/neuroatlas` to use another checkout.

The shared expansion court consumes three coordinate-keyed fixtures generated
with `Rscript tools/r-parity/generate_atlas_parcel_expansion.R`. Its manifest
records the reference commit and actual source/generator/fixture hashes. Metric
interop tests consume documents produced by actual JVM and Scala.js executions;
their payload digests were independently checked using Python `hashlib`.

## Tests

`AtlasCompose.volume` composes assignments on an exact shared grid with explicit
overlap and occluded-parcel policies. `subsetEither` selects through those
assignments. `AtlasGrouping.hemisphere` and `.network` create typed parcel-to-group
maps; missing annotations are explicitly rejected, dropped, or assigned a group.
Grouped means choose equal-parcel or actual spatial-support weighting.

`AtlasDilation.volume` is a thin wrapper over the image assignment kernel. A
finite non-negative radius, grid/world Euclidean metric, tie policy and exact
destination mask are explicit. Existing seeds remain assigned and the mask must
contain them. World distances include the full grid affine, including shear.
Dilation records parent identity, radius bits and mask digest in provenance.

JVM pinned volume families are interpreted by `FslAtlasLoader`, `HcpExLoader`,
`OlsenMtlLoader` and `JulichVisualLoader`. FSL named requests currently cover
the verified 2mm/25% HarvardOxford cortical/subcortical and Jülich hard summaries.
HCPex v1.1 supports its native 1mm/2mm files. Olsen hippocampus is an exact
selection of the pinned MTL labels. Every load verifies source SHA-256 and
records executed sources. HCPex/Olsen headers declare scanner coordinates;
their source template claims remain descriptive, and realized coordinates are
bound to the pinned artifact with uncertain confidence. Standard-template
routing is deliberately unqualified for those files.

`SubcorticalAtlasLoader` uses pinned original native sources for unsplit CIT168,
HCP thalamic nuclei, MDTB10, and an exact hippocampus/amygdala selection from HCP
ROI labels. These requests do not claim the unavailable TemplateFlow/AtlasPack
harmonized grids or CIT168's derived hemisphere split. Scanner/aligned headers
retain artifact coordinate identities; the HCP ROI's template-coded header is
admitted strictly.

The pinned image4s revision supports MDTB10's native signed INT8 NIfTI datatype.
All four families have source-backed loading checks on their original grids;
source bytes are neither converted nor resampled. Coordinate-header limitations
and uncertain source-template claims remain explicit in the loaded provenance.

`StandardSurfaceAnnotationRequest` names the verified Schaefer 100/7 fsaverage6
pair and Kathryn Mills' Figshare v2 Glasser fsaverage projection.
`StandardSurfaceAtlasLoader` checks pins and maps hemisphere annotation codes to
canonical parcels; the Glasser projection preserves full anatomical keys.
The verified-geometry entry point also checks Schaefer white-surface pins.
Glasser geometry is explicitly caller-supplied; the fsaverage derivative makes
no claim of native fsLR equivalence. `SurfaceAtlasFields` reduces scalar fields
or timepoint fields on the exact bilateral assignment, with typed cortical-side
support selection.

Outer parcel-series/connectivity, batch and affine categorical transport
adapters live in [`atlas-workflows`](../atlas-workflows/README.md).

```sh
sbt atlasJVM/test
sbt atlasJS/test
sbt imageJVM/test
sbt imageJS/test
sbt surfaceJVM/test
sbt surfaceJS/test
```

The JVM tests cover loader IO. The shared tests run on both JVM and Scala.js.
