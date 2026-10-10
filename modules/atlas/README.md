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

- `AtlasRef` is `AtlasRefOf[R]` indexed by representation kind.
  `AtlasRef.volume`, `AtlasRef.surface`, and `AtlasRef.derived` return
  `VolumeAtlasRef`, `SurfaceAtlasRef`, and `DerivedAtlasRef`; their space
  parameters are typed by space kind (`VolumeOrUnknownSpaceId`,
  `SurfaceOrUnknownSpaceId`).
- `AtlasProvenance` is the typed audit trail behind `AtlasRef`: identity,
  spatial support, label schema, source artifacts, derivation steps, citations,
  confidence, and validation issues.
- `RegionId`, `Hemisphere`, `NetworkId`, `AtlasRegionMetadata`, and
  `RegionIndex` describe typed metadata. `AtlasRegionMetadata.checked` admits
  raw strings with typed `AtlasError`s; `typedLabel`, `typedFullLabel`, and
  `typedAttributes` expose the checked `RegionLabel` and `RegionAttributes`
  views. Extensional regions are `locus4s.Region`.
- `VolumeAtlas` owns one exact `VolumeAtlasRealization` and its spatial-to-parcel
  assignment. `labelVolume` is a derived dense materialization; source region
  IDs must match the non-zero payload IDs. `subset` keeps a non-empty set of
  regions and rebuilds the atlas on the same grid.
- `SurfaceAtlas` owns one bilateral `SurfaceAtlasRealization`, with retained
  geometry and label-table metadata. Vertex labels derive from its assignment.
  Single-side access takes `scalafim.surface.Hemisphere` and accepts only
  `Left` or `Right`.
- Every atlas `realization` carries a `parcelAssignment` (the only retained
  membership truth), parcel-indexed `metadata` and `parcelKeys`, an explicit
  display `Selection` (`displayOrder`), and an optional `networkAssignment`
  whose parcel-to-network map is a validated `Surjection`. Network regions are
  derived by composing the two maps.
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
`atlas.ref.coordSpace` unless the caller supplies `fromSpace`.

```scala
val ref: VolumeAtlasRef = Schaefer2018.default.atlasRef()
val metadata: Either[AtlasError, AtlasRegionMetadata] =
  AtlasRegionMetadata.checked(RegionId(1), "V1", labelFull = Some("L_V1_ROI"))
val label: Either[AtlasError, RegionLabel] = RegionLabel.from("V1")
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
on the exact persistent parcel domain. Each key combines a namespace (atlas
family, model, parcel variant, and release) with the integer `RegionId`, and
the domain is restored from a finite-indexed publication descriptor whose
fingerprint covers those keys.

`realization.assignmentAlignedTo(target)` retargets the assignment only through
exact persistent ordered domain identity; equal cardinality is not enough.
Different domains require an explicit locus4s `Bijection`
(`assignmentRetargeted`). `neuropublishProjection` and
`validateNeuropublishProjection` export and check the publication form.

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
import scalafim.surface.{Hemisphere as SurfaceHemisphere, VertexId}

val surfaceAtlas =
  SurfaceAtlas.fromLabeledSurfaces(
    ref = Schaefer2018Surface.default.atlasRef(),
    regions = regionIndex,
    left = leftLabels,
    right = rightLabels
  )

val region =
  surfaceAtlas.regionAt(SurfaceHemisphere.Left, VertexId(1024))

val contacts =
  surfaceAtlas.boundaryContacts(SurfaceHemisphere.Right)
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

## Standard Surface Routes

`StandardSurfaceRoutes` is the public way to obtain the qualified
MNI152NLin2009cAsym -> fsLR 32k volume-to-surface route. The atlas module
composes it from `scalafim.surface.reference` primitives; surface does not
depend on atlas. On the JVM, `StandardSurfaceRouteFiles` reads the locked
TemplateFlow assets from local caches (`TemplateFlowCache.roots`:
`$TEMPLATEFLOW_HOME`, `~/.cache/templateflow`, `~/Library/Caches/templateflow`)
and never downloads:

```scala
import scalafim.atlas.*
import scalafim.atlas.io.StandardSurfaceRouteFiles
import scalafim.surface.CorticalHemisphere
import scalafim.surface.reference.*

// `volume`: the consumer's DeclaredVolume, declared in Fslr32kFrom2009c.sourceFrame
val source = VolumeReference.make(Fslr32kFrom2009c.sourceFrame, volume.volume.space)

val mapped =
  for
    route <- StandardSurfaceRouteFiles.fsLR32kFrom2009c()   // StandardRoutePolicy.Frozen
    left  <- route.admit(source.toOption.get, CorticalHemisphere.Left)
  yield (route.identity.token, route.disclosure.fields, left.map(volume))
```

What it loads and checks (`Fslr32kFrom2009c` is the lock):

- fsLR 32k midthickness and `desc-nomedialwall` labels for both hemispheres
  at `templateflow@d79aacb1`, each digest-checked before decoding, and the
  templateflow4s point map under
  `.templateflow4s/derived/point-map/<sha256>` whose source must be the
  admitted `TemplateFlowXfm.Mni6ToMni2009c` composite and whose manifest must
  match the locked digest.
- The anatomy frame is `FrameBasis.PublisherMethods` (Van Essen et al. 2012,
  Cereb Cortex 22:2241, p. 2245: affine-aligned 69-subject Conte69 average in
  MNI152NLin6Asym). The bridge is the point map's pointwise inverse, so
  `BridgeExactness.Approximate(PointwiseFixedPoint(policy))`, with per-vertex
  residual, iteration and status receipts in each admitted route's
  `bridgePlacement`.
- `StandardRoutePolicy.Frozen` (reframe4s defaults: 1e-8 mm, 100 iterations,
  divergence ratio 4) is the policy P1-P6 passed under (ticket
  bd-01M3WCQD1MFW1WRTJP6C6A2ZFS). `StandardRoutePolicy.overriding(policy,
  reason)` is the explicit, reasoned alternative; it is labelled
  not budget-qualified and changes the route identity.
- Refusals are typed `StandardRouteRefusal`s: `AssetsMissing` (every missing
  locked path, zero-byte TemplateFlow placeholders included), `AssetRefused`
  (digest, decoding or declaration), `NotLocked` (an input that is not the
  locked asset), `PolicyRefused`, `Route` (the surface route's own refusal),
  and `PlacementGate` (any cortical vertex left unplaced by the inverse).

`admit` binds the route to one source grid, which must be declared in
`Fslr32kFrom2009c.sourceFrame` (MNI152NLin2009cAsym at the locked release);
`admitBoth` admits left then right. Shared composition from already-loaded
inputs is `StandardSurfaceRoutes.fsLR32kFrom2009c(pointMap, hemispheres,
policy)`; it runs on both platforms, but the medial-wall reader is JVM-only, so
assembling the real route from files is JVM-only today.

A consumer must persist and show, with every result and export:

- `route.identity.token` (`scalafim-route:sha256:...`), the SHA-256 of
  `route.identity.canonical`: locked asset digests, point-map manifest and
  stage digests, anatomy basis, bridge method and policy, mapping method and
  target mesh;
- `route.disclosure.fields`: route, identity, source and anatomy frames,
  anatomy basis, bridge and its exactness, policy and its qualification,
  method, every locked asset receipt, and the remaining limits (publisher
  frame declaration, not a registration proof; numerical inverse placements;
  source-volume provenance and consumer qualification belong to the consumer);
- each admitted route's `disclosure` (source grid, hemisphere, semantics,
  lookup, medial-wall asset) and, where vertices are inspected,
  `bridgePlacement` evidence.

The P1-P3, P5 and P6 budgets run through this entry point in
`Fslr32kRouteBudgetSuite`; P4 consumer fixtures are measured with
`sbt "atlasJVM/Test/runMain scalafim.atlas.reference.FslrBridgeDisagreement pointwise <spec.json> <out.json>"`,
and `scalafim.atlas.reference.Fslr32kRouteExample <templateflow-home> <receipt.json>`
is a consumer example. Real-asset suites skip when the locked cache is absent
unless `SCALAFIM_REQUIRE_REAL_ASSETS=1`.

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

## Parcel Reduction

`VolumeAtlas` summarizes a 3D map or a 4D time series by parcel. Inputs must be
on the atlas's exact grid. Every atlas region is kept in the output.

```scala
import scalafim.atlas.syntax.*
import scalafim.image.*

val values: ParcelValues = atlas.reduce(statMap)
val checked: Either[AtlasError, ParcelValues] = atlas.reduceEither(statMap)
val summed: ParcelValues = atlas.reduce(statMap, Reducers.sum)

val series: SomeScalarParcelSeries[Double] = atlas.reduce(boldSeries)
val maskedSeries =
  AtlasReduce.reduceSeries(atlas, boldSeries, mask = Some(brainMask))
```

`Reducers.mean` and `Reducers.sum` use a one-pass aggregation and skip NaN
samples; an all-NaN mean is NaN and an all-NaN sum is zero. Other reducers are
plain `Array[Double] => Double` functions over each parcel's samples. For
series, a parcel with no samples after masking follows
`EmptyParcelPolicy.Fill(Double.NaN)` (the default) or `EmptyParcelPolicy.Reject`.

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

## Signed INT8 Label Volumes

The pinned image4s revision (`2c0638fb`) decodes NIfTI datatype 256 (signed
INT8) as signed bytes; datatype 2 (UInt8) stays unsigned. `AtlasLabelMaps.readIntVolume`
therefore reads INT8 label volumes with 0 as background and 1..127 as region
IDs, and rejects a negative stored label with `IllegalArgumentException`.
`AtlasIoSuite` covers this on a synthetic INT8 file; `NiftiSuite` covers the
signed storage, boundaries, and scaling in the image adapter.

## Tests

```sh
sbt atlasJVM/test
sbt atlasJS/test
sbt imageJVM/test
sbt imageJS/test
sbt surfaceJVM/test
sbt surfaceJS/test
```

The JVM tests cover loader IO. The shared tests run on both JVM and Scala.js.
