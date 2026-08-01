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

- `AtlasRef`, `AtlasArtifact`, and `AtlasHistoryStep` describe atlas identity,
  source files, citations, and transform/load provenance.
- `AtlasProvenance` is the typed audit trail behind `AtlasRef`: identity,
  spatial support, label schema, source artifacts, derivation steps, citations,
  confidence, and validation issues.
- `RegionId`, `Hemisphere`, `NetworkId`, `AtlasRegionMetadata`, and
  `RegionIndex` replace ad hoc atlas list fields with typed metadata. The old
  atlas `Region` name is a deprecated compatibility alias; extensional regions
  are `scalafim.locus.Region`.
- `VolumeAtlas` wraps a `ClusteredNeuroVol`, enforces that metadata region IDs
  match the non-zero payload IDs, and exposes its labels as a typed
  `Parcellation`.
- `SurfaceAtlas` wraps bilateral `LabeledSurface` payloads from
  `scalafim-surface` and enforces that non-zero vertex labels match the region
  metadata IDs. It exposes the same quotient-level API as `VolumeAtlas`.
- Every atlas `quotient` carries parcel-indexed metadata, an explicit display
  `Selection`, and an optional validated parcel-to-network `Surjection`.
  Network regions are derived from quotient composition.
- `AtlasRegistry` and `AtlasSpec` provide immutable discovery for known atlas
  families and aliases.
- `Schaefer2018`, `GlasserHcpMmp1`, `Schaefer2018Surface`, and
  `GlasserHcpMmp1Surface` provide typed standard-atlas descriptors.
- `SpaceTransforms` is a route planner over known coordinate/template-space
  steps. Executable affine routes can transform points and lower to
  `scalafim.image.SpatialMorphism` pullback values; planned nonlinear and
  surface routes are represented explicitly but not silently executed.

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
  including template space, coordinate space, voxel size, surface density, and
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
import scalafim.surface.{Hemisphere as SurfaceHemisphere, *}

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
steps. `scalafim-atlas` does not execute Workbench, TemplateFlow, or projection
backends from the shared core.

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

`VolumeAtlas` can summarize a 3D map or a 4D time series by parcel. The shared
core keeps all atlas regions in the output. If a mask removes every voxel from a
region, the region is retained and its value is `NaN`.

```scala
import scalafim.image.*

val values: ParcelValues =
  atlas.reduce(statMap)

val series: ClusteredNeuroVec[Double] =
  atlas.reduce(boldSeries)

val maskedSeries =
  AtlasReduce.reduceVec(atlas, boldSeries, mask = Some(brainMask))
```

The standard mean and sum reducers use a one-pass quotient aggregation. Custom
compatibility reducers remain plain functions over parcel voxels:

```scala
val summed =
  atlas.reduce(statMap, reducer = Reducers.sum)
```

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
  RegionGraph.relation(atlas, VoxelConnectivity.Connect6)

val weightedContacts =
  RegionGraph.contactCounts(atlas, VoxelConnectivity.Connect6)
```

Overlap rows include Dice, Jaccard, overlap counts, and both source region
sizes. Exact grid agreement is the default; resampling requires an explicit
alignment value. Parcel adjacency is an unweighted relation, while boundary
contact counts are a separate weighted result.

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

## Tests

```sh
sbt atlasJVM/test
sbt atlasJS/test
sbt surfaceJVM/test
sbt surfaceJS/test
```

The JVM tests cover loader IO. The shared tests run on both JVM and Scala.js.
