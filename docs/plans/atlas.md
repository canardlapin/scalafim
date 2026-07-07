# Atlas - Standard Atlases And Parcel Workflows

`scalafim-atlas` is the typed atlas layer for ScalaFIM. It takes the useful
semantic center of `neuroatlas` and recasts it as immutable Scala 3 values:
atlas references, standard descriptors, region metadata, transform plans, and
parcel operations over `scalafim-image` payloads.

The module is not a plotting port. Visualization, Shiny-style exploration,
ggseg surfaces, and report rendering belong in later adapters. The atlas core
answers computational questions:

- Which standard atlas is this, and where did it come from?
- Which region contains this point?
- What is the parcel-wise summary of this volume or time series?
- How do two parcellations overlap?
- Which parcels touch each other?
- Which coordinate or surface transform route would be needed?

## Module Boundary

The shared module cross-compiles to JVM and Scala.js:

```scala
import scalafim.atlas.*
import scalafim.atlas.syntax.*
```

Shared code owns pure data and algorithms:

- `AtlasRef`, `AtlasArtifact`, `AtlasHistoryStep`
- `AtlasProvenance`, `SourceArtifact`, `SpatialSupport`, `LabelSchema`,
  `DerivationStep`, `Citation`, and provenance validation ADTs
- `RegionId`, `Hemisphere`, `NetworkId`, `Region`, `RegionIndex`
- `AtlasSpec`, `AtlasRegistry`
- `Schaefer2018`, `GlasserHcpMmp1`, `Schaefer2018Surface`,
  `GlasserHcpMmp1Surface`, and standard descriptor enums
- `SpaceTransforms`
- `VolumeAtlas`, `SurfaceAtlas`
- `AtlasQuery`, `AtlasReduce`, `AtlasOverlap`, `RegionGraph`

The JVM-only package owns file and network IO:

```scala
import scalafim.atlas.io.*
```

JVM IO includes `FileAtlasStore`, `AtlasLabelMaps`, `SchaeferLoader`, and
`GlasserLoader`. This split keeps browser/Scala.js builds free of filesystem
and download assumptions.

## Standard Descriptors

Descriptors are pure values that identify a standard atlas and its intended
source. They do not touch the filesystem.

```scala
val schaefer =
  Schaefer2018(
    parcels = SchaeferParcels.P400,
    networks = YeoNetworks.Seventeen,
    resolution = VoxelResolution.TwoMm
  )

val glasser =
  GlasserHcpMmp1(GlasserSource.Mni2009c)

val spec =
  AtlasRegistry.default("glasser360")
```

The current registry includes Schaefer, Glasser HCP-MMP1.0, Brainnetome, and
FreeSurfer ASEG volume families plus Schaefer and Glasser surface families.
The volume families have JVM loader entry points; ASEG uses built-in
neuroatlas-compatible metadata plus an explicit volume asset. Surface families
are represented as typed descriptors and shared payloads over already-loaded
`scalafim-surface` labels.

## Provenance Contract

`AtlasRef` remains the compact compatibility header: it names the atlas family,
model, representation, spaces, resolution/density, source, lineage, confidence,
artifacts, and legacy history. `AtlasProvenance` is the stricter typed audit
layer exposed by every `Atlas`.

```scala
val provenance: AtlasProvenance =
  atlas.provenance

val sources: Vector[SourceArtifact] =
  provenance.sourceArtifacts

val issues: Vector[ProvenanceIssue] =
  provenance.validate(strict = true)

val report: String =
  provenance.summary
```

The typed model has these pieces:

- `AtlasIdentity`: family, model, optional variant, and optional release.
- `SpatialSupport`: volume support with template/coordinate space and voxel
  size; surface support with template space, density, and hemisphere coverage;
  or derived support.
- `SourceArtifact`: source name/ref/URI/local path plus typed `ArtifactRole`,
  optional DOI, license, and digest.
- `LicenseInfo`: known license, restricted terms, explicitly unspecified
  upstream terms, or missing license accounting.
- `LabelSchema`: label encoding, background value, sorted region IDs, and the
  label-table artifact when known.
- `DerivationStep`: loaded artifacts, parsed/validated/filtered labels,
  resampling, volume/surface projection, and legacy load history.
- `Citation`: DOI/text citation values extracted from source artifacts.

Loaders construct `AtlasProvenance` from the same metadata that populates
`AtlasRef`. They also attach the concrete local path and SHA-256 digest for
files passed through `loadFromPaths` or resolved through an `AtlasStore`, and
record label filtering. For example, synthetic loader tests that include more
LUT rows than payload labels now produce a `FilteredLabels(kept, dropped)`
derivation step.

Standard provenance uses conservative release accounting: Schaefer2018,
HCP-MMP1.0, Brainnetome 246, and the bundled FreeSurfer ASEG source each have a
release label, with version and commit reported when known and explicitly shown
as unknown otherwise. License accounting follows the same rule: known terms,
restricted terms, and explicitly unspecified upstream terms are distinct typed
states. Strict validation reports truly missing license accounting, not
honest unknowns.

`summaryLines` and `summary` provide readable audit output for logs, reports,
and test failures:

```scala
atlas.provenance.summaryLines.foreach(println)
```

Descriptor-only surface atlas values remain usable but validate with explicit
audit issues such as `NoLoadedArtifact` or `MissingDigest` under strict mode.

This gives a Scala 3 provenance style where the vague parts are values, not
comments: artifact roles are enums, spatial support is an enum, label schemas
are typed records, and validation returns structured issues.

## Surface Payloads

`SurfaceAtlas` is the surface counterpart to `VolumeAtlas`. It carries an
`AtlasRef`, a `RegionIndex`, and a bilateral `HemispherePair[LabeledSurface]`
from `scalafim-surface`.

```scala
import scalafim.atlas.*
import scalafim.surface.{Hemisphere as SurfaceHemisphere, *}

val ref =
  Schaefer2018Surface(
    parcels = SchaeferParcels.P400,
    networks = YeoNetworks.Seventeen,
    surface = StandardSurface.FsAverage6
  ).atlasRef()

val atlas =
  SurfaceAtlas.fromLabeledSurfaces(ref, regions, leftLabels, rightLabels)

val label =
  atlas.labelIdAt(SurfaceHemisphere.Left, VertexId(100))

val contacts =
  atlas.boundaryContacts(SurfaceHemisphere.Right)
```

The contract is intentionally strict:

- left and right payloads must use left and right surface geometries;
- label `0` is background;
- every non-zero vertex label must appear in `RegionIndex`;
- every region in `RegionIndex` must be present in the bilateral payload;
- label-table IDs must not introduce unknown regions.

The shared layer does not parse annotation, CIFTI, or FreeSurfer annotation
files. JVM GIFTI adapters now cover typed GIFTI payloads, label tables, and
left/right label payload loading into `SurfaceAtlas`.

## Loading Policy

Atlas loading is explicit about cache and network policy.

```scala
import java.nio.file.Path
import scalafim.atlas.*
import scalafim.atlas.io.*

val store = FileAtlasStore(Path.of("atlas-cache"))

val atlas =
  SchaeferLoader.load(
    Schaefer2018.default,
    store = store,
    policy = AssetPolicy.CacheOnly
  )
```

Use `CacheOnly` for reproducible pipelines. Use `CacheOrDownload` only where a
runtime download is acceptable. `FileAtlasStore` validates minimum byte size and
optional SHA-256 hashes before returning a path.

When files come from TemplateFlow, a workflow manager, a test fixture, or a
local data release, use explicit paths:

```scala
val atlas =
  GlasserLoader.loadFromPaths(
    GlasserHcpMmp1(GlasserSource.Mni2009c),
    volumePath = Path.of("MMP_in_MNI_corr.nii.gz"),
    labelPath = Path.of("glasser360NodeNames.txt")
  )
```

`AtlasLabelMaps` converts finite integer-valued NIfTI volumes to `NeuroVol[Int]`
and rejects fractional labelmaps. `VolumeAtlas.fromLabelVolume` then checks that
metadata IDs and non-zero payload IDs match exactly.

## Point Query

`AtlasQuery` maps world coordinates to region metadata. The syntax import adds
the direct `atlas.query(...)` form.

```scala
val exact =
  atlas.query(Point3D(0.0, 0.0, 0.0)).head

val nearby =
  atlas.query(
    Point3D(10.0, -20.0, 35.0),
    radiusMm = 3.0,
    fromSpace = SpaceId.MNI152
  )
```

For exact lookup, background returns a hit with no region. For radius lookup,
one hit is returned per nearest region found within the search radius.

## Parcel Reduction

The atlas can summarize a 3D image:

```scala
val parcelValues =
  atlas.reduce(statMap)

val region10 =
  parcelValues.value(RegionId(10))
```

It can also summarize every time point in a 4D series:

```scala
val parcelSeries =
  atlas.reduce(boldSeries)

val maskedSeries =
  AtlasReduce.reduceVec(
    atlas,
    boldSeries,
    mask = Some(brainMask),
    reducer = Reducers.mean
  )
```

The output keeps every atlas parcel even when a mask removes all voxels from a
parcel. Empty parcel/time cells are `NaN`. Non-contiguous atlas IDs remain
metadata IDs; they are not used as direct dense matrix column indices.

## Overlap And Graphs

Overlap compares two `VolumeAtlas` payloads:

```scala
val rows =
  atlas.overlap(other, resample = false)
```

Each row carries the source regions, Dice, Jaccard, overlap count, and both
region sizes. If dimensions differ and `resample = true`, the second atlas is
nearest-neighbor resampled into the first atlas space.

Adjacency builds a parcel graph from shared voxel boundaries:

```scala
val edges =
  atlas.adjacency(VoxelConnectivity.Connect6)
```

The edge weight is the number of neighboring voxel pairs observed between two
regions.

## Transform Planning

`SpaceTransforms` stores the known transform graph. Identity and MNI305/MNI152
affine routes are executable for point coordinates. TemplateFlow nonlinear
warps, fsaverage/fsLR sphere routes, and volume/surface bridges are represented
as planned routes until their IO and execution backends land.

```scala
val plan =
  SpaceTransforms.plan(
    from = SpaceId.MNI152NLin6Asym,
    to = SpaceId.MNI152NLin2009cAsym
  )

val transformed =
  SpaceTransforms.transformCoords(
    Vector(Point3D(10.0, -20.0, 35.0)),
    from = SpaceId.MNI305,
    to = SpaceId.MNI152
  )
```

This makes missing execution support visible in the type-level plan instead of
performing hidden downloads or approximate transforms.

For volume/surface bridges, use the narrower planning type:

```scala
val toSurface =
  VolumeSurfaceTransformPlan.volumeToSurface(
    fromVolumeSpace = SpaceId.MNI152NLin6Asym,
    toSurfaceSpace = SpaceId.FsAverage
  )

val toVolume =
  VolumeSurfaceTransformPlan.surfaceToVolume(
    fromSurfaceSpace = SpaceId.FsAverage,
    toVolumeSpace = SpaceId.MNI152NLin2009cAsym
  )
```

These plans must include a `VolToSurf` or `SurfToVol` step. They expose route
status, confidence, warnings, and sampling intent, but they do not execute
projection, ribbon filling, Workbench sphere resampling, or TemplateFlow warps.

## Parity And Verification

The deterministic Scala parity corpus lives in:

- `modules/atlas/shared/src/test/scala/scalafim/atlas/fixtures/AtlasParityFixtures.scala`
- `modules/atlas/shared/src/test/scala/scalafim/atlas/AtlasParityCorpusSuite.scala`
- `modules/atlas/jvm/src/test/scala/scalafim/atlas/io/AtlasIoSuite.scala`

It covers:

- typed provenance conversion, validation, and label-filtering derivation
- standard release/version/commit summaries
- local path, SHA-256, and license-status completeness for JVM loaders
- non-contiguous region IDs
- ROI metadata preservation
- exact/radius point query behavior
- parcel reduction over volumes and time series
- `NaN` output for masked-away parcels
- Dice/Jaccard overlap values
- duplicate metadata ID rejection
- unknown payload label rejection
- bilateral surface atlas validation and per-hemisphere parcel contacts
- explicit planned volume/surface transform routes
- JVM `loadFromPaths` coverage using synthetic NIfTI fixtures

Run the Scala checks:

```sh
sbt atlasJVM/test atlasJS/test surfaceJVM/test surfaceJS/test
```

Run the optional local R comparison against `neuroatlas`:

```sh
Rscript tools/r-parity/check_neuroatlas_atlas_parity.R
```

The R script uses `~/code/neuroatlas` by default and can be redirected with
`NEUROATLAS_R=/path/to/neuroatlas`.

## Remaining Atlas Work

The next atlas implementation tasks should be separate tracker children rather
than hidden inside docs:

- Optional live-download smoke checks outside the default suite.
- Optional TemplateFlow asset resolution and nonlinear warp execution.
- JVM FreeSurfer annotation and CIFTI loaders for real surface-atlas assets.
- Optional `ExternalFileBinary` GIFTI payload support where a caller supplies
  explicit sidecar files and digest policy.
