# scalafim-surface

`scalafim-surface` is ScalaFIM's renderer-free neuroimaging layer over mesh4s
triangle topology and geometry. It provides typed compatibility APIs, exact
locus4s-owned vertex and edge data, pure surface algorithms, cross-platform
GIFTI ingestion, and JVM-only FreeSurfer readers.

```scala
import scalafim.surface.*
```

Platform readers live in a platform package:

```scala
import scalafim.surface.io.*
```

## Scope

The shared module cross-compiles to JVM and Scala.js and contains:

- `VertexId` and `FaceId` opaque ids over zero-based mesh indices.
- `TriangleMesh`, a compatibility facade over one mesh4s `TriangleTopology`
  and one `SurfaceRealization`, plus `Point3D` (an alias for
  `scalafim.image.SpatialPoint`), `Triangle`, and `SurfaceGeometry`.
- `Hemisphere`, `SurfaceKind`, `SurfaceSet`, and `HemispherePair`.
- `MeshTopology`, a compatibility view that delegates incidence, boundaries,
  components, and Euler characteristic to mesh4s. Its named
  `primalGraphProjection` retains exact mesh-edge correspondence.
- Vertex-indexed facades (`SurfaceField`, `SurfaceMatrix`, `SurfaceRoi`, and
  `LabeledSurface`) backed by locus4s fields, selections, regions, and sections
  over the exact `topology.vertices` owner.
- `SurfaceEdgeWeights`, a finite non-negative field over the exact
  `topology.edges` owner. The former lexicographic sequence order is available
  only through an endpoint-checked compatibility permutation.
- `VolToSurfMorphism` and `SurfToSurfMorphism` wrappers for reusable
  volume-to-surface sampling plans and explicit surface-to-surface vertex maps.
- Pure algorithms for connected components, thresholded clusters, edge-graph
  shortest-path and spherical neighborhoods, parcel representatives, parcel
  distances, and parcel boundary contacts. Edge-graph distance is not a
  continuous surface geodesic.

The platform modules add matching GIFTI APIs:

- `GiftiReader` for typed GIFTI documents, metadata, label tables,
  coordinate-system transforms, and ASCII/Base64/GZip-Base64 `DataArray`
  payloads. Decoded payloads expose `GiftiVector` and `GiftiMatrix` views so
  row-major and column-major indexing are explicit.
- `GiftiSurfaceReader` adapters from GIFTI POINTSET/TRIANGLE geometry and
  LABEL/NODE_INDEX data into `SurfaceGeometry` and `LabeledSurface`.
- JVM entry points read `Path` values synchronously, including outer
  `.gii.gz` files. Scala.js entry points read `Uint8Array` values
  asynchronously, accept raw `.gii` or gzip-wrapped bytes, and work in browser
  windows and workers through the standard `DecompressionStream` boundary.

The JVM module additionally adds:

- `FreeSurferSurfaceReader` for FreeSurfer/SUMA ASCII and binary triangle
  geometry.

## Ownership boundary

- mesh4s owns `TriangleTopology`, all cell domains and incidence, topology
  audits, connectivity fingerprints, `SurfaceRealization`, intrinsic metric
  primitives, and the optional graph4s projection.
- locus4s owns fields, regions, selections, sections, and typed maps over those
  exact mesh cell domains.
- ScalaFIM owns GIFTI and FreeSurfer ingestion, hemisphere and `SurfaceKind`,
  `surfaceToWorld`, RAS+ behavior, surface/volume sampling, atlases, scene
  compatibility, picking contracts, and named numerical policy.
- Surface ingestion or compilation may cache a renderer-local packed coordinate
  and index rendition. It remains a derived view of the typed surface. The
  module does not define a portable mesh byte format.

## Conventions

Faces and vertices are zero-based internally. FreeSurfer ASCII, FreeSurfer
binary, and GIFTI triangle indices are preserved as zero-based indices when
loaded into `TriangleMesh`.

`TriangleMesh` owns one mesh4s topology and one coordinate realization.
`MeshTopology` delegates to that owner; it does not store another edge table or
neighbor matrix. `MeshTopology.primalGraphProjection` provides the explicitly
named topology-forgetting graph4s view with total vertex and edge
correspondence. The older `toGraph` result is a transient compatibility value.
Thresholded components share one surface-local filtered traversal rather than
allocating induced graphs per field.

Vertex and edge counts or matching connectivity fingerprints do not authorize
data reuse. Fields, labels, mappings, masks, and custom weights must carry the
same runtime mesh owner. Structural comparison belongs at ingestion, where a
coordinate realization can be checked and rebound to the canonical topology.

`SurfaceGeometry.surfaceToWorld` stores a 4x4 affine. Readers use identity when
the source format has no usable transform.

## Examples

Build a mesh and topology:

```scala
val mesh =
  TriangleMesh.fromRows(
    vertices = Vector(
      Vector(0.0, 0.0, 0.0),
      Vector(1.0, 0.0, 0.0),
      Vector(0.0, 1.0, 0.0)
    ),
    faces = Vector((0, 1, 2))
  )

val geometry = SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.Pial)
val topology = MeshTopology.from(mesh)
val neighborsOfZero = topology.neighborsOf(VertexId(0))
```

Attach vertex data:

```scala
val field =
  SurfaceField.full(
    geometry,
    data = Vector(0.2, 1.5, -0.7),
    label = "activation"
  )

val roi = SurfaceRoi.fromField(field, Vector(VertexId(1), VertexId(2)), "roi")
```

Query shortest-path neighborhoods along mesh edges:

```scala
val hits =
  SurfaceGeodesics.neighborsWithin(
    topology,
    radius = 2.0,
    sources = Vector(VertexId(0)),
    metric = DistanceMetric.EdgeGraphShortestPath
  )
```

Custom weights use the mesh4s edge-domain order:

```scala
val weightedHits =
  SurfaceEdgeWeights
    .fromTopologyOrder(topology, Vector(1.0, 1.0, 1.0))
    .map: weights =>
      SurfaceGeodesics.neighborsWithin(
        topology,
        radius = 1.0,
        sources = Vector(VertexId(0)),
        edgeWeights = Some(weights)
      )
// Either[SurfaceEdgeWeightError, Vector[NeighborHit]]
```

For old data stored in ScalaFIM's lexicographic endpoint order, call
`SurfaceEdgeWeights.fromLegacyLexicographic`. It matches values by endpoints
before creating the typed field; it never treats the old positions as mesh4s
edge ordinals.

Work with parcels:

```scala
val labels =
  LabeledSurface.fromIndexed(
    geometry,
    indices = Vector(VertexId(0), VertexId(1), VertexId(2)),
    labels = Vector(1, 1, 2),
    table = Vector(LabelInfo(1, "A"), LabelInfo(2, "B"))
  )

val parcels = SurfaceParcels.units(labels, topology)
val contacts = SurfaceParcels.boundaryContacts(parcels, topology)
```

Load geometry on the JVM:

```scala
val fs = FreeSurferSurfaceReader.read(java.nio.file.Path.of("lh.white"))
val gii = GiftiSurfaceReader.read(java.nio.file.Path.of("sub-01_hemi-L_pial.surf.gii"))
```

Load raw GIFTI bytes in Scala.js without choosing a serialized mesh format:

```scala
import scala.scalajs.js.typedarray.Uint8Array

def decode(bytes: Uint8Array) =
  GiftiSurfaceReader.read(
    bytes,
    Hemisphere.Left,
    SurfaceKind.Pial
  )
// Future[Either[GiftiError, SurfaceGeometry]]
```

The asynchronous boundary covers gzip/zlib inflation. The returned
`SurfaceGeometry` retains the GIFTI coordinate-system transform, so the
renderer or ingestion worker can choose its own transfer rendition.

Parse GIFTI explicitly when you need provenance-like payload inspection before
building a surface value:

```scala
val doc = GiftiReader.read(java.nio.file.Path.of("lh.aparc.label.gii"))
val labels = doc.flatMap(GiftiSurfaceReader.labeledSurface(_, gii, "aparc"))
val matrix = doc.flatMap(_.pointSet.toRight(GiftiError.MissingDataArray(GiftiIntent.PointSet))).flatMap(GiftiReader.doubleMatrix)
```

Compiled runnable examples live in
[`../../examples/surface-jvm`](../../examples/surface-jvm). They cover
FreeSurfer/GIFTI IO and labeled-surface parcel workflows:

```sh
sbt surfaceExamplesJVM/test
sbt "surfaceExamplesJVM/runMain scalafim.examples.surface.inspectExampleSurfaces"
sbt "surfaceExamplesJVM/runMain scalafim.examples.surface.summarizeSurfaceParcels"
```

## Migration Stance

This is not an S4 or plotting port. The target mapping from `neurosurf` is:

- `SurfaceGeometry` style mesh metadata -> `TriangleMesh` plus
  `SurfaceGeometry`.
- neighbor/graph helpers -> the `MeshTopology` compatibility view over mesh4s.
- vertex data, ROI, and label containers -> `SurfaceField`, `SurfaceMatrix`,
  `SurfaceRoi`, and `LabeledSurface`.
- cluster, neighborhood, edge-graph distance, and parcel operations -> pure
  functions over the typed model.
- GIFTI geometry/label readers -> JVM and Scala.js platform IO; FreeSurfer
  geometry readers -> JVM-only IO.

Non-goals for this module:

- RGL, htmlwidgets, screenshots, camera state, and interactive viewers.
- Color-map rendering classes.
- Mutable graph-library objects as public data structures.
- A portable mesh serialization, arbitrary cell complexes, continuous surface
  geodesics, FEM, DEC, or curvature systems.

## Verification

Run the surface module directly:

```sh
sbt surfaceJVM/test
sbt surfaceJS/test
sbt testFullOptScalafimJS
```

The deterministic fixture corpus is documented in
`tools/r-parity/surface-fixtures.md`; JVM IO resources are under
`modules/surface/jvm/src/test/resources/surface/`.
