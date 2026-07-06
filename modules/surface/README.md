# scalafim-surface

`scalafim-surface` is the renderer-free surface-mesh layer for ScalaFIM. It
ports the useful data-structure and algorithmic ideas from `neurosurf` into a
Scala 3 shape: typed ids, immutable values, explicit topology views, pure
algorithms, and JVM-only file readers.

```scala
import scalafim.surface.*
```

JVM file readers live in a platform package:

```scala
import scalafim.surface.io.*
```

## Scope

The shared module cross-compiles to JVM and Scala.js and contains:

- `VertexId` and `FaceId` opaque ids over zero-based mesh indices.
- `TriangleMesh`, `Point3D` (an alias for `scalafim.image.SpatialPoint`),
  `Triangle`, and `SurfaceGeometry`.
- `Hemisphere`, `SurfaceKind`, `SurfaceSet`, and `HemispherePair`.
- `MeshTopology` derived from triangle faces: edges, neighbors, edge lengths,
  face areas, normals, and Euler characteristic.
- Vertex-indexed containers: `SurfaceField`, `SurfaceMatrix`, `SurfaceRoi`,
  and `LabeledSurface`.
- `VolToSurfMorphism` and `SurfToSurfMorphism` wrappers for reusable
  volume-to-surface sampling plans and explicit surface-to-surface vertex maps.
- Pure algorithms for connected components, thresholded clusters, geodesic and
  spherical neighborhoods, parcel representatives, parcel distances, and parcel
  boundary contacts.

The JVM module adds:

- `FreeSurferSurfaceReader` for FreeSurfer/SUMA ASCII and binary triangle
  geometry.
- `GiftiSurfaceReader` for ASCII GIFTI POINTSET/TRIANGLE geometry and
  POINTSET affine extraction.

## Conventions

Faces and vertices are zero-based internally. FreeSurfer ASCII, FreeSurfer
binary, and GIFTI triangle indices are preserved as zero-based indices when
loaded into `TriangleMesh`.

`TriangleMesh` stores only geometry. `MeshTopology` is a derived view over the
mesh, so graph-like queries do not become a second source of truth.

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
val neighborsOfZero = topology.neighbors(VertexId(0).index)
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

Query geodesic neighborhoods:

```scala
val hits =
  SurfaceGeodesics.neighborsWithin(
    topology,
    radius = 2.0,
    sources = Vector(VertexId(0)),
    metric = DistanceMetric.Geodesic
  )
```

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
- neighbor/graph helpers -> `MeshTopology`.
- vertex data, ROI, and label containers -> `SurfaceField`, `SurfaceMatrix`,
  `SurfaceRoi`, and `LabeledSurface`.
- cluster, neighborhood, geodesic, and parcel operations -> pure functions over
  the typed model.
- FreeSurfer and GIFTI geometry readers -> JVM-only IO.

Non-goals for this module:

- RGL, htmlwidgets, screenshots, camera state, and interactive viewers.
- Color-map rendering classes.
- Mutable graph-library objects as public data structures.
- Volume-to-surface projection until it can reuse `scalafim-image` spaces and
  affine transforms cleanly.

## Verification

Run the surface module directly:

```sh
sbt surfaceJVM/test
sbt surfaceJS/test
```

The deterministic fixture corpus is documented in
`tools/r-parity/surface-fixtures.md`; JVM IO resources are under
`modules/surface/jvm/src/test/resources/surface/`.
