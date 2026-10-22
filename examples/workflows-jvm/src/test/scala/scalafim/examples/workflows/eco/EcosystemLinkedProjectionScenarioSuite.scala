package scalafim.examples.workflows.eco

import image4s.geometry.{Affine, D3}
import intaglio.*
import neuroim4s.image.*
import neuroim4s.image.SampleSpaces.*
import neuroim4s.image.world.*
import neuroim4s.spaces.*
import neurosurf4s.surface.*
import neurosurf4s.surface.view.*
import neurosurf4s.surface.view.raster.*
import reframe4s.core.WorldLink
import scalafim.scenarios.ScenarioHarness

/** Synthetic shared-world projection and software raster evidence; no native or WebGL claim. */
class EcosystemLinkedProjectionScenarioSuite extends munit.FunSuite:
  private def close(actual: Double, expected: Double): Boolean =
    actual.isFinite && expected.isFinite && math.abs(actual - expected) <= 1e-12

  private def closeValues(actual: Seq[Double], expected: Seq[Double]): Boolean =
    actual.length == expected.length && actual.zip(expected).forall((a, e) => close(a, e))

  test("volume projection reaches a displayed pick and returns through the shared-world cursor"):
    val world = WorldSpace.declare("eco-projection-world").toOption.get
    val otherWorld = WorldSpace.declare("eco-other-world").toOption.get
    val surfaceFrame = FrameCatalog.frame(world)
    val volumeFrame = FrameCatalog.frame(world)
    val otherFrame = FrameCatalog.frame(otherWorld)
    val unplacedSpace = SampleSpaces(Vector(5, 5, 3), origin = Some(Vector(-1.0, -1.0, 0.0)))
    // This is the fixture author's explicit assertion that the sampling grid and mesh share a world.
    val space = SampleSpaces.inWorld(unplacedSpace, world).toOption.get
    val volume = SomeScalarVolume.unsafeCopyFromCanonicalArray(
      Array.tabulate[Double](75): index =>
        val grid = Indexing.indexToGrid3D(space.grid.shape, index)
        grid(0).toDouble + 10.0 * grid(1).toDouble + 100.0 * grid(2).toDouble,
      space, "synthetic-asymmetric-volume")
    def geometry(z: Double, kind: SurfaceKind, faces: Vector[(Int, Int, Int)]): SurfaceGeometry =
      SurfaceGeometry(TriangleMesh.fromRows(
        Vector(Vector(-1.0, -1.0, z), Vector(1.0, -1.0, z), Vector(0.0, 2.0, z)), faces),
        Hemisphere.Left, kind)
    val faces = Vector((2, 0, 1))
    val white = geometry(0.0, SurfaceKind.White, faces)
    val pial = geometry(2.0, SurfaceKind.Pial, faces)
    val framed = FramedSurface.in(surfaceFrame)(white, SurfacePlacement.SurfaceToWorld).toOption.get
    val link = WorldLink.shared[surfaceFrame.type, volumeFrame.type, D3](surfaceFrame, volumeFrame).toOption.get
    val surfaceId = SurfaceId.unsafe("left")
    val cursor = SurfaceVolumeCursor.make(surfaceId, framed, link).toOption.get
    val gridAffine = Affine.fromRowMajor[D3](Vector(1.0,0,0,-1, 0,1,0,-1, 0,0,1,0, 0,0,0,1)).toOption.get
    val grid = GridSpec.in(volumeFrame)(SpatialDims(5, 5, 3), gridAffine).toOption.get
    val pair = SurfaceGeometryPair(white, pial)
    val morphism = VolToSurfMorphism(SurfaceDomainId("volume"), SurfaceDomainId("left"),
      VolumeSurfaceSamplingPlan(pair, SurfaceSamplingPath.Midpoint, SurfaceSampleAggregation.Nearest))
    val projection = SurfaceVolumeProjection.materialize(morphism, volume)
    val palette = Vector(Rgba32.unsafe(255,0,0), Rgba32.unsafe(0,255,0), Rgba32.unsafe(0,0,255))
    val colorizer = new Colorizer[Double]:
      def color(value: Double): Rgba32 =
        if close(value, 100.0) then palette(0) else if close(value, 102.0) then palette(1) else palette(2)
    val layerId = SurfaceLayerId.unsafe("projected")
    val layer = SurfaceVolumeProjection.scalarLayer(projection, layerId, surfaceId, white, colorizer).toOption.get
    val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(surfaceId, white).toOption.get), Vector(layer)).toOption.get
    val state = SurfaceViewer.reduce(model, SurfaceViewerState.initial(model),
      SurfaceViewerAction.SetLighting(SurfaceLighting.Unlit)).toOption.get
    val plan = SurfaceCompiler.compile(model, state).toOption.get
    val camera = SurfaceReadbackScenario.centeredCamera(surfaceFrame)
    val rendered = SurfaceRasterizer.render(plan.copy(camera = camera), RasterDimensions.unsafe(129, 129),
      SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
    // The raster samples pixel centers on a 129-wide viewport. At (56,72),
    // original-vertex weights are (193/387,97/387,97/387): a unique vertex 0,
    // with rounded RGB (127,64,64), independent of centroid tie arithmetic.
    val hit = rendered.pick(56, 72).toOption.flatten.get
    val selection = SurfaceSelection(hit.surface, VertexId(hit.vertex))
    val volumePoint = cursor.toVolume(selection).toOption.get
    val roundtrip = cursor.toSurface(volumePoint, SurfaceLinkRadius.unsafe(0.0)).toOption.get
    val wrongTopology = geometry(0.0, SurfaceKind.White, Vector((0, 2, 1)))
    val badLink = WorldLink.shared[surfaceFrame.type, otherFrame.type, D3](surfaceFrame, otherFrame)
    val result = ScenarioHarness.result("ecosystem-linked-volume-projection", Vector(
      ScenarioHarness.fact("world-and-placement", SampleSpaces.worldOf(space) == Right(world) && framed.world == Right(world) &&
        closeValues(space.grid.indexToFrame.rowMajor, gridAffine.rowMajor) &&
        closeValues(framed.vertex(VertexId(0)).toOption.get.coordinates, Vector(-1.0,-1.0,0.0)),
        "fixture-declared shared world, SurfaceToWorld placement, matching grid affine"),
      ScenarioHarness.fact("projected-values", projection.values.vertexIds.map(_.index) == Vector(0,1,2) &&
        closeValues(projection.values.data.toVector, Vector(100.0,102.0,131.0)) &&
        projection.receipt.requestedSamples == 3L && projection.receipt.acceptedSamples == 3L &&
        projection.receipt.qualifiedVertices == 3, "midpoint voxel values in vertex order: 100, 102, 131"),
      ScenarioHarness.fact("pixel-and-pick", rendered.image.pixelUnsafe(56,72) == Rgba32.unsafe(127,64,64) &&
        hit.surface == surfaceId && hit.face == 0 && hit.vertex == 0,
        "checked orthographic camera at (56,72): RGB 127,64,64; face 0, unique vertex 0"),
      ScenarioHarness.fact("actual-pick-cursor", closeValues(volumePoint.coordinates, Vector(-1.0,-1.0,0.0)) &&
        grid.voxelAt(volumePoint).exists(v => closeValues(Vector(v.x, v.y, v.z), Vector(0.0,0.0,0.0))) && roundtrip.selection == selection &&
        closeValues(roundtrip.vertex.coordinates, Vector(-1.0,-1.0,0.0)) && close(roundtrip.distance, 0.0),
        "picked vertex moves to voxel 0,0,0 and back with zero distance"),
      ScenarioHarness.fact("refusals", badLink.isLeft && SampleSpaces.inWorld(space, otherWorld).isLeft &&
        SurfaceGeometryPair.fromEither(white, wrongTopology).isLeft &&
        SurfaceVolumeProjection.scalarLayer(projection, layerId, surfaceId, wrongTopology, colorizer).isLeft,
        "different worlds and changed ordered connectivity are refused")))
    if result.ciPass then println(result.render)
    assert(result.ciPass, result.render)
