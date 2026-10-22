package scalafim.examples.workflows.eco

import intaglio.*
import neurosurf4s.surface.*
import neurosurf4s.surface.freesurfer.FreeSurferMorphometryCodec
import neurosurf4s.surface.gifti.*
import neurosurf4s.surface.view.*
import neurosurf4s.surface.view.raster.*
import scalafim.scenarios.{ScenarioHarness, ScenarioResult}
import scala.concurrent.ExecutionContext.Implicits.global

/** Synthetic format/readback court; neither a real cortex nor a native or WebGL renderer. */
class EcosystemSurfaceReadbackScenarioSuite extends munit.FunSuite:
  test("GIFTI geometry and FreeSurfer scalar survive scene restoration on each platform"):
    val valid = SurfaceAdmissionPlatform.gifti(SurfaceReadbackScenario.gifti)
    val malformed = SurfaceAdmissionPlatform.gifti(SurfaceReadbackScenario.gifti.replace("<Data>2 0 1</Data>", "<Data>2 0 3</Data>"))
    valid.zip(malformed).map: (decoded, rejected) =>
      val declared = decoded.fold(message => fail(message), identity)
      val declaration = declared.coordinates.declaredSpaces == Vector(
        (Some(GiftiDeclaredSpace.AlignedAnatomical), Some(GiftiDeclaredSpace.Talairach))) &&
        declared.placement == GiftiPlacement.NativeCoordinates && rejected.isLeft
      val result = SurfaceReadbackScenario.run("gifti-freesurfer-scalar", declared.geometry,
        "GIFTI native coordinates; declared aligned-to-Talairach transform deliberately not applied", declaration)
      assert(result.ciPass, result.render)

/** One triangle and the independently specified morphometry and raster witnesses.
  * The raster center tie is the provider's SurfaceNearestRasterSuite centroid witness. */
object SurfaceReadbackScenario:
  private def close(actual: Double, expected: Double): Boolean =
    actual.isFinite && expected.isFinite && math.abs(actual - expected) <= 1e-12

  private def closeValues(actual: Seq[Double], expected: Seq[Double]): Boolean =
    actual.length == expected.length && actual.zip(expected).forall((a, e) => close(a, e))

  val gifti = """<?xml version="1.0" encoding="UTF-8"?>
<GIFTI Version="1.0" NumberOfDataArrays="2">
<DataArray Intent="NIFTI_INTENT_POINTSET" DataType="NIFTI_TYPE_FLOAT32" ArrayIndexingOrder="RowMajorOrder" Dimensionality="2" Dim0="3" Dim1="3" Encoding="ASCII" Endian="LittleEndian">
<CoordinateSystemTransformMatrix><DataSpace>NIFTI_XFORM_ALIGNED_ANAT</DataSpace><TransformedSpace>NIFTI_XFORM_TALAIRACH</TransformedSpace><MatrixData>1 0 0 10 0 1 0 20 0 0 1 30 0 0 0 1</MatrixData></CoordinateSystemTransformMatrix>
<Data>-1 -1 0  1 -1 0  0 2 0</Data></DataArray>
<DataArray Intent="NIFTI_INTENT_TRIANGLE" DataType="NIFTI_TYPE_INT32" ArrayIndexingOrder="RowMajorOrder" Dimensionality="2" Dim0="1" Dim1="3" Encoding="ASCII" Endian="LittleEndian"><Data>2 0 1</Data></DataArray>
</GIFTI>"""
  // Modern big-endian float32 witness from FreeSurferMorphometrySuite.
  val morphometry = Array(255,255,255, 0,0,0,3, 0,0,0,1, 0,0,0,1,
    192,32,0,0, 0,0,0,0, 63,160,0,0).map(_.toByte)
  private val surface = SurfaceId.unsafe("left")
  private val layerId = SurfaceLayerId.unsafe("sulc")
  private val palette = Vector(Rgba32.unsafe(255, 0, 0), Rgba32.unsafe(0, 255, 0), Rgba32.unsafe(0, 0, 255))
  private def reference(uri: String, digit: Char): SurfaceExternalReference =
    SurfaceExternalReference(SurfaceAssetUri.unsafe(uri), SurfaceContentDigest.unsafe(digit.toString * 64))

  def run(name: String, geometry: SurfaceGeometry, source: String, sourceValid: Boolean): ScenarioResult =
    val field = FreeSurferMorphometryCodec.decode(morphometry, geometry, "sulc").toOption.get
    val colorizer = new Colorizer[Double]:
      def color(value: Double): Rgba32 =
        if close(value, -2.5) then palette(0) else if close(value, 0.0) then palette(1) else palette(2)
    val layer = SurfaceLayer.scalar(layerId, surface, geometry, field.data, colorizer,
      interpolation = SurfaceVertexInterpolation.NearestSample).toOption.get
    val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), Vector(layer)).toOption.get
    val initial = SurfaceViewerState.initial(model)
    val state = Vector(SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Dorsal),
      SurfaceViewerAction.SetLighting(SurfaceLighting.Unlit),
      SurfaceViewerAction.SetProjection(CameraProjection.Orthographic(OrthographicScale.unsafe(1.25))))
      .foldLeft(initial)((s, action) => SurfaceViewer.reduce(model, s, action).toOption.get)
    val bindings = SurfaceSceneBindings(Map(surface -> reference("asset://synthetic/geometry", 'a')),
      Map(layerId -> reference("asset://synthetic/sulc", 'b')))
    val provenance = SurfaceProvenance.make("scalafim-eco", "1", "2026-10-22T00:00:00Z",
      Vector("source-frame" -> source, "scope" -> "synthetic triangle; FreeSurfer geometry reader is JVM-only")).toOption.get
    val document = SurfaceSceneDocument.capture(model, state, bindings, provenance).toOption.get
    val wire = SurfaceSceneCodec.encode(document)
    val decoded = SurfaceSceneCodec.decode(wire).toOption.get
    val restored = decoded.restore(model, bindings).toOption.get
    val before = SurfaceCompiler.compile(model, state).toOption.get
    val after = SurfaceCompiler.compile(model, restored).toOption.get
    // This explicit identity camera reproduces the provider's mathematical centroid oracle.
    val identity = new FloatBufferView(Array[Float](1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1))
    val camera = SurfaceCameraPacket(identity, identity, 0.0, 0.0, 1.0)
    val dimensions = RasterDimensions.unsafe(129, 129)
    val style = SurfaceRasterStyle(culling = TriangleCulling.None)
    val originalView = SurfaceRasterizer.render(before, dimensions, style).toOption.get
    val restoredView = SurfaceRasterizer.render(after, dimensions, style).toOption.get
    val first = SurfaceRasterizer.render(before.copy(camera = camera), dimensions, style).toOption.get
    val second = SurfaceRasterizer.render(after.copy(camera = camera), dimensions, style).toOption.get
    val pick = second.pick(64, 64).toOption.flatten
    val reordered = SurfaceGeometry(TriangleMesh.fromRows(
      Vector(Vector(-1.0,-1.0,0.0), Vector(1.0,-1.0,0.0), Vector(0.0,2.0,0.0)), Vector((0,2,1))),
      Hemisphere.Left, SurfaceKind.Pial)
    val wrongLayer = SurfaceLayer.scalar(layerId, surface, reordered, field.data, colorizer,
      interpolation = SurfaceVertexInterpolation.NearestSample).toOption.get
    val wrongModel = SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, reordered).toOption.get), Vector(wrongLayer)).toOption.get
    val incomplete = SurfaceBackendCapabilities(SurfaceBackendId.unsafe("test"), SurfacePlanRevision.Current, Set.empty)
    val badCount = morphometry.clone()
    badCount(6) = 4.toByte
    val sameImage = (0 until 129).forall(y => (0 until 129).forall(x =>
      first.image.pixelUnsafe(x,y) == second.image.pixelUnsafe(x,y) &&
        originalView.image.pixelUnsafe(x,y) == restoredView.image.pixelUnsafe(x,y)))
    val orderedVertices = geometry.mesh.coordinates.toVector
    val orderedFaces = geometry.mesh.faceIndices.toVector
    val result = ScenarioHarness.result(name, Vector(
      ScenarioHarness.fact("geometry-order", closeValues(orderedVertices, Vector(-1.0,-1.0,0.0,1.0,-1.0,0.0,0.0,2.0,0.0)) && orderedFaces == Vector(2,0,1), orderedFaces.toString),
      ScenarioHarness.fact("scalar-order", field.geometry == geometry && field.vertexIds.map(_.index) == Vector(0,1,2) && closeValues(field.data.toVector, Vector(-2.5,0.0,1.25)), field.data.toVector.toString),
      ScenarioHarness.fact("source-frame", sourceValid && decoded.provenance == provenance, source),
      ScenarioHarness.fact("scene-wire", wire.contains("\"schema\":\"scalafim.surface-scene\"") && wire.contains("\"revision\":9") && wire.contains("\"encoding\":\"scalar\"") && wire.contains("\"association\":\"vertex\"") && wire.contains("\"vertexInterpolation\":\"nearest-sample\"") && SurfaceSceneCodec.encode(decoded) == wire, "revision 9 scalar vertex scene"),
      ScenarioHarness.fact("restored-resources", restored == state && before.meshes.map(_.resourceKey) == after.meshes.map(_.resourceKey) && before.layers.map(_.resourceKey) == after.layers.map(_.resourceKey), "state and resource identities"),
      ScenarioHarness.fact("raster-oracle", sameImage && second.image.pixelUnsafe(64,64) == palette(0) && pick.exists(p => p.surface == surface && p.face == 0 && p.vertex == 0) && first.pick(64,64).toOption.flatten.exists(p => pick.exists(q =>
        p.surface == q.surface && p.face == q.face && p.vertex == q.vertex &&
          closeValues(Vector(p.barycentricA, p.barycentricB, p.barycentricC, p.depth),
            Vector(q.barycentricA, q.barycentricB, q.barycentricC, q.depth)))), "129x129 identity-camera centroid: red, face 0, vertex 0"),
      ScenarioHarness.fact("admission-refusals", FreeSurferMorphometryCodec.decode(badCount, geometry).isLeft && decoded.admit(incomplete).isLeft && decoded.restore(wrongModel, bindings).isLeft && decoded.restore(model, bindings.copy(surfaces = Map(surface -> reference("asset://synthetic/geometry", 'f')))).isLeft, "scalar count, missing nearest-sampling capability, ordered topology, and reference digest")))

    if result.ciPass then println(result.render)
    result
