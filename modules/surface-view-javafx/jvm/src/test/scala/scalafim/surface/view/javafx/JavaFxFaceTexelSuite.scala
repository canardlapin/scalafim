package scalafim.surface.view.javafx

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

class JavaFxFaceTexelSuite extends munit.FunSuite:
  private val surface = SurfaceId.unsafe("face-texel")
  private val sulcal = Rgba32.unsafe(65, 72, 82)
  private val gyral = Rgba32.unsafe(202, 205, 209)
  private val binary = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-1.0, 1.0),
    ScalarRamp.make(Vector(0.0 -> gyral, 0.5 -> gyral, 0.5000001 -> sulcal, 1.0 -> sulcal)).toOption.get))
  private def onset(cutoff: Double, warm: Rgba32 = Rgba32.unsafe(255, 64, 0)): ScalarMapping =
    ScalarMapping(ScalarScale.split(DisplayWindow.unsafe(-1.0, 1.0), 0.0, -cutoff, cutoff,
      ScalarRamp.linear(Rgba32.unsafe(0, 255, 255), Rgba32.unsafe(0, 64, 255)),
      ScalarRamp.linear(warm, Rgba32.unsafe(255, 255, 0))).toOption.get)

  private val n = 20
  private val vertices = (n + 1) * (n + 1)
  private val geometry: SurfaceGeometry =
    val coordinates = new Array[Double](vertices * 3)
    for j <- 0 to n; i <- 0 to n do
      coordinates((j * (n + 1) + i) * 3) = 2.0 * i / n - 1.0
      coordinates((j * (n + 1) + i) * 3 + 1) = 2.0 * j / n - 1.0
    val faces = new Array[Int](n * n * 6)
    for cell <- 0 until n * n do
      val a = (cell / n) * (n + 1) + cell % n
      faces(cell * 6) = a; faces(cell * 6 + 1) = a + 1; faces(cell * 6 + 2) = a + n + 2
      faces(cell * 6 + 3) = a; faces(cell * 6 + 4) = a + n + 2; faces(cell * 6 + 5) = a + n + 1
    SurfaceGeometry(TriangleMesh.fromArrays(coordinates, faces), Hemisphere.Left, SurfaceKind.Inflated)
  private val random = new scala.util.Random(11)
  private val values = Array.tabulate(vertices)(i => if i % 23 == 4 then Double.NaN else random.nextDouble() * 2 - 1)
  private val otherValues = Array.tabulate(vertices)(i => if i % 19 == 2 then Double.NaN else random.nextDouble() * 2 - 1)
  private val sulc = Array.tabulate(vertices)(_ => random.nextDouble() * 2 - 1)

  private def plan(values: Array[Double], mapping: ScalarMapping,
      reduction: SurfaceFaceReduction = SurfaceFaceReduction.Mean): SurfaceRenderPlan =
    val under = SurfaceLayer.faceFlatScalar(SurfaceLayerId.unsafe("sulc"), surface, geometry, sulc, binary, SurfaceFaceReduction.Mean).toOption.get
    val over = SurfaceLayer.faceFlatScalar(SurfaceLayerId.unsafe("map"), surface, geometry, values, mapping, reduction).toOption.get
    val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), Vector(under, over)).toOption.get
    SurfaceCompiler.compile(model, SurfaceFaceFixture.state(model)).toOption.get

  private val original = plan(values, onset(0.25))
  private def config(encoding: JavaFxAtlasEncoding) = JavaFxAtlasConfig.make(encoding = encoding).toOption.get

  /** Independent bilinear sample of the actual uploaded image at a texture coordinate. */
  private def sample(atlas: JavaFxFaceAtlas, u: Double, v: Double): Int =
    val reader = atlas.image.getPixelReader
    val x = u * atlas.width - 0.5
    val y = v * atlas.height - 0.5
    val x0 = math.floor(x).toInt
    val y0 = math.floor(y).toInt
    val fx = x - x0
    val fy = y - y0
    def at(px: Int, py: Int): Int = reader.getArgb(((px % atlas.width) + atlas.width) % atlas.width, ((py % atlas.height) + atlas.height) % atlas.height)
    def channel(shift: Int): Int =
      def c(px: Int, py: Int): Double = ((at(px, py) >>> shift) & 255).toDouble
      math.round((c(x0, y0) * (1 - fx) + c(x0 + 1, y0) * fx) * (1 - fy) + (c(x0, y0 + 1) * (1 - fx) + c(x0 + 1, y0 + 1) * fx) * fy).toInt
    (channel(24) << 24) | (channel(16) << 16) | (channel(8) << 8) | channel(0)

  private def argb(rgba: Int): Int = (rgba << 24) | (rgba >>> 8)

  private def assertTexels(chunk: JavaFxSurfaceChunk, next: SurfaceRenderPlan): Unit =
    val expected = SurfaceFaceTexels.colors(next, next.meshes.head).toOption.get
    val faces = chunk.mesh.getFaces.toArray(null: Array[Int])
    val uv = chunk.mesh.getTexCoords.toArray(null: Array[Float])
    val offsets = if chunk.atlas.encoding.faceTexels == 2 then Vector(-0.45, 0.0, 0.45) else Vector(0.0)
    for face <- 0 until chunk.faceCount do
      val coordinate = faces(face * 9 + 2)
      for corner <- 0 until 3 do assertEquals(faces(face * 9 + corner * 3 + 2), coordinate)
      for dx <- offsets; dy <- offsets do
        val actual = sample(chunk.atlas, uv(coordinate * 2) + dx / chunk.atlas.width, uv(coordinate * 2 + 1) + dy / chunk.atlas.height)
        assertEquals(actual, argb(expected(face)), s"face $face offset ($dx, $dy)")

  test("every corner of a face addresses one constant coordinate whose texels hold the face-rule colour"):
    for encoding <- Vector(JavaFxAtlasEncoding.FaceTexelFlat, JavaFxAtlasEncoding.FaceTexelFlatSingle) do
      val result = JavaFxSurfaceProbe.compile(original, config = config(encoding)).toOption.get
      assertEquals(result.chunks.length, 1)
      val chunk = result.chunks.head
      val faceCount = n * n * 2
      assertEquals(chunk.renderedFaceCount, faceCount)
      assertEquals(result.receipt.facesUploaded, faceCount)
      assertEquals(chunk.mesh.getPoints.size, vertices * 3)
      assertEquals(chunk.mesh.getTexCoords.size, faceCount * 2)
      val faces = chunk.mesh.getFaces.toArray(null: Array[Int])
      for face <- 0 until faceCount; corner <- 0 until 3 do
        // Points and normals are the scientific vertices, in the plan's own order.
        assertEquals(faces(face * 9 + corner * 3), original.meshes.head.indices(face * 3 + corner))
        assertEquals(faces(face * 9 + corner * 3 + 1), faces(face * 9 + corner * 3))
      for face <- 0 until faceCount do assertEquals(chunk.packetFace(face), Some(face))
      assertEquals(chunk.packetFace(faceCount), None)
      assertTexels(chunk, original)
      val layout = chunk.atlas.faceLayout.get
      assertEquals(layout.texelsPerFace, encoding.faceTexels)
      assertEquals((chunk.atlas.width, chunk.atlas.height), (layout.width, layout.height))
      assert(chunk.material.getDiffuseMap eq chunk.atlas.image)
      assert(chunk.material.getSelfIlluminationMap == null)

  test("palette, cutoff and map-value changes rewrite only texels, never the native mesh"):
    for encoding <- Vector(JavaFxAtlasEncoding.FaceTexelFlat, JavaFxAtlasEncoding.FaceTexelFlatSingle) do
      val result = JavaFxSurfaceProbe.compile(original, config = config(encoding)).toOption.get
      val chunk = result.chunks.head
      val mesh = chunk.mesh
      val image = chunk.atlas.image
      val points = mesh.getPoints.toArray(null: Array[Float]).toVector
      val normals = mesh.getNormals.toArray(null: Array[Float]).toVector
      val uv = mesh.getTexCoords.toArray(null: Array[Float]).toVector
      val faces = mesh.getFaces.toArray(null: Array[Int]).toVector
      val palette = plan(values, onset(0.25, Rgba32.unsafe(200, 0, 160)))
      val cutoff = plan(values, onset(0.6))
      val mapped = plan(otherValues, onset(0.25))
      for next <- Vector(palette, cutoff, mapped, original) do
        val receipt = result.updateColors(next).toOption.get
        assertEquals(receipt.geometryRebuilt, false)
        assertEquals(receipt.textureCoordinateBytesUpdated, 0L)
        assertEquals(receipt.bytesUpdated, 0L)
        assertEquals(receipt.verticesUpdated, 0)
        assertEquals(receipt.atlasesUpdated, 1)
        assert(receipt.dirtyPixels > 0L)
        assert(result.chunks.head eq chunk)
        assert(chunk.mesh eq mesh)
        assert(chunk.atlas.image eq image)
        assertEquals(mesh.getPoints.toArray(null: Array[Float]).toVector, points)
        assertEquals(mesh.getNormals.toArray(null: Array[Float]).toVector, normals)
        assertEquals(mesh.getTexCoords.toArray(null: Array[Float]).toVector, uv)
        assertEquals(mesh.getFaces.toArray(null: Array[Int]).toVector, faces)
        assertTexels(chunk, next)
      // An unchanged plan dirties nothing.
      val same = result.updateColors(original).toOption.get
      assertEquals((same.atlasesUpdated, same.dirtyPixels), (0, 0L))

  test("missing and below-cutoff faces show the curvature underlay of their mean, not grey"):
    val result = JavaFxSurfaceProbe.compile(original, config = config(JavaFxAtlasEncoding.FaceTexelFlat)).toOption.get
    val chunk = result.chunks.head
    val indices = original.meshes.head.indices
    val reader = chunk.atlas.image.getPixelReader
    val layout = chunk.atlas.faceLayout.get
    var missing = 0
    var below = 0
    for face <- 0 until chunk.faceCount do
      val corners = Vector(indices(face * 3), indices(face * 3 + 1), indices(face * 3 + 2))
      val v = corners.map(values(_))
      val underlay = DisplayBlendMode.Normal.composite(SurfaceFaceTexels.Base,
        if corners.map(sulc(_)).sum / 3 > 0 then sulcal else gyral, DisplayOpacity.Opaque)
      val hidden = !v.forall(_.isFinite) || math.abs(v.sum / 3) < 0.25
      if hidden then
        if !v.forall(_.isFinite) then missing += 1 else below += 1
        for dy <- 0 until 2; dx <- 0 until 2 do
          assertEquals(reader.getArgb(layout.blockX(face) + dx, layout.blockY(face) + dy), argb(underlay.toPackedInt), s"face $face")
    assert(missing > 20 && below > 50, s"missing $missing below $below")

  test("each encoding refuses layers whose declared policy it would not show"):
    def interpolatedPlan(values: Array[Double]): SurfaceRenderPlan =
      val under = SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe("sulc"), surface, geometry, sulc, binary).toOption.get
      val over = SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe("map"), surface, geometry, values, onset(0.25)).toOption.get
      val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), Vector(under, over)).toOption.get
      SurfaceCompiler.compile(model, SurfaceFaceFixture.state(model)).toOption.get
    val interpolated = interpolatedPlan(values)
    for encoding <- Vector(JavaFxAtlasEncoding.FaceTexelFlat, JavaFxAtlasEncoding.FaceTexelFlatSingle) do
      val refused = JavaFxSurfaceProbe.compile(interpolated, config = config(encoding))
      assert(refused.left.toOption.exists(_.message.contains("declares VertexScalar, not a face-flat policy")), refused.toString)
      assert(JavaFxSurfaceBackend.validateCapabilities(interpolated, encoding).isLeft)
      assert(JavaFxSurfaceBackend.validateCapabilities(original, encoding).isRight)
    val lookup = JavaFxSurfaceBackend.validateCapabilities(original, JavaFxAtlasEncoding.ScalarLutInterpolated)
    assert(lookup.left.toOption.exists(_.message.contains("ScalarLutInterpolated interpolates vertex scalars")), lookup.toString)
    assert(JavaFxSurfaceBackend.validateCapabilities(interpolated, JavaFxAtlasEncoding.ScalarLutInterpolated).isRight)
    for encoding <- Vector(JavaFxAtlasEncoding.LegacyTriangle, JavaFxAtlasEncoding.AdaptiveAffineOpaque, JavaFxAtlasEncoding.AffineMidpointOpaque) do
      assert(JavaFxSurfaceBackend.validateCapabilities(original, encoding).isLeft, encoding.toString)
      assert(JavaFxSurfaceProbe.compile(original, config = config(encoding)).isLeft, encoding.toString)

  test("the face-texel backend declares face-flat composition and never scalar interpolation"):
    val report = JavaFxCapabilityReport(scene3d = true, depthBuffer = true, antialiasing = true, fallback = None, faceFragments = true)
    val features = report.admissionCapabilities.features
    assert(features(SurfaceBackendFeature.FaceFlatScalar))
    assert(features(SurfaceBackendFeature.FragmentComposition))
    assert(!features(SurfaceBackendFeature.ScalarInterpolation))
    assertEquals(report.admissionCapabilities.id.value, "javafx-scene3d-face-texel-v1")
    val lookup = report.copy(faceFragments = false, lookupFragments = true).admissionCapabilities.features
    assert(lookup(SurfaceBackendFeature.ScalarInterpolation) && !lookup(SurfaceBackendFeature.FaceFlatScalar))

  test("max-magnitude texels colour every face with a corner at or beyond the cutoff"):
    val maxPlan = plan(values, onset(0.25), SurfaceFaceReduction.MaxMagnitude)
    val result = JavaFxSurfaceProbe.compile(maxPlan, config = config(JavaFxAtlasEncoding.FaceTexelFlat)).toOption.get
    assertTexels(result.chunks.head, maxPlan)
    val indices = maxPlan.meshes.head.indices
    val reader = result.chunks.head.atlas.image.getPixelReader
    val layout = result.chunks.head.atlas.faceLayout.get
    var coloured = 0
    for face <- 0 until result.chunks.head.faceCount do
      val corners = Vector(indices(face * 3), indices(face * 3 + 1), indices(face * 3 + 2)).map(values(_)).filter(_.isFinite)
      val underlay = DisplayBlendMode.Normal.composite(SurfaceFaceTexels.Base,
        if Vector(indices(face * 3), indices(face * 3 + 1), indices(face * 3 + 2)).map(sulc(_)).sum / 3 > 0 then sulcal else gyral, DisplayOpacity.Opaque)
      val shown = reader.getArgb(layout.blockX(face), layout.blockY(face)) != argb(underlay.toPackedInt)
      assertEquals(shown, corners.exists(v => math.abs(v) >= 0.25), s"face $face corners $corners")
      if shown then coloured += 1
    assert(coloured > 100)

  test("palette and cutoff changes reuse cached face values; a value change reduces again"):
    val result = JavaFxSurfaceProbe.compile(original, config = config(JavaFxAtlasEncoding.FaceTexelFlat)).toOption.get
    val atlas = result.chunks.head.atlas
    val before = atlas.faceValues(SurfaceLayerId.unsafe("map")).values
    val sulcBefore = atlas.faceValues(SurfaceLayerId.unsafe("sulc")).values
    val palette = result.updateColors(plan(values, onset(0.25, Rgba32.unsafe(200, 0, 160)))).toOption.get
    assert(atlas.faceValues(SurfaceLayerId.unsafe("map")).values eq before)
    assert(palette.reductionNanos >= 0L && palette.evaluationNanos > 0L && palette.textureWriteNanos > 0L)
    result.updateColors(plan(values, onset(0.6))).toOption.get
    assert(atlas.faceValues(SurfaceLayerId.unsafe("map")).values eq before)
    result.updateColors(plan(otherValues, onset(0.6))).toOption.get
    assert(!(atlas.faceValues(SurfaceLayerId.unsafe("map")).values eq before))
    assert(atlas.faceValues(SurfaceLayerId.unsafe("sulc")).values eq sulcBefore)
    val switched = result.updateColors(plan(otherValues, onset(0.6), SurfaceFaceReduction.MaxMagnitude)).toOption.get
    assert(switched.atlasesUpdated == 1)
    assertTexels(result.chunks.head, plan(otherValues, onset(0.6), SurfaceFaceReduction.MaxMagnitude))

  test("plans whose render vertices are generated corners are refused"):
    val refused = JavaFxSurfaceProbe.compile(SurfaceFaceFixture.plan, config = config(JavaFxAtlasEncoding.FaceTexelFlat))
    assert(refused.isLeft, "face-constant fixture compiles with separated corners")
    assert(refused.left.toOption.get.message.contains("generated corners"), refused.left.toOption.get.message)
