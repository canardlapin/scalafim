package scalafim.surface.view

import intaglio.*

class SurfaceScalarLookupSuite extends munit.FunSuite:
  private val blue = Rgba32.unsafe(0, 0, 200)
  private val white = Rgba32.unsafe(240, 240, 240)
  private val red = Rgba32.unsafe(200, 0, 0)
  private val diverging = ScalarMapping(ScalarScale.diverging(DisplayWindow.unsafe(-1.0, 1.0), 0.0,
    ScalarRamp.linear(blue, white), ScalarRamp.linear(white, red)).toOption.get)
  private val thresholded = diverging.resolve(threshold = Some(DisplayThreshold.transparentBand(-0.3, 0.3).toOption.get)).toOption.get
  private val curvature = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-1.0, 1.0),
    ScalarRamp.linear(Rgba32.unsafe(48, 48, 48), Rgba32.unsafe(208, 208, 208))))
  private val overlay = SurfaceLookupLayer(thresholded, DisplayOpacity.unsafe(0.65), DisplayBlendMode.Normal)
  private val underlay = SurfaceLookupLayer(curvature, DisplayOpacity.Opaque, DisplayBlendMode.Normal)

  private def underColor(curve: Double): Rgba32 =
    DisplayBlendMode.Normal.composite(SurfaceScalarLookup.Base, curvature.color(curve), DisplayOpacity.Opaque)

  private def vertexColor(value: Double, curve: Double): Rgba32 =
    DisplayBlendMode.Normal.composite(underColor(curve), thresholded.color(value), DisplayOpacity.unsafe(0.65))

  private def channelDistance(a: Rgba32, b: Rgba32): Int =
    Vector(a.red - b.red, a.green - b.green, a.blue - b.blue).map(math.abs).max

  test("axis coordinates round-trip through texel centres, mirror continuously and reserve only the seam"):
    val axis = SurfaceLookupAxis.Overlay
    assertEquals(axis.dataTexels, 768)
    for i <- 0 until axis.dataTexels do
      val t = i.toDouble / (axis.dataTexels - 1)
      assertEquals(axis.dataIndex(axis.coordinate(t)), i)
      assertEqualsDouble(axis.fraction(axis.hidden + axis.pad + i), t, 1e-12)
      assertEqualsDouble(axis.fraction(axis.texels - 1 - axis.hidden - axis.pad - i), t, 1e-12)
    assertEquals((0 until axis.texels).filter(axis.hiddenTexel).toVector, ((0 until 128) ++ (1920 until 2048)).toVector)
    assertEqualsDouble(axis.coordinate(-4.0).toDouble, axis.coordinate(0.0).toDouble, 0.0)
    assertEqualsDouble(axis.coordinate(9.0).toDouble, axis.coordinate(1.0).toDouble, 0.0)
    assert(SurfaceLookupAxis.make(1023, 32, 64).isLeft)
    assert(SurfaceLookupAxis.make(1024, 32, 0).isLeft)
    assert(SurfaceLookupAxis.make(16, 2, 4).isLeft)

  test("baked texels equal the ordered vertex composition at their sample values"):
    val table = SurfaceScalarLookup.bake(overlay, Some(underlay))
    assertEquals(table.width, 2048)
    assertEquals(table.height, 1024)
    var checked = 0
    for x <- 0 until table.width by 7; y <- 0 until table.height by 11 do
      if !table.overlayAxis.hiddenTexel(x) && !table.underlayAxis.hiddenTexel(y) then
        val value = -1.0 + table.overlayAxis.fraction(x) * 2.0
        val curve = -1.0 + table.underlayAxis.fraction(y) * 2.0
        assertEquals(Rgba32.fromPackedInt(table.pixel(x, y)), vertexColor(value, curve), s"texel $x,$y value $value curve $curve")
        checked += 1
    assert(checked > 10000)
    // The seam holds each mapping's invalid colour: the underlay row, or the base under a missing underlay.
    for y <- 0 until table.height by 13 do
      val expected =
        if table.underlayAxis.hiddenTexel(y) then
          DisplayBlendMode.Normal.composite(SurfaceScalarLookup.Base, curvature.invalid, DisplayOpacity.Opaque)
        else underColor(-1.0 + table.underlayAxis.fraction(y) * 2.0)
      assertEquals(Rgba32.fromPackedInt(table.pixel(0, y)),
        DisplayBlendMode.Normal.composite(expected, thresholded.invalid, DisplayOpacity.unsafe(0.65)))
    // Repeat wrapping is continuous: borders equal their mirror neighbours.
    for y <- 0 until table.height by 13 do
      assertEquals(table.pixel(0, y), table.pixel(table.width - 1, y))
      assertEquals(table.pixel(1023, y), table.pixel(1024, y))
    for x <- 0 until table.width by 13 do
      assertEquals(table.pixel(x, 0), table.pixel(x, table.height - 1))

  test("missing samples sit on the hidden seam and no data texel is reserved"):
    val table = SurfaceScalarLookup.bake(overlay, Some(underlay))
    assertEqualsDouble(table.overlayCoordinate(Double.NaN).toDouble, 0.0, 0.0)
    assertEqualsDouble(table.underlayCoordinate(Double.PositiveInfinity).toDouble, 0.0, 0.0)
    val v = table.underlayCoordinate(0.25)
    assert(channelDistance(table.sample(table.missingCoordinate, v), vertexColor(Double.NaN, 0.25)) <= 1)
    // A mapping without a threshold stays continuous through zero: nothing is forced there.
    val plain = SurfaceScalarLookup.bake(SurfaceLookupLayer(diverging, DisplayOpacity.Opaque, DisplayBlendMode.Normal), None)
    assertEquals(plain.height, 16)
    for value <- Vector(-0.01, 0.0, 0.004, 0.5) do
      val expected = DisplayBlendMode.Normal.composite(SurfaceScalarLookup.Base, diverging.color(value), DisplayOpacity.Opaque)
      val actual = plain.sample(plain.overlayCoordinate(value), plain.underlayCoordinate(Double.NaN))
      assert(channelDistance(actual, expected) <= 1, s"$value: $actual vs $expected")
    assertEquals(plain.sample(plain.missingCoordinate, plain.underlayCoordinate(Double.NaN)), SurfaceScalarLookup.Base)

  test("interpolating across a cutoff yields the interpolated-scalar ramp colour"):
    val table = SurfaceScalarLookup.bake(overlay, Some(underlay))
    val v = table.underlayCoordinate(0.1)
    // Suprathreshold 0.8 and sub-cutoff 0.1: the midpoint scalar 0.45 is visible.
    val above = table.overlayCoordinate(0.8)
    val below = table.overlayCoordinate(0.1)
    val midpoint = table.sample((above + below) / 2, v)
    assert(channelDistance(midpoint, vertexColor(0.45, 0.1)) <= 1, s"$midpoint")
    // A quarter of the way from the sub-cutoff vertex the scalar is 0.275: still hidden.
    val quarter = table.sample(below + (above - below) * 0.25, v)
    assert(channelDistance(quarter, vertexColor(0.275, 0.1)) <= 1, s"$quarter")
    // The step itself spans one texel of the axis, not a face.
    val step = table.overlayCoordinate(0.3)
    val texel = 1.0 / table.width
    val valueTexel = 2.0 / (table.overlayAxis.dataTexels - 1)
    assert(channelDistance(table.sample(step - texel, v), vertexColor(0.3 - valueTexel, 0.1)) <= 1)
    assert(channelDistance(table.sample(step + texel, v), vertexColor(0.3 + valueTexel, 0.1)) <= 1)
    assert(channelDistance(vertexColor(0.3 - valueTexel, 0.1), vertexColor(0.3 + valueTexel, 0.1)) > 40)

  test("plan lowering selects underlay then overlay, hides faces with missing corners and refuses other shapes"):
    val geometry = SurfaceFaceFixture.geometry
    val surface = SurfaceFaceFixture.Surface
    val under = SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe("curv"), surface, geometry,
      Array(-1.0, -0.5, 0.5, 1.0), curvature).toOption.get
    def lowered(values: Array[Double]): SurfaceScalarLookupPlan =
      val over = SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe("map"), surface, geometry,
        values, thresholded, opacity = DisplayOpacity.unsafe(0.65)).toOption.get
      val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), Vector(under, over)).toOption.get
      val compiled = SurfaceCompiler.compile(model, SurfaceFaceFixture.state(model)).toOption.get
      assert(compiled.fragmentSurfaces(surface))
      assertEquals(compiled.meshes.head.indices.unsafeArray.toVector, Vector(0, 1, 2, 0, 2, 3))
      SurfaceScalarLookup.plan(compiled, compiled.meshes.head).toOption.get
    val complete = lowered(Array(-2.0, 0.8, 0.4, 0.1))
    assertEquals(complete.table.key, SurfaceScalarLookup.bake(overlay, Some(underlay)).key)
    assertEquals(complete.missingFaces, 0)
    assertEquals(complete.vertexCount, 4)
    assertEquals(complete.coordinates.length, 8)
    assertEquals(complete.faceCoordinates.toVector, Vector(0, 1, 2, 0, 2, 3))
    assertEqualsDouble(complete.coordinates(0).toDouble, complete.table.overlayCoordinate(-2.0).toDouble, 0.0)
    assertEqualsDouble(complete.coordinates(7).toDouble, complete.table.underlayCoordinate(1.0).toDouble, 0.0)
    // Vertex 1 is missing: face (0, 1, 2) owns three seam corners; face (0, 2, 3) keeps its shared vertices.
    val partial = lowered(Array(-2.0, Double.NaN, 0.4, 0.1))
    assertEquals(partial.missingFaces, 1)
    assertEquals(partial.coordinates.length, (4 + 3) * 2)
    assertEquals(partial.faceCoordinates.toVector, Vector(4, 5, 6, 0, 2, 3))
    for corner <- 0 until 3 do assertEqualsDouble(partial.coordinates((4 + corner) * 2).toDouble, 0.0, 0.0)
    assertEqualsDouble(partial.coordinates(4 * 2 + 1).toDouble, partial.table.underlayCoordinate(-1.0).toDouble, 0.0)
    assertEqualsDouble(partial.coordinates(6 * 2 + 1).toDouble, partial.table.underlayCoordinate(0.5).toDouble, 0.0)
    // A missing underlay corner moves only the underlay coordinate of its faces onto the seam.
    val table = complete.table
    val hiddenUnder = SurfaceScalarLookup.lower(table, new DoubleBufferView(Array(0.5, 0.5, 0.5, 0.5)),
      Some(new DoubleBufferView(Array(Double.NaN, 0.0, 0.0, 0.0))), new IntBufferView(Array(0, 1, 2, 0, 2, 3)))
    assertEquals(hiddenUnder.missingFaces, 2)
    for corner <- 0 until 6 do
      assertEqualsDouble(hiddenUnder.coordinates((4 + corner) * 2 + 1).toDouble, 0.0, 0.0)
      assertEqualsDouble(hiddenUnder.coordinates((4 + corner) * 2).toDouble, table.overlayCoordinate(0.5).toDouble, 0.0)
    val legacy = SurfaceFaceFixture.plan
    assertEquals(SurfaceScalarLookup.plan(legacy, legacy.meshes.head), Left(SurfaceLookupError.NotAFragmentSurface(surface)))
