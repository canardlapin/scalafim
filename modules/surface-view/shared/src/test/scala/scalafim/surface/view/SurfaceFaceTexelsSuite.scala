package scalafim.surface.view

import intaglio.*
import scalafim.surface.*

class SurfaceFaceTexelsSuite extends munit.FunSuite:
  private val surface = SurfaceId.unsafe("face-texel")
  private val sulcal = Rgba32.unsafe(65, 72, 82)
  private val gyral = Rgba32.unsafe(202, 205, 209)
  private val missingGrey = Rgba32.unsafe(135, 143, 145)
  // PLS Neuro recipe: transparent below the cutoff, saturated onset at the cutoff.
  private val onset = ScalarMapping(ScalarScale.split(DisplayWindow.unsafe(-1.0, 1.0), 0.0, -0.25, 0.25,
    ScalarRamp.linear(Rgba32.unsafe(0, 255, 255), Rgba32.unsafe(0, 64, 255)),
    ScalarRamp.linear(Rgba32.unsafe(255, 64, 0), Rgba32.unsafe(255, 255, 0))).toOption.get)
  // FreeSurfer sulc, PositiveIsSulcal: a binary step at zero.
  private val binary = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-1.0, 1.0),
    ScalarRamp.make(Vector(0.0 -> gyral, 0.5 -> gyral, 0.5000001 -> sulcal, 1.0 -> sulcal)).toOption.get))
  private val opacity = DisplayOpacity.unsafe(0.8)

  /** n x n cells over [-1, 1]^2, two faces per cell, vertex (i, j) at index j * (n + 1) + i. */
  private def grid(n: Int): SurfaceGeometry =
    val coordinates = new Array[Double]((n + 1) * (n + 1) * 3)
    for j <- 0 to n; i <- 0 to n do
      val index = j * (n + 1) + i
      coordinates(index * 3) = 2.0 * i / n - 1.0
      coordinates(index * 3 + 1) = 2.0 * j / n - 1.0
    val faces = new Array[Int](n * n * 6)
    for cell <- 0 until n * n do
      val a = (cell / n) * (n + 1) + cell % n
      faces(cell * 6) = a; faces(cell * 6 + 1) = a + 1; faces(cell * 6 + 2) = a + n + 2
      faces(cell * 6 + 3) = a; faces(cell * 6 + 4) = a + n + 2; faces(cell * 6 + 5) = a + n + 1
    SurfaceGeometry(TriangleMesh.fromArrays(coordinates, faces), Hemisphere.Left, SurfaceKind.Inflated)

  private def compiled(geometry: SurfaceGeometry, values: Array[Double], sulc: Array[Double],
      reduction: SurfaceFaceReduction = SurfaceFaceReduction.Mean): SurfaceRenderPlan =
    val under = SurfaceLayer.faceFlatScalar(SurfaceLayerId.unsafe("sulc"), surface, geometry, sulc, binary, SurfaceFaceReduction.Mean).toOption.get
    val over = SurfaceLayer.faceFlatScalar(SurfaceLayerId.unsafe("map"), surface, geometry, values, onset, reduction,
      opacity = opacity).toOption.get
    val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), Vector(under, over)).toOption.get
    SurfaceCompiler.compile(model, SurfaceFaceFixture.state(model)).toOption.get

  /** Max-magnitude corner written out from the declared rule. */
  private def maxCorner(x: Double, y: Double, z: Double): Double =
    val finite = Vector(x, y, z).filter(_.isFinite)
    if finite.isEmpty then Double.NaN
    else
      val top = finite.map(math.abs).max
      val tied = finite.filter(v => math.abs(v) == top)
      tied.max

  /** The declared rule written out directly from the recipe, without the fragment evaluator. */
  private def expected(a: Int, b: Int, c: Int, values: Array[Double], sulc: Array[Double]): Rgba32 =
    def mean(samples: Array[Double]): Double =
      val corners = Vector(samples(a), samples(b), samples(c))
      if corners.forall(_.isFinite) then corners.sum / 3 else Double.NaN
    val under = DisplayBlendMode.Normal.composite(SurfaceFaceTexels.Base, binary.color(mean(sulc)), DisplayOpacity.Opaque)
    DisplayBlendMode.Normal.composite(under, onset.color(mean(values)), opacity)

  private def distance(a: Rgba32, b: Rgba32): Int =
    Vector(a.red - b.red, a.green - b.green, a.blue - b.blue, a.alpha - b.alpha).map(math.abs).max

  test("each face takes its layers' mappings of the mean sample, composited in plan order"):
    val n = 12
    val geometry = grid(n)
    val random = new scala.util.Random(20260911)
    val vertices = (n + 1) * (n + 1)
    val values = Array.tabulate(vertices)(i => if i % 17 == 5 then Double.NaN else random.nextDouble() * 2 - 1)
    val sulc = Array.tabulate(vertices)(_ => random.nextDouble() * 2 - 1)
    val plan = compiled(geometry, values, sulc)
    val mesh = plan.meshes.head
    val colors = SurfaceFaceTexels.colors(plan, mesh).toOption.get
    assertEquals(colors.length, n * n * 2)
    var offByOne = 0
    for face <- colors.indices do
      val (a, b, c) = (mesh.indices(face * 3), mesh.indices(face * 3 + 1), mesh.indices(face * 3 + 2))
      val d = distance(Rgba32.fromPackedInt(colors(face)), expected(a, b, c, values, sulc))
      // The evaluator divides weighted sums; the direct mean sums then divides. Only ramp rounding may differ.
      assert(d <= 1, s"face $face differs by $d")
      if d == 1 then offByOne += 1
    assert(offByOne <= 2, s"$offByOne faces differ by one level")

  test("below-cutoff and missing faces show the underlay of the face's mean curvature, never grey"):
    val geometry = grid(1)
    // Faces (0, 1, 3) and (0, 3, 2).
    val under = DisplayBlendMode.Normal.composite(SurfaceFaceTexels.Base, gyral, DisplayOpacity.Opaque)
    val underSulcal = DisplayBlendMode.Normal.composite(SurfaceFaceTexels.Base, sulcal, DisplayOpacity.Opaque)
    def faces(values: Array[Double], sulc: Array[Double]): Vector[Rgba32] =
      val plan = compiled(geometry, values, sulc)
      SurfaceFaceTexels.colors(plan, plan.meshes.head).toOption.get.toVector.map(Rgba32.fromPackedInt)
    // Mean 0.15 is below the 0.25 cutoff although vertex 3 alone is above it.
    val belowCutoff = faces(Array(0.05, 0.1, 0.9, 0.3), Array(-0.4, -0.2, 0.9, -0.3))
    assertEquals(belowCutoff(0), under)
    // One missing corner hides the overlay across the face, as in the interpolated reference.
    val oneMissing = faces(Array(0.9, Double.NaN, 0.9, 0.9), Array(-0.4, -0.2, 0.9, -0.3))
    assertEquals(oneMissing(0), under)
    // Face (0, 3, 2) has mean curvature (-0.4 - 0.3 + 0.9) / 3 > 0: a sulcal underlay under the overlay.
    assertEquals(oneMissing(1), DisplayBlendMode.Normal.composite(underSulcal, onset.color(0.9), opacity))
    val allMissing = faces(Array.fill(4)(Double.NaN), Array(0.4, 0.2, -0.9, 0.3))
    // Mean curvature of face (0, 1, 3) is +0.3 (sulcal); of (0, 3, 2) it is -0.0667 (gyral).
    assertEquals(allMissing, Vector(underSulcal, under))
    for color <- belowCutoff ++ oneMissing.take(1) ++ allMissing do
      assertNotEquals(color, SurfaceFaceTexels.Base)
      assertNotEquals(color, missingGrey)
    // Saturated onset: a face whose mean just clears the cutoff takes the ramp's first colour.
    val onsetFace = faces(Array(0.2501, 0.2501, 0.2501, 0.2501), Array(0.4, 0.2, -0.9, 0.3))
    assertEquals(onsetFace(0), DisplayBlendMode.Normal.composite(underSulcal, Rgba32.unsafe(255, 64, 0), opacity))

  test("constant faces take exactly their shared sample's colour"):
    val geometry = grid(3)
    val values = Array.fill(16)(0.1 * 3)
    val plan = compiled(geometry, values, Array.fill(16)(0.2))
    val colors = SurfaceFaceTexels.colors(plan, plan.meshes.head).toOption.get
    val expectedColor = DisplayBlendMode.Normal.composite(
      DisplayBlendMode.Normal.composite(SurfaceFaceTexels.Base, sulcal, DisplayOpacity.Opaque), onset.color(0.1 * 3), opacity)
    assert(colors.forall(_ == expectedColor.toPackedInt))

  /** Independent bilinear sampler at mip level zero with repeat wrapping. */
  private def sample(texels: Array[Int], width: Int, height: Int, u: Double, v: Double, shift: Int): Double =
    def wrap(index: Int, size: Int): Int = ((index % size) + size) % size
    val x = u * width - 0.5
    val y = v * height - 0.5
    val x0 = math.floor(x).toInt
    val y0 = math.floor(y).toInt
    val fx = x - x0
    val fy = y - y0
    def at(px: Int, py: Int): Double = ((texels(wrap(py, height) * width + wrap(px, width)) >>> shift) & 255).toDouble
    (at(x0, y0) * (1 - fx) + at(x0 + 1, y0) * fx) * (1 - fy) + (at(x0, y0 + 1) * (1 - fx) + at(x0 + 1, y0 + 1) * fx) * fy

  test("blocks never overlap and a constant coordinate filters to its own block colour"):
    val random = new scala.util.Random(7)
    for texels <- Vector(1, 2); faces <- Vector(1, 3, 1000, 4097) do
      val layout = SurfaceFaceTexelLayout.make(faces, texels, 4096).toOption.get
      assertEquals(Integer.bitCount(layout.width), 1)
      assertEquals(layout.height % texels, 0)
      val owner = Array.fill(layout.width * layout.height)(-1)
      val colors = Array.fill(faces)(random.nextInt())
      for face <- 0 until faces; dy <- 0 until texels; dx <- 0 until texels do
        val index = (layout.blockY(face) + dy) * layout.width + layout.blockX(face) + dx
        assertEquals(owner(index), -1, s"texel $index owned twice")
        owner(index) = face
      val image = owner.map(face => if face < 0 then random.nextInt() else colors(face))
      val coordinates = layout.coordinates
      val offsets = if texels == 2 then Vector(-0.49, -0.25, 0.0, 0.25, 0.49) else Vector(0.0)
      for face <- 0 until faces; ox <- offsets; oy <- offsets; shift <- Vector(0, 8, 16, 24) do
        assertEquals(coordinates(face * 2), layout.u(face))
        val value = sample(image, layout.width, layout.height,
          coordinates(face * 2) + ox / layout.width, coordinates(face * 2 + 1) + oy / layout.height, shift)
        // Float coordinates leave weight-sum rounding only; a neighbour texel would add >= 1/255 per unit weight.
        assertEqualsDouble(value, ((colors(face) >>> shift) & 255).toDouble, 1e-3)
    // A single texel is only exact at its centre: a quarter-texel error mixes the neighbour.
    val single = SurfaceFaceTexelLayout.make(2, 1, 64).toOption.get
    val image = Array.fill(single.width * single.height)(0)
    image(0) = 0xff000000
    assertEqualsDouble(sample(image, single.width, single.height, single.u(0) + 0.25 / single.width, single.v(0), 24), 191.25, 1e-9)

  test("layouts use a power-of-two width and refuse faces that do not fit"):
    val paired = SurfaceFaceTexelLayout.make(276566, 2, 4096).toOption.get
    assertEquals((paired.width, paired.height), (2048, 542))
    val single = SurfaceFaceTexelLayout.make(276566, 1, 4096).toOption.get
    assertEquals((single.width, single.height), (1024, 271))
    assert(SurfaceFaceTexelLayout.make(0, 2, 4096).isLeft)
    assert(SurfaceFaceTexelLayout.make(10, 3, 4096).isLeft)
    assertEquals(SurfaceFaceTexelLayout.make(100000, 2, 64), Left(SurfaceFaceTexelError.InvalidLayout(100000, 2, 64)))

  test("generated corners and mismatched sample domains are refused"):
    val geometry = grid(2)
    val plan = compiled(geometry, Array.fill(9)(0.5), Array.fill(9)(0.5))
    val mesh = plan.meshes.head
    val separated = mesh.copy(sourceVertices = Some(new IntBufferView(Array.tabulate(mesh.indices.length)(mesh.indices(_)))))
    assertEquals(SurfaceFaceTexels.colors(plan, separated), Left(SurfaceFaceTexelError.GeneratedCorners(surface)))
    val short = plan.copy(layers = plan.layers.map(layer =>
      layer.copy(scalarField = layer.scalarField.map(field => new SurfaceScalarPacket(new DoubleBufferView(Array(0.5)), field.mapping)))))
    assert(SurfaceFaceTexels.colors(short, mesh).left.toOption.exists(_.isInstanceOf[SurfaceFaceTexelError.SampleDomainMismatch]))

  test("layers declaring vertex interpolation are refused: face texels never stand in for an interpolated field"):
    val geometry = grid(2)
    val interpolated = SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe("map"), surface, geometry, Array.fill(9)(0.5), onset).toOption.get
    val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), Vector(interpolated)).toOption.get
    val plan = SurfaceCompiler.compile(model, SurfaceFaceFixture.state(model)).toOption.get
    assertEquals(SurfaceFaceTexels.colors(plan, plan.meshes.head),
      Left(SurfaceFaceTexelError.InterpolatedLayer(SurfaceLayerId.unsafe("map"), SurfaceMapInterpolation.VertexScalar)))
    val packed = SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("rgb"), surface, geometry, Vector.fill(9)(sulcal)).toOption.get
    val colourModel = SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), Vector(packed)).toOption.get
    val colourPlan = SurfaceCompiler.compile(colourModel, SurfaceFaceFixture.state(colourModel)).toOption.get
    assert(SurfaceFaceTexels.colors(colourPlan, colourPlan.meshes.head).left.toOption.exists(_.isInstanceOf[SurfaceFaceTexelError.InterpolatedLayer]))

  test("max-magnitude faces take their largest finite corner, positive on a tie, and ignore missing corners"):
    val r = SurfaceFaceReduction.MaxMagnitude
    assertEqualsDouble(r.reduce(0.1, -0.7, 0.5), -0.7, 0.0)
    assertEqualsDouble(r.reduce(0.3, -0.3, 0.1), 0.3, 0.0)
    assertEqualsDouble(r.reduce(-0.3, 0.3, 0.1), 0.3, 0.0)
    assertEqualsDouble(r.reduce(Double.NaN, 0.2, -0.1), 0.2, 0.0)
    assertEqualsDouble(r.reduce(Double.NaN, Double.PositiveInfinity, -0.4), -0.4, 0.0)
    assert(r.reduce(Double.NaN, Double.NaN, Double.NaN).isNaN)
    // Mean keeps the interpolated reference's rule: any missing corner hides the face.
    assert(SurfaceFaceReduction.Mean.reduce(Double.NaN, 0.2, 0.3).isNaN)
    assertEqualsDouble(SurfaceFaceReduction.Mean.reduce(0.3, 0.3, 0.3), 0.3, 0.0)
    val random = new scala.util.Random(3)
    for _ <- 0 until 2000 do
      val v = Vector.fill(3)(if random.nextInt(9) == 0 then Double.NaN else math.round((random.nextDouble() * 2 - 1) * 8) / 8.0)
      val expected = maxCorner(v(0), v(1), v(2))
      val actual = r.reduce(v(0), v(1), v(2))
      assert(actual == expected || (actual.isNaN && expected.isNaN), s"$v: $actual vs $expected")

  test("a max-magnitude face is coloured when any finite corner reaches the cutoff; below-cutoff and all-missing show the underlay"):
    val geometry = grid(1)
    val underGyral = DisplayBlendMode.Normal.composite(SurfaceFaceTexels.Base, gyral, DisplayOpacity.Opaque)
    val underSulcal = DisplayBlendMode.Normal.composite(SurfaceFaceTexels.Base, sulcal, DisplayOpacity.Opaque)
    def faces(values: Array[Double], sulc: Array[Double]): Vector[Rgba32] =
      val plan = compiled(geometry, values, sulc, SurfaceFaceReduction.MaxMagnitude)
      SurfaceFaceTexels.colors(plan, plan.meshes.head).toOption.get.toVector.map(Rgba32.fromPackedInt)
    // Faces (0, 1, 3) and (0, 3, 2); curvature means -0.3 (gyral) and +0.067 (sulcal).
    val sulc = Array(-0.4, -0.2, 0.9, -0.3)
    // A single corner above the cutoff colours both faces that touch it (the mean rule hides face (0, 1, 3)).
    val single = faces(Array(0.05, 0.1, 0.0, 0.3), sulc)
    assertEquals(single, Vector(DisplayBlendMode.Normal.composite(underGyral, onset.color(0.3), opacity),
      DisplayBlendMode.Normal.composite(underSulcal, onset.color(0.3), opacity)))
    // Opposite signs: the larger magnitude sets the hue.
    val mixed = faces(Array(0.6, -0.8, 0.0, 0.1), sulc)
    assertEquals(mixed(0), DisplayBlendMode.Normal.composite(underGyral, onset.color(-0.8), opacity))
    assertEquals(mixed(1), DisplayBlendMode.Normal.composite(underSulcal, onset.color(0.6), opacity))
    // Missing corners are excluded rather than hiding the face.
    val missing = faces(Array(Double.NaN, Double.NaN, 0.5, Double.NaN), sulc)
    assertEquals(missing, Vector(underGyral, DisplayBlendMode.Normal.composite(underSulcal, onset.color(0.5), opacity)))
    // Every corner below the cutoff: the underlay, never grey.
    assertEquals(faces(Array(0.2, -0.24, 0.1, 0.0), sulc), Vector(underGyral, underSulcal))

  test("the lowered rule composes every face exactly as the fragment evaluator does for that face"):
    val n = 9
    val geometry = grid(n)
    val random = new scala.util.Random(99)
    val vertices = (n + 1) * (n + 1)
    for reduction <- SurfaceFaceReduction.values do
      val values = Array.tabulate(vertices)(i => if i % 11 == 3 then Double.NaN else random.nextDouble() * 2 - 1)
      val sulc = Array.tabulate(vertices)(_ => random.nextDouble() * 2 - 1)
      val plan = compiled(geometry, values, sulc, reduction)
      assert(plan.fragmentSurfaces(surface))
      assertEquals(plan.layers.map(_.interpolation), Vector(SurfaceMapInterpolation.FaceScalarMean, reduction.policy))
      val mesh = plan.meshes.head
      val colors = SurfaceFaceTexels.colors(plan, mesh).toOption.get
      val evaluator = new SurfaceFragmentEvaluator(mesh, plan.layers, SurfaceLighting.Unlit, SurfaceFaceTexels.Base)
      for face <- colors.indices; (wa, wb) <- Vector((1.0 / 3, 1.0 / 3), (0.9, 0.05), (0.0, 0.0)) do
        val (a, b, c) = (mesh.indices(face * 3), mesh.indices(face * 3 + 1), mesh.indices(face * 3 + 2))
        // Flat: every point of the face evaluates to the face colour.
        assertEquals(evaluator.color(face, a, b, c, wa, wb, 1.0 - wa - wb).toPackedInt, colors(face), s"$reduction face $face")
        if wa == 1.0 / 3 then
          val expectedValue = if reduction == SurfaceFaceReduction.Mean then
            (if Vector(values(a), values(b), values(c)).forall(_.isFinite) then (values(a) + values(b) + values(c)) / 3 else Double.NaN)
          else maxCorner(values(a), values(b), values(c))
          val direct = DisplayBlendMode.Normal.composite(DisplayBlendMode.Normal.composite(SurfaceFaceTexels.Base,
            binary.color(Vector(sulc(a), sulc(b), sulc(c)).sum / 3), DisplayOpacity.Opaque), onset.color(expectedValue), opacity)
          assert(distance(Rgba32.fromPackedInt(colors(face)), direct) <= 1, s"$reduction face $face")
