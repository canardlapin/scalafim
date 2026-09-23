package scalafim.surface.reference

import image4s.geometry.{Affine, D3}
import scalafim.image.*
import scalafim.image.SampleSpaces.*
import scalafim.surface.*

/** Independent route contracts. Vertices are placed at chosen continuous voxel
  * indices through the forward affine, and volume values encode their voxel, so
  * every expectation is derived from construction rather than from the kernel's
  * world-to-voxel inverse.
  */
class SurfaceRouteSuite extends munit.FunSuite:
  private val dims = Vector(6, 5, 4)
  private val spacing = Vector(2.0, 3.0, 1.5)
  private val shift = Vector(-7.25, 4.5, 11.0)

  // Oblique: rotation about z (30 deg) then about x (20 deg), anisotropic, shifted.
  private val rotation: Vector[Vector[Double]] =
    val (cz, sz) = (math.cos(math.Pi / 6), math.sin(math.Pi / 6))
    val (cx, sx) = (math.cos(math.Pi / 9), math.sin(math.Pi / 9))
    val rz = Vector(Vector(cz, -sz, 0.0), Vector(sz, cz, 0.0), Vector(0.0, 0.0, 1.0))
    val rx = Vector(Vector(1.0, 0.0, 0.0), Vector(0.0, cx, -sx), Vector(0.0, sx, cx))
    Vector.tabulate(3, 3)((r, c) => (0 until 3).map(k => rx(r)(k) * rz(k)(c)).sum)

  private def affineOf(rows: Vector[Vector[Double]]): Affine[D3] = Affine.fromRowMajor[D3](rows.flatten).toOption.get
  private def spaceOf(d: Vector[Int], rows: Vector[Vector[Double]]): SomeSampleSpace = SampleSpaces(d, affine = Some(affineOf(rows)))

  private val affineRows = Vector.tabulate(3)(r =>
    Vector.tabulate(3)(c => rotation(r)(c) * spacing(c)) :+ shift(r)) :+ Vector(0.0, 0.0, 0.0, 1.0)
  private val space = spaceOf(dims, affineRows)

  private def world(index: Vector[Double]): Vector[Double] =
    Vector.tabulate(3)(r => (0 until 3).map(c => rotation(r)(c) * spacing(c) * index(c)).sum + shift(r))

  private val code: (Int, Int, Int) => Double = (i, j, k) => 1.0 + i + 10.0 * j + 100.0 * k
  private def voxel(t: (Int, Int, Int)): VoxelCoord = VoxelCoord(t._1, t._2, t._3)

  /** Values are placed by voxel coordinate, independent of the canonical ordinal order. */
  private def volumeOf(value: (Int, Int, Int) => Double, on: SomeSampleSpace = space): SomeScalarVolume[Double] =
    SomeScalarVolume.unsafeCopyFromCanonicalArray(Array.tabulate(on.spatialDims.product) { ordinal =>
      val g = on.indexToGrid3D(ordinal)
      value(g(0), g(1), g(2))
    }, on, "route-fixture")

  private val ramp = volumeOf(code)

  /** Nearest voxel of a continuous index, or None outside [-0.5, dim - 0.5). */
  private def expectedVoxel(index: Vector[Double]): Option[(Int, Int, Int)] =
    val r = index.map(v => math.floor(v + 0.5).toInt)
    Option.when(r.indices.forall(a => r(a) >= 0 && r(a) < dims(a)))((r(0), r(1), r(2)))

  // Six vertices; v4 falls outside the grid, v5 is on the medial wall.
  private val midIndices = Vector(
    Vector(1.3, 2.2, 0.7), Vector(4.7, 0.2, 2.8), Vector(0.2, 3.6, 1.1),
    Vector(2.4, 1.3, 3.3), Vector(6.4, 1.0, 1.0), Vector(3.1, 4.2, 0.2))
  private val depth = Vector(0.0, 0.0, 1.9)
  private val faces = Vector((0, 1, 2), (2, 3, 4), (3, 4, 5))

  private def surface(points: Vector[Vector[Double]], kind: SurfaceKind, hemisphere: Hemisphere = Hemisphere.Left,
      rows: Vector[(Int, Int, Int)] = faces): SurfaceGeometry =
    SurfaceGeometry(TriangleMesh.fromRows(points, rows), hemisphere, kind)

  private val frameA = TemplateFrame.unsafe("MNI152NLin2009cAsym", "templateflow-24.2.0")
  private val frameB = TemplateFrame.unsafe("MNI152NLin6Asym", "templateflow-24.2.0")
  private val fsLR = TemplateId.unsafe("fsLR")

  /** Test-only declarations: fixture geometries have no file bytes to verify. */
  private def declaration(frame: TemplateFrame, name: String): FrameDeclaration =
    FrameDeclaration.make(frame,
      FrameBasis.literature("10.1093/cercor/bhr291", "synthetic fixture declared in this frame").toOption.get,
      AssetProvenance.make(fsLR, s"tpl-fsLR/$name", "test", "0" * 64).toOption.get).toOption.get

  private def declared(geometry: SurfaceGeometry, frame: TemplateFrame): DeclaredSurface =
    DeclaredSurface.unsafeAssumeVerified(declaration(frame, s"${geometry.kind.label}.surf.gii"), geometry)

  private val testMesh = StandardCorticalMesh.declare(CorticalMeshFamily.FsLR, "test6", 6).toOption.get

  private val midthickness = surface(midIndices.map(world), SurfaceKind.Midthickness)
  private val reference =
    val domain = midthickness.meshDomainEither.toOption.get
    val wall = MedialWallMask.fromCortexFlags(domain, Vector(true, true, true, true, true, false)).toOption.get
    CorticalMeshReference.make(testMesh, midthickness, wall).toOption.get

  private val whitePial = AnatomicalGeometry.WhitePial(
    declared(surface(midIndices.map(world), SurfaceKind.White), frameA),
    declared(surface(midIndices.map(i => world(Vector.tabulate(3)(a => i(a) + depth(a)))), SurfaceKind.Pial), frameA))

  private val midAnatomy = SamplingAnatomy.make(reference, AnatomicalGeometry.Midthickness(declared(midthickness, frameA))).toOption.get
  private val ribbonAnatomy = SamplingAnatomy.make(reference, whitePial).toOption.get
  private val source = VolumeReference.make(frameA, space).toOption.get

  private def request(method: MappingMethod = MappingMethod.MidthicknessNearest,
      semantics: ValueSemantics = ValueSemantics.Continuous, from: VolumeReference = source) =
    RouteRequest(from, testMesh, CorticalHemisphere.Left, method, semantics)

  private def admitted(anatomy: SamplingAnatomy, req: RouteRequest = request(), bridge: Option[FrameBridge] = None) =
    SurfaceRoute.admit(req, anatomy, bridge).fold(r => fail(r.message), identity)

  private def mapped(route: AdmittedSurfaceRoute, volume: SomeScalarVolume[Double]) =
    route.map(volume).fold(e => fail(e.message), identity)

  private def assertSameValue(actual: Option[Double], expected: Option[Double]): Unit =
    (actual, expected) match
      case (Some(a), Some(e)) => assertEqualsDouble(a, e, 1e-9)
      case _ => assertEquals(actual, expected)

  test("template identity is exact and family names are refused as ambiguous"):
    for alias <- Vector("MNI", "mni152", "ICBM152", "Talairach", "MNI2009", "MNI152NLin2009", "MNIICBM152",
        "ICBM2009c", "mni152nlin2009casym") do
      assertEquals(TemplateId.make(alias), Left(ReferenceError.AmbiguousTemplate(alias)))
    assert(TemplateId.make("").isLeft)
    assert(TemplateId.make("MNI152 NLin6Asym").isLeft)
    assert(TemplateId.make("MNI152NLin2009cAsym").isRight)
    assert(TemplateId.make("fsLR").isRight)
    assert(TemplateRelease.make(" ").isLeft)
    assertNotEquals(frameA, TemplateFrame.unsafe("MNI152NLin2009cAsym", "templateflow-23.1.0"))
    assertNotEquals(frameA, TemplateFrame.unsafe("MNI152NLin2009cAsym", "templateflow-24.2.0", Some("1")))

  test("admitted affines are owned copies that later caller mutation cannot change"):
    val raw = Array(1.0, 0.0, 0.0, 2.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0)
    val bridge = FrameBridge.affine(frameB, frameA, Affine.fromRowMajor[D3](raw).toOption.get, "test").toOption.get
    raw(3) = Double.NaN
    assertEqualsDouble(bridge.matrix.matrix(0, 3), 2.0, 0.0)
    val source = VolumeReference.make(frameA, space).toOption.get
    assertEqualsDouble(source.voxelToWorld.matrix(0, 3), shift(0), 0.0)

  test("volume reference refuses a support mask on another grid"):
    val shifted = spaceOf(dims, affineRows.updated(0, affineRows(0).updated(3, 0.0)))
    val support = SomeMaskVolume.unsafeCopyFromCanonicalArray(Array.fill(dims.product)(true), shifted, "support")
    assertEquals(VolumeReference.make(frameA, space, Some(support)), Left(ReferenceError.SupportGridMismatch))

  test("fsLR 32k admission binds exact vertex count, cortical hemisphere and mask domain"):
    def strip(n: Int, hemisphere: Hemisphere) = SurfaceGeometry(
      TriangleMesh.fromRows(Vector.tabulate(n)(i => Vector(i.toDouble, (i % 7).toDouble, (i % 3).toDouble)),
        Vector.tabulate(n - 2)(i => (i, i + 1, i + 2))), hemisphere, SurfaceKind.Midthickness)
    def wall(g: SurfaceGeometry) = MedialWallMask.fromCortexFlags(g.meshDomainEither.toOption.get,
      Vector.tabulate(g.vertexCount)(_ % 10 != 0)).toOption.get
    val exact = strip(32492, Hemisphere.Right)
    val admittedMesh = CorticalMeshReference.make(StandardCorticalMesh.FsLR32k, exact, wall(exact)).toOption.get
    assertEquals(admittedMesh.hemisphere, CorticalHemisphere.Right)
    assertEquals(admittedMesh.medialWall.medialWallCount, 3250)
    val short = strip(32491, Hemisphere.Right)
    assert(CorticalMeshReference.make(StandardCorticalMesh.FsLR32k, short, wall(short)).isLeft)
    val left = strip(32492, Hemisphere.Left)
    assert(CorticalMeshReference.make(StandardCorticalMesh.FsLR32k, exact, wall(left)).isLeft, "mask from other hemisphere")
    assertEquals(CorticalMeshReference.make(StandardCorticalMesh.FsLR32k, strip(32492, Hemisphere.Unknown), wall(exact)),
      Left(ReferenceError.InvalidCorticalMesh(SurfaceError.InvalidHemisphereTag(Hemisphere.Unknown).message)))
    assert(MedialWallMask.fromCortexFlags(exact.meshDomainEither.toOption.get, Vector.fill(32492)(false)).isLeft)

  test("anatomy refuses display shapes and topology that differs from the reference"):
    val inflated = surface(midIndices.map(world), SurfaceKind.Inflated)
    assert(SamplingAnatomy.make(reference, AnatomicalGeometry.Midthickness(declared(inflated, frameA))).isLeft)
    val rewound = surface(midIndices.map(world), SurfaceKind.Midthickness, rows = Vector((0, 2, 1), (2, 3, 4), (3, 4, 5)))
    assert(SamplingAnatomy.make(reference, AnatomicalGeometry.Midthickness(declared(rewound, frameA))).isLeft)
    val swapped = AnatomicalGeometry.WhitePial(
      declared(surface(midIndices.map(world), SurfaceKind.Pial), frameA),
      declared(surface(midIndices.map(world), SurfaceKind.White), frameA))
    assert(SamplingAnatomy.make(reference, swapped).isLeft)

  test("route admission refuses frame, bridge, hemisphere, mesh and method mismatches"):
    val otherFrame = SamplingAnatomy.make(reference, AnatomicalGeometry.Midthickness(declared(midthickness, frameB))).toOption.get
    assertEquals(SurfaceRoute.admit(request(), otherFrame), Left(RouteRefusal.FrameMismatch(frameA, frameB)))
    val forward = FrameBridge.affine(frameB, frameA, Affine.identity[D3], "declared test identity").toOption.get
    val reversed = FrameBridge.affine(frameA, frameB, Affine.identity[D3], "declared test identity").toOption.get
    val frameC = TemplateFrame.unsafe("MNIColin27", "templateflow-24.2.0")
    val unrelated = FrameBridge.affine(frameC, frameA, Affine.identity[D3], "declared test identity").toOption.get
    assert(SurfaceRoute.admit(request(), otherFrame, Some(forward)).isRight)
    assertEquals(SurfaceRoute.admit(request(), otherFrame, Some(reversed)), Left(RouteRefusal.ReversedBridge((frameB, frameA))))
    assertEquals(SurfaceRoute.admit(request(), otherFrame, Some(unrelated)),
      Left(RouteRefusal.BridgeMismatch((frameB, frameA), (frameC, frameA))))
    assertEquals(SurfaceRoute.admit(request(), midAnatomy, Some(forward)), Left(RouteRefusal.UnexpectedBridge(frameA)))
    assertEquals(SurfaceRoute.admit(request().copy(hemisphere = CorticalHemisphere.Right), midAnatomy),
      Left(RouteRefusal.HemisphereMismatch(CorticalHemisphere.Right, CorticalHemisphere.Left)))
    assertEquals(SurfaceRoute.admit(request().copy(targetMesh = StandardCorticalMesh.FsLR32k), midAnatomy),
      Left(RouteRefusal.TargetMeshMismatch(StandardCorticalMesh.FsLR32k, testMesh)))
    val ribbon = MappingMethod.DepthNearest(Vector(0.0, 1.0))
    assertEquals(SurfaceRoute.admit(request(ribbon), midAnatomy), Left(RouteRefusal.WhitePialRequired(ribbon)))
    assertEquals(SurfaceRoute.admit(request(MappingMethod.DepthNearest(Vector(1.2))), ribbonAnatomy),
      Left(RouteRefusal.InvalidMethod("depth fractions must lie in [0, 1]")))
    assertEquals(SurfaceRoute.admit(request(MappingMethod.DepthNearest(Vector.empty)), ribbonAnatomy),
      Left(RouteRefusal.InvalidMethod("depth fractions must be non-empty")))
    assert(FrameBridge.affine(frameA, frameA, Affine.identity[D3], "x").isLeft)
    assert(FrameBridge.affine(frameB, frameA, Affine.identity[D3], " ").isLeft)

  test("the PLSNeuro source space is refused for fsLR anatomy declared in another template frame"):
    val mni2 = spaceOf(Vector(97, 115, 97), Vector(
      Vector(2.0, 0.0, 0.0, -96.5), Vector(0.0, 2.0, 0.0, -132.5), Vector(0.0, 0.0, 2.0, -78.5), Vector(0.0, 0.0, 0.0, 1.0)))
    val groupSource = VolumeReference.make(frameA, mni2).toOption.get
    val fslrIn6Asym = SamplingAnatomy.make(reference, AnatomicalGeometry.Midthickness(declared(midthickness, frameB))).toOption.get
    val refusal = SurfaceRoute.admit(request(from = groupSource), fslrIn6Asym)
    assertEquals(refusal, Left(RouteRefusal.FrameMismatch(frameA, frameB)))
    assert(refusal.left.toOption.get.message.contains("MNI152NLin6Asym"))

  test("oblique anisotropic shifted grid: midthickness values, coverage and medial wall match construction"):
    val result = mapped(admitted(midAnatomy), ramp)
    for (index, v) <- midIndices.zipWithIndex do
      val vertex = VertexId(v)
      val expected = if v == 5 then None else expectedVoxel(index).map(code.tupled)
      assertSameValue(result.valueAt(vertex), expected)
    assertEquals(result.coverageAt(VertexId(4)), Some(VertexCoverage.NoSupport))
    assertEquals(result.coverageAt(VertexId(5)), Some(VertexCoverage.MedialWall))
    assertEquals(result.coverageAt(VertexId(6)), None)
    assertEquals(result.valueAt(VertexId(6)), None)
    assertEquals(result.count(VertexCoverage.Mapped), 4)
    assert(result.valuesCopy(5).isNaN && result.valuesCopy(4).isNaN)
    assertEquals(result.disclosure.qualification, RouteQualification.NumericalContract)
    assertEquals(result.disclosure.anatomy, "midthickness")
    assertEquals(result.disclosure.anatomyDeclarations.map(_.frame), Vector(frameA))

  test("white/pial midpoint and depth lookups follow construction, excluding lookups outside the grid"):
    val fractions = Vector(0.0, 0.5, 1.0)
    val midpoint = mapped(admitted(ribbonAnatomy), ramp)
    val depthRoute = mapped(admitted(ribbonAnatomy, request(MappingMethod.DepthNearest(fractions))), ramp)
    for (index, v) <- midIndices.zipWithIndex if v != 5 do
      val at = (f: Double) => Vector.tabulate(3)(a => index(a) + f * depth(a))
      assertSameValue(midpoint.valueAt(VertexId(v)), expectedVoxel(at(0.5)).map(code.tupled))
      val hits = fractions.flatMap(f => expectedVoxel(at(f))).map(code.tupled)
      assertSameValue(depthRoute.valueAt(VertexId(v)), Option.when(hits.nonEmpty)(hits.sum / hits.size))
      assertEquals(depthRoute.acceptedLookupsAt(VertexId(v)), Some(hits.size))
    assert(depthRoute.acceptedLookupsAt(VertexId(1)).get < 3, "fixture must exercise partial depth coverage")

  test("an asymmetric impulse reaches only the vertex whose lookup selects it; zero is data"):
    val target = expectedVoxel(midIndices(2)).get
    val impulse = volumeOf((i, j, k) => if (i, j, k) == target then 1.0 else 0.0)
    val result = mapped(admitted(midAnatomy), impulse)
    assertEquals((0 until 4).map(v => result.valueAt(VertexId(v))), Vector(Some(0.0), Some(0.0), Some(1.0), Some(0.0)))

  test("an explicit affine bridge from the anatomy frame reproduces the same-frame mapping"):
    val (c, s) = (math.cos(0.26), math.sin(0.26))
    val r = Vector(Vector(c, 0.0, s), Vector(0.0, 1.0, 0.0), Vector(-s, 0.0, c))
    val t = Vector(3.0, -2.0, 5.0)
    val toA = affineOf(Vector.tabulate(3)(i => r(i) :+ t(i)) :+ Vector(0.0, 0.0, 0.0, 1.0))
    def inB(p: Vector[Double]) = Vector.tabulate(3)(i => (0 until 3).map(k => r(k)(i) * (p(k) - t(k))).sum)
    // Surface coordinates carry their own non-commuting surfaceToWorld into frame B.
    val scale = Vector(2.0, 1.0, 0.5)
    val offset = Vector(1.0, 2.0, 3.0)
    val toFrameB = affineOf(Vector.tabulate(3)(i => Vector.tabulate(3)(k => if i == k then scale(i) else 0.0) :+ offset(i)) :+
      Vector(0.0, 0.0, 0.0, 1.0))
    def native(p: Vector[Double]) = Vector.tabulate(3)(i => (p(i) - offset(i)) / scale(i))
    val midB = SurfaceGeometry(TriangleMesh.fromRows(midIndices.map(i => native(inB(world(i)))), faces),
      Hemisphere.Left, SurfaceKind.Midthickness, toFrameB)
    val anatomyB = SamplingAnatomy.make(reference, AnatomicalGeometry.Midthickness(declared(midB, frameB))).toOption.get
    val bridge = FrameBridge.affine(frameB, frameA, toA, "synthetic rigid").toOption.get
    val route = admitted(anatomyB, bridge = Some(bridge))
    val bridged = mapped(route, ramp)
    val direct = mapped(admitted(midAnatomy), ramp)
    for v <- 0 until 6 do assertSameValue(bridged.valueAt(VertexId(v)), direct.valueAt(VertexId(v)))
    assertEquals(route.disclosure.bridge, Some(bridge))
    val evidence = route.inspect(ramp, VertexId(0)).toOption.get
    val w = world(midIndices(0))
    assertEqualsDouble(evidence.samples.head.world.x, w(0), 1e-9)
    assertEqualsDouble(evidence.samples.head.world.z, w(2), 1e-9)

  test("nearest-voxel ties round up and support is half-open [-0.5, dim - 0.5)"):
    val simple = spaceOf(Vector(4, 4, 4), Vector(
      Vector(2.0, 0.0, 0.0, -1.0), Vector(0.0, 2.0, 0.0, -1.0), Vector(0.0, 0.0, 2.0, -1.0), Vector(0.0, 0.0, 0.0, 1.0)))
    val idx = Vector(Vector(1.5, 1.0, 1.0), Vector(-0.5, 1.0, 1.0), Vector(3.5, 1.0, 1.0),
      Vector(3.49, 1.0, 1.0), Vector(-0.51, 1.0, 1.0), Vector(2.5, 2.5, 2.5))
    val points = idx.map(i => i.map(v => 2.0 * v - 1.0))
    val geometry = surface(points, SurfaceKind.Midthickness)
    val ref = CorticalMeshReference.make(testMesh, geometry, MedialWallMask.fromCortexFlags(
      geometry.meshDomainEither.toOption.get, Vector.fill(6)(true)).toOption.get).toOption.get
    val anatomy = SamplingAnatomy.make(ref, AnatomicalGeometry.Midthickness(declared(geometry, frameA))).toOption.get
    val src = VolumeReference.make(frameA, simple).toOption.get
    val result = mapped(admitted(anatomy, request(from = src)), volumeOf(code, simple))
    assertEquals((0 until 6).map(v => result.valueAt(VertexId(v))),
      Vector(Some(code(2, 1, 1)), Some(code(0, 1, 1)), None, Some(code(3, 1, 1)), None, Some(code(3, 3, 3))))

  test("nonfinite and unsupported voxels are excluded, disclosed per lookup, and never poison averages"):
    val fractions = Vector(0.0, 0.5, 1.0)
    val route = admitted(ribbonAnatomy, request(MappingMethod.DepthNearest(fractions)))
    val at = (f: Double) => Vector.tabulate(3)(a => midIndices(0)(a) + f * depth(a))
    val voxels = fractions.map(f => expectedVoxel(at(f)).get)
    val poisoned = volumeOf((i, j, k) =>
      if (i, j, k) == voxels(1) then Double.NaN else if (i, j, k) == voxels(2) then Double.PositiveInfinity else code(i, j, k))
    val result = mapped(route, poisoned)
    assertSameValue(result.valueAt(VertexId(0)), Some(code.tupled(voxels(0))))
    val evidence = route.inspect(poisoned, VertexId(0)).toOption.get
    val kinds = evidence.samples.map(_.contribution)
    assertEquals(kinds(0), Contribution.Included(voxel(voxels(0)), code.tupled(voxels(0)), 1.0))
    assert(kinds(1) match { case Contribution.NonFinite(v, x) => v == voxel(voxels(1)) && x.isNaN; case _ => false })
    assertEquals(kinds(2), Contribution.NonFinite(voxel(voxels(2)), Double.PositiveInfinity))

    val allNaN = mapped(route, volumeOf((_, _, _) => Double.NaN))
    assertEquals(allNaN.count(VertexCoverage.NoSupport), 5)

    val support = SomeMaskVolume.unsafeCopyFromCanonicalArray(Array.tabulate(dims.product) { ordinal =>
      val g = space.indexToGrid3D(ordinal)
      (g(0), g(1), g(2)) != voxels(0)
    }, space, "support")
    val supported = VolumeReference.make(frameA, space, Some(support)).toOption.get
    val restricted = admitted(ribbonAnatomy, request(MappingMethod.DepthNearest(fractions), from = supported))
    val restrictedEvidence = restricted.inspect(ramp, VertexId(0)).toOption.get
    assertEquals(restrictedEvidence.samples.head.contribution, Contribution.OutsideSupport(voxel(voxels(0))))
    val rest = voxels.drop(1).map(code.tupled)
    assertSameValue(restrictedEvidence.value, Some(rest.sum / rest.size))

  test("per-vertex evidence reconstructs every mapped value and links picks to source voxels"):
    val route = admitted(ribbonAnatomy, request(MappingMethod.DepthNearest(Vector(0.0, 0.0, 0.5, 1.0))))
    val result = mapped(route, ramp)
    for v <- 0 until 6 do
      val evidence = route.inspect(ramp, VertexId(v)).toOption.get
      assertEquals(Some(evidence.coverage), result.coverageAt(VertexId(v)))
      val included = evidence.samples.collect { case ContributionSample(_, Contribution.Included(voxel, value, weight)) =>
        assertEqualsDouble(value, code(voxel.x, voxel.y, voxel.z), 0.0)
        value * weight }
      assertSameValue(evidence.value, result.valueAt(VertexId(v)))
      if evidence.coverage == VertexCoverage.Mapped then assertEqualsDouble(included.sum, result.valueAt(VertexId(v)).get, 1e-9)
    assertEquals(route.inspect(ramp, VertexId(6)), Left(RouteError.VertexOutOfRange(6, 6)))

  test("categorical semantics take the modal integral label and refuse non-integral volumes"):
    val route = admitted(ribbonAnatomy, request(MappingMethod.DepthNearest(Vector(0.0, 0.5, 1.0)), ValueSemantics.Categorical))
    val at = (f: Double) => Vector.tabulate(3)(a => midIndices(0)(a) + f * depth(a))
    val voxels = Vector(0.0, 0.5, 1.0).map(f => expectedVoxel(at(f)).get)
    val labels = volumeOf((i, j, k) => if (i, j, k) == voxels(0) then 7.0 else if voxels.contains((i, j, k)) then 3.0 else 0.0)
    assertSameValue(mapped(route, labels).valueAt(VertexId(0)), Some(3.0))
    val tie = admitted(ribbonAnatomy, request(MappingMethod.DepthNearest(Vector(0.0, 1.0)), ValueSemantics.Categorical))
    assertSameValue(mapped(tie, labels).valueAt(VertexId(0)), Some(3.0))
    val continuous = admitted(ribbonAnatomy, request(MappingMethod.DepthNearest(Vector(0.0, 0.5, 1.0))))
    assertSameValue(mapped(continuous, labels).valueAt(VertexId(0)), Some(13.0 / 3.0))
    assert(route.map(volumeOf((i, j, k) => code(i, j, k) + 0.25)).left.exists(_.isInstanceOf[RouteError.NonIntegralLabel]))
    val evidence = route.inspect(labels, VertexId(0)).toOption.get
    val weights = evidence.samples.collect { case ContributionSample(_, Contribution.Included(_, value, weight)) => value -> weight }
    assertEquals(weights, Vector(7.0 -> 0.0, 3.0 -> 0.5, 3.0 -> 0.5))
    assertEqualsDouble(weights.map((v, w) => v * w).sum, 3.0, 1e-12)

  test("a volume on any other grid is refused at execution"):
    val shifted = spaceOf(dims, affineRows.updated(1, affineRows(1).updated(3, 4.75)))
    val route = admitted(midAnatomy)
    assert(route.map(volumeOf(code, shifted)).left.exists(_.isInstanceOf[RouteError.SourceGridMismatch]))
    assert(route.inspect(volumeOf(code, shifted), VertexId(0)).isLeft)

  test("grid identity is the persistent grid key, not the live object that admitted the reference"):
    // An independently constructed space with identical dims and bitwise-equal affine
    // is the same persistent grid; its volumes and support masks are admitted.
    val twin = spaceOf(dims, affineRows)
    assert(!twin.grid.sameRuntimeOwnerAs(space.grid), "fixture must use a distinct live grid")
    val route = admitted(midAnatomy)
    val onTwin = mapped(route, volumeOf(code, twin))
    val direct = mapped(route, ramp)
    for v <- 0 until 6 do assertSameValue(onTwin.valueAt(VertexId(v)), direct.valueAt(VertexId(v)))
    val twinSupport = SomeMaskVolume.unsafeCopyFromCanonicalArray(Array.fill(dims.product)(true), twin, "support")
    assert(VolumeReference.make(frameA, space, Some(twinSupport)).isRight)
    // One ulp in one affine element is a different grid.
    val nudged = affineRows.updated(2, affineRows(2).updated(3, math.nextUp(shift(2))))
    assert(route.map(volumeOf(code, spaceOf(dims, nudged))).left.exists(_.isInstanceOf[RouteError.SourceGridMismatch]))

  test("inflated and very-inflated display keep identical values, coverage and vertex ids"):
    val result = mapped(admitted(midAnatomy), ramp)
    val inflatedPoints = midIndices.map(i => world(i).map(x => 3.0 * x + 50.0))
    val inflated = DisplaySurface.make(reference, surface(inflatedPoints, SurfaceKind.Inflated)).toOption.get
    val very = DisplaySurface.make(reference, surface(inflatedPoints.map(_.map(_ * 1.7)), SurfaceKind.Custom("very_inflated"))).toOption.get
    assertEquals(very.form, DisplayForm.VeryInflated)
    val shown = Vector(inflated, very).map(d => result.onDisplay(d).toOption.get)
    for displayed <- shown; v <- 0 until 6 do
      assert(displayed.mapped eq result)
      assertEquals(displayed.mapped.coverageAt(VertexId(v)), result.coverageAt(VertexId(v)))
      assertSameValue(displayed.mapped.valueAt(VertexId(v)), result.valueAt(VertexId(v)))
    val rewound = surface(inflatedPoints, SurfaceKind.Inflated, rows = Vector((0, 2, 1), (2, 3, 4), (3, 4, 5)))
    assert(DisplaySurface.make(reference, rewound).isLeft)
    assert(DisplaySurface.make(reference, surface(inflatedPoints, SurfaceKind.Custom("flat"))).isLeft)
    val otherTopology = surface(inflatedPoints, SurfaceKind.Inflated, rows = Vector((0, 1, 2), (1, 3, 4), (3, 4, 5)))
    val otherRef = CorticalMeshReference.make(testMesh, otherTopology, MedialWallMask.fromCortexFlags(
      otherTopology.meshDomainEither.toOption.get, Vector.fill(6)(true)).toOption.get).toOption.get
    val foreign = DisplaySurface.make(otherRef, otherTopology).toOption.get
    assert(result.onDisplay(foreign).left.exists(_.isInstanceOf[RouteError.DisplayMismatch]))
    val otherWall = CorticalMeshReference.make(testMesh, midthickness, MedialWallMask.fromCortexFlags(
      reference.domain, Vector.fill(6)(true)).toOption.get).toOption.get
    val sameShape = DisplaySurface.make(otherWall, surface(inflatedPoints, SurfaceKind.Inflated)).toOption.get
    assert(result.onDisplay(sameShape).left.exists(_.isInstanceOf[RouteError.DisplayMismatch]), "medial wall differs")
    val twin = CorticalMeshReference.make(testMesh, midthickness, reference.medialWall).toOption.get
    assert(result.onDisplay(DisplaySurface.make(twin, surface(inflatedPoints, SurfaceKind.Inflated)).toOption.get).isRight)

  test("selection prefers same-frame anatomy and reports every refusal when none is admissible"):
    val otherFrame = SamplingAnatomy.make(reference, AnatomicalGeometry.Midthickness(declared(midthickness, frameB))).toOption.get
    val bridge = FrameBridge.affine(frameB, frameA, Affine.identity[D3], "declared test identity").toOption.get
    val chosen = SurfaceRoute.select(request(), Vector(RouteCandidate(otherFrame), RouteCandidate(otherFrame, Some(bridge)),
      RouteCandidate(midAnatomy))).toOption.get
    assertEquals(chosen.bridge, None)
    assertEquals(chosen.anatomy, midAnatomy)
    val refused = SurfaceRoute.select(request(), Vector(RouteCandidate(otherFrame), RouteCandidate(midAnatomy, Some(bridge))))
    assertEquals(refused.left.toOption.get.map(_._2),
      Vector(RouteRefusal.FrameMismatch(frameA, frameB), RouteRefusal.UnexpectedBridge(frameA)))
