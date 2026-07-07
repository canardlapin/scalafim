package scalafim.image

class MorphismSuite extends munit.FunSuite:

  private val native = SpatialDomainId("native")
  private val mni = SpatialDomainId("mni")
  private val template = SpatialDomainId("template")
  private val report = SpatialDomainId("report")

  private def affine(
      source: SpatialDomainId,
      target: SpatialDomainId,
      matrix: DMat,
      cost: Double = 1.0
  ): Affine3DMorphism =
    Affine3DMorphism.make(source, target, matrix, cost).fold(err => fail(err.message), identity)

  private def translation(x: Double, y: Double, z: Double): DMat =
    DMat.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, x),
        Vector(0.0, 1.0, 0.0, y),
        Vector(0.0, 0.0, 1.0, z),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  private def scale(x: Double, y: Double, z: Double): DMat =
    DMat.fromRows(
      Vector(
        Vector(x, 0.0, 0.0, 0.0),
        Vector(0.0, y, 0.0, 0.0),
        Vector(0.0, 0.0, z, 0.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  private def assertClose(actual: Double, expected: Double, tol: Double = 1e-10): Unit =
    assert(math.abs(actual - expected) <= tol, clue = s"actual=$actual expected=$expected")

  private def assertClose(actual: Vector[Double], expected: Vector[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length, clue = "")
    actual.zip(expected).foreach { case (a, e) => assertClose(a, e, tol) }

  private def assertClose(actual: SpatialPoint, expected: SpatialPoint, tol: Double): Unit =
    assertClose(actual.toVector, expected.toVector, tol)

  private def assertClose(actual: WorldPoint, expected: WorldPoint, tol: Double): Unit =
    assertClose(actual.toVector, expected.toVector, tol)

  private def denseField(grid: GridSpec)(f: (VoxelCoord, Int) => Double): NDArray[Double] =
    val data =
      NArrayUtil.tabulate[Double](grid.nVoxels * 3) { i =>
        val component = i / grid.nVoxels
        val lin = i % grid.nVoxels
        val coord = Indexing.indexToGrid3D(grid.shape, lin)
        f(coord, component)
      }
    NDArray(data, grid.dims :+ 3)

  test("affine morphism applies target-to-source pullback coordinates") {
    val morphism = affine(native, mni, translation(10.0, 20.0, 30.0))

    assertClose(
      morphism.transform(Vector(1.0, 2.0, 3.0)),
      Vector(11.0, 22.0, 33.0),
      1e-10
    )
  }

  test("morphisms expose typed SpatialPoint transform and jacobian helpers") {
    val morphism = affine(native, mni, translation(10.0, 20.0, 30.0))
    val point = SpatialPoint(1.0, 2.0, 3.0)
    val expected = SpatialPoint(11.0, 22.0, 33.0)

    assertClose(morphism.transform(point), expected, 1e-10)
    assertEquals(morphism.transformPoints(Vector(point)), Vector(expected), clue = "")

    val jacobian = morphism.jacobianAt(point).fold(err => fail(err.message), identity)
    assertClose(jacobian(0, 0), 1.0)
    assertClose(jacobian(1, 1), 1.0)
    assertClose(jacobian(2, 2), 1.0)

    val det = morphism.jacobianDetAt(point).fold(err => fail(err.message), identity)
    assertClose(det, 1.0)
  }

  test("morphisms expose role-specific WorldPoint transform and jacobian helpers") {
    val morphism = affine(native, mni, translation(10.0, 20.0, 30.0))
    val point = WorldPoint(1.0, 2.0, 3.0)
    val expected = WorldPoint(11.0, 22.0, 33.0)

    assertClose(morphism.transform(point), expected, 1e-10)
    assertEquals(morphism.transformWorldPoints(Vector(point)), Vector(expected), clue = "")

    val jacobian = morphism.jacobianAtWorld(point).fold(err => fail(err.message), identity)
    assertClose(jacobian(0, 0), 1.0)
    assertClose(jacobian(1, 1), 1.0)
    assertClose(jacobian(2, 2), 1.0)

    val det = morphism.jacobianDetAtWorld(point).fold(err => fail(err.message), identity)
    assertClose(det, 1.0)
  }

  test("composition f.andThen(g) stores source-to-target and applies pullback in reverse") {
    val f = affine(native, mni, translation(10.0, 20.0, 30.0))
    val g = affine(mni, template, scale(2.0, 3.0, 4.0))

    val composed = f.andThen(g).fold(err => fail(err.message), identity)
    assert(composed.isInstanceOf[Affine3DMorphism], clue = "affine composition should fuse into one affine")

    val pointInTemplate = Vector(1.0, 2.0, 3.0)
    val expected = f.transform(g.transform(pointInTemplate))
    assertClose(composed.transform(pointInTemplate), expected, 1e-10)
    assertClose(composed.transform(pointInTemplate), Vector(12.0, 26.0, 42.0), 1e-10)
  }

  test("morphism paths validate adjacent domains and preserve pullback order") {
    val f = affine(native, mni, translation(10.0, 0.0, 0.0))
    val g = affine(mni, template, scale(2.0, 3.0, 4.0))
    val h = affine(template, report, translation(-1.0, 5.0, 2.0))

    val path = MorphismPath.make(Vector(f, g, h)).fold(err => fail(err.message), identity)
    val pointInReport = Vector(2.0, 2.0, 2.0)
    val expected = f.transform(g.transform(h.transform(pointInReport)))
    assertClose(path.transform(pointInReport), expected, 1e-10)

    val mismatch = f.andThen(h)
    assert(mismatch.isLeft, clue = "domain mismatch should be represented as an error")
  }

  test("identity morphism is neutral for composition") {
    val id = IdentityMorphism(native)
    val f = affine(native, mni, translation(1.0, 2.0, 3.0))

    assertEquals(id.andThen(f), Right(f), clue = "")
    assertEquals(f.andThen(IdentityMorphism(mni)), Right(f), clue = "")
  }

  test("affine morphisms invert analytically") {
    val morphism = affine(native, mni, translation(10.0, 20.0, 30.0))
    val inverse = morphism.invert.fold(err => fail(err.message), identity)

    val pointInMni = Vector(4.0, 5.0, 6.0)
    val pointInNative = morphism.transform(pointInMni)
    assertClose(inverse.transform(pointInNative), pointInMni, 1e-10)
  }

  test("affine jacobians expose pullback and pushforward determinants") {
    val morphism = affine(native, mni, scale(2.0, 3.0, 4.0))
    val coords = Vector(Vector(0.0, 0.0, 0.0), Vector(1.0, 2.0, 3.0))

    val pullback = morphism.jacobian(coords).fold(err => fail(err.message), identity)
    assertEquals(pullback.size, 2, clue = "")
    assertClose(pullback(0)(0, 0), 2.0)
    assertClose(pullback(0)(1, 1), 3.0)
    assertClose(pullback(0)(2, 2), 4.0)

    val dets = morphism.jacobianDet(coords).fold(err => fail(err.message), identity)
    assertClose(dets(0), 24.0)
    assertClose(dets(1), 24.0)

    val pushforward =
      morphism.jacobian(coords, JacobianMode.Pushforward).fold(err => fail(err.message), identity)
    assertClose(pushforward(0)(0, 0), 0.5)
    assertClose(pushforward(0)(1, 1), 1.0 / 3.0)
    assertClose(pushforward(0)(2, 2), 0.25)

    val pushDets =
      morphism.jacobianDet(coords, mode = JacobianMode.Pushforward).fold(err => fail(err.message), identity)
    assertClose(pushDets(0), 1.0 / 24.0)
    assertClose(pushDets(1), 1.0 / 24.0)
  }

  test("dense displacement fields apply interpolated target-to-source pullback displacements") {
    val grid = GridSpec.identity(Vector(2, 1, 1))
    val field =
      denseField(grid) { (coord, component) =>
        component match
          case 0 => if coord.x == 0 then 1.0 else 3.0
          case _ => 0.0
      }
    val morphism =
      DenseFieldMorphism.displacement(native, mni, grid, field, Resample.Method.Linear)
        .fold(err => fail(err.message), identity)

    assertEquals(morphism.kind, MorphismKind.DenseDisplacementField, clue = "")
    assertEquals(morphism.inverseKind, InverseKind.Unavailable, clue = "")
    assertClose(morphism.transform(Vector(0.5, 0.0, 0.0)), Vector(2.5, 0.0, 0.0), 1e-10)
    assert(morphism.invert.isLeft, clue = "dense fields should not claim a direct inverse")
  }

  test("dense coordinate fields return interpolated absolute source coordinates") {
    val grid = GridSpec.identity(Vector(2, 1, 1))
    val field =
      denseField(grid) { (coord, component) =>
        component match
          case 0 => 10.0 + 2.0 * coord.x.toDouble
          case 1 => 20.0
          case _ => 30.0
      }
    val morphism =
      DenseFieldMorphism.coordinates(native, mni, grid, field, Resample.Method.Linear)
        .fold(err => fail(err.message), identity)

    assertEquals(morphism.kind, MorphismKind.DenseCoordinateField, clue = "")
    assertClose(morphism.transform(Vector(0.5, 0.0, 0.0)), Vector(11.0, 20.0, 30.0), 1e-10)
  }

  test("dense field jacobians use numeric derivatives of the pullback transform") {
    val grid = GridSpec.identity(Vector(3, 3, 3))
    val field =
      denseField(grid) { (coord, component) =>
        component match
          case 0 => 0.2 * coord.x.toDouble
          case 1 => -0.1 * coord.y.toDouble
          case _ => 0.5 * coord.z.toDouble
      }
    val morphism =
      DenseFieldMorphism.displacement(native, mni, grid, field, Resample.Method.Linear)
        .fold(err => fail(err.message), identity)
    val coords = Vector(Vector(1.0, 1.0, 1.0))

    val pullback = morphism.jacobian(coords).fold(err => fail(err.message), identity)
    assertClose(pullback(0)(0, 0), 1.2, 1e-6)
    assertClose(pullback(0)(1, 1), 0.9, 1e-6)
    assertClose(pullback(0)(2, 2), 1.5, 1e-6)
    assertClose(pullback.determinants.head, 1.2 * 0.9 * 1.5, 1e-6)

    val pushforward =
      morphism.jacobian(coords, JacobianMode.Pushforward).fold(err => fail(err.message), identity)
    assertClose(pushforward(0)(0, 0), 1.0 / 1.2, 1e-6)
    assertClose(pushforward(0)(1, 1), 1.0 / 0.9, 1e-6)
    assertClose(pushforward(0)(2, 2), 1.0 / 1.5, 1e-6)
  }

  test("dense field constructors validate shape, interpolation, values, and grid invertibility") {
    val grid = GridSpec.identity(Vector(2, 1, 1))
    val wrongShape = NDArray(NArrayUtil.fillConst[Double](6, 0.0), Vector(2, 3))
    val shapeResult =
      DenseFieldMorphism.displacement(native, mni, grid, wrongShape, Resample.Method.Linear)
    shapeResult match
      case Left(MorphismError.DenseFieldShapeMismatch(expected, actual)) =>
        assertEquals(expected, Vector(2, 1, 1, 3), clue = "")
        assertEquals(actual, Vector(2, 3), clue = "")
      case other => fail(s"expected dense field shape error, got $other")

    val cubicResult =
      DenseFieldMorphism.displacement(native, mni, grid, denseField(grid)((_, _) => 0.0), Resample.Method.Cubic)
    assert(cubicResult.isRight, clue = "cubic dense fields should now be supported")

    val badData = NArrayUtil.fillConst[Double](grid.nVoxels * 3, 0.0)
    badData(1) = Double.NaN
    val badField = NDArray[Double](badData, grid.dims :+ 3)
    val valueResult =
      DenseFieldMorphism.coordinates(native, mni, grid, badField, Resample.Method.Nearest)
    assertEquals(valueResult, Left(MorphismError.NonFiniteFieldValue(1)), clue = "")

    val singular = DMat.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, 0.0),
        Vector(0.0, 0.0, 0.0, 0.0),
        Vector(0.0, 0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )
    val singularGrid = GridSpec(Vector(2, 1, 1), singular)
    val singularResult =
      DenseFieldMorphism.displacement(native, mni, singularGrid, denseField(singularGrid)((_, _) => 0.0))
    singularResult match
      case Left(MorphismError.SingularMatrix(_)) => ()
      case other => fail(s"expected singular grid error, got $other")
  }

  test("path jacobians use the chain rule in pullback order") {
    val f = affine(native, mni, scale(2.0, 3.0, 4.0))
    val g = affine(mni, template, scale(5.0, 6.0, 7.0))
    val path = MorphismPath.make(Vector(f, g)).fold(err => fail(err.message), identity)
    val coords = Vector(Vector(1.0, 2.0, 3.0))

    val field = path.jacobian(coords).fold(err => fail(err.message), identity)
    assertClose(field(0)(0, 0), 10.0)
    assertClose(field(0)(1, 1), 18.0)
    assertClose(field(0)(2, 2), 28.0)
    assertClose(field.determinants.head, 10.0 * 18.0 * 28.0)
  }

  test("execution plans validate paths, drop identities, and fuse adjacent affines") {
    val id = IdentityMorphism(native)
    val f = affine(native, mni, translation(1.0, 0.0, 0.0))
    val g = affine(mni, template, translation(0.0, 2.0, 0.0))
    val plan =
      MorphismExecutionPlan.make(Vector(id, f, g, IdentityMorphism(template)))
        .fold(err => fail(err.message), identity)

    assertEquals(plan.steps.length, 1, clue = "")
    assertEquals(plan.fusedAffinePairs, 1, clue = "")
    assertClose(plan.transform(Vector(Vector(0.0, 0.0, 0.0))).head, Vector(1.0, 2.0, 0.0), 1e-10)
    assertClose(plan.transform(SpatialPoint.Origin), SpatialPoint(1.0, 2.0, 0.0), 1e-10)
    assertClose(plan.transform(WorldPoint.Origin), WorldPoint(1.0, 2.0, 0.0), 1e-10)
    assertEquals(plan.transformPoints(Vector(SpatialPoint.Origin)), Vector(SpatialPoint(1.0, 2.0, 0.0)), clue = "")
    assertEquals(plan.transformWorldPoints(Vector(WorldPoint.Origin)), Vector(WorldPoint(1.0, 2.0, 0.0)), clue = "")

    val identities =
      MorphismExecutionPlan.make(Vector(IdentityMorphism(native), IdentityMorphism(native)))
        .fold(err => fail(err.message), identity)
    assertEquals(identities.steps.length, 1, clue = "")
    assertEquals(identities.source, native, clue = "")
  }
