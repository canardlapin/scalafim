package scalafim.image

class DenseFieldInterpolationPlanSuite extends munit.FunSuite:

  private def assertClose(actual: Double, expected: Double, tol: Double = 1e-10): Unit =
    assert(math.abs(actual - expected) <= tol, clue = s"actual=$actual expected=$expected")

  private def assertClose(actual: Vector[Double], expected: Vector[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length, clue = "")
    actual.zip(expected).foreach { case (a, e) => assertClose(a, e, tol) }

  private def denseField(grid: GridSpec)(f: (VoxelCoord, Int) => Double): NDArray[Double] =
    val data =
      NArrayUtil.tabulate[Double](grid.nVoxels * 3) { i =>
        val component = i / grid.nVoxels
        val lin = i % grid.nVoxels
        val coord = Indexing.indexToGrid3D(grid.shape, lin)
        f(coord, component)
      }
    NDArray(data, grid.dims :+ 3)

  test("linear plans reuse interpolation weights across compatible dense fields") {
    val grid = GridSpec.identity(Vector(2, 1, 1))
    val points = Vector(Vector(0.5, 0.0, 0.0))
    val plan =
      DenseFieldInterpolationPlan.make(grid, points, Resample.Method.Linear)
        .fold(err => fail(err.message), identity)
    val first =
      denseField(grid) { (coord, component) =>
        component match
          case 0 => if coord.x == 0 then 1.0 else 3.0
          case 1 => 10.0
          case _ => 20.0
      }
    val second =
      denseField(grid) { (coord, component) =>
        component match
          case 0 => if coord.x == 0 then -2.0 else 2.0
          case 1 => -10.0
          case _ => -20.0
      }

    assertEquals(plan.queryCount, 1, clue = "")
    assertEquals(plan.method, Resample.Method.Linear, clue = "")
    val firstSample = plan.sample(first, DenseFieldOutside.Zero).fold(err => fail(err.message), identity)
    val secondSample = plan.sample(second, DenseFieldOutside.Zero).fold(err => fail(err.message), identity)
    assertClose(firstSample.head, Vector(2.0, 10.0, 20.0), 1e-10)
    assertClose(secondSample.head, Vector(0.0, -10.0, -20.0), 1e-10)
  }

  test("query-point outside policy blends missing linear field corners") {
    val grid = GridSpec.identity(Vector(1, 1, 1))
    val point = Vector(0.5, 0.0, 0.0)
    val plan =
      DenseFieldInterpolationPlan.make(grid, Vector(point), Resample.Method.Linear)
        .fold(err => fail(err.message), identity)
    val field =
      denseField(grid) { (_, component) =>
        component match
          case 0 => 10.0
          case 1 => 20.0
          case _ => 30.0
      }

    val sample = plan.sample(field, DenseFieldOutside.QueryPoint).fold(err => fail(err.message), identity)
    assertClose(sample.head, Vector(5.25, 10.0, 15.0), 1e-10)
  }

  test("nearest plans use outside policy for out-of-bounds samples") {
    val grid = GridSpec.identity(Vector(2, 1, 1))
    val point = Vector(10.0, 5.0, 6.0)
    val plan =
      DenseFieldInterpolationPlan.make(grid, Vector(point), Resample.Method.Nearest)
        .fold(err => fail(err.message), identity)
    val field = denseField(grid)((_, _) => 99.0)

    val zero = plan.sample(field, DenseFieldOutside.Zero).fold(err => fail(err.message), identity)
    val query = plan.sample(field, DenseFieldOutside.QueryPoint).fold(err => fail(err.message), identity)
    assertClose(zero.head, Vector(0.0, 0.0, 0.0), 1e-10)
    assertClose(query.head, point, 1e-10)
  }

  test("cubic plans reuse Catmull-Rom interpolation weights") {
    val grid = GridSpec.identity(Vector(5, 4, 4))
    val point = Vector(1.5, 1.0, 1.0)
    val plan =
      DenseFieldInterpolationPlan.make(grid, Vector(point), Resample.Method.Cubic)
        .fold(err => fail(err.message), identity)
    val field =
      denseField(grid) { (coord, component) =>
        component match
          case 0 => coord.x.toDouble * coord.x.toDouble
          case 1 => coord.x.toDouble
          case _ => 1.0
      }

    val sample = plan.sample(field, DenseFieldOutside.Zero).fold(err => fail(err.message), identity)
    assertClose(sample.head, Vector(2.25, 1.5, 1.0), 1e-10)
  }

  test("plans reject incompatible fields explicitly") {
    val grid = GridSpec.identity(Vector(2, 1, 1))
    val point = Vector(0.0, 0.0, 0.0)
    val plan =
      DenseFieldInterpolationPlan.make(grid, Vector(point), Resample.Method.Nearest)
        .fold(err => fail(err.message), identity)
    val wrongShape = NDArray(NArrayUtil.fillConst[Double](6, 0.0), Vector(2, 3))
    val wrong = plan.sample(wrongShape, DenseFieldOutside.Zero)
    wrong match
      case Left(MorphismError.DenseFieldShapeMismatch(expected, actual)) =>
        assertEquals(expected, Vector(2, 1, 1, 3), clue = "")
        assertEquals(actual, Vector(2, 3), clue = "")
      case other => fail(s"expected dense field shape error, got $other")
  }

  test("dense morphisms expose reusable interpolation plans") {
    val grid = GridSpec.identity(Vector(2, 1, 1))
    val native = SpatialDomainId("native")
    val mni = SpatialDomainId("mni")
    val field =
      denseField(grid) { (coord, component) =>
        if component == 0 then coord.x.toDouble else 0.0
      }
    val morphism =
      DenseFieldMorphism.displacement(native, mni, grid, field, Resample.Method.Linear)
        .fold(err => fail(err.message), identity)
    val plan =
      morphism.interpolationPlan(Vector(Vector(0.5, 0.0, 0.0)))
        .fold(err => fail(err.message), identity)
    val sampled = plan.sample(field, DenseFieldOutside.Zero).fold(err => fail(err.message), identity)

    assertClose(sampled.head, Vector(0.5, 0.0, 0.0), 1e-10)
    assertClose(morphism.transform(Vector(0.5, 0.0, 0.0)), Vector(1.0, 0.0, 0.0), 1e-10)
  }

  test("dense cubic morphisms expose reusable interpolation plans") {
    val grid = GridSpec.identity(Vector(5, 4, 4))
    val native = SpatialDomainId("native")
    val mni = SpatialDomainId("mni")
    val field =
      denseField(grid) { (coord, component) =>
        if component == 0 then coord.x.toDouble * coord.x.toDouble else 0.0
      }
    val morphism =
      DenseFieldMorphism.displacement(native, mni, grid, field, Resample.Method.Cubic)
        .fold(err => fail(err.message), identity)

    val sampled =
      morphism.interpolationPlan(Vector(Vector(1.5, 1.0, 1.0)))
        .flatMap(_.sample(field, DenseFieldOutside.Zero))
        .fold(err => fail(err.message), identity)

    assertClose(sampled.head, Vector(2.25, 0.0, 0.0), 1e-10)
    assertClose(morphism.transform(Vector(1.5, 1.0, 1.0)), Vector(3.75, 1.0, 1.0), 1e-10)
  }
