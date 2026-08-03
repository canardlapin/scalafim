package scalafim.image

import ravel.NDArray as RavelArray
import ravel.Rank

class DenseFieldInverseSuite extends munit.FunSuite:

  private val native = SpatialDomainId("native")
  private val mni = SpatialDomainId("mni")

  private def assertClose(actual: Double, expected: Double, tol: Double = 1e-10): Unit =
    assert(math.abs(actual - expected) <= tol, clue = s"actual=$actual expected=$expected")

  private def assertClose(actual: Vector[Double], expected: Vector[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length, clue = "")
    actual.zip(expected).foreach { case (a, e) => assertClose(a, e, tol) }

  private def assertClose(actual: WorldPoint, expected: WorldPoint, tol: Double): Unit =
    assertClose(actual.toVector, expected.toVector, tol)

  private def denseField(
      grid: GridSpec
  )(f: (VoxelCoord, Int) => Double): RavelArray[Double, Rank[4]] =
    RavelArray.tabulate[Double](
      grid.shape.x,
      grid.shape.y,
      grid.shape.z,
      3
    )((i, j, k, component) => f(VoxelCoord(i, j, k), component))

  private def translation(x: Double, y: Double, z: Double): DMat =
    DMat.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, x),
        Vector(0.0, 1.0, 0.0, y),
        Vector(0.0, 0.0, 1.0, z),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  test("approximate inverse turns a small displacement field into a reverse displacement field") {
    val grid = GridSpec.identity(Vector(4, 4, 4))
    val shift = Vector(0.2, -0.1, 0.05)
    val field =
      denseField(grid) { (_, component) =>
        shift(component)
      }
    val morphism =
      DenseFieldMorphism.displacement(native, mni, grid, field, Resample.Method.Linear)
        .fold(err => fail(err.message), identity)
    val inverseGrid = GridSpec(Vector(2, 1, 1), translation(1.0, 1.0, 1.0))

    val result =
      morphism.approximateInverse(inverseGrid)
        .fold(err => fail(err.message), identity)
    val query = Vector(1.0, 1.0, 1.0)
    val inversePoint = result.morphism.transform(query)
    val roundtrip = morphism.transform(inversePoint)
    val typedQuery = WorldPoint(1.0, 1.0, 1.0)
    val typedInversePoint = result.morphism.transform(typedQuery)
    val typedRoundtrip = morphism.transform(typedInversePoint)

    assert(result.converged, clue = s"maxResidual=${result.maxResidual}")
    assert(result.iterations <= 2, clue = s"iterations=${result.iterations}")
    assert(result.maxResidual <= 1e-8, clue = s"maxResidual=${result.maxResidual}")
    assertEquals(result.morphism.source, mni, clue = "")
    assertEquals(result.morphism.target, native, clue = "")
    assertClose(inversePoint, Vector(0.8, 1.1, 0.95), 1e-8)
    assertClose(roundtrip, query, 1e-8)
    assertClose(typedInversePoint, WorldPoint(0.8, 1.1, 0.95), 1e-8)
    assertClose(typedRoundtrip, typedQuery, 1e-8)
  }

  test("approximate inverse works for absolute-coordinate dense fields") {
    val grid = GridSpec.identity(Vector(4, 4, 4))
    val shift = Vector(-0.15, 0.1, 0.2)
    val field =
      denseField(grid) { (coord, component) =>
        component match
          case 0 => coord.x.toDouble + shift(0)
          case 1 => coord.y.toDouble + shift(1)
          case _ => coord.z.toDouble + shift(2)
      }
    val morphism =
      DenseFieldMorphism.coordinates(native, mni, grid, field, Resample.Method.Linear)
        .fold(err => fail(err.message), identity)
    val inverseGrid = GridSpec(Vector(1, 1, 1), translation(1.0, 1.0, 1.0))

    val result =
      DenseFieldInverse.approximate(morphism, inverseGrid)
        .fold(err => fail(err.message), identity)
    val query = Vector(1.0, 1.0, 1.0)
    val inversePoint = result.morphism.transform(query)
    val typedQuery = WorldPoint(1.0, 1.0, 1.0)
    val typedInversePoint = result.morphism.transform(typedQuery)

    assert(result.converged, clue = s"maxResidual=${result.maxResidual}")
    assertClose(inversePoint, Vector(1.15, 0.9, 0.8), 1e-8)
    assertClose(morphism.transform(inversePoint), query, 1e-8)
    assertClose(typedInversePoint, WorldPoint(1.15, 0.9, 0.8), 1e-8)
    assertClose(morphism.transform(typedInversePoint), typedQuery, 1e-8)
  }

  test("inverse options validate directly") {
    val grid = GridSpec.identity(Vector(2, 2, 2))
    val field = denseField(grid)((_, _) => 0.0)
    val morphism =
      DenseFieldMorphism.displacement(native, mni, grid, field, Resample.Method.Linear)
        .fold(err => fail(err.message), identity)

    val result =
      morphism.approximateInverse(
        grid,
        DenseFieldInverseOptions(maxIterations = 0)
      )

    assertEquals(
      result.left.toOption,
      Some(MorphismError.InvalidInverseParameters("maxIterations must be positive")),
      clue = ""
    )
  }
