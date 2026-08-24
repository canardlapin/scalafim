package scalafim.image

import SampleSpaces.*

import image4s.SamplingAlignment
import image4s.geometry.Affine
import image4s.geometry.D3
import ravel.NDArray as RavelArray
import ravel.Rank

class ResamplingPlanSuite extends munit.FunSuite:

  private def assertClose(actual: Double, expected: Double, tol: Double = 1e-10): Unit =
    assert(math.abs(actual - expected) <= tol, clue = s"actual=$actual expected=$expected")

  private def assertClose(actual: Vector[Double], expected: Vector[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length, clue = "")
    actual.zip(expected).foreach { case (a, e) => assertClose(a, e, tol) }

  private def assertClose(actual: WorldPoint, expected: WorldPoint, tol: Double): Unit =
    assertClose(actual.toVector, expected.toVector, tol)

  private def assertClose(actual: VoxelPoint, expected: VoxelPoint, tol: Double): Unit =
    assertClose(actual.toVector, expected.toVector, tol)

  private def assertSameVolume(actual: SomeScalarVolume[Double], expected: SomeScalarVolume[Double], tol: Double = 1e-10): Unit =
    assertEquals(actual.space, expected.space, clue = "")
    assertEquals(actual.values.shape, expected.values.shape, clue = "")
    var i = 0
    while i < actual.copyToCanonicalArray.length do
      assertClose(actual.copyToCanonicalArray(i), expected.copyToCanonicalArray(i), tol)
      i += 1

  private def assertSameVec(actual: SomeScalarSeries[Double], expected: SomeScalarSeries[Double], tol: Double = 1e-10): Unit =
    val alignment =
      for
        left <- SampleSpaces.requireD3(actual.space).left.map(_.message)
        right <- SampleSpaces.requireD3(expected.space).left.map(_.message)
        evidence <- SamplingAlignment.exact(left, right).left.map(_.message)
      yield evidence
    assert(alignment.isRight)
    assertEquals(actual.values.shape, expected.values.shape, clue = "")
    assertEquals(actual.nVolumes, expected.nVolumes, clue = "")
    var i = 0
    while i < actual.copyToCanonicalArray.length do
      assertClose(actual.copyToCanonicalArray(i), expected.copyToCanonicalArray(i), tol)
      i += 1

  private def testVolume(space: SomeSampleSpace): SomeScalarVolume[Double] =
    val dims = space.spatialDims
    val data =
      PrimitiveBuffers.tabulate[Double](dims.product) { lin =>
        val g = Indexing.indexToGrid3D(dims, lin)
        valueAt(g(0), g(1), g(2))
      }
    SomeScalarVolume.unsafeCopyFromCanonicalArray(data, space, "plan-fixture")

  private def testVec(space: SomeSampleSpace, nVolumes: Int): SomeScalarSeries[Double] =
    val spatial = space.spatialSpace
    val dims = spatial.spatialDims
    val data =
      RavelArray.tabulate[Double](dims(0), dims(1), dims(2), nVolumes) { (x, y, z, t) =>
        valueAt(x, y, z) + 1000.0 * t.toDouble
      }
    SomeScalarSeries.unsafeFromRavel(data, spatial.addDim(ProviderAxes.time(nVolumes)), "plan-vec-fixture")

  private def denseField(
      grid: GridSpec
  )(f: (VoxelCoord, Int) => Double): RavelArray[Double, Rank[4]] =
    RavelArray.tabulate[Double](
      grid.shape.x,
      grid.shape.y,
      grid.shape.z,
      3
    )((i, j, k, component) => f(VoxelCoord(i, j, k), component))

  private def valueAt(x: Int, y: Int, z: Int): Double =
    x.toDouble + 10.0 * y.toDouble + 100.0 * z.toDouble

  private def translation(x: Double, y: Double, z: Double): Affine[D3] =
    ProviderSpaces.affine(
      Vector(
        Vector(1.0, 0.0, 0.0, x),
        Vector(0.0, 1.0, 0.0, y),
        Vector(0.0, 0.0, 1.0, z),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  private def scale(x: Double, y: Double, z: Double): Affine[D3] =
    ProviderSpaces.affine(
      Vector(
        Vector(x, 0.0, 0.0, 0.0),
        Vector(0.0, y, 0.0, 0.0),
        Vector(0.0, 0.0, z, 0.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  private def affine(
      source: GridSpec,
      target: GridSpec,
      matrix: Affine[D3]
  ): SpatialPullback =
    SpatialPullbacks.affine(source, target, matrix)

  private def plan(
      source: GridSpec,
      target: GridSpec,
      pullback: SpatialPullback,
      method: Resample.Method
  ): ResamplingPlan =
    ResamplingPlan.make(source, target, pullback, method).fold(err => fail(err.message), identity)

  test("identity plan matches existing nearest and linear resampling") {
    val space = SampleSpaces(Vector(4, 4, 4))
    val volume = testVolume(space)
    val grid = GridSpec.fromSpace(space)
    val id = SpatialPullbacks.worldAligned(grid, grid)

    val nearestPlan = plan(grid, grid, id, Resample.Method.Nearest)
    val nearest = nearestPlan(volume).fold(err => fail(err.message), identity)
    val nearestExisting = Resample.nearest(volume, space)

    val linearPlan = plan(grid, grid, id, Resample.Method.Linear)
    val linear = linearPlan(volume).fold(err => fail(err.message), identity)
    val linearExisting = Resample.trilinear(volume, space)

    assertSameVolume(nearest, nearestExisting)
    assertSameVolume(linear, linearExisting)
  }

  test("identity plan matches existing nearest and linear SomeNeuroSeries resampling") {
    val space = SampleSpaces(Vector(4, 4, 4))
    val vec = testVec(space, nVolumes = 2)
    val grid = GridSpec.fromSpace(space)
    val id = SpatialPullbacks.worldAligned(grid, grid)

    val nearestPlan = plan(grid, grid, id, Resample.Method.Nearest)
    val nearest = nearestPlan(vec).fold(err => fail(err.message), identity)
    val nearestExisting = Resample.nearest(vec, space)

    val linearPlan = plan(grid, grid, id, Resample.Method.Linear)
    val linear = linearPlan(vec).fold(err => fail(err.message), identity)
    val linearExisting = Resample.trilinear(vec, space)

    assertSameVec(nearest, nearestExisting)
    assertSameVec(linear, linearExisting)
  }

  test("Resample plan helpers build and execute morphism-aware plans") {
    val space = SampleSpaces(Vector(3, 2, 1))
    val grid = GridSpec.fromSpace(space)
    val id = SpatialPullbacks.worldAligned(grid, grid)
    val volume = testVolume(space)
    val vec = testVec(space, nVolumes = 2)

    val byGrid = Resample.plan(grid, grid, id, Resample.Method.Nearest).fold(err => fail(err.message), identity)
    val bySpace = Resample.plan(space, space, id, Resample.Method.Nearest).fold(err => fail(err.message), identity)
    assertEquals(byGrid.source, bySpace.source, clue = "")
    assertEquals(byGrid.target, bySpace.target, clue = "")

    val volumeOut =
      Resample.resampleTo(volume, grid, id, Resample.Method.Nearest, outside = 0.0)
        .fold(err => fail(err.message), identity)
    val vecOut =
      Resample.resampleTo(vec, grid, id, Resample.Method.Nearest, outside = 0.0)
        .fold(err => fail(err.message), identity)

    assertSameVolume(volumeOut, volume)
    assertSameVec(vecOut, vec)
  }

  test("affine translation plan samples shifted source voxels") {
    val sourceSpace = SampleSpaces(Vector(4, 2, 1))
    val sourceGrid = GridSpec.fromSpace(sourceSpace)
    val targetGrid = GridSpec.identity(Vector(4, 2, 1))
    val volume = testVolume(sourceSpace)
    val morphism = affine(sourceGrid, targetGrid, translation(1.0, 0.0, 0.0))

    val p = plan(sourceGrid, targetGrid, morphism, Resample.Method.Nearest)
    assertEquals(p.executionModel, ResamplingExecutionModel.ProviderAffine)
    assertEquals(p.materializedCoordinateCount, 0)

    val resampled = p
      .apply(volume, outside = -1.0)
      .fold(err => fail(err.message), identity)

    assertEquals(resampled.space.spatialDims, targetGrid.dims, clue = "")
    assertClose(resampled(0, 0, 0), valueAt(1, 0, 0))
    assertClose(resampled(1, 0, 0), valueAt(2, 0, 0))
    assertClose(resampled(2, 0, 0), valueAt(3, 0, 0))
    assertClose(resampled(3, 0, 0), -1.0)
    assertClose(resampled(0, 1, 0), valueAt(1, 1, 0))
  }

  test("affine translation plan samples every SomeNeuroSeries volume") {
    val sourceSpace = SampleSpaces(Vector(4, 1, 1))
    val sourceGrid = GridSpec.fromSpace(sourceSpace)
    val targetGrid = GridSpec.identity(Vector(4, 1, 1))
    val vec = testVec(sourceSpace, nVolumes = 2)
    val morphism = affine(sourceGrid, targetGrid, translation(1.0, 0.0, 0.0))

    val resampled = plan(sourceGrid, targetGrid, morphism, Resample.Method.Nearest)
      .apply(vec, outside = -1.0)
      .fold(err => fail(err.message), identity)

    assertEquals(resampled.space.dims, Vector(4, 1, 1, 2), clue = "")
    assertEquals(resampled.label, vec.label, clue = "")
    assertClose(resampled(0, 0, 0, 0), valueAt(1, 0, 0))
    assertClose(resampled(1, 0, 0, 0), valueAt(2, 0, 0))
    assertClose(resampled(2, 0, 0, 0), valueAt(3, 0, 0))
    assertClose(resampled(3, 0, 0, 0), -1.0)
    assertClose(resampled(0, 0, 0, 1), valueAt(1, 0, 0) + 1000.0)
    assertClose(resampled(1, 0, 0, 1), valueAt(2, 0, 0) + 1000.0)
    assertClose(resampled(2, 0, 0, 1), valueAt(3, 0, 0) + 1000.0)
    assertClose(resampled(3, 0, 0, 1), -1.0)
  }

  test("dense displacement morphisms drive resampling plans") {
    val sourceSpace = SampleSpaces(Vector(4, 1, 1))
    val sourceGrid = GridSpec.fromSpace(sourceSpace)
    val targetGrid = GridSpec.identity(Vector(4, 1, 1))
    val volume = testVolume(sourceSpace)
    val field =
      denseField(targetGrid) { (_, component) =>
        if component == 0 then 1.0 else 0.0
      }
    val morphism =
      SpatialPullbacks
        .displacement(
          sourceGrid,
          targetGrid,
          field,
          Resample.Method.Nearest
        )
        .fold(err => fail(err.message), identity)

    val p = plan(sourceGrid, targetGrid, morphism, Resample.Method.Nearest)
    assertEquals(p.executionModel, ResamplingExecutionModel.ProviderMapped)
    assertEquals(p.materializedCoordinateCount, targetGrid.nVoxels)

    val resampled = p
      .apply(volume, outside = -1.0)
      .fold(err => fail(err.message), identity)

    assertClose(resampled(0, 0, 0), valueAt(1, 0, 0))
    assertClose(resampled(1, 0, 0), valueAt(2, 0, 0))
    assertClose(resampled(2, 0, 0), valueAt(3, 0, 0))
    assertClose(resampled(3, 0, 0), -1.0)
  }

  test("affine scale plan delegates without coordinate materialization") {
    val sourceSpace = SampleSpaces(Vector(5, 1, 1))
    val sourceGrid = GridSpec.fromSpace(sourceSpace)
    val targetGrid = GridSpec.identity(Vector(3, 1, 1))
    val volume = testVolume(sourceSpace)
    val morphism = affine(sourceGrid, targetGrid, scale(2.0, 1.0, 1.0))
    val p = plan(sourceGrid, targetGrid, morphism, Resample.Method.Nearest)

    assertEquals(p.executionModel, ResamplingExecutionModel.ProviderAffine)
    assertEquals(p.materializedCoordinateCount, 0)

    val resampled = p(volume).fold(err => fail(err.message), identity)
    assertClose(resampled(0, 0, 0), valueAt(0, 0, 0))
    assertClose(resampled(1, 0, 0), valueAt(2, 0, 0))
    assertClose(resampled(2, 0, 0), valueAt(4, 0, 0))
  }

  test("linear plan samples continuous source voxel coordinates") {
    val sourceSpace = SampleSpaces(Vector(3, 3, 3))
    val sourceGrid = GridSpec.fromSpace(sourceSpace)
    val targetGrid = GridSpec.identity(Vector(1, 1, 1))
    val volume = testVolume(sourceSpace)
    val morphism = affine(sourceGrid, targetGrid, translation(0.5, 0.5, 0.5))
    val p = plan(sourceGrid, targetGrid, morphism, Resample.Method.Linear)

    assertEquals(p.executionModel, ResamplingExecutionModel.ProviderAffine)
    assertEquals(p.materializedCoordinateCount, 0)

    val resampled = p(volume).fold(err => fail(err.message), identity)
    val expected =
      Vector(
        valueAt(0, 0, 0),
        valueAt(1, 0, 0),
        valueAt(0, 1, 0),
        valueAt(1, 1, 0),
        valueAt(0, 0, 1),
        valueAt(1, 0, 1),
        valueAt(0, 1, 1),
        valueAt(1, 1, 1)
      ).sum / 8.0
    assertClose(resampled(0, 0, 0), expected, 1e-10)
  }

  test("outside value is explicit for out-of-bounds nearest and linear samples") {
    val sourceSpace = SampleSpaces(Vector(2, 2, 2))
    val sourceGrid = GridSpec.fromSpace(sourceSpace)
    val targetGrid = GridSpec.identity(Vector(1, 1, 1))
    val volume = testVolume(sourceSpace)
    val outsideMorphism = affine(sourceGrid, targetGrid, translation(9.0, 0.0, 0.0))

    val nearest = plan(sourceGrid, targetGrid, outsideMorphism, Resample.Method.Nearest)
      .apply(volume, outside = -99.0)
      .fold(err => fail(err.message), identity)
    assertClose(nearest(0, 0, 0), -99.0)

    val edgeMorphism = affine(sourceGrid, targetGrid, translation(1.5, 0.0, 0.0))
    val linear = plan(sourceGrid, targetGrid, edgeMorphism, Resample.Method.Linear)
      .apply(volume, outside = -10.0)
      .fold(err => fail(err.message), identity)
    assertClose(linear(0, 0, 0), (valueAt(1, 0, 0) + -10.0) / 2.0, 1e-10)
  }

  test("cubic plans execute and match existing identity tricubic resampling") {
    val sourceSpace = SampleSpaces(Vector(4, 4, 4))
    val grid = GridSpec.fromSpace(sourceSpace)
    val volume = testVolume(sourceSpace)
    val p = plan(
      grid,
      grid,
      SpatialPullbacks.worldAligned(grid, grid),
      Resample.Method.Cubic
    )

    assertEquals(p.executionModel, ResamplingExecutionModel.ProviderAffine)
    assertEquals(p.materializedCoordinateCount, 0)

    val result = p(volume).fold(err => fail(err.message), identity)
    val existing = Resample.tricubic(volume, sourceSpace)
    assertSameVolume(result, existing)
  }

  test("resampling plans can apply Jacobian and square-root Jacobian modulation") {
    val sourceSpace = SampleSpaces(Vector(1, 1, 1))
    val grid = GridSpec.fromSpace(sourceSpace)
    val volume =
      SomeScalarVolume.unsafeCopyFromCanonicalArray(PrimitiveBuffers.fillConst[Double](1, 2.0), sourceSpace, "constant")
    val morphism = affine(grid, grid, scale(2.0, 3.0, 1.0))
    val p = plan(grid, grid, morphism, Resample.Method.Nearest)

    val jacobian =
      p(volume, outside = 0.0, modulation = JacobianModulation.Jacobian)
        .fold(err => fail(err.message), identity)
    val sqrtJacobian =
      p(volume, outside = 0.0, modulation = JacobianModulation.SqrtJacobian)
        .fold(err => fail(err.message), identity)

    assertClose(jacobian(0, 0, 0), 12.0, 1e-10)
    assertClose(sqrtJacobian(0, 0, 0), 2.0 * math.sqrt(6.0), 1e-10)
  }

  test("coordinate-field rendition returns a provider map with preserved owners") {
    val grid = GridSpec.identity(Vector(2, 1, 1))
    val coordinates =
      denseField(grid): (voxel, component) =>
        if component == 0 then voxel.x.toDouble + 1.0
        else voxel.toVector(component).toDouble
    val map =
      SpatialPullbacks
        .coordinates(grid, grid, coordinates)
        .fold(err => fail(err.message), identity)
    val sourceFrame: image4s.geometry.Frame[D3] = map.source
    val point = image4s.geometry.Point
      .in[D3](sourceFrame)(0.5, 0.0, 0.0)
      .fold(err => fail(err.message), identity)
    val rebound = image4s.geometry.Frame
      .alignOwners[D3, sourceFrame.type, image4s.geometry.Frame[D3]](
        sourceFrame,
        sourceFrame
      )
      .flatMap(_.pointToRight(point))
      .fold(err => fail(err.message), identity)
    val mapped = map(rebound).fold(err => fail(err.message), identity)

    assertClose(mapped.coordinates, Vector(1.5, 0.0, 0.0), 1e-10)
    assert(mapped.frame eq map.target)
  }

  test("plan rejects volumes whose source grid differs from the planned source") {
    val plannedSource = GridSpec.identity(Vector(2, 2, 2))
    val target = GridSpec.identity(Vector(2, 2, 2))
    val volume = testVolume(SampleSpaces(Vector(3, 2, 2)))
    val p = plan(
      plannedSource,
      target,
      SpatialPullbacks.worldAligned(plannedSource, target),
      Resample.Method.Nearest
    )

    p(volume) match
      case Left(
            ResamplingPlanError.Geometry(
              image4s.geometry.GeometryError.GridsNotCongruent(0.0)
            )
          ) =>
        ()
      case other =>
        fail(s"expected typed provider geometry mismatch, found $other")
  }
