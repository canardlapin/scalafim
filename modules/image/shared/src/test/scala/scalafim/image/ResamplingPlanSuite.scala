package scalafim.image

import ravel.NDArray as RavelArray
import ravel.Rank

class ResamplingPlanSuite extends munit.FunSuite:

  private val sourceDomain = SpatialDomainId("source")
  private val targetDomain = SpatialDomainId("target")

  private def assertClose(actual: Double, expected: Double, tol: Double = 1e-10): Unit =
    assert(math.abs(actual - expected) <= tol, clue = s"actual=$actual expected=$expected")

  private def assertClose(actual: Vector[Double], expected: Vector[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length, clue = "")
    actual.zip(expected).foreach { case (a, e) => assertClose(a, e, tol) }

  private def assertClose(actual: WorldPoint, expected: WorldPoint, tol: Double): Unit =
    assertClose(actual.toVector, expected.toVector, tol)

  private def assertClose(actual: VoxelPoint, expected: VoxelPoint, tol: Double): Unit =
    assertClose(actual.toVector, expected.toVector, tol)

  private def assertSameVolume(actual: NeuroVol[Double], expected: NeuroVol[Double], tol: Double = 1e-10): Unit =
    assertEquals(actual.space, expected.space, clue = "")
    assertEquals(actual.values.shape, expected.values.shape, clue = "")
    var i = 0
    while i < actual.copyToCanonicalArray.length do
      assertClose(actual.copyToCanonicalArray(i), expected.copyToCanonicalArray(i), tol)
      i += 1

  private def assertSameVec(actual: NeuroVec[Double], expected: NeuroVec[Double], tol: Double = 1e-10): Unit =
    assertEquals(
      GridCompatibility.exact(actual.space, expected.space),
      Right(()),
      clue = ""
    )
    assertEquals(actual.values.shape, expected.values.shape, clue = "")
    assertEquals(actual.nVolumes, expected.nVolumes, clue = "")
    var i = 0
    while i < actual.copyToCanonicalArray.length do
      assertClose(actual.copyToCanonicalArray(i), expected.copyToCanonicalArray(i), tol)
      i += 1

  private def testVolume(space: NeuroSpace): NeuroVol[Double] =
    val dims = space.spatialDims
    val data =
      PrimitiveBuffers.tabulate[Double](dims.product) { lin =>
        val g = Indexing.indexToGrid3D(dims, lin)
        valueAt(g(0), g(1), g(2))
      }
    NeuroVol.copyFromCanonicalArray(data, space, "plan-fixture")

  private def testVec(space: NeuroSpace, nVolumes: Int): NeuroVec[Double] =
    val spatial = space.spatialSpace
    val dims = spatial.spatialDims
    val data =
      RavelArray.tabulate[Double](dims(0), dims(1), dims(2), nVolumes) { (x, y, z, t) =>
        valueAt(x, y, z) + 1000.0 * t.toDouble
      }
    NeuroVec.fromRavel(data, spatial.addDim(nVolumes, Some(Axis.Time)), "plan-vec-fixture")

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

  private def affine(matrix: DMat): Affine3DMorphism =
    Affine3DMorphism.make(sourceDomain, targetDomain, matrix).fold(err => fail(err.message), identity)

  private def plan(
      source: GridSpec,
      target: GridSpec,
      morphism: SpatialMorphism,
      method: Resample.Method
  ): ResamplingPlan =
    ResamplingPlan.make(source, target, morphism, method).fold(err => fail(err.message), identity)

  test("identity plan matches existing nearest and linear resampling") {
    val space = NeuroSpace(Vector(4, 4, 4))
    val volume = testVolume(space)
    val grid = GridSpec.fromSpace(space)
    val id = IdentityMorphism(sourceDomain)

    val nearestPlan = plan(grid, grid, id, Resample.Method.Nearest)
    val nearest = nearestPlan(volume).fold(err => fail(err.message), identity)
    val nearestExisting = Resample.nearest(volume, space)

    val linearPlan = plan(grid, grid, id, Resample.Method.Linear)
    val linear = linearPlan(volume).fold(err => fail(err.message), identity)
    val linearExisting = Resample.trilinear(volume, space)

    assertSameVolume(nearest, nearestExisting)
    assertSameVolume(linear, linearExisting)
  }

  test("identity plan matches existing nearest and linear NeuroVec resampling") {
    val space = NeuroSpace(Vector(4, 4, 4))
    val vec = testVec(space, nVolumes = 2)
    val grid = GridSpec.fromSpace(space)
    val id = IdentityMorphism(sourceDomain)

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
    val space = NeuroSpace(Vector(3, 2, 1))
    val grid = GridSpec.fromSpace(space)
    val id = IdentityMorphism(sourceDomain)
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
    val sourceSpace = NeuroSpace(Vector(4, 2, 1))
    val sourceGrid = GridSpec.fromSpace(sourceSpace)
    val targetGrid = GridSpec.identity(Vector(4, 2, 1))
    val volume = testVolume(sourceSpace)
    val morphism = affine(translation(1.0, 0.0, 0.0))

    val resampled = plan(sourceGrid, targetGrid, morphism, Resample.Method.Nearest)
      .apply(volume, outside = -1.0)
      .fold(err => fail(err.message), identity)

    assertEquals(resampled.space.spatialDims, targetGrid.dims, clue = "")
    assertClose(resampled(0, 0, 0), valueAt(1, 0, 0))
    assertClose(resampled(1, 0, 0), valueAt(2, 0, 0))
    assertClose(resampled(2, 0, 0), valueAt(3, 0, 0))
    assertClose(resampled(3, 0, 0), -1.0)
    assertClose(resampled(0, 1, 0), valueAt(1, 1, 0))
  }

  test("affine translation plan samples every NeuroVec volume") {
    val sourceSpace = NeuroSpace(Vector(4, 1, 1))
    val sourceGrid = GridSpec.fromSpace(sourceSpace)
    val targetGrid = GridSpec.identity(Vector(4, 1, 1))
    val vec = testVec(sourceSpace, nVolumes = 2)
    val morphism = affine(translation(1.0, 0.0, 0.0))

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
    val sourceSpace = NeuroSpace(Vector(4, 1, 1))
    val sourceGrid = GridSpec.fromSpace(sourceSpace)
    val targetGrid = GridSpec.identity(Vector(4, 1, 1))
    val volume = testVolume(sourceSpace)
    val field =
      denseField(targetGrid) { (_, component) =>
        if component == 0 then 1.0 else 0.0
      }
    val morphism =
      DenseFieldMorphism.displacement(sourceDomain, targetDomain, targetGrid, field, Resample.Method.Nearest)
        .fold(err => fail(err.message), identity)

    val resampled = plan(sourceGrid, targetGrid, morphism, Resample.Method.Nearest)
      .apply(volume, outside = -1.0)
      .fold(err => fail(err.message), identity)

    assertClose(resampled(0, 0, 0), valueAt(1, 0, 0))
    assertClose(resampled(1, 0, 0), valueAt(2, 0, 0))
    assertClose(resampled(2, 0, 0), valueAt(3, 0, 0))
    assertClose(resampled(3, 0, 0), -1.0)
  }

  test("affine scale plan precomputes source voxel coordinates") {
    val sourceSpace = NeuroSpace(Vector(5, 1, 1))
    val sourceGrid = GridSpec.fromSpace(sourceSpace)
    val targetGrid = GridSpec.identity(Vector(3, 1, 1))
    val volume = testVolume(sourceSpace)
    val morphism = affine(scale(2.0, 1.0, 1.0))
    val p = plan(sourceGrid, targetGrid, morphism, Resample.Method.Nearest)

    assertEquals(p.targetWorldCoords, targetGrid.worldCoords, clue = "")
    assertClose(p.targetWorldPoints(0), WorldPoint(0.0, 0.0, 0.0), 1e-10)
    assertClose(p.targetWorldPoints(1), WorldPoint(1.0, 0.0, 0.0), 1e-10)
    assertClose(p.targetWorldPoints(2), WorldPoint(2.0, 0.0, 0.0), 1e-10)
    assertClose(p.sourceWorldCoords(0), Vector(0.0, 0.0, 0.0), 1e-10)
    assertClose(p.sourceWorldCoords(1), Vector(2.0, 0.0, 0.0), 1e-10)
    assertClose(p.sourceWorldCoords(2), Vector(4.0, 0.0, 0.0), 1e-10)
    assertClose(p.sourceWorldPoints(0), WorldPoint(0.0, 0.0, 0.0), 1e-10)
    assertClose(p.sourceWorldPoints(1), WorldPoint(2.0, 0.0, 0.0), 1e-10)
    assertClose(p.sourceWorldPoints(2), WorldPoint(4.0, 0.0, 0.0), 1e-10)
    assertClose(p.sourceVoxelPoints(0), VoxelPoint(0.0, 0.0, 0.0), 1e-10)
    assertClose(p.sourceVoxelPoints(1), VoxelPoint(2.0, 0.0, 0.0), 1e-10)
    assertClose(p.sourceVoxelPoints(2), VoxelPoint(4.0, 0.0, 0.0), 1e-10)
    assertEquals(p.sourceVoxelCoords, p.sourceWorldCoords, clue = "")

    val resampled = p(volume).fold(err => fail(err.message), identity)
    assertClose(resampled(0, 0, 0), valueAt(0, 0, 0))
    assertClose(resampled(1, 0, 0), valueAt(2, 0, 0))
    assertClose(resampled(2, 0, 0), valueAt(4, 0, 0))
  }

  test("linear plan samples continuous source voxel coordinates") {
    val sourceSpace = NeuroSpace(Vector(3, 3, 3))
    val sourceGrid = GridSpec.fromSpace(sourceSpace)
    val targetGrid = GridSpec.identity(Vector(1, 1, 1))
    val volume = testVolume(sourceSpace)
    val morphism = affine(translation(0.5, 0.5, 0.5))
    val p = plan(sourceGrid, targetGrid, morphism, Resample.Method.Linear)

    assertClose(p.sourceVoxelCoords.head, Vector(0.5, 0.5, 0.5), 1e-10)
    assertClose(p.sourceVoxelPoints.head, VoxelPoint(0.5, 0.5, 0.5), 1e-10)

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
    val sourceSpace = NeuroSpace(Vector(2, 2, 2))
    val sourceGrid = GridSpec.fromSpace(sourceSpace)
    val targetGrid = GridSpec.identity(Vector(1, 1, 1))
    val volume = testVolume(sourceSpace)
    val outsideMorphism = affine(translation(9.0, 0.0, 0.0))

    val nearest = plan(sourceGrid, targetGrid, outsideMorphism, Resample.Method.Nearest)
      .apply(volume, outside = -99.0)
      .fold(err => fail(err.message), identity)
    assertClose(nearest(0, 0, 0), -99.0)

    val edgeMorphism = affine(translation(1.5, 0.0, 0.0))
    val linear = plan(sourceGrid, targetGrid, edgeMorphism, Resample.Method.Linear)
      .apply(volume, outside = -10.0)
      .fold(err => fail(err.message), identity)
    assertClose(linear(0, 0, 0), (valueAt(1, 0, 0) + -10.0) / 2.0, 1e-10)
  }

  test("cubic plans execute and match existing identity tricubic resampling") {
    val sourceSpace = NeuroSpace(Vector(4, 4, 4))
    val grid = GridSpec.fromSpace(sourceSpace)
    val volume = testVolume(sourceSpace)
    val p = plan(grid, grid, IdentityMorphism(sourceDomain), Resample.Method.Cubic)

    val result = p(volume).fold(err => fail(err.message), identity)
    val existing = Resample.tricubic(volume, sourceSpace)
    assertSameVolume(result, existing)
  }

  test("resampling plans can apply Jacobian and square-root Jacobian modulation") {
    val sourceSpace = NeuroSpace(Vector(1, 1, 1))
    val grid = GridSpec.fromSpace(sourceSpace)
    val volume =
      NeuroVol.copyFromCanonicalArray(PrimitiveBuffers.fillConst[Double](1, 2.0), sourceSpace, "constant")
    val morphism = affine(scale(2.0, 3.0, 1.0))
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

  test("morphism fields materialize source coordinates, displacements, and Jacobian determinants") {
    val grid = GridSpec.identity(Vector(2, 1, 1))
    val morphism = affine(translation(1.0, 0.0, 0.0))

    val source = MorphismFields.sourceCoordinates(morphism, grid)
    val displacement = MorphismFields.displacement(morphism, grid)
    val jacobian =
      MorphismFields.jacobianDeterminant(morphism, grid)
        .fold(err => fail(err.message), identity)

    assertEquals(source.kind, DenseVectorFieldKind.SourceCoordinates, clue = "")
    assertEquals(displacement.kind, DenseVectorFieldKind.Displacement, clue = "")
    assertClose(source(VoxelCoord(0, 0, 0), 0), 1.0)
    assertClose(source(VoxelCoord(1, 0, 0), 0), 2.0)
    assertClose(displacement(VoxelCoord(0, 0, 0), 0), 1.0)
    assertClose(displacement(VoxelCoord(1, 0, 0), 0), 1.0)
    assertClose(jacobian(0, 0, 0), 1.0)
    assertClose(jacobian(1, 0, 0), 1.0)
  }

  test("plan rejects volumes whose source grid differs from the planned source") {
    val plannedSource = GridSpec.identity(Vector(2, 2, 2))
    val target = GridSpec.identity(Vector(2, 2, 2))
    val volume = testVolume(NeuroSpace(Vector(3, 2, 2)))
    val p = plan(plannedSource, target, IdentityMorphism(sourceDomain), Resample.Method.Nearest)

    val result = p(volume)
    assert(result.isLeft, clue = "source mismatch should be represented directly")
  }
