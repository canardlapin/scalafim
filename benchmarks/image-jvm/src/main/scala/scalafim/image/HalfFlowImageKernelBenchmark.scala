package scalafim.image

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*
import scala.compiletime.uninitialized

/** Dynamic pull-map composition against both current reference paths. */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 250, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 10, time = 250, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
class HalfFlowImageKernelBenchmark:

  @Param(Array("64"))
  var side: Int = 0

  private var grid: GridSpec = uninitialized
  private var left: DenseVectorField = uninitialized
  private var right: DenseVectorField = uninitialized
  private var points: Vector[Vector[Double]] = uninitialized
  private var preparedPlan: DenseFieldInterpolationPlan = uninitialized
  private var destination: narr.NArray[Double] = uninitialized
  private var valid: narr.NArray[Boolean] = uninitialized
  private var sampler: DenseFieldSampler = uninitialized
  private var scalarSource: NeuroVol[Double] = uninitialized
  private var scalarDestination: narr.NArray[Double] = uninitialized
  private var scalarValid: narr.NArray[Boolean] = uninitialized
  private var scalarSampler: DenseFieldSampler = uninitialized
  private var coarseGrid: GridSpec = uninitialized
  private var regridSource: DenseVectorField = uninitialized
  private var regridDestination: narr.NArray[Double] = uninitialized
  private var regridValid: narr.NArray[Boolean] = uninitialized
  private var regridSampler: DenseFieldSampler = uninitialized
  private var jacobianDestination: narr.NArray[Double] = uninitialized
  private var jacobianValid: narr.NArray[Boolean] = uninitialized
  private var jacobianSampler: DenseFieldSampler = uninitialized
  private var jacobianReduction: JacobianReduction = uninitialized
  private var forwardTranslation: DenseVectorField = uninitialized
  private var backwardTranslation: DenseVectorField = uninitialized
  private var forwardSampler: DenseFieldSampler = uninitialized
  private var backwardSampler: DenseFieldSampler = uninitialized
  private var forwardReduction: InverseErrorReduction = uninitialized
  private var backwardReduction: InverseErrorReduction = uninitialized
  private var pyramidSource: NeuroVol[Double] = uninitialized
  private var pyramidWorkspace: PyramidWorkspace = uninitialized
  private var pyramidDestination: narr.NArray[Double] = uninitialized
  private var pyramidValid: narr.NArray[Boolean] = uninitialized
  private var pyramidSampler: DenseFieldSampler = uninitialized
  private var boxSource: narr.NArray[Double] = uninitialized
  private var boxDestination: narr.NArray[Double] = uninitialized
  private var boxWorkspace: BoxSumWorkspace = uninitialized
  private var gaussianSupport: narr.NArray[Double] = uninitialized
  private var gaussianDestination: narr.NArray[Double] = uninitialized
  private var gaussianWeight: narr.NArray[Double] = uninitialized
  private var gaussianWorkspace: GaussianWorkspace = uninitialized
  private var gaussianReduction: GaussianReduction = uninitialized
  private var boxRadius: VoxelWindowRadius = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    grid = GridSpec.identity(Vector(side, side, side))
    left = coordinateField(grid) { (x, y, z) =>
      (x + 0.25, y + 0.125, z + 0.375)
    }
    right = coordinateField(grid) { (x, y, z) =>
      (1.01 * x + 0.015 * y + 0.1, 0.99 * y + 0.01 * z - 0.2, 1.005 * z + 0.02 * x + 0.05)
    }
    points =
      Vector.tabulate(grid.nVoxels) { i =>
        Vector(
          left.values.data(i),
          left.values.data(i + grid.nVoxels),
          left.values.data(i + 2 * grid.nVoxels)
        )
      }
    preparedPlan =
      DenseFieldInterpolationPlan.make(grid, points, Resample.Method.Linear)
        .fold(err => throw new IllegalArgumentException(err.message), value => value)
    destination = NArrayUtil.ofSize[Double](grid.nVoxels * 3)
    valid = NArrayUtil.ofSize[Boolean](grid.nVoxels)
    sampler = DenseFieldSampler(grid)
    DenseFieldKernels.composePullInto(
      left,
      right,
      destination,
      valid,
      sampler,
      FieldValidity.All,
      FieldValidity.All
    )
    val reference =
      preparedPlan.sample(right.values, DenseFieldOutside.QueryPoint)
        .fold(err => throw new IllegalArgumentException(err.message), value => value)
    val referenceChecksum = checksumReference(reference)
    val primitiveChecksum = checksumPrimitive()
    require(
      referenceChecksum == primitiveChecksum,
      s"benchmark setup checksum mismatch: reference=$referenceChecksum primitive=$primitiveChecksum"
    )

    scalarSource = analyticScalarVolume(grid)
    scalarDestination = NArrayUtil.ofSize[Double](grid.nVoxels)
    scalarValid = NArrayUtil.ofSize[Boolean](grid.nVoxels)
    scalarSampler = DenseFieldSampler(grid)
    DenseFieldKernels.pullScalarInto(
      scalarSource,
      left,
      scalarDestination,
      scalarValid,
      scalarSampler,
      FieldValidity.All,
      FieldValidity.All,
      0.0
    )
    val center = centerIndex(grid)
    require(scalarValid(center), "scalar benchmark center must be valid")
    require(
      math.abs(scalarDestination(center) - analyticScalarAt(left, center)) <= 1e-10,
      "scalar benchmark failed its analytic setup oracle"
    )

    coarseGrid = DenseFieldKernels.pyramidGrid(grid, 2)
    regridSource = coordinateField(coarseGrid) { (x, y, z) =>
      (1.02 * x + 0.01 * y + 0.2, 0.98 * y + 0.015 * z - 0.1, 1.01 * z + 0.005 * x)
    }
    regridDestination = NArrayUtil.ofSize[Double](grid.nVoxels * 3)
    regridValid = NArrayUtil.ofSize[Boolean](grid.nVoxels)
    regridSampler = DenseFieldSampler(coarseGrid)
    DenseFieldKernels.regridPullInto(
      regridSource,
      grid,
      regridDestination,
      regridValid,
      regridSampler,
      FieldValidity.All
    )
    require(regridValid(center), "regrid benchmark center must be valid")
    require(maximumRegridCenterError(center) <= 1e-10, "regrid benchmark failed its analytic setup oracle")

    jacobianDestination = NArrayUtil.ofSize[Double](grid.nVoxels)
    jacobianValid = NArrayUtil.ofSize[Boolean](grid.nVoxels)
    jacobianSampler = DenseFieldSampler(grid)
    jacobianReduction = JacobianReduction()
    DenseFieldKernels.jacobianDeterminantsReduceInto(
      right,
      jacobianDestination,
      jacobianValid,
      jacobianSampler,
      FieldValidity.All,
      jacobianReduction
    )
    require(jacobianReduction.evaluated == (side - 2) * (side - 2) * (side - 2), "Jacobian support mismatch")
    require(
      math.abs(jacobianReduction.meanOrNaN - rightDeterminant) <= 1e-10,
      "Jacobian benchmark failed its analytic determinant oracle"
    )

    forwardTranslation = coordinateField(grid) { (x, y, z) =>
      (x + 0.2, y - 0.15, z + 0.1)
    }
    backwardTranslation = coordinateField(grid) { (x, y, z) =>
      (x - 0.2, y + 0.15, z - 0.1)
    }
    forwardSampler = DenseFieldSampler(grid)
    backwardSampler = DenseFieldSampler(grid)
    forwardReduction = InverseErrorReduction()
    backwardReduction = InverseErrorReduction()
    DenseFieldKernels.inversePairErrorInto(
      forwardTranslation,
      backwardTranslation,
      forwardSampler,
      backwardSampler,
      FieldValidity.All,
      FieldValidity.All,
      forwardReduction,
      backwardReduction
    )
    require(forwardReduction.maximumMmOrNaN <= 1e-10, "forward inverse oracle failed")
    require(backwardReduction.maximumMmOrNaN <= 1e-10, "backward inverse oracle failed")

    val constant = NArrayUtil.ofSize[Double](grid.nVoxels)
    var i = 0
    while i < constant.length do
      constant(i) = 7.25
      i += 1
    pyramidSource = NeuroVol.fromLinear(constant, grid.toNeuroSpace, "constant")
    pyramidWorkspace = PyramidWorkspace(grid.nVoxels)
    pyramidDestination = NArrayUtil.ofSize[Double](coarseGrid.nVoxels)
    pyramidValid = NArrayUtil.ofSize[Boolean](coarseGrid.nVoxels)
    pyramidSampler = DenseFieldSampler(grid)
    DenseFieldKernels.buildPyramidLevelInto(
      pyramidSource,
      coarseGrid,
      1.2,
      pyramidWorkspace,
      pyramidDestination,
      pyramidValid,
      pyramidSampler,
      FieldValidity.All,
      1e-8
    )
    require(maximumPyramidConstantError() <= 1e-10, "pyramid benchmark failed its constant-field oracle")

    boxSource = scalarSource.values.data
    boxDestination = NArrayUtil.ofSize[Double](grid.nVoxels)
    boxWorkspace = BoxSumWorkspace(grid)
    boxRadius = VoxelWindowRadius(2, 2, 2)
    BoxSum3D.sumInto(
      boxSource,
      grid,
      boxRadius,
      boxDestination,
      boxWorkspace
    )
    val boxReference = NArrayUtil.ofSize[Double](grid.nVoxels)
    BoxSum3D.referenceInto(boxSource, grid, boxRadius, boxReference)
    require(
      math.abs(boxDestination(center) - boxReference(center)) <= 1e-10,
      "box benchmark failed its reference oracle"
    )

    gaussianSupport = NArrayUtil.ofSize[Double](grid.nVoxels)
    gaussianDestination = NArrayUtil.ofSize[Double](grid.nVoxels)
    gaussianWeight = NArrayUtil.ofSize[Double](grid.nVoxels)
    gaussianWorkspace = GaussianWorkspace(grid)
    gaussianReduction = GaussianReduction()
    i = 0
    while i < gaussianSupport.length do
      gaussianSupport(i) = if (i % side) > 1 then 1.0 else 0.0
      i += 1
    val gaussian = Gaussian3D.normalizedInto(
      boxSource,
      gaussianSupport,
      grid,
      sigmaMm = 1.5,
      minimumWeight = 1e-8,
      gaussianDestination,
      gaussianWeight,
      gaussianWorkspace
    )
    require(gaussian.validVoxels > 0, "Gaussian benchmark has no valid support")

  @Benchmark
  def referencePlanAndSample(): Double =
    val plan =
      DenseFieldInterpolationPlan.make(grid, points, Resample.Method.Linear)
        .fold(err => throw new IllegalArgumentException(err.message), value => value)
    val values =
      plan.sample(right.values, DenseFieldOutside.QueryPoint)
        .fold(err => throw new IllegalArgumentException(err.message), value => value)
    values(grid.nVoxels / 2)(0)

  @Benchmark
  def referencePreparedSample(): Double =
    val values =
      preparedPlan.sample(right.values, DenseFieldOutside.QueryPoint)
        .fold(err => throw new IllegalArgumentException(err.message), value => value)
    values(grid.nVoxels / 2)(0)

  @Benchmark
  @OperationsPerInvocation(128)
  def primitiveComposeInto(): Double =
    var repetition = 0
    while repetition < 128 do
      DenseFieldKernels.composePullInto(
        left,
        right,
        destination,
        valid,
        sampler,
        FieldValidity.All,
        FieldValidity.All
      )
      repetition += 1
    destination(grid.nVoxels / 2)

  @Benchmark
  @OperationsPerInvocation(128)
  def primitivePullScalarInto(): Double =
    var repetition = 0
    while repetition < 128 do
      DenseFieldKernels.pullScalarInto(
        scalarSource,
        left,
        scalarDestination,
        scalarValid,
        scalarSampler,
        FieldValidity.All,
        FieldValidity.All,
        0.0
      )
      repetition += 1
    scalarDestination(grid.nVoxels / 2)

  @Benchmark
  @OperationsPerInvocation(128)
  def primitiveRegridPullInto(): Double =
    var repetition = 0
    while repetition < 128 do
      DenseFieldKernels.regridPullInto(
        regridSource,
        grid,
        regridDestination,
        regridValid,
        regridSampler,
        FieldValidity.All
      )
      repetition += 1
    regridDestination(grid.nVoxels / 2)

  @Benchmark
  @OperationsPerInvocation(128)
  def primitiveJacobianReduction(): Double =
    var repetition = 0
    while repetition < 128 do
      DenseFieldKernels.jacobianDeterminantsReduceInto(
        right,
        jacobianDestination,
        jacobianValid,
        jacobianSampler,
        FieldValidity.All,
        jacobianReduction
      )
      repetition += 1
    jacobianReduction.meanOrNaN

  @Benchmark
  @OperationsPerInvocation(128)
  def primitiveInversePairReduction(): Double =
    var repetition = 0
    while repetition < 128 do
      DenseFieldKernels.inversePairErrorInto(
        forwardTranslation,
        backwardTranslation,
        forwardSampler,
        backwardSampler,
        FieldValidity.All,
        FieldValidity.All,
        forwardReduction,
        backwardReduction
      )
      repetition += 1
    forwardReduction.maximumMmOrNaN

  @Benchmark
  @OperationsPerInvocation(256)
  def primitivePyramidLevelInto(): Double =
    var repetition = 0
    while repetition < 256 do
      DenseFieldKernels.buildPyramidLevelInto(
        pyramidSource,
        coarseGrid,
        1.2,
        pyramidWorkspace,
        pyramidDestination,
        pyramidValid,
        pyramidSampler,
        FieldValidity.All,
        1e-8
      )
      repetition += 1
    pyramidDestination(coarseGrid.nVoxels / 2)

  @Benchmark
  @OperationsPerInvocation(64)
  def separableBoxSumInto(): Double =
    var repetition = 0
    while repetition < 64 do
      BoxSum3D.sumInto(
        boxSource,
        grid,
        boxRadius,
        boxDestination,
        boxWorkspace
      )
      repetition += 1
    boxDestination(grid.nVoxels / 2)

  @Benchmark
  @OperationsPerInvocation(32)
  def physicalGaussianInto(): Double =
    var repetition = 0
    while repetition < 32 do
      Gaussian3D.smoothInto(
        boxSource,
        grid,
        sigmaMm = 1.5,
        gaussianDestination,
        gaussianWorkspace
      )
      repetition += 1
    gaussianDestination(grid.nVoxels / 2)

  @Benchmark
  @OperationsPerInvocation(32)
  def normalizedGaussianInto(): Double =
    var repetition = 0
    while repetition < 32 do
      Gaussian3D.normalizedInto(
        boxSource,
        gaussianSupport,
        grid,
        sigmaMm = 1.5,
        minimumWeight = 1e-8,
        gaussianDestination,
        gaussianWeight,
        gaussianWorkspace,
        GaussianBoundary.Reflect,
        gaussianReduction
      )
      repetition += 1
    gaussianDestination(grid.nVoxels / 2)

  private def coordinateField(
      grid: GridSpec
  )(
      f: (Double, Double, Double) => (Double, Double, Double)
  ): DenseVectorField =
    val values = NArrayUtil.ofSize[Double](grid.nVoxels * 3)
    var z = 0
    while z < grid.shape.z do
      var y = 0
      while y < grid.shape.y do
        var x = 0
        while x < grid.shape.x do
          val i = x + y * grid.shape.x + z * grid.shape.x * grid.shape.y
          val world = grid.voxelToWorld(SpatialPoint(x.toDouble, y.toDouble, z.toDouble))
          val mapped = f(world.x, world.y, world.z)
          values(i) = mapped._1
          values(i + grid.nVoxels) = mapped._2
          values(i + 2 * grid.nVoxels) = mapped._3
          x += 1
        y += 1
      z += 1
    DenseVectorField(grid, NDArray(values, grid.dims :+ 3), DenseVectorFieldKind.SourceCoordinates)

  private def analyticScalarVolume(grid: GridSpec): NeuroVol[Double] =
    val values = NArrayUtil.ofSize[Double](grid.nVoxels)
    var z = 0
    while z < grid.shape.z do
      var y = 0
      while y < grid.shape.y do
        var x = 0
        while x < grid.shape.x do
          val i = x + y * grid.shape.x + z * grid.shape.x * grid.shape.y
          val world = grid.voxelToWorld(SpatialPoint(x.toDouble, y.toDouble, z.toDouble))
          values(i) = analyticScalar(world.x, world.y, world.z)
          x += 1
        y += 1
      z += 1
    NeuroVol.fromLinear(values, grid.toNeuroSpace, "analytic")

  private inline def analyticScalar(x: Double, y: Double, z: Double): Double =
    1.5 + 0.75 * x - 0.5 * y + 0.25 * z

  private def analyticScalarAt(field: DenseVectorField, index: Int): Double =
    analyticScalar(
      field.values.data(index),
      field.values.data(index + field.grid.nVoxels),
      field.values.data(index + 2 * field.grid.nVoxels)
    )

  private def centerIndex(grid: GridSpec): Int =
    val x = grid.shape.x / 2
    val y = grid.shape.y / 2
    val z = grid.shape.z / 2
    x + y * grid.shape.x + z * grid.shape.x * grid.shape.y

  private def maximumRegridCenterError(index: Int): Double =
    val x = index % grid.shape.x
    val yz = index / grid.shape.x
    val y = yz % grid.shape.y
    val z = yz / grid.shape.y
    val world = grid.voxelToWorld(SpatialPoint(x.toDouble, y.toDouble, z.toDouble))
    val expectedX = 1.02 * world.x + 0.01 * world.y + 0.2
    val expectedY = 0.98 * world.y + 0.015 * world.z - 0.1
    val expectedZ = 1.01 * world.z + 0.005 * world.x
    math.max(
      math.abs(regridDestination(index) - expectedX),
      math.max(
        math.abs(regridDestination(index + grid.nVoxels) - expectedY),
        math.abs(regridDestination(index + 2 * grid.nVoxels) - expectedZ)
      )
    )

  private val rightDeterminant: Double =
    1.01 * (0.99 * 1.005 - 0.01 * 0.0) -
      0.015 * (0.0 * 1.005 - 0.01 * 0.02)

  private def maximumPyramidConstantError(): Double =
    var maximum = 0.0
    var i = 0
    while i < pyramidDestination.length do
      require(pyramidValid(i), s"pyramid benchmark voxel $i must be valid")
      maximum = math.max(maximum, math.abs(pyramidDestination(i) - 7.25))
      i += 1
    maximum

  private def checksumReference(values: Vector[Vector[Double]]): Double =
    var sum = 0.0
    var i = 0
    while i < values.length do
      if valid(i) then
        val point = values(i)
        val weight = (i % 17 + 1).toDouble
        sum += weight * (point(0) + 0.5 * point(1) + 0.25 * point(2))
      i += 1
    sum

  private def checksumPrimitive(): Double =
    var sum = 0.0
    var i = 0
    while i < grid.nVoxels do
      if valid(i) then
        val weight = (i % 17 + 1).toDouble
        sum += weight * (
          destination(i) +
            0.5 * destination(i + grid.nVoxels) +
            0.25 * destination(i + 2 * grid.nVoxels)
        )
      i += 1
    sum
