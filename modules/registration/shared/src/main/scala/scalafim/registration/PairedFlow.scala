package scalafim.registration

import narr.NArray
import scalafim.image.*

final case class Velocity[A] private (
    frame: Frame[A],
    field: DenseVectorField
)

object Velocity:
  def make[A](frame: Frame[A], field: DenseVectorField): Either[RegistrationError, Velocity[A]] =
    if field.grid != frame.grid then Left(RegistrationError.GridMismatch("velocity"))
    else if field.kind != DenseVectorFieldKind.Displacement then
      Left(RegistrationError.InvalidField("velocity kind"))
    else if !allFinite(field.values.data) then Left(RegistrationError.InvalidField("velocity"))
    else Right(new Velocity(frame, field))

  private def allFinite(values: NArray[Double]): Boolean =
    var finite = true
    var index = 0
    while index < values.length && finite do
      finite = values(index).isFinite
      index += 1
    finite

final case class FlowConfig(
    maximumInitialDisplacementMm: Double = 0.4,
    maximumInitialGradient: Double = 0.2,
    minimumSquaringDepth: Int = 0,
    maximumSquaringDepth: Int = 12
):
  require(maximumInitialDisplacementMm.isFinite && maximumInitialDisplacementMm > 0.0)
  require(maximumInitialGradient.isFinite && maximumInitialGradient > 0.0)
  require(minimumSquaringDepth >= 0 && maximumSquaringDepth >= minimumSquaringDepth)

final case class FlowDiagnostics(
    squaringDepth: Int,
    compositionCalls: Int,
    maximumVelocityNormMm: Double,
    maximumVelocityGradientBound: Double,
    initialDisplacementBoundMm: Double,
    initialGradientBound: Double,
    plusValid: Int,
    minusValid: Int,
    voxels: Int
)

final case class PairedFlow[A](
    pair: InversePair[A, A],
    diagnostics: FlowDiagnostics
)

final class PairedFlowWorkspace[A] private[registration] (
    val frame: Frame[A],
    private[registration] val plus: PairedFlowBuffers,
    private[registration] val minus: PairedFlowBuffers,
    private[registration] val inverseAffine: DMat
):
  val ownedCoordinateBuffers: Int = 4
  val ownedValidityBuffers: Int = 4

object PairedFlowWorkspace:
  def apply[A](frame: Frame[A]): PairedFlowWorkspace[A] =
    val inverse = DMat.invert(frame.grid.affine).fold(
      reason => throw new IllegalArgumentException(s"velocity grid affine is singular: $reason"),
      identity
    )
    new PairedFlowWorkspace(frame, buffers(frame.grid), buffers(frame.grid), inverse)

  private def buffers(grid: GridSpec): PairedFlowBuffers =
    val firstValues = NArrayUtil.ofSize[Double](grid.nVoxels * 3)
    val secondValues = NArrayUtil.ofSize[Double](grid.nVoxels * 3)
    PairedFlowBuffers(
      DenseVectorField(
        grid,
        NDArray(firstValues, grid.dims :+ 3),
        DenseVectorFieldKind.SourceCoordinates
      ),
      NArrayUtil.ofSize[Boolean](grid.nVoxels),
      DenseVectorField(
        grid,
        NDArray(secondValues, grid.dims :+ 3),
        DenseVectorFieldKind.SourceCoordinates
      ),
      NArrayUtil.ofSize[Boolean](grid.nVoxels),
      DenseFieldSampler(grid)
    )

private[registration] final case class PairedFlowBuffers(
    first: DenseVectorField,
    firstValid: NArray[Boolean],
    second: DenseVectorField,
    secondValid: NArray[Boolean],
    sampler: DenseFieldSampler
)

object PairedScalingAndSquaring:
  def expHalfPair[A](
      velocity: Velocity[A],
      config: FlowConfig = FlowConfig()
  ): Either[RegistrationError, PairedFlow[A]] =
    expHalfPairWith(velocity, PairedFlowWorkspace(velocity.frame), config)

  /** Writes through reusable ping-pong buffers.
    *
    * The returned maps borrow the workspace and remain valid only until the
    * next call using the same workspace.
    */
  def expHalfPairWith[A](
      velocity: Velocity[A],
      workspace: PairedFlowWorkspace[A],
      config: FlowConfig = FlowConfig()
  ): Either[RegistrationError, PairedFlow[A]] =
    if velocity.frame.domain != workspace.frame.domain then
      Left(
        RegistrationError.FrameMismatch(
          "paired flow workspace",
          velocity.frame.domain,
          workspace.frame.domain
        )
      )
    else if velocity.frame.grid != workspace.frame.grid then
      Left(RegistrationError.GridMismatch("paired flow workspace"))
    else compute(velocity, workspace, config)

  private def compute[A](
      velocity: Velocity[A],
      workspace: PairedFlowWorkspace[A],
      config: FlowConfig
  ): Either[RegistrationError, PairedFlow[A]] =
    val grid = velocity.frame.grid
    val values = velocity.field.values.data
    val maximumNorm = maximumVectorNorm(values, grid.nVoxels)
    val maximumGradient = maximumGradientBound(velocity.field, workspace.inverseAffine)
    val displacementDepth = requiredDepth(0.5 * maximumNorm, config.maximumInitialDisplacementMm)
    val gradientDepth = requiredDepth(0.5 * maximumGradient, config.maximumInitialGradient)
    val depth = math.max(config.minimumSquaringDepth, math.max(displacementDepth, gradientDepth))
    if depth > config.maximumSquaringDepth then
      Left(RegistrationError.SquaringDepthExceeded(depth, config.maximumSquaringDepth))
    else
      val scale = 1.0 / math.pow(2.0, depth.toDouble + 1.0)
      initialMapInto(grid, values, scale, workspace.plus)
      initialMapInto(grid, values, -scale, workspace.minus)
      val squaredPlus = square(workspace.plus, depth)
      val squaredMinus = square(workspace.minus, depth)
      val plusPull = DensePull.unsafe(
        velocity.frame,
        velocity.frame,
        squaredPlus.field,
        FieldValidity.Mask(squaredPlus.valid)
      )
      val minusPull = DensePull.unsafe(
        velocity.frame,
        velocity.frame,
        squaredMinus.field,
        FieldValidity.Mask(squaredMinus.valid)
      )
      val pair = InversePair.unsafe(plusPull, minusPull)
      Right(
        PairedFlow(
          pair,
          FlowDiagnostics(
            depth,
            depth * 2,
            maximumNorm,
            maximumGradient,
            0.5 * maximumNorm / math.pow(2.0, depth.toDouble),
            0.5 * maximumGradient / math.pow(2.0, depth.toDouble),
            countValid(squaredPlus.valid),
            countValid(squaredMinus.valid),
            grid.nVoxels
          )
        )
      )

  private def initialMapInto(
      grid: GridSpec,
      velocity: NArray[Double],
      scale: Double,
      buffers: PairedFlowBuffers
  ): Unit =
    val coordinates = buffers.first.values.data
    DenseFieldKernels.identityInto(grid, coordinates, buffers.firstValid)
    var index = 0
    while index < coordinates.length do
      coordinates(index) += scale * velocity(index)
      index += 1

  private def square(buffers: PairedFlowBuffers, depth: Int): DensePullResult =
    var currentField = buffers.first
    var currentValid = buffers.firstValid
    var destinationField = buffers.second
    var destinationValid = buffers.secondValid
    var iteration = 0
    while iteration < depth do
      DenseFieldKernels.composePullInto(
        currentField,
        currentField,
        destinationField.values.data,
        destinationValid,
        buffers.sampler,
        FieldValidity.Mask(currentValid),
        FieldValidity.Mask(currentValid),
        CoordinateMapOutside.Identity
      )
      val previousField = currentField
      val previousValid = currentValid
      currentField = destinationField
      currentValid = destinationValid
      destinationField = previousField
      destinationValid = previousValid
      iteration += 1
    DensePullResult(currentField, currentValid)

  private def requiredDepth(value: Double, limit: Double): Int =
    var depth = 0
    var scaled = value
    while scaled > limit * (1.0 + 1e-12) do
      scaled *= 0.5
      depth += 1
    depth

  private def maximumVectorNorm(values: NArray[Double], n: Int): Double =
    var maximum = 0.0
    var index = 0
    while index < n do
      val x = values(index)
      val y = values(index + n)
      val z = values(index + 2 * n)
      maximum = math.max(maximum, math.sqrt(x * x + y * y + z * z))
      index += 1
    maximum

  /** Frobenius norm is a conservative upper bound on the local operator norm. */
  private def maximumGradientBound(field: DenseVectorField, inverse: DMat): Double =
    val grid = field.grid
    if grid.shape.x < 3 || grid.shape.y < 3 || grid.shape.z < 3 then 0.0
    else
      val values = field.values.data
      val nx = grid.shape.x
      val ny = grid.shape.y
      val nz = grid.shape.z
      val plane = nx * ny
      val n = grid.nVoxels
      var maximum = 0.0
      var z = 1
      while z < nz - 1 do
        var y = 1
        while y < ny - 1 do
          var x = 1
          while x < nx - 1 do
            val index = x + nx * y + plane * z
            var squared = 0.0
            var component = 0
            while component < 3 do
              val offset = component * n
              val dx = 0.5 * (values(offset + index + 1) - values(offset + index - 1))
              val dy = 0.5 * (values(offset + index + nx) - values(offset + index - nx))
              val dz = 0.5 * (values(offset + index + plane) - values(offset + index - plane))
              var axis = 0
              while axis < 3 do
                val derivative = dx * inverse(0, axis) + dy * inverse(1, axis) + dz * inverse(2, axis)
                squared += derivative * derivative
                axis += 1
              component += 1
            maximum = math.max(maximum, math.sqrt(squared))
            x += 1
          y += 1
        z += 1
      maximum

  private def countValid(values: NArray[Boolean]): Int =
    var count = 0
    var index = 0
    while index < values.length do
      if values(index) then count += 1
      index += 1
    count
