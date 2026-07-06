package scalafim.image

final case class DenseFieldInverseOptions(
    maxIterations: Int = 32,
    tolerance: Double = 1e-6,
    relaxation: Double = 1.0,
    interpolation: Resample.Method = Resample.Method.Linear
)

final case class DenseFieldInverseResult(
    morphism: DenseFieldMorphism,
    iterations: Int,
    maxResidual: Double,
    converged: Boolean
):
  require(iterations >= 0, "iteration count must be non-negative")
  require(maxResidual.isFinite && maxResidual >= 0.0, "maxResidual must be finite and non-negative")

object DenseFieldInverse:

  def approximate(
      morphism: DenseFieldMorphism,
      inverseGrid: GridSpec,
      options: DenseFieldInverseOptions = DenseFieldInverseOptions()
  ): Either[MorphismError, DenseFieldInverseResult] =
    validateOptions(options).flatMap { _ =>
      val query = inverseGrid.worldCoords
      val data = NArrayUtil.ofSize[Double](inverseGrid.nVoxels * 3)
      var maxResidual = 0.0
      var maxIterationsUsed = 0
      var converged = true
      var i = 0
      while i < query.length do
        val target = query(i)
        val solved = solvePoint(morphism, target, options)
        maxResidual = math.max(maxResidual, solved.residual)
        maxIterationsUsed = math.max(maxIterationsUsed, solved.iterations)
        if !solved.converged then converged = false
        var component = 0
        while component < 3 do
          data(component * inverseGrid.nVoxels + i) = solved.point(component) - target(component)
          component += 1
        i += 1

      DenseFieldMorphism
        .displacement(
          source = morphism.target,
          target = morphism.source,
          grid = inverseGrid,
          field = NDArray(data, inverseGrid.dims :+ 3),
          interpolation = options.interpolation,
          cost = morphism.cost + inversePenalty(maxResidual, converged),
          methodTag = s"${morphism.methodTag}:approx-inverse"
        )
        .map { inverse =>
          DenseFieldInverseResult(
            morphism = inverse,
            iterations = maxIterationsUsed,
            maxResidual = maxResidual,
            converged = converged
          )
        }
    }

  private final case class PointSolve(
      point: Vector[Double],
      iterations: Int,
      residual: Double,
      converged: Boolean
  )

  private def solvePoint(
      morphism: DenseFieldMorphism,
      target: Vector[Double],
      options: DenseFieldInverseOptions
  ): PointSolve =
    var current = target
    var residual = Double.PositiveInfinity
    var iterations = 0
    var done = false
    while iterations < options.maxIterations && !done do
      val mapped = morphism.transform(current)
      val delta = Vector.tabulate(3)(axis => target(axis) - mapped(axis))
      residual = norm(delta)
      if residual <= options.tolerance then done = true
      else
        current = Vector.tabulate(3)(axis => current(axis) + options.relaxation * delta(axis))
        iterations += 1

    if done then PointSolve(current, iterations, residual, converged = true)
    else
      val mapped = morphism.transform(current)
      val delta = Vector.tabulate(3)(axis => target(axis) - mapped(axis))
      val finalResidual = norm(delta)
      PointSolve(current, iterations, finalResidual, converged = finalResidual <= options.tolerance)

  private def validateOptions(options: DenseFieldInverseOptions): Either[MorphismError, Unit] =
    if options.maxIterations <= 0 then
      Left(MorphismError.InvalidInverseParameters("maxIterations must be positive"))
    else if !options.tolerance.isFinite || options.tolerance <= 0.0 then
      Left(MorphismError.InvalidInverseParameters("tolerance must be finite and positive"))
    else if !options.relaxation.isFinite || options.relaxation <= 0.0 || options.relaxation > 1.0 then
      Left(MorphismError.InvalidInverseParameters("relaxation must be in (0, 1]"))
    else DenseFieldInterpolationPlan.validateMethod(options.interpolation)

  private def inversePenalty(maxResidual: Double, converged: Boolean): Double =
    if converged then maxResidual else maxResidual + 1.0

  private def norm(vector: Vector[Double]): Double =
    math.sqrt(vector.map(value => value * value).sum)
