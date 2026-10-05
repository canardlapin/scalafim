package scalafim.fmri.fit

import gale.linalg.{DMat, DVec, LinAlgError, Matrix, QR, QROptions, QRPivoting}
import gale.optim.{FirstOrderConfig, FirstOrderError, FirstOrderSolvers, FirstOrderStoppingStatus, FirstOrderTolerance, ProjectionSet, SmoothObjective}
import gale.spectral.{SingularSelection, Svds}
import scalafim.fmri.ar.{WhiteningPlan, WhiteningTransform}
import scalafim.fmri.model.{FitEngine, ReducedRankComponentSpec, ReducedRankGlsConfig, ReducedRankInferencePolicy, ReducedRankSolverConfig}

/** Domain objective adapter. Gale owns numerical iteration and decompositions. */
private[fit] object VoxelwiseReducedRankGls:
  private[fit] final case class Geometry(
      whitenedDesign: DMat,
      whitenedResponse: DMat,
      target: DMat,
      nuisance: DMat,
      residualizedTarget: DMat,
      residualizedResponse: DMat,
      gram: DMat,
      score: DMat,
      fullCoefficients: DMat,
      fullResidual: DMat,
      residualLeverage: DVec
  )

  private[fit] final case class Solution(
      coefficients: DMat,
      targetCoefficients: DMat,
      residualVariance: DVec,
      objective: Double,
      achievedRank: Int,
      selectedStart: Int,
      starts: Vector[ReducedRankStartDiagnostic]
  )

  def prepare(
      gls: GlsPrepared,
      response: ResponseBlock,
      config: ReducedRankGlsConfig,
      partition: ReducedRankDesignPartition
  ): Either[FitError, VoxelwiseReducedRankPrepared] =
    val order = gls.selectedVoxelIndices.indices.toVector.sortBy(gls.selectedVoxelIndices)
    val ids = order.map(gls.selectedVoxelIndices)
    val y = columns(response.value, order)
    val plans = gls.whitening match
      case GlsWhitening.Voxelwise(values) => order.map(values)
      case GlsWhitening.Shared(plan) => Vector.fill(ids.length)(plan)
    for
      rank <- requestedRank(config.components, partition.targetPredictors, ids.length)
      geometries <- geometry(gls.design.value, y, plans, partition)
      fit <- solve(geometries, partition, rank, config.solver)
      df <- ResidualDegreesOfFreedom(gls.design.timepoints - gls.design.predictors)
      diagnostic = VoxelwiseReducedRankDiagnostics(
        rank, fit.achievedRank, fit.objective, fit.selectedStart, fit.starts, config.solver,
        partition.targetColumns, ids, plans, fingerprint(gls.design.value, y, ids), config.autocorrelation
      )
      uncertainty <- config.inference match
        case ReducedRankInferencePolicy.EstimatesOnly => Right(VoxelwiseReducedRankUncertainty.Unavailable)
        case ReducedRankInferencePolicy.VoxelwiseBootstrap(bootstrap) =>
          VoxelwiseReducedRankBootstrap.run(gls.design, y, gls.partitions, ids, plans, geometries, partition, rank, fit, config, bootstrap)
        case _ => Left(FitError.UnsupportedEngine("voxelwise reduced-rank fitting requires EstimatesOnly or explicit VoxelwiseBootstrap inference"))
      estimate = VoxelwiseReducedRankEstimate(CoefficientBlock(fit.coefficients), uncertainty, fit.residualVariance, df, diagnostic)
    yield new VoxelwiseReducedRankPrepared(gls.design.value, y, ids, gls.partitions, estimate)

  private[fit] def requestedRank(request: ReducedRankComponentSpec, targets: Int, voxels: Int): Either[FitError, Int] =
    val maximum = math.min(targets, voxels)
    request match
      case ReducedRankComponentSpec.Full => Right(maximum)
      case ReducedRankComponentSpec.Fixed(count) if count.value <= maximum => Right(count.value)
      case ReducedRankComponentSpec.Fixed(count) => Left(FitError.InvalidFitAxis("voxelwise reduced rank", s"rank ${count.value} exceeds $maximum"))
      case _ => Left(FitError.UnsupportedEngine("voxelwise reduced-rank geometry supports explicit fixed rank or Full; adaptive energy/RSS policies require a separate definition"))

  private[fit] def geometry(
      design: DMat,
      response: DMat,
      plans: Vector[WhiteningPlan],
      partition: ReducedRankDesignPartition
  ): Either[FitError, Vector[Geometry]] =
    if plans.length != response.cols then Left(FitError.InvalidFitAxis("voxelwise whitening", "plan count differs from response"))
    else if design.rows <= design.cols then Left(FitError.NonPositiveResidualDegreesOfFreedom(design.rows - design.cols))
    else
      val out = Vector.newBuilder[Geometry]
      var voxel = 0
      while voxel < response.cols do
        val result = for
          _ <- validateWhitening(plans(voxel))
          w <- WhiteningTransform(plans(voxel), design, columns(response, Vector(voxel))).left.map(Gls.arToFitError)
          fullQr <- fullRankQr(w.design)
          full <- fullQr.solveLeastSquares(w.response).left.map(FitError.SingularDesign.apply)
          fullCov <- fullQr.normalizedCovariance.left.map(FitError.SingularDesign.apply)
          x = columns(w.design, partition.targetColumns)
          z = columns(w.design, partition.nuisanceColumns)
          projected <- residualize(x, w.response, z)
          (d, y) = projected
          targetQr <- fullRankQr(d)
          cov <- targetQr.normalizedCovariance.left.map(FitError.SingularDesign.apply)
          gram = d.t * d
          conditionBound = rowNorm(gram) * rowNorm(cov)
          _ <- if conditionBound.isFinite && conditionBound <= 1e12 then Right(())
            else Left(FitError.InvalidFitAxis("voxelwise reduced-rank conditioning", s"voxel $voxel Gram condition bound $conditionBound exceeds 1e12"))
          leverages = DVec.fromSeq((0 until design.rows).map { row =>
            var h = 0.0
            var a = 0
            while a < design.cols do
              var b = 0
              while b < design.cols do
                h += w.design(row, a) * fullCov(a, b) * w.design(row, b)
                b += 1
              a += 1
            1.0 - h
          })
        yield Geometry(w.design, w.response, x, z, d, y, gram, d.t * y, full, subtract(w.response, w.design * full), leverages)
        result match
          case Left(error) => return Left(error)
          case Right(value) => out += value
        voxel += 1
      Right(out.result())

  private def validateWhitening(plan: WhiteningPlan): Either[FitError, Unit] =
    val scales = plan.segments.map(s => plan.coefficientsFor(s).firstScale(plan.initialCondition))
    scales.collectFirst { case Left(error) => error } match
      case Some(error) => Left(Gls.arToFitError(error))
      case None =>
        val invalid = scales.collect { case Right(scale) => scale }.find(s => !s.isFinite || s <= 1e-10)
        invalid match
          case Some(scale) => Left(FitError.InvalidFitAxis("voxelwise whitening first scale", s"requires a finite scale > 1e-10, got $scale"))
          case None => Right(())

  private[fit] def solve(
      geometries: Vector[Geometry],
      partition: ReducedRankDesignPartition,
      rank: Int,
      config: ReducedRankSolverConfig
  ): Either[FitError, Solution] =
    val p = partition.targetPredictors
    val m = geometries.length
    val unrestricted = Matrix.tabulate(p, m)((r, v) => geometries(v).fullCoefficients(partition.targetColumns(r), 0))
    val inactive = rank >= math.min(p, m)
    val loss = (b: DMat) => targetLoss(geometries, b)
    // Invertible row/overall scaling preserves rank and the original loss.
    // Solve in unit-stable coordinates so stopping is invariant to X/Y units.
    val scales = Vector.tabulate(p)(r => math.sqrt(geometries.map(_.gram(r, r)).sum / m.toDouble))
    val responseRms = math.sqrt(geometries.map(g => squaredNorm(g.residualizedResponse)).sum /
      (geometries.head.residualizedResponse.rows.toDouble * m.toDouble))
    val responseScale = if responseRms > 0.0 then responseRms else 1.0
    def decode(at: DMat): DMat = Matrix.tabulate(p, m)((r, v) => at(r, v) * responseScale / scales(r))
    val normalizedFull = Matrix.tabulate(p, m)((r, v) => unrestricted(r, v) * scales(r) / responseScale)
    val solved: Either[FitError, (DMat, Int, Vector[ReducedRankStartDiagnostic])] =
      if inactive then Right((unrestricted, 0, Vector(ReducedRankStartDiagnostic(0, ReducedRankSolverStatus.Converged, 0, loss(unrestricted), 0.0))))
      else
        val normalizedGrams = geometries.map(g => Matrix.tabulate(p, p)((r, c) => g.gram(r, c) / (scales(r) * scales(c))))
        val bound = normalizedGrams.map(rowNorm).max
        val objective = new SmoothObjective:
          val variableRows: Int = p
          val lipschitz: Double = bound
          def value(at: DMat): Either[FirstOrderError, Double] = Right(0.5 * loss(decode(at)) / (responseScale * responseScale))
          def gradient(at: DMat): Either[FirstOrderError, DMat] =
            Right(Matrix.tabulate(p, m) { (r, v) =>
              var sum = -geometries(v).score(r, 0) / (scales(r) * responseScale)
              var c = 0
              while c < p do
                sum += normalizedGrams(v)(r, c) * at(c, v)
                c += 1
              sum
            })
        val projection = new ProjectionSet:
          val variableRows: Int = p
          def project(at: DMat): Either[FirstOrderError, DMat] =
            truncate(at, rank).left.map(error => FirstOrderError.OracleFailure("rank projection", error.message))
        for
          projected <- truncate(normalizedFull, rank)
          tolerance <- FirstOrderTolerance.from(config.tolerance, config.tolerance).left.map(optimizerError)
          settings <- FirstOrderConfig.from(config.maxIterations, tolerance).left.map(optimizerError)
          candidates <-
            val initial = Vector(projected, Matrix.zeros(p, m), Matrix.tabulate(p, m)((r, v) => if r < rank then normalizedFull(r, v) else 0.0))
            val values = Vector.newBuilder[(DMat, ReducedRankStartDiagnostic)]
            var i = 0
            var error: Option[FitError] = None
            while i < initial.length && error.isEmpty do
              FirstOrderSolvers.projectedGradient(objective, projection, initial(i), settings) match
                case Left(value) => error = Some(optimizerError(value))
                case Right(value) =>
                  // Gale's step certificate describes the preceding iterate.
                  // Certify the returned point using the same domain oracles.
                  val step = settings.stepSafety / bound
                  val finalMapping = for
                    gradient <- objective.gradient(value.primal)
                    projected <- projection.project(Matrix.tabulate(p, m)((r, v) => value.primal(r, v) - step * gradient(r, v)))
                  yield (0 until p).flatMap(r => (0 until m).map(v => math.abs(projected(r, v) - value.primal(r, v)) / step)).max
                  finalMapping match
                    case Left(failure) => error = Some(optimizerError(failure))
                    case Right(residual) =>
                      val scale = math.max(1.0, (0 until p).flatMap(r => (0 until m).map(v => math.abs(value.primal(r, v)))).max)
                      val status = value.status match
                        case FirstOrderStoppingStatus.Converged if residual <= settings.tolerance.threshold(scale) => ReducedRankSolverStatus.Converged
                        case FirstOrderStoppingStatus.Converged => ReducedRankSolverStatus.FinalStationarityRejected
                        case FirstOrderStoppingStatus.IterationLimit => ReducedRankSolverStatus.IterationLimit
                      val original = decode(value.primal)
                      values += original -> ReducedRankStartDiagnostic(i, status, value.certificate.iterations, loss(original), residual)
              i += 1
            error.fold[Either[FitError, Vector[(DMat, ReducedRankStartDiagnostic)]]](Right(values.result()))(Left.apply)
          best <-
            val converged = candidates.filter(_._2.status == ReducedRankSolverStatus.Converged)
            if converged.isEmpty then Left(FitError.InvalidFitAxis("voxelwise reduced-rank solver", s"no start converged in ${config.maxIterations} iterations"))
            else Right(converged.minBy(v => (v._2.objective, v._2.start)))
        yield (best._1, best._2.start, candidates.map(_._2))
    for
      selected <- solved
      (targets, selectedStart, starts) = selected
      coefficients <- recoverNuisance(geometries, partition, targets)
      normalizedTargets = Matrix.tabulate(p, m)((r, v) => targets(r, v) * scales(r) / responseScale)
      singular <- Svds.svd(normalizedTargets, SingularSelection.All).left.map(e => FitError.InvalidFitAxis("achieved reduced rank", e.getMessage))
      converged <- singular.requireConverged.left.map(e => FitError.InvalidFitAxis("achieved reduced rank", e.getMessage))
      maxSingular = if converged.singularValues.length == 0 then 0.0 else converged.singularValues(0)
      rankTolerance = 2.0 * math.max(p, m).toDouble * 2.220446049250313e-16 * maxSingular
      actualRank = converged.singularValues.toSeq.count(_ > rankTolerance)
      _ <- if actualRank <= rank then Right(()) else Left(FitError.InvalidFitAxis("rank projection", s"returned rank $actualRank exceeds requested $rank"))
      variances = DVec.fromSeq(geometries.indices.map { v =>
        val g = geometries(v)
        val residual = subtract(g.whitenedResponse, g.whitenedDesign * columns(coefficients, Vector(v)))
        squaredNorm(residual) / (g.whitenedDesign.rows - g.whitenedDesign.cols).toDouble
      })
    yield Solution(coefficients, targets, variances, loss(targets), actualRank, selectedStart, starts)

  private def recoverNuisance(gs: Vector[Geometry], partition: ReducedRankDesignPartition, targets: DMat): Either[FitError, DMat] =
    val out = Matrix.newBuilder(partition.predictors, gs.length)
    var v = 0
    while v < gs.length do
      val g = gs(v)
      var r = 0
      while r < partition.targetPredictors do
        out(partition.targetColumns(r), v) = targets(r, v)
        r += 1
      if partition.nuisancePredictors > 0 then
        val result = for
          qr <- fullRankQr(g.nuisance)
          gamma <- qr.solveLeastSquares(subtract(g.whitenedResponse, g.target * columns(targets, Vector(v)))).left.map(FitError.SingularDesign.apply)
        yield gamma
        result match
          case Left(error) => return Left(error)
          case Right(gamma) =>
            r = 0
            while r < partition.nuisancePredictors do
              out(partition.nuisanceColumns(r), v) = gamma(r, 0)
              r += 1
      v += 1
    Right(out.result())

  private def truncate(matrix: DMat, rank: Int): Either[FitError, DMat] =
    for
      svd <- Svds.svd(matrix, SingularSelection.All).left.map(e => FitError.InvalidFitAxis("rank projection", e.getMessage))
      s <- svd.requireConverged.left.map(e => FitError.InvalidFitAxis("rank projection", e.getMessage))
    yield Matrix.tabulate(matrix.rows, matrix.cols) { (r, c) =>
      var sum = 0.0
      var k = 0
      while k < math.min(rank, s.singularValues.length) do
        sum += s.u(r, k) * s.singularValues(k) * s.vt(k, c)
        k += 1
      sum
    }

  private def targetLoss(gs: Vector[Geometry], b: DMat): Double =
    var sum = 0.0
    var v = 0
    while v < gs.length do
      val g = gs(v)
      var t = 0
      while t < g.residualizedTarget.rows do
        var residual = g.residualizedResponse(t, 0)
        var p = 0
        while p < b.rows do
          residual -= g.residualizedTarget(t, p) * b(p, v)
          p += 1
        sum += residual * residual
        t += 1
      v += 1
    sum

  private def residualize(x: DMat, y: DMat, z: DMat): Either[FitError, (DMat, DMat)] =
    if z.cols == 0 then Right((x, y))
    else for
      qr <- fullRankQr(z)
      d <- qr.residualize(x).left.map(FitError.SingularDesign.apply)
      response <- qr.residualize(y).left.map(FitError.SingularDesign.apply)
    yield (d, response)

  private def fullRankQr(x: DMat): Either[FitError, QR] =
    val qr = x.qr(QROptions(QRPivoting.Column, None))
    val rank = qr.diagnostics.rank.getOrElse(x.cols)
    if rank != x.cols then Left(FitError.SingularDesign(LinAlgError.RankDeficient(rank, x.cols))) else Right(qr)

  private def optimizerError(error: FirstOrderError): FitError = FitError.InvalidFitAxis("voxelwise reduced-rank solver", error.message)

  private[fit] def columns(x: DMat, indices: Vector[Int]): DMat = Matrix.tabulate(x.rows, indices.length)((r, c) => x(r, indices(c)))
  private[fit] def subtract(a: DMat, b: DMat): DMat = Matrix.tabulate(a.rows, a.cols)((r, c) => a(r, c) - b(r, c))
  private def squaredNorm(a: DMat): Double =
    var sum = 0.0
    var r = 0
    while r < a.rows do
      var c = 0
      while c < a.cols do
        sum += a(r, c) * a(r, c)
        c += 1
      r += 1
    sum
  private def rowNorm(a: DMat): Double =
    (0 until a.rows).map(r => (0 until a.cols).map(c => math.abs(a(r, c))).sum).max

  private def fingerprint(x: DMat, y: DMat, ids: Vector[Int]): String =
    val h = new RrgFingerprint
    h.add(x.rows.toLong)
    h.add(x.cols.toLong)
    h.add(y.cols.toLong)
    ids.foreach(v => h.add(v.toLong))
    Vector(x, y).foreach { matrix =>
      var r = 0
      while r < matrix.rows do
        var c = 0
        while c < matrix.cols do
          h.add(java.lang.Double.doubleToLongBits(matrix(r, c)))
          c += 1
        r += 1
    }
    h.value

/** Non-cryptographic reproducibility fingerprint; prepared input equality is checked separately. */
private[fit] final class RrgFingerprint:
  private var hash = -3750763034362895579L
  def add(value: Long): Unit =
    var shift = 0
    while shift < 64 do
      hash = (hash ^ ((value >>> shift) & 255L)) * 1099511628211L
      shift += 8
  def value: String = java.lang.Long.toHexString(hash)

private[fit] final class VoxelwiseReducedRankPrepared(
    design: DMat,
    response: DMat,
    ids: Vector[Int],
    partitions: Vector[RunPartition],
    estimate: VoxelwiseReducedRankEstimate
):
  private val positions = ids.zipWithIndex.toMap
  private val timepoints = partitions.flatMap(_.timepoints)
  def fitBlock(input: FitBlockInput): Either[FitError, FitBlockResult] =
    if input.timepoints != timepoints || input.partitions != partitions then
      Left(FitError.InvalidFitAxis("voxelwise reduced-rank chunk", "row or run identity differs from preparation"))
    else if input.design.value.rows != design.rows || input.design.value.cols != design.cols ||
        !(0 until design.rows).forall(r => (0 until design.cols).forall(c => input.design.value(r, c) == design(r, c))) then
      Left(FitError.InvalidFitAxis("voxelwise reduced-rank chunk", "design differs from prepared design"))
    else if input.voxelIndices.exists(v => !positions.contains(v)) then
      Left(FitError.InvalidFitAxis("voxelwise reduced-rank chunk", "voxel was absent from preparation"))
    else
      val selected = input.voxelIndices.map(positions)
      val sameResponse = input.response.timepoints == response.rows && selected.indices.forall { v =>
        (0 until response.rows).forall(t => input.response.value(t, v) == response(t, selected(v)))
      }
      if !sameResponse then Left(FitError.InvalidFitAxis("voxelwise reduced-rank chunk", "response differs from prepared response"))
      else estimate.selectVoxelPositions(selected).map { value =>
        VoxelwiseReducedRankFitBlockResult(
          estimate = value,
          voxelIndices = input.voxelIndices,
          timepoints = input.timepoints,
          engine = FitEngine.ReducedRankGls,
          coefficientAxis = input.coefficientAxis,
          preparationProvenance = input.preparationProvenance,
          voxelStatuses = Some(VoxelFitStatus.refine(input.resolvedVoxelStatuses, value.residualVariance)),
          fitExclusions = input.fitExclusions
        )
      }
