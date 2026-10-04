package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.ar.{NoiseEstimationLayout, WhiteningPlan}
import scalafim.fmri.model.{ReducedRankGlsConfig, VoxelwiseBootstrapMode, VoxelwiseReducedRankBootstrapConfig}

private[fit] object VoxelwiseReducedRankBootstrap:
  import VoxelwiseReducedRankGls.*

  def run(
      design: DesignMatrix,
      response: DMat,
      partitions: Vector[RunPartition],
      ids: Vector[Int],
      plans: Vector[WhiteningPlan],
      geometries: Vector[Geometry],
      partition: ReducedRankDesignPartition,
      rank: Int,
      fitted: Solution,
      fitConfig: ReducedRankGlsConfig,
      config: VoxelwiseReducedRankBootstrapConfig
  ): Either[FitError, VoxelwiseReducedRankUncertainty] =
    for
      layout <- Gls.noiseEstimationLayout(partitions, fitConfig.autocorrelation.toLegacy.censoredTimepoints)
      _ <- if plans.forall(_.segments == layout.whiteningSegments) then Right(())
        else Left(FitError.InvalidFitAxis("bootstrap whitening segments", "all voxel plans must match the noise layout"))
      donors <- donorStarts(layout, config.resampling.blockSize.value)
      residuals <- correctedResiduals(geometries, layout)
      (innovations, minimumLeverage) = residuals
      bootstrap <- replicate(design, response, partitions, ids, plans, layout, donors, innovations, partition, rank, fitted, fitConfig, config, minimumLeverage)
    yield bootstrap

  private def replicate(
      design: DesignMatrix,
      response: DMat,
      partitions: Vector[RunPartition],
      ids: Vector[Int],
      plans: Vector[WhiteningPlan],
      layout: NoiseEstimationLayout,
      donors: Vector[(Int, Vector[Int])],
      innovations: DMat,
      partition: ReducedRankDesignPartition,
      rank: Int,
      fitted: Solution,
      fitConfig: ReducedRankGlsConfig,
      config: VoxelwiseReducedRankBootstrapConfig,
      minimumLeverage: Double
  ): Either[FitError, VoxelwiseReducedRankUncertainty.Bootstrap] =
    val mean = design.value * fitted.coefficients
    val rng = new BootstrapRng(config.resampling.seed.value)
    val trace = new RrgFingerprint
    val results = Vector.newBuilder[DMat]
    val objectives = Vector.newBuilder[Double]
    var replicate = 0
    while replicate < config.resampling.replicates.value do
      val indices = sampleIndices(layout, donors, config.resampling.blockSize.value, rng)
      indices.foreach(i => trace.add(i.toLong))
      val sampled = Matrix.newBuilder(response.rows, response.cols)
      var voxel = 0
      while voxel < response.cols do
        val noise = Matrix.tabulate(response.rows, 1)((t, _) => innovations(indices(t), voxel))
        inverseWhiten(plans(voxel), noise) match
          case Left(error) => return Left(FitError.ReducedRankBootstrapFailed(replicate + 1, error))
          case Right(raw) =>
            var t = 0
            while t < response.rows do
              sampled(t, voxel) = mean(t, voxel) + raw(t, 0)
              t += 1
        voxel += 1
      val y = sampled.result()
      val replicatePlans = config.mode match
        case VoxelwiseBootstrapMode.FrozenWhitening => Right(plans)
        case VoxelwiseBootstrapMode.RefitAutocorrelation =>
          Gls.prepare(design, ResponseBlock.unsafe(y), partitions, fitConfig.autocorrelation.toLegacy, ids).map { prepared =>
            prepared.whitening match
              case GlsWhitening.Voxelwise(values) => values
              case GlsWhitening.Shared(plan) => Vector.fill(ids.length)(plan)
          }
      val result = for
        ws <- replicatePlans
        gs <- geometry(design.value, y, ws, partition)
        fit <- solve(gs, partition, rank, fitConfig.solver)
      yield fit
      result match
        case Left(error) => return Left(FitError.ReducedRankBootstrapFailed(replicate + 1, error))
        case Right(value) =>
          results += value.targetCoefficients
          objectives += value.objective
      replicate += 1
    val samples = results.result()
    val count = samples.length
    val p = partition.targetPredictors
    val m = response.cols
    val averages = Matrix.tabulate(p, m)((r, v) => samples.iterator.map(_(r, v)).sum / count.toDouble)
    val covariance = Vector.tabulate(m) { v =>
      Matrix.tabulate(p, p) { (a, b) =>
        samples.iterator.map(s => (s(a, v) - averages(a, v)) * (s(b, v) - averages(b, v))).sum / (count - 1).toDouble
      }
    }
    val lower = Matrix.newBuilder(p, m)
    val upper = Matrix.newBuilder(p, m)
    val alpha = (1.0 - config.confidenceLevel) / 2.0
    var r = 0
    while r < p do
      var v = 0
      while v < m do
        val values = samples.map(_(r, v)).sorted
        lower(r, v) = quantile(values, alpha)
        upper(r, v) = quantile(values, 1.0 - alpha)
        v += 1
      r += 1
    CoefficientCovariance.voxelwise(covariance).map { cov =>
      VoxelwiseReducedRankUncertainty.Bootstrap(
        partition.targetColumns,
        cov,
        StandardErrorBlock(Matrix.tabulate(p, m)((r, v) => math.sqrt(math.max(0.0, covariance(v)(r, r))))),
        CoefficientBlock(lower.result()),
        CoefficientBlock(upper.result()),
        VoxelwiseReducedRankBootstrapDiagnostics(
          config,
          donors.map { case (run, starts) => run -> starts.length },
          layout.excludedRows,
          minimumLeverage,
          objectives.result(),
          if config.mode == VoxelwiseBootstrapMode.RefitAutocorrelation then count else 0,
          trace.value
        )
      )
    }

  private def correctedResiduals(gs: Vector[Geometry], layout: NoiseEstimationLayout): Either[FitError, (DMat, Double)] =
    val out = Matrix.newBuilder(layout.rows, gs.length)
    var minimum = 1.0
    val runs = layout.coveredSegments.runs
    var run = 0
    while run < runs.length do
      val rows = layout.segmentsForRun(runs(run)).flatMap(s => s.start until s.endExclusive)
      if rows.isEmpty then return Left(FitError.InvalidFitAxis("bootstrap donor rows", s"run ${runs(run)} has no eligible noise rows"))
      var v = 0
      while v < gs.length do
        val values = new Array[Double](rows.length)
        var i = 0
        var sum = 0.0
        while i < rows.length do
          val leverage = gs(v).residualLeverage(rows(i))
          if !leverage.isFinite || leverage <= 1e-10 then
            return Left(FitError.InvalidFitAxis("bootstrap residual leverage", s"voxel $v row ${rows(i)} has 1-h=$leverage; requires > 1e-10"))
          minimum = math.min(minimum, leverage)
          values(i) = gs(v).fullResidual(rows(i), 0) / math.sqrt(leverage)
          sum += values(i)
          i += 1
        val mean = sum / rows.length.toDouble
        i = 0
        while i < rows.length do
          out(rows(i), v) = values(i) - mean
          i += 1
        v += 1
      run += 1
    Right((out.result(), minimum))

  private[fit] def donorStarts(layout: NoiseEstimationLayout, blockSize: Int): Either[FitError, Vector[(Int, Vector[Int])]] =
    if blockSize <= 0 then Left(FitError.InvalidFitAxis("bootstrap block size", "must be positive"))
    else
      val runs = layout.coveredSegments.runs.map { run =>
        run -> layout.segmentsForRun(run).flatMap(s => s.start to (s.endExclusive - blockSize))
      }
      runs.find(_._2.isEmpty) match
        case Some((run, _)) => Left(FitError.InvalidFitAxis("bootstrap donor blocks", s"run $run has no eligible contiguous block of length $blockSize"))
        case None => Right(runs)

  /** Uniform valid donor starts; output blocks truncate at every whitening reset. */
  private[fit] def sampleIndices(
      layout: NoiseEstimationLayout,
      donors: Vector[(Int, Vector[Int])],
      blockSize: Int,
      rng: BootstrapRng
  ): Vector[Int] =
    val byRun = donors.toMap
    val out = new Array[Int](layout.rows)
    layout.whiteningSegments.foreach { segment =>
      val starts = byRun(segment.runIndex)
      var target = segment.start
      while target < segment.endExclusive do
        val donor = starts(rng.nextInt(starts.length))
        val length = math.min(blockSize, segment.endExclusive - target)
        var i = 0
        while i < length do
          out(target + i) = donor + i
          i += 1
        target += length
    }
    out.toVector

  /** Exact inverse of WhiteningTransform's ARMA recurrence, including first scaling. */
  private[fit] def inverseWhiten(plan: WhiteningPlan, innovations: DMat): Either[FitError, DMat] =
    if innovations.rows != plan.nTimepoints then Left(FitError.RowMismatch(plan.nTimepoints, innovations.rows))
    else
      val out = Matrix.newBuilder(innovations.rows, innovations.cols)
      var segmentIndex = 0
      while segmentIndex < plan.segments.length do
        val segment = plan.segments(segmentIndex)
        val coefficients = plan.coefficientsFor(segment)
        val scale = coefficients.firstScale(plan.initialCondition).left.map(Gls.arToFitError) match
          case Left(error) => return Left(error)
          case Right(value) => value
        if !scale.isFinite || scale <= 1e-10 then
          return Left(FitError.InvalidFitAxis("inverse whitening first scale", s"requires a finite scale > 1e-10, got $scale"))
        var row = segment.start
        while row < segment.endExclusive do
          var col = 0
          while col < innovations.cols do
            var value = innovations(row, col) / (if row == segment.start then scale else 1.0)
            var lag = 0
            while lag < coefficients.phi.length do
              val previous = row - lag - 1
              if previous >= segment.start then value += coefficients.phi(lag) * out(previous, col)
              lag += 1
            lag = 0
            while lag < coefficients.theta.length do
              val previous = row - lag - 1
              if previous >= segment.start then value += coefficients.theta(lag) * innovations(previous, col)
              lag += 1
            if !value.isFinite then return Left(FitError.NonFiniteInput("inverse-whitened bootstrap innovations"))
            out(row, col) = value
            col += 1
          row += 1
        segmentIndex += 1
      Right(out.result())

  private def quantile(sorted: Vector[Double], probability: Double): Double =
    val position = probability * (sorted.length - 1).toDouble
    val lower = math.floor(position).toInt
    val upper = math.min(lower + 1, sorted.length - 1)
    sorted(lower) + (position - lower.toDouble) * (sorted(upper) - sorted(lower))

  private[fit] final class BootstrapRng(seed: Int):
    private var state = if seed == 0 || seed == Int.MaxValue then 1L else seed.toLong
    def nextInt(bound: Int): Int =
      require(bound > 0, "random bound must be positive")
      state = (state * 48271L) % 2147483647L
      ((state - 1L) % bound.toLong).toInt
