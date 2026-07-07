package scalafim.fmri.motion

import scalafim.image.{NeuroVec, NeuroVol}

object MotionEstimator:
  def estimate(
      run: NeuroVec[Double],
      mask: Option[NeuroVol[Boolean]] = None,
      plan: MotionPlan = MotionPlan.default
  ): Either[MotionError, MotionEstimate] =
    if plan.engine != MotionEngine.RigidRobust then Left(MotionError.NotImplemented(s"${plan.engine} estimator"))
    else
      for
        _ <- validateSupportedPlan(plan)
        _ <- MotionMetrics.validateMask(run, mask)
        _ <- validateFiniteRun(run)
        refIndex <- referenceIndex(run, plan.reference)
        levels <- buildPyramidLevels(run, mask, plan.control)
      yield
        val ctx = EstimatorContext(run, plan.control, refIndex, levels)
        val template = buildTemplate(run, plan.reference, refIndex)
        val first = fitRun(ctx, template)
        refreshTemplate(ctx, template, first) match
          case None => first
          case Some(refreshed) => fitRun(ctx, refreshed)

  private final case class SamplePoint(i: Int, j: Int, k: Int, linear: Int)

  private final case class PyramidLevel(downsample: Int, maxIterations: Int, samples: Vector[SamplePoint])

  private final case class EstimatorContext(
      run: NeuroVec[Double],
      control: MotionControl,
      referenceIndex: Int,
      levels: Vector[PyramidLevel]
  ):
    val dims: Vector[Int] = run.space.spatialDims
    val nx: Int = dims(0)
    val ny: Int = dims(1)
    val nz: Int = dims(2)
    val nxyz: Int = nx * ny * nz
    val px: Double = run.space.spacing(0)
    val py: Double = run.space.spacing(1)
    val pz: Double = run.space.spacing(2)
    val diagnosticLevel: PyramidLevel = levels.last

  private final case class CostResult(cost: Double, overlap: Double)

  private final case class FitResult(pose: RigidPose, diagnostics: FrameFitDiagnostics)

  private final case class CaptureStart(pose: RigidPose, warmCost: CostResult, startCost: CostResult)

  private final case class SeedCost(pose: RigidPose, cost: CostResult)

  private def validateSupportedPlan(plan: MotionPlan): Either[MotionError, Unit] =
    if !plan.acquisitionTiming.isVolume then Left(MotionError.NotImplemented("slice/packet-aware estimation"))
    else if plan.control.execution.policy == ExecutionPolicy.ParallelFrames then Left(MotionError.NotImplemented("parallel frame estimation"))
    else if !plan.control.whitening.implemented then Left(MotionError.NotImplemented(s"${plan.control.whitening.policy} whitening"))
    else Right(())

  private def fitRun(ctx: EstimatorContext, template: Array[Double]): MotionEstimate =
    val nt = ctx.run.nVolumes
    val poses = Array.fill(nt)(RigidPose.identity)
    val diagnostics = Array.ofDim[FrameFitDiagnostics](nt)

    val refCost = evaluate(ctx, ctx.diagnosticLevel, template, ctx.referenceIndex, RigidPose.identity)
    diagnostics(ctx.referenceIndex) =
      FrameFitDiagnostics(
        costInitial = refCost.cost,
        costFinal = refCost.cost,
        iterations = 0,
        overlap = refCost.overlap,
        restarted = false,
        converged = true
      )

    var t = ctx.referenceIndex + 1
    while t < nt do
      val result = fitFrame(ctx, template, t, poses(t - 1))
      poses(t) = result.pose
      diagnostics(t) = result.diagnostics
      t += 1

    t = ctx.referenceIndex - 1
    while t >= 0 do
      val result = fitFrame(ctx, template, t, poses(t + 1))
      poses(t) = result.pose
      diagnostics(t) = result.diagnostics
      t -= 1

    val outputPoses = regularizeTrace(ctx, poses)
    val outputDiagnostics =
      if outputPoses eq poses then diagnostics
      else recomputeFinalDiagnostics(ctx, template, outputPoses, diagnostics)

    MotionEstimate(
      trace = MotionTrace.unsafe(outputPoses.toVector),
      diagnostics = outputDiagnostics.toVector,
      control = ctx.control
    )

  private def regularizeTrace(ctx: EstimatorContext, poses: Array[RigidPose]): Array[RigidPose] =
    ctx.control.temporal.regularization match
      case TemporalRegularizationPolicy.Disabled => poses
      case TemporalRegularizationPolicy.Enabled =>
        val out = poses.clone()
        var t = 0
        while t < poses.length do
          if t == ctx.referenceIndex then out(t) = RigidPose.identity
          else
            val left = if t > 0 then poses(t - 1) else poses(t)
            val right = if t < poses.length - 1 then poses(t + 1) else poses(t)
            out(t) =
              if t == 0 then blendPoses(poses(t), left, right, 2.0 / 3.0, 0.0, 1.0 / 3.0)
              else if t == poses.length - 1 then blendPoses(poses(t), left, right, 2.0 / 3.0, 1.0 / 3.0, 0.0)
              else blendPoses(poses(t), left, right, 0.5, 0.25, 0.25)
          t += 1
        out

  private def blendPoses(
      center: RigidPose,
      left: RigidPose,
      right: RigidPose,
      centerWeight: Double,
      leftWeight: Double,
      rightWeight: Double
  ): RigidPose =
    RigidPose.unsafe(
      center.tx * centerWeight + left.tx * leftWeight + right.tx * rightWeight,
      center.ty * centerWeight + left.ty * leftWeight + right.ty * rightWeight,
      center.tz * centerWeight + left.tz * leftWeight + right.tz * rightWeight,
      center.rx * centerWeight + left.rx * leftWeight + right.rx * rightWeight,
      center.ry * centerWeight + left.ry * leftWeight + right.ry * rightWeight,
      center.rz * centerWeight + left.rz * leftWeight + right.rz * rightWeight
    )

  private def recomputeFinalDiagnostics(
      ctx: EstimatorContext,
      template: Array[Double],
      poses: Array[RigidPose],
      diagnostics: Array[FrameFitDiagnostics]
  ): Array[FrameFitDiagnostics] =
    val out = diagnostics.clone()
    var t = 0
    while t < poses.length do
      val finalCost = evaluate(ctx, ctx.diagnosticLevel, template, t, poses(t))
      val d = diagnostics(t)
      out(t) =
        d.copy(
          costFinal = finalCost.cost,
          overlap = finalCost.overlap
        )
      t += 1
    out

  private def fitFrame(
      ctx: EstimatorContext,
      template: Array[Double],
      frame: Int,
      warmStart: RigidPose
  ): FitResult =
    val capture = captureStart(ctx, ctx.levels.head, template, frame, warmStart)
    var pose = capture.pose
    var lambda = math.max(1e-6, ctx.control.optimizer.lambda0)
    var totalIterations = 0
    var converged = false
    var levelIndex = 0

    while levelIndex < ctx.levels.length do
      val level = ctx.levels(levelIndex)
      var current =
        if levelIndex == 0 then capture.startCost
        else evaluate(ctx, level, template, frame, pose)
      var iter = 0
      var levelConverged = false

      while iter < level.maxIterations && !levelConverged do
        val system = buildSystem(ctx, level, template, frame, pose, lambda)
        solve6(system.hessian, system.gradient.map(-_)) match
          case None =>
            levelConverged = true
          case Some(rawStep) =>
            val step = limitStep(rawStep)
            val stepNorm = poseStepNorm(step)
            if stepNorm <= ctx.control.optimizer.stepTolerance then levelConverged = true
            else
              var accepted = false
              var attempt = 0
              var trialStep = step
              while attempt < 6 && !accepted do
                val trialPose = addStep(pose, trialStep)
                val trialCost = evaluate(ctx, level, template, frame, trialPose)
                if trialCost.cost < current.cost then
                  val relDrop =
                    if current.cost == 0.0 then current.cost - trialCost.cost
                    else (current.cost - trialCost.cost) / math.max(1e-12, math.abs(current.cost))
                  pose = trialPose
                  current = trialCost
                  lambda = math.max(1e-8, lambda * 0.5)
                  accepted = true
                  if relDrop <= ctx.control.optimizer.costTolerance then levelConverged = true
                else
                  lambda *= 4.0
                  trialStep = trialStep.map(_ * 0.5)
                  attempt += 1
              if !accepted then levelConverged = true
        iter += 1

      totalIterations += iter
      converged = levelConverged
      levelIndex += 1

    val outputPose = shrinkLowMotionPose(ctx, pose)
    val initial = evaluate(ctx, ctx.diagnosticLevel, template, frame, warmStart)
    val start = evaluate(ctx, ctx.diagnosticLevel, template, frame, capture.pose)
    val finalCost = evaluate(ctx, ctx.diagnosticLevel, template, frame, outputPose)

    FitResult(
      pose = outputPose,
      diagnostics = FrameFitDiagnostics(
        costInitial = initial.cost,
        costFinal = finalCost.cost,
        iterations = totalIterations,
        overlap = finalCost.overlap,
        restarted = start.cost < initial.cost,
        converged = converged
      )
    )

  private final case class NormalSystem(hessian: Array[Double], gradient: Array[Double])

  private def buildSystem(
      ctx: EstimatorContext,
      level: PyramidLevel,
      template: Array[Double],
      frame: Int,
      pose: RigidPose,
      lambda: Double
  ): NormalSystem =
    val h = Array.fill(36)(0.0)
    val g = Array.fill(6)(0.0)
    val eps = Array(1e-2, 1e-2, 1e-2, 1e-4, 1e-4, 1e-4)
    val deriv = Array.ofDim[Double](6)
    val meanDeriv = Array.ofDim[Double](6)
    val centered = ctx.control.residual.removeFrameMean
    var meanResidual = 0.0
    var meanCount = 0

    val map = MotionSampling.voxelMap(ctx.nx, ctx.ny, ctx.nz, zpad = 0, ctx.px, ctx.py, ctx.pz, pose)
    if centered then
      var s0 = 0
      while s0 < level.samples.length do
        val sample = level.samples(s0)
        val sx = MotionSampling.sourceX(map, sample.i, sample.j, sample.k)
        val sy = MotionSampling.sourceY(map, sample.i, sample.j, sample.k)
        val sz = MotionSampling.sourceZ(map, sample.i, sample.j, sample.k)
        if inBounds(ctx, sx, sy, sz) then
          val fitted = MotionSampling.trilinear(ctx.run.values.data, ctx.nx, ctx.ny, ctx.nz, ctx.nxyz, frame, sx, sy, sz, zeroPad = false)
          meanResidual += fitted - template(sample.linear)
          var p = 0
          while p < 6 do
            val plus = addOne(pose, p, eps(p))
            val minus = addOne(pose, p, -eps(p))
            val fPlus = sampleAt(ctx, frame, plus, sample)
            val fMinus = sampleAt(ctx, frame, minus, sample)
            meanDeriv(p) += (fPlus - fMinus) / (2.0 * eps(p))
            p += 1
          meanCount += 1
        s0 += 1
      if meanCount > 0 then
        meanResidual /= meanCount.toDouble
        var p = 0
        while p < 6 do
          meanDeriv(p) /= meanCount.toDouble
          p += 1

    var s = 0
    while s < level.samples.length do
      val sample = level.samples(s)
      val sx = MotionSampling.sourceX(map, sample.i, sample.j, sample.k)
      val sy = MotionSampling.sourceY(map, sample.i, sample.j, sample.k)
      val sz = MotionSampling.sourceZ(map, sample.i, sample.j, sample.k)
      if inBounds(ctx, sx, sy, sz) then
        val fitted = MotionSampling.trilinear(ctx.run.values.data, ctx.nx, ctx.ny, ctx.nz, ctx.nxyz, frame, sx, sy, sz, zeroPad = false)
        val residual =
          if centered then fitted - template(sample.linear) - meanResidual
          else fitted - template(sample.linear)
        val weight = huberWeight(residual, ctx.control.optimizer.huberK)
        var p = 0
        while p < 6 do
          val plus = addOne(pose, p, eps(p))
          val minus = addOne(pose, p, -eps(p))
          val fPlus = sampleAt(ctx, frame, plus, sample)
          val fMinus = sampleAt(ctx, frame, minus, sample)
          val rawDeriv = (fPlus - fMinus) / (2.0 * eps(p))
          deriv(p) = if centered then rawDeriv - meanDeriv(p) else rawDeriv
          p += 1

        var a = 0
        while a < 6 do
          g(a) += weight * deriv(a) * residual
          var b = 0
          while b < 6 do
            h(a * 6 + b) += weight * deriv(a) * deriv(b)
            b += 1
          a += 1
      s += 1

    var d = 0
    while d < 6 do
      h(d * 6 + d) += lambda * (math.abs(h(d * 6 + d)) + 1e-6)
      d += 1

    NormalSystem(h, g)

  private def captureStart(
      ctx: EstimatorContext,
      level: PyramidLevel,
      template: Array[Double],
      frame: Int,
      warmStart: RigidPose
  ): CaptureStart =
    val warmCost = evaluate(ctx, level, template, frame, warmStart)
    if !ctx.control.capture.enabled then CaptureStart(warmStart, warmCost, warmCost)
    else
      val step = math.min(1.0, math.max(ctx.px, math.max(ctx.py, ctx.pz)))
      val maxTx = math.min(ctx.control.capture.translationHalfWidthMm, step)
      val offsets = Array(-maxTx, 0.0, maxTx)
      val rot = math.toRadians(ctx.control.capture.rotationHalfWidthDeg)
      var best = SeedCost(warmStart, warmCost)
      val translations = Vector.newBuilder[SeedCost]
      var ix = 0
      while ix < offsets.length do
        var iy = 0
        while iy < offsets.length do
          var iz = 0
          while iz < offsets.length do
            val candidate =
              RigidPose.unsafe(
                warmStart.tx + offsets(ix),
                warmStart.ty + offsets(iy),
                warmStart.tz + offsets(iz),
                warmStart.rx,
                warmStart.ry,
                warmStart.rz
              )
            val cost = evaluate(ctx, level, template, frame, candidate)
            val seed = SeedCost(candidate, cost)
            translations += seed
            if cost.cost < best.cost.cost then best = seed
            iz += 1
          iy += 1
        ix += 1

      val rotations =
        Vector(
          SeedCost(warmStart, warmCost),
          rotationSeed(ctx, level, template, frame, warmStart, 3, rot),
          rotationSeed(ctx, level, template, frame, warmStart, 3, -rot),
          rotationSeed(ctx, level, template, frame, warmStart, 4, rot),
          rotationSeed(ctx, level, template, frame, warmStart, 4, -rot),
          rotationSeed(ctx, level, template, frame, warmStart, 5, rot),
          rotationSeed(ctx, level, template, frame, warmStart, 5, -rot)
        )
      var r = 0
      while r < rotations.length do
        if rotations(r).cost.cost < best.cost.cost then best = rotations(r)
        r += 1

      val nKeep = math.max(1, ctx.control.capture.topK)
      val topTranslations = translations.result().sortBy(_.cost.cost).take(nKeep)
      val topRotations = rotations.sortBy(_.cost.cost).take(nKeep)
      var ti = 0
      while ti < topTranslations.length do
        var ri = 0
        while ri < topRotations.length do
          val tPose = topTranslations(ti).pose
          val rPose = topRotations(ri).pose
          val candidate =
            RigidPose.unsafe(
              tPose.tx,
              tPose.ty,
              tPose.tz,
              rPose.rx,
              rPose.ry,
              rPose.rz
            )
          val cost = evaluate(ctx, level, template, frame, candidate)
          if cost.cost < best.cost.cost then best = SeedCost(candidate, cost)
          ri += 1
        ti += 1

      CaptureStart(best.pose, warmCost, best.cost)

  private def rotationSeed(
      ctx: EstimatorContext,
      level: PyramidLevel,
      template: Array[Double],
      frame: Int,
      warmStart: RigidPose,
      index: Int,
      delta: Double
  ): SeedCost =
    val pose = addOne(warmStart, index, delta)
    SeedCost(pose, evaluate(ctx, level, template, frame, pose))

  private def evaluate(
      ctx: EstimatorContext,
      level: PyramidLevel,
      template: Array[Double],
      frame: Int,
      pose: RigidPose
  ): CostResult =
    val map = MotionSampling.voxelMap(ctx.nx, ctx.ny, ctx.nz, zpad = 0, ctx.px, ctx.py, ctx.pz, pose)
    val centered = ctx.control.residual.removeFrameMean
    var meanResidual = 0.0
    var meanCount = 0
    if centered then
      var s0 = 0
      while s0 < level.samples.length do
        val sample = level.samples(s0)
        val sx = MotionSampling.sourceX(map, sample.i, sample.j, sample.k)
        val sy = MotionSampling.sourceY(map, sample.i, sample.j, sample.k)
        val sz = MotionSampling.sourceZ(map, sample.i, sample.j, sample.k)
        if inBounds(ctx, sx, sy, sz) then
          val fitted = MotionSampling.trilinear(ctx.run.values.data, ctx.nx, ctx.ny, ctx.nz, ctx.nxyz, frame, sx, sy, sz, zeroPad = false)
          meanResidual += fitted - template(sample.linear)
          meanCount += 1
        s0 += 1
      if meanCount > 0 then meanResidual /= meanCount.toDouble

    var s = 0
    var loss = 0.0
    var n = 0
    var inside = 0
    while s < level.samples.length do
      val sample = level.samples(s)
      val sx = MotionSampling.sourceX(map, sample.i, sample.j, sample.k)
      val sy = MotionSampling.sourceY(map, sample.i, sample.j, sample.k)
      val sz = MotionSampling.sourceZ(map, sample.i, sample.j, sample.k)
      if inBounds(ctx, sx, sy, sz) then
        inside += 1
        val fitted = MotionSampling.trilinear(ctx.run.values.data, ctx.nx, ctx.ny, ctx.nz, ctx.nxyz, frame, sx, sy, sz, zeroPad = false)
        val residual =
          if centered then fitted - template(sample.linear) - meanResidual
          else fitted - template(sample.linear)
        loss += huberLoss(residual, ctx.control.optimizer.huberK)
        n += 1
      s += 1

    if n == 0 then CostResult(Double.PositiveInfinity, 0.0)
    else CostResult(loss / n.toDouble, inside.toDouble / level.samples.length.toDouble)

  private def sampleAt(ctx: EstimatorContext, frame: Int, pose: RigidPose, sample: SamplePoint): Double =
    val map = MotionSampling.voxelMap(ctx.nx, ctx.ny, ctx.nz, zpad = 0, ctx.px, ctx.py, ctx.pz, pose)
    val sx = MotionSampling.sourceX(map, sample.i, sample.j, sample.k)
    val sy = MotionSampling.sourceY(map, sample.i, sample.j, sample.k)
    val sz = MotionSampling.sourceZ(map, sample.i, sample.j, sample.k)
    MotionSampling.trilinear(ctx.run.values.data, ctx.nx, ctx.ny, ctx.nz, ctx.nxyz, frame, sx, sy, sz, zeroPad = false)

  private def buildTemplate(run: NeuroVec[Double], reference: ReferenceStrategy, refIndex: Int): Array[Double] =
    val nxyz = run.space.spatialDims.product
    val out = Array.ofDim[Double](nxyz)
    reference match
      case ReferenceStrategy.RobustMean =>
        var lin = 0
        while lin < nxyz do
          var t = 0
          var sum = 0.0
          while t < run.nVolumes do
            sum += run.values.data(lin + t * nxyz)
            t += 1
          out(lin) = sum / run.nVolumes.toDouble
          lin += 1
      case _ =>
        var lin = 0
        while lin < nxyz do
          out(lin) = run.values.data(lin + refIndex * nxyz)
          lin += 1
    out

  private def refreshTemplate(
      ctx: EstimatorContext,
      initial: Array[Double],
      estimate: MotionEstimate
  ): Option[Array[Double]] =
    if !ctx.control.template.robustTemplate && !ctx.control.template.refreshValidOnly then None
    else
      val included = refreshedFrameMask(ctx, estimate)
      if ctx.run.nVolumes < 3 || countIncluded(included) == 0 then None
      else
        val out = Array.ofDim[Double](ctx.nxyz)
        val counts = Array.ofDim[Int](ctx.nxyz)
        val maps = Array.ofDim[MotionSampling.VoxelMap](ctx.run.nVolumes)
        var t = 0
        while t < ctx.run.nVolumes do
          if included(t) then
            maps(t) = MotionSampling.voxelMap(
              ctx.nx,
              ctx.ny,
              ctx.nz,
              zpad = 0,
              ctx.px,
              ctx.py,
              ctx.pz,
              estimate.trace.unsafeFrame(t)
            )
          t += 1

        t = 0
        while t < ctx.run.nVolumes do
          if included(t) then
            val map = maps(t)
            var lin = 0
            while lin < ctx.nxyz do
              val i = lin % ctx.nx
              val j = (lin / ctx.nx) % ctx.ny
              val k = lin / (ctx.nx * ctx.ny)
              val sx = MotionSampling.sourceX(map, i, j, k)
              val sy = MotionSampling.sourceY(map, i, j, k)
              val sz = MotionSampling.sourceZ(map, i, j, k)
              if inBounds(ctx, sx, sy, sz) then
                out(lin) += MotionSampling.trilinear(
                  ctx.run.values.data,
                  ctx.nx,
                  ctx.ny,
                  ctx.nz,
                  ctx.nxyz,
                  t,
                  sx,
                  sy,
                  sz,
                  zeroPad = false
                )
                counts(lin) += 1
              lin += 1
          t += 1

        var lin = 0
        while lin < ctx.nxyz do
          if counts(lin) > 0 then out(lin) /= counts(lin).toDouble
          else out(lin) = initial(lin)
          lin += 1
        Some(out)

  private def refreshedFrameMask(ctx: EstimatorContext, estimate: MotionEstimate): Array[Boolean] =
    val out = Array.fill(ctx.run.nVolumes)(true)
    if ctx.control.template.refreshValidOnly then
      var t = 0
      while t < out.length do
        val d = estimate.diagnostics(t)
        out(t) = d.converged && d.costFinal.isFinite && d.overlap >= 0.5
        t += 1

    if ctx.control.template.robustTemplate then
      val costs = Vector.newBuilder[Double]
      var t = 0
      while t < out.length do
        val c = estimate.diagnostics(t).costFinal
        if out(t) && c.isFinite then costs += c
        t += 1
      val finite = costs.result().sorted
      if finite.nonEmpty then
        val med = MotionMetrics.quantileSorted(finite, 0.5)
        val deviations = finite.map(c => math.abs(c - med)).sorted
        val mad = MotionMetrics.quantileSorted(deviations, 0.5)
        val threshold = med + 3.0 * math.max(mad, 1e-12)
        t = 0
        while t < out.length do
          out(t) = out(t) && estimate.diagnostics(t).costFinal <= threshold
          t += 1

    if !out.exists(identity) then out(ctx.referenceIndex) = true
    out

  private def countIncluded(included: Array[Boolean]): Int =
    var n = 0
    var i = 0
    while i < included.length do
      if included(i) then n += 1
      i += 1
    n

  private def referenceIndex(run: NeuroVec[Double], strategy: ReferenceStrategy): Either[MotionError, Int] =
    strategy match
      case ReferenceStrategy.Middle | ReferenceStrategy.RobustMean =>
        Right(run.nVolumes / 2)
      case ReferenceStrategy.Frame(index) =>
        val i = index.value
        if i >= 0 && i < run.nVolumes then Right(i)
        else Left(MotionError.FrameIndexOutOfBounds(i, run.nVolumes))

  private def buildPyramidLevels(
      run: NeuroVec[Double],
      mask: Option[NeuroVol[Boolean]],
      control: MotionControl
  ): Either[MotionError, Vector[PyramidLevel]] =
    buildCandidateSamples(run, mask, control).map { all =>
      val pyramid = control.pyramid
      if pyramid.enabled then
        val levels = Vector.newBuilder[PyramidLevel]
        var i = 0
        while i < pyramid.downsample.length do
          val stride = pyramid.downsample(i)
          val coarse = strideSamples(all, stride)
          val candidates = if coarse.isEmpty then all else coarse
          val samples = subsample(candidates, scheduleAt(pyramid.sampleCounts, i))
          levels += PyramidLevel(stride, scheduleAt(pyramid.maxIterations, i), samples)
          i += 1
        levels.result()
      else
        Vector(
          PyramidLevel(
            downsample = 1,
            maxIterations = pyramid.maxIterations.lastOption.getOrElse(12),
            samples = subsample(all, pyramid.sampleCounts.lastOption.getOrElse(all.length))
          )
        )
    }

  private def buildCandidateSamples(
      run: NeuroVec[Double],
      mask: Option[NeuroVol[Boolean]],
      control: MotionControl
  ): Either[MotionError, Vector[SamplePoint]] =
    val dims = run.space.spatialDims
    val nx = dims(0)
    val ny = dims(1)
    val nz = dims(2)
    val edgeX = math.floor(nx.toDouble * control.template.edgeExcludeFraction).toInt
    val edgeY = math.floor(ny.toDouble * control.template.edgeExcludeFraction).toInt
    val edgeZ = math.floor(nz.toDouble * control.template.edgeExcludeFraction).toInt
    val builder = Vector.newBuilder[SamplePoint]
    var k = 0
    while k < nz do
      var j = 0
      while j < ny do
        var i = 0
        while i < nx do
          val lin = i + nx * (j + ny * k)
          val inMask = mask.forall(_.values.data(lin))
          val inInterior =
            i >= edgeX && i < nx - edgeX &&
              j >= edgeY && j < ny - edgeY &&
              k >= edgeZ && k < nz - edgeZ
          if inMask && inInterior then builder += SamplePoint(i, j, k, lin)
          i += 1
        j += 1
      k += 1

    val all = builder.result()
    if all.isEmpty then Left(MotionError.ShapeMismatch("estimation mask", Vector(1), Vector(0)))
    else Right(all)

  private def strideSamples(samples: Vector[SamplePoint], stride: Int): Vector[SamplePoint] =
    if stride <= 1 then samples
    else
      val out = Vector.newBuilder[SamplePoint]
      var i = 0
      while i < samples.length do
        val sample = samples(i)
        if sample.i % stride == 0 && sample.j % stride == 0 && sample.k % stride == 0 then out += sample
        i += 1
      out.result()

  private def scheduleAt(values: Vector[Int], index: Int): Int =
    if values.length == 1 then values.head
    else values(math.min(index, values.length - 1))

  private def subsample(samples: Vector[SamplePoint], target: Int): Vector[SamplePoint] =
    if target <= 0 || target >= samples.length then samples
    else
      val out = Vector.newBuilder[SamplePoint]
      val step = samples.length.toDouble / target.toDouble
      var p = 0
      var last = -1
      while p < target do
        val idx = math.min(samples.length - 1, math.floor(p.toDouble * step).toInt)
        if idx != last then
          out += samples(idx)
          last = idx
        p += 1
      out.result()

  private def validateFiniteRun(run: NeuroVec[Double]): Either[MotionError, Unit] =
    var i = 0
    while i < run.values.data.length do
      if !run.values.data(i).isFinite then return Left(MotionError.NonFiniteData("run", i))
      i += 1
    Right(())

  private def inBounds(ctx: EstimatorContext, x: Double, y: Double, z: Double): Boolean =
    x >= 0.0 && x <= (ctx.nx - 1).toDouble &&
      y >= 0.0 && y <= (ctx.ny - 1).toDouble &&
      z >= 0.0 && z <= (ctx.nz - 1).toDouble

  private def huberLoss(residual: Double, k: Double): Double =
    val a = math.abs(residual)
    if a <= k then 0.5 * residual * residual
    else k * (a - 0.5 * k)

  private def huberWeight(residual: Double, k: Double): Double =
    val a = math.abs(residual)
    if a <= k || a == 0.0 then 1.0 else k / a

  private def addStep(pose: RigidPose, step: Array[Double]): RigidPose =
    RigidPose.unsafe(
      pose.tx + step(0),
      pose.ty + step(1),
      pose.tz + step(2),
      pose.rx + step(3),
      pose.ry + step(4),
      pose.rz + step(5)
    )

  private def addOne(pose: RigidPose, index: Int, delta: Double): RigidPose =
    index match
      case 0 => RigidPose.unsafe(pose.tx + delta, pose.ty, pose.tz, pose.rx, pose.ry, pose.rz)
      case 1 => RigidPose.unsafe(pose.tx, pose.ty + delta, pose.tz, pose.rx, pose.ry, pose.rz)
      case 2 => RigidPose.unsafe(pose.tx, pose.ty, pose.tz + delta, pose.rx, pose.ry, pose.rz)
      case 3 => RigidPose.unsafe(pose.tx, pose.ty, pose.tz, pose.rx + delta, pose.ry, pose.rz)
      case 4 => RigidPose.unsafe(pose.tx, pose.ty, pose.tz, pose.rx, pose.ry + delta, pose.rz)
      case _ => RigidPose.unsafe(pose.tx, pose.ty, pose.tz, pose.rx, pose.ry, pose.rz + delta)

  private def limitStep(step: Array[Double]): Array[Double] =
    val out = step.clone()
    var i = 0
    while i < 3 do
      out(i) = math.max(-1.0, math.min(1.0, out(i)))
      i += 1
    while i < 6 do
      out(i) = math.max(-0.05, math.min(0.05, out(i)))
      i += 1
    out

  private def shrinkLowMotionPose(ctx: EstimatorContext, pose: RigidPose): RigidPose =
    val temporal = ctx.control.temporal
    if temporal.lowMotionPoseShrink && poseStepNorm(poseComponents(pose)) <= temporal.lowMotionThresholdMm then
      RigidPose.unsafe(
        pose.tx * temporal.lowMotionPoseScale,
        pose.ty * temporal.lowMotionPoseScale,
        pose.tz * temporal.lowMotionPoseScale,
        pose.rx * temporal.lowMotionPoseScale,
        pose.ry * temporal.lowMotionPoseScale,
        pose.rz * temporal.lowMotionPoseScale
      )
    else pose

  private def poseComponents(pose: RigidPose): Array[Double] =
    Array(pose.tx, pose.ty, pose.tz, pose.rx, pose.ry, pose.rz)

  private def poseStepNorm(step: Array[Double], radius: Double = 50.0): Double =
    math.sqrt(
      step(0) * step(0) +
        step(1) * step(1) +
        step(2) * step(2) +
        radius * radius * (step(3) * step(3) + step(4) * step(4) + step(5) * step(5))
    )

  private def solve6(aIn: Array[Double], bIn: Array[Double]): Option[Array[Double]] =
    val n = 6
    val a = aIn.clone()
    val b = bIn.clone()
    var i = 0
    while i < n do
      var pivot = i
      var maxAbs = math.abs(a(i * n + i))
      var r = i + 1
      while r < n do
        val value = math.abs(a(r * n + i))
        if value > maxAbs then
          pivot = r
          maxAbs = value
        r += 1
      if maxAbs < 1e-12 || !maxAbs.isFinite then return None
      if pivot != i then
        var c = i
        while c < n do
          val tmp = a(i * n + c)
          a(i * n + c) = a(pivot * n + c)
          a(pivot * n + c) = tmp
          c += 1
        val tb = b(i)
        b(i) = b(pivot)
        b(pivot) = tb

      val diag = a(i * n + i)
      var c = i
      while c < n do
        a(i * n + c) /= diag
        c += 1
      b(i) /= diag

      r = 0
      while r < n do
        if r != i then
          val factor = a(r * n + i)
          if factor != 0.0 then
            c = i
            while c < n do
              a(r * n + c) -= factor * a(i * n + c)
              c += 1
            b(r) -= factor * b(i)
        r += 1
      i += 1

    Some(b)
