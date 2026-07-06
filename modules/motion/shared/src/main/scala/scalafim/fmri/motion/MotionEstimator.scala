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
        samples <- buildSamples(run, mask, plan.control)
      yield
        val ctx = EstimatorContext(run, plan.control, refIndex, samples)
        val template = buildTemplate(run, plan.reference, refIndex)
        fitRun(ctx, template)

  private final case class SamplePoint(i: Int, j: Int, k: Int, linear: Int)

  private final case class EstimatorContext(
      run: NeuroVec[Double],
      control: MotionControl,
      referenceIndex: Int,
      samples: Vector[SamplePoint]
  ):
    val dims: Vector[Int] = run.space.spatialDims
    val nx: Int = dims(0)
    val ny: Int = dims(1)
    val nz: Int = dims(2)
    val nxyz: Int = nx * ny * nz
    val px: Double = run.space.spacing(0)
    val py: Double = run.space.spacing(1)
    val pz: Double = run.space.spacing(2)

  private final case class CostResult(cost: Double, overlap: Double)

  private final case class FitResult(pose: RigidPose, diagnostics: FrameFitDiagnostics)

  private final case class CaptureStart(pose: RigidPose, warmCost: CostResult, startCost: CostResult)

  private def validateSupportedPlan(plan: MotionPlan): Either[MotionError, Unit] =
    if plan.control.pyramid.enabled then Left(MotionError.NotImplemented("motion pyramid estimator levels"))
    else if plan.control.temporal.regularizationEnabled then Left(MotionError.NotImplemented("temporal regularization"))
    else if plan.control.temporal.lowMotionPoseShrink then Left(MotionError.NotImplemented("low-motion pose shrink"))
    else Right(())

  private def fitRun(ctx: EstimatorContext, template: Array[Double]): MotionEstimate =
    val nt = ctx.run.nVolumes
    val poses = Array.fill(nt)(RigidPose.identity)
    val diagnostics = Array.ofDim[FrameFitDiagnostics](nt)

    val refCost = evaluate(ctx, template, ctx.referenceIndex, RigidPose.identity)
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

    MotionEstimate(
      trace = MotionTrace.unsafe(poses.toVector),
      diagnostics = diagnostics.toVector,
      control = ctx.control
    )

  private def fitFrame(
      ctx: EstimatorContext,
      template: Array[Double],
      frame: Int,
      warmStart: RigidPose
  ): FitResult =
    val capture = captureStart(ctx, template, frame, warmStart)
    var pose = capture.pose
    var current = capture.startCost
    var lambda = math.max(1e-6, ctx.control.optimizer.lambda0)
    val maxIter = ctx.control.pyramid.maxIterations.lastOption.getOrElse(12)
    var iter = 0
    var converged = false

    while iter < maxIter && !converged do
      val system = buildSystem(ctx, template, frame, pose, lambda)
      solve6(system.hessian, system.gradient.map(-_)) match
        case None =>
          converged = true
        case Some(rawStep) =>
          val step = limitStep(rawStep)
          val stepNorm = poseStepNorm(step)
          if stepNorm <= ctx.control.optimizer.stepTolerance then converged = true
          else
            var accepted = false
            var attempt = 0
            var trialStep = step
            while attempt < 6 && !accepted do
              val trialPose = addStep(pose, trialStep)
              val trialCost = evaluate(ctx, template, frame, trialPose)
              if trialCost.cost < current.cost then
                val relDrop =
                  if current.cost == 0.0 then current.cost - trialCost.cost
                  else (current.cost - trialCost.cost) / math.max(1e-12, math.abs(current.cost))
                pose = trialPose
                current = trialCost
                lambda = math.max(1e-8, lambda * 0.5)
                accepted = true
                if relDrop <= ctx.control.optimizer.costTolerance then converged = true
              else
                lambda *= 4.0
                trialStep = trialStep.map(_ * 0.5)
                attempt += 1
            if !accepted then converged = true
      iter += 1

    FitResult(
      pose = pose,
      diagnostics = FrameFitDiagnostics(
        costInitial = capture.warmCost.cost,
        costFinal = current.cost,
        iterations = iter,
        overlap = current.overlap,
        restarted = capture.startCost.cost < capture.warmCost.cost,
        converged = converged
      )
    )

  private final case class NormalSystem(hessian: Array[Double], gradient: Array[Double])

  private def buildSystem(
      ctx: EstimatorContext,
      template: Array[Double],
      frame: Int,
      pose: RigidPose,
      lambda: Double
  ): NormalSystem =
    val h = Array.fill(36)(0.0)
    val g = Array.fill(6)(0.0)
    val eps = Array(1e-2, 1e-2, 1e-2, 1e-4, 1e-4, 1e-4)
    val deriv = Array.ofDim[Double](6)

    val map = MotionSampling.voxelMap(ctx.nx, ctx.ny, ctx.nz, zpad = 0, ctx.px, ctx.py, ctx.pz, pose)
    var s = 0
    while s < ctx.samples.length do
      val sample = ctx.samples(s)
      val sx = MotionSampling.sourceX(map, sample.i, sample.j, sample.k)
      val sy = MotionSampling.sourceY(map, sample.i, sample.j, sample.k)
      val sz = MotionSampling.sourceZ(map, sample.i, sample.j, sample.k)
      if inBounds(ctx, sx, sy, sz) then
        val fitted = MotionSampling.trilinear(ctx.run.values.data, ctx.nx, ctx.ny, ctx.nz, ctx.nxyz, frame, sx, sy, sz, zeroPad = false)
        val residual = fitted - template(sample.linear)
        val weight = huberWeight(residual, ctx.control.optimizer.huberK)
        var p = 0
        while p < 6 do
          val plus = addOne(pose, p, eps(p))
          val minus = addOne(pose, p, -eps(p))
          val fPlus = sampleAt(ctx, frame, plus, sample)
          val fMinus = sampleAt(ctx, frame, minus, sample)
          deriv(p) = (fPlus - fMinus) / (2.0 * eps(p))
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
      template: Array[Double],
      frame: Int,
      warmStart: RigidPose
  ): CaptureStart =
    val warmCost = evaluate(ctx, template, frame, warmStart)
    if !ctx.control.capture.enabled then CaptureStart(warmStart, warmCost, warmCost)
    else
      val step = math.min(1.0, math.max(ctx.px, math.max(ctx.py, ctx.pz)))
      val maxTx = math.min(ctx.control.capture.translationHalfWidthMm, step)
      val offsets = Array(-maxTx, 0.0, maxTx)
      var best = warmStart
      var bestCost = warmCost
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
            val cost = evaluate(ctx, template, frame, candidate)
            if cost.cost < bestCost.cost then
              best = candidate
              bestCost = cost
            iz += 1
          iy += 1
        ix += 1
      CaptureStart(best, warmCost, bestCost)

  private def evaluate(
      ctx: EstimatorContext,
      template: Array[Double],
      frame: Int,
      pose: RigidPose
  ): CostResult =
    val map = MotionSampling.voxelMap(ctx.nx, ctx.ny, ctx.nz, zpad = 0, ctx.px, ctx.py, ctx.pz, pose)
    var s = 0
    var loss = 0.0
    var n = 0
    var inside = 0
    while s < ctx.samples.length do
      val sample = ctx.samples(s)
      val sx = MotionSampling.sourceX(map, sample.i, sample.j, sample.k)
      val sy = MotionSampling.sourceY(map, sample.i, sample.j, sample.k)
      val sz = MotionSampling.sourceZ(map, sample.i, sample.j, sample.k)
      if inBounds(ctx, sx, sy, sz) then
        inside += 1
        val fitted = MotionSampling.trilinear(ctx.run.values.data, ctx.nx, ctx.ny, ctx.nz, ctx.nxyz, frame, sx, sy, sz, zeroPad = false)
        val residual = fitted - template(sample.linear)
        loss += huberLoss(residual, ctx.control.optimizer.huberK)
        n += 1
      s += 1

    if n == 0 then CostResult(Double.PositiveInfinity, 0.0)
    else CostResult(loss / n.toDouble, inside.toDouble / ctx.samples.length.toDouble)

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

  private def referenceIndex(run: NeuroVec[Double], strategy: ReferenceStrategy): Either[MotionError, Int] =
    strategy match
      case ReferenceStrategy.Middle | ReferenceStrategy.RobustMean =>
        Right(run.nVolumes / 2)
      case ReferenceStrategy.Frame(index) =>
        val i = index.value
        if i >= 0 && i < run.nVolumes then Right(i)
        else Left(MotionError.FrameIndexOutOfBounds(i, run.nVolumes))

  private def buildSamples(
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
    else Right(subsample(all, control.pyramid.sampleCounts.lastOption.getOrElse(all.length)))

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
