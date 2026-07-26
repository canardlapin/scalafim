package scalafim.registration

import narr.NArray
import scalafim.image.*
import scalafim.image.io.Nifti

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/** JVM-only real-data admission runner. Labels are held out from optimization:
  * the fixed mask defines metric support, while the moving mask is used only
  * after registration for nearest-neighbour Dice evaluation.
  */
object HalfFlowEvaluationCli:
  sealed trait Work
  sealed trait Fixed
  sealed trait Moving

  private final case class Config(
      fixed: Path,
      moving: Path,
      fixedMask: Path,
      movingMask: Path,
      output: Path
  )

  def main(args: Array[String]): Unit =
    parse(args.toVector) match
      case Left(message) =>
        Console.err.println(message)
        sys.exit(2)
      case Right(config) =>
        try run(config)
        catch
          case error: Exception =>
            Console.err.println(Option(error.getMessage).getOrElse(error.getClass.getSimpleName))
            error.printStackTrace(Console.err)
            sys.exit(1)

  private def parse(args: Vector[String]): Either[String, Config] =
    if args.length != 5 then
      Left("usage: HalfFlowEvaluationCli <fixed.nii[.gz]> <moving.nii[.gz]> <fixed-mask.nii[.gz]> <moving-mask.nii[.gz]> <output-dir>")
    else
      Right(Config(Paths.get(args(0)), Paths.get(args(1)), Paths.get(args(2)), Paths.get(args(3)), Paths.get(args(4))))

  private def run(config: Config): Unit =
    Files.createDirectories(config.output)
    val totalStarted = System.nanoTime()
    val fixedOriginal = Nifti.readVol(config.fixed)
    val movingOriginal = Nifti.readVol(config.moving)
    val fixedMaskVolume = Nifti.readVol(config.fixedMask)
    val movingMaskVolume = Nifti.readVol(config.movingMask)
    require(fixedMaskVolume.space == fixedOriginal.space, "fixed mask grid differs from fixed image")
    require(movingMaskVolume.space == movingOriginal.space, "moving mask grid differs from moving image")
    val readSeconds = elapsed(totalStarted)

    // Evaluation needs fixed-grid pre-registration images. Optimization does
    // not: its moving image retains the native grid and independent pyramid.
    val resampleStarted = System.nanoTime()
    val movingForEvaluation = Resample.trilinear(movingOriginal, fixedOriginal.space)
    val preregisteredMask = Resample.nearest(movingMaskVolume, fixedOriginal.space)
    val resampleSeconds = elapsed(resampleStarted)

    val grid = GridSpec.fromSpace(fixedOriginal.space)
    val movingGrid = GridSpec.fromSpace(movingOriginal.space)
    val workFrame = Frame[Work](SpatialDomainId("half-flow-evaluation-work"), grid)
    val fixedFrame = Frame[Fixed](SpatialDomainId("half-flow-evaluation-fixed"), grid)
    val movingFrame = Frame[Moving](SpatialDomainId("half-flow-evaluation-moving"), movingGrid)
    val fixedMask = binaryMask(fixedMaskVolume)
    val fixed = RegistrationImage
      .make(fixedFrame, fixedOriginal, FieldValidity.Mask(fixedMask))
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val movingImage = RegistrationImage
      .make(movingFrame, movingOriginal)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

    val affineStarted = System.nanoTime()
    val affine = AffineInitializer
      .estimate(fixed, movingImage, workFrame, affineConfig)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val affineSeconds = elapsed(affineStarted)
    println(s"phase|affine_seconds=$affineSeconds|initial=${affine.diagnostics.initialValue}|final=${affine.diagnostics.finalValue}|overlap=${affine.diagnostics.overlap}")
    printInitialGuard(affine.midpoint, halfFlowPlan.levels.head.shrink)

    val nonlinearStarted = System.nanoTime()
    val registration = HalfFlowLm
      .register(fixed, movingImage, affine.midpoint, halfFlowPlan)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val nonlinearSeconds = elapsed(nonlinearStarted)

    val outputStarted = System.nanoTime()
    val warpedMoving = DenseFieldKernels.pullScalar(
      movingOriginal,
      registration.transform.forward.sourceCoordinates,
      registration.transform.forward.validity,
      FieldValidity.All,
      outside = 0.0
    )
    val warpedMask = DenseFieldKernels.pullScalarNearest(
      movingMaskVolume,
      registration.transform.forward.sourceCoordinates,
      registration.transform.forward.validity,
      FieldValidity.All,
      outside = 0.0
    )
    Nifti.writeVol(config.output.resolve("header-moving.nii"), movingForEvaluation)
    Nifti.writeVol(config.output.resolve("header-mask.nii"), preregisteredMask)
    Nifti.writeVol(config.output.resolve("warped-moving.nii"), warpedMoving.values)
    Nifti.writeVol(config.output.resolve("warped-mask.nii"), warpedMask.values)

    val preMask = binaryMask(preregisteredMask)
    val postMask = binaryMask(warpedMask.values)
    val guard = registration.guard
    val nonlinearMagnitude = fieldDifference(
      affine.affine.dense.forward.sourceCoordinates,
      registration.transform.forward.sourceCoordinates,
      fixedMask
    )
    val metrics = Vector(
      "dice_pre" -> dice(fixedMask, preMask),
      "dice_post" -> dice(fixedMask, postMask),
      "global_ncc_pre" -> correlation(fixedOriginal, movingForEvaluation, None),
      "global_ncc_post" -> correlation(fixedOriginal, warpedMoving.values, None),
      "masked_ncc_pre" -> correlation(fixedOriginal, movingForEvaluation, Some(fixedMask)),
      "masked_ncc_post" -> correlation(fixedOriginal, warpedMoving.values, Some(fixedMask)),
      "com_dist_pre_mm" -> centerOfMassDistance(grid, fixedMask, preMask),
      "com_dist_post_mm" -> centerOfMassDistance(grid, fixedMask, postMask),
      "nonlinear_rms_mm" -> nonlinearMagnitude._1,
      "nonlinear_max_mm" -> nonlinearMagnitude._2
    )
    val outputSeconds = elapsed(outputStarted)
    val totalSeconds = elapsed(totalStarted)
    writeSummary(
      config.output.resolve("summary.csv"),
      metrics,
      affine,
      registration,
      guard,
      readSeconds,
      resampleSeconds,
      affineSeconds,
      nonlinearSeconds,
      outputSeconds,
      totalSeconds
    )
    writeAttempts(config.output.resolve("attempts.csv"), registration.diagnostics)
    printSummary(metrics, affine, registration, guard, readSeconds, resampleSeconds, affineSeconds, nonlinearSeconds, outputSeconds, totalSeconds)

  private val affineConfig: AffineInitializationConfig =
    AffineInitializationConfig
      .make(
        coarseShrink = 4,
        fineShrink = 1,
        smoothingSigmaMm = 2.0,
        maximumSamplesPerImage = 16000,
        rigidSweepsPerLevel = 6,
        affineSweepsPerLevel = 5
      )
      .fold(error => throw new IllegalStateException(error.message), identity)

  private val guardConfig = GuardConfig(
    minimumJacobian = 0.05,
    maximumInverseErrorMm = 0.50,
    maximumInverseErrorVox = 0.25,
    minimumValidFraction = 0.80,
    minimumInverseValidFraction = Some(0.60)
  )

  private val halfFlowPlan: HalfFlowPlan =
    HalfFlowPlan
      .make(
        Vector(
          level(4, 4.0, 8.0, 2.5, 3.0, 10, 30),
          level(2, 2.0, 5.0, 1.5, 2.0, 8, 24),
          level(1, 0.0, 3.0, 0.8, 1.0, 6, 18)
        )
      )
      .fold(error => throw new IllegalStateException(error.message), identity)

  private def level(
      shrink: Int,
      sigmaMm: Double,
      featureRadiusMm: Double,
      smoothLengthMm: Double,
      maximumDisplacementMm: Double,
      acceptedSteps: Int,
      maximumAttempts: Int
  ): HalfFlowLevel =
    val feature = T1FeatureConfig
      .make(
        Vector(1.0, 2.0, 3.0).map(scale => FeatureRadiusMm(scale * featureRadiusMm)),
        minimumValidWindowFraction = 0.60,
        minimumActiveVoxels = 64
      )
      .fold(error => throw new IllegalStateException(error.message), identity)
    val local = LocalLmConfig
      .make(
        damping = 1e-2,
        minimumActiveVoxels = 64,
        variant = LocalLmVariant.MultiChannel
      )
      .fold(error => throw new IllegalStateException(error.message), identity)
    val sobolev = SobolevConfig
      .make(
        SmoothLengthMm(smoothLengthMm),
        power = 2,
        relativeTolerance = 1e-6,
        maximumIterations = 80,
        maximumDisplacementMm = maximumDisplacementMm,
        maximumStrain = 0.25
      )
      .fold(error => throw new IllegalStateException(error.message), identity)
    val trust = TrustConfig
      .make(targetAcceptedSteps = acceptedSteps, maximumAttempts = maximumAttempts)
      .fold(error => throw new IllegalStateException(error.message), identity)
    HalfFlowLevel
      .make(
        shrink,
        sigmaMm,
        feature,
        local,
        sobolev,
        FlowConfig(maximumInitialDisplacementMm = 0.4, maximumInitialGradient = 0.2),
        guardConfig,
        trust,
        minimumUsefulVelocityMm = 1e-5
      )
      .fold(error => throw new IllegalStateException(error.message), identity)

  private def binaryMask(volume: NeuroVol[Double]): NArray[Boolean] =
    val out = NArrayUtil.ofSize[Boolean](volume.values.data.length)
    var index = 0
    while index < out.length do
      out(index) = volume.values.data(index).isFinite && volume.values.data(index) > 0.5
      index += 1
    out

  private def dice(left: NArray[Boolean], right: NArray[Boolean]): Double =
    require(left.length == right.length)
    var intersection = 0L
    var leftCount = 0L
    var rightCount = 0L
    var index = 0
    while index < left.length do
      if left(index) then leftCount += 1
      if right(index) then rightCount += 1
      if left(index) && right(index) then intersection += 1
      index += 1
    if leftCount + rightCount == 0L then 1.0
    else 2.0 * intersection.toDouble / (leftCount + rightCount).toDouble

  private def correlation(
      left: NeuroVol[Double],
      right: NeuroVol[Double],
      mask: Option[NArray[Boolean]]
  ): Double =
    require(left.space == right.space)
    val a = left.values.data
    val b = right.values.data
    var count = 0L
    var sumA = 0.0
    var sumB = 0.0
    var index = 0
    while index < a.length do
      val active = mask.forall(_(index)) && a(index).isFinite && b(index).isFinite
      if active then
        count += 1
        sumA += a(index)
        sumB += b(index)
      index += 1
    val meanA = sumA / count.toDouble
    val meanB = sumB / count.toDouble
    var cross = 0.0
    var squareA = 0.0
    var squareB = 0.0
    index = 0
    while index < a.length do
      val active = mask.forall(_(index)) && a(index).isFinite && b(index).isFinite
      if active then
        val da = a(index) - meanA
        val db = b(index) - meanB
        cross += da * db
        squareA += da * da
        squareB += db * db
      index += 1
    cross / math.sqrt(squareA * squareB)

  private def centerOfMassDistance(
      grid: GridSpec,
      left: NArray[Boolean],
      right: NArray[Boolean]
  ): Double =
    val a = centerOfMass(grid, left)
    val b = centerOfMass(grid, right)
    val dx = a.x - b.x
    val dy = a.y - b.y
    val dz = a.z - b.z
    math.sqrt(dx * dx + dy * dy + dz * dz)

  private def centerOfMass(grid: GridSpec, mask: NArray[Boolean]): WorldPoint =
    var count = 0L
    var sx = 0.0
    var sy = 0.0
    var sz = 0.0
    val nx = grid.shape.x
    val ny = grid.shape.y
    val affine = grid.affine
    var index = 0
    while index < mask.length do
      if mask(index) then
        val x = (index % nx).toDouble
        val yz = index / nx
        val y = (yz % ny).toDouble
        val z = (yz / ny).toDouble
        sx += affine(0, 0) * x + affine(0, 1) * y + affine(0, 2) * z + affine(0, 3)
        sy += affine(1, 0) * x + affine(1, 1) * y + affine(1, 2) * z + affine(1, 3)
        sz += affine(2, 0) * x + affine(2, 1) * y + affine(2, 2) * z + affine(2, 3)
        count += 1
      index += 1
    require(count > 0, "centre-of-mass mask is empty")
    WorldPoint(sx / count.toDouble, sy / count.toDouble, sz / count.toDouble)

  private def fieldDifference(
      affine: DenseVectorField,
      result: DenseVectorField,
      mask: NArray[Boolean]
  ): (Double, Double) =
    val a = affine.values.data
    val b = result.values.data
    val n = affine.grid.nVoxels
    var sum = 0.0
    var maximum = 0.0
    var count = 0L
    var index = 0
    while index < n do
      if mask(index) then
        val dx = b(index) - a(index)
        val dy = b(index + n) - a(index + n)
        val dz = b(index + 2 * n) - a(index + 2 * n)
        val squared = dx * dx + dy * dy + dz * dz
        sum += squared
        maximum = math.max(maximum, math.sqrt(squared))
        count += 1
      index += 1
    (math.sqrt(sum / count.toDouble), maximum)

  private def printInitialGuard(
      midpoint: Midpoint[Work, Fixed, Moving],
      shrink: Int
  ): Unit =
    val coarseGrid = DenseFieldKernels.pyramidGrid(midpoint.work.grid, shrink)
    val work = Frame[Work](midpoint.work.domain, coarseGrid)
    val fixed = Frame[Fixed](
      midpoint.fixed.endpoint.domain,
      DenseFieldKernels.pyramidGrid(midpoint.fixed.endpoint.grid, shrink)
    )
    val moving = Frame[Moving](
      midpoint.moving.endpoint.domain,
      DenseFieldKernels.pyramidGrid(midpoint.moving.endpoint.grid, shrink)
    )
    val regridded = midpoint
      .regrid(work, fixed, moving)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val report = TopologyGuard.evaluateMidpoint(regridded, guardConfig)
    val fixedReport = report.fixed
    val movingReport = report.moving
    println(s"initial-guard|arm=fixed|safe=${fixedReport.safe}|forwardCoverage=${fixedReport.forward.validFraction}|backwardCoverage=${fixedReport.backward.validFraction}|forwardInverseEvaluated=${fixedReport.inverse.forwardThenBackward.evaluated}|backwardInverseEvaluated=${fixedReport.inverse.backwardThenForward.evaluated}|reasons=${fixedReport.reasons.mkString(";")}")
    println(s"initial-guard|arm=moving|safe=${movingReport.safe}|forwardCoverage=${movingReport.forward.validFraction}|backwardCoverage=${movingReport.backward.validFraction}|forwardInverseEvaluated=${movingReport.inverse.forwardThenBackward.evaluated}|backwardInverseEvaluated=${movingReport.inverse.backwardThenForward.evaluated}|reasons=${movingReport.reasons.mkString(";")}")

  private def writeSummary(
      path: Path,
      metrics: Vector[(String, Double)],
      affine: AffineInitializationResult[Work, Fixed, Moving],
      registration: RegistrationResult[Fixed, Moving],
      guard: PairGuardReport,
      readSeconds: Double,
      resampleSeconds: Double,
      affineSeconds: Double,
      nonlinearSeconds: Double,
      outputSeconds: Double,
      totalSeconds: Double
  ): Unit =
    val values = metrics ++ Vector(
      "affine_initial_value" -> affine.diagnostics.initialValue,
      "affine_final_value" -> affine.diagnostics.finalValue,
      "affine_overlap" -> affine.diagnostics.overlap,
      "affine_determinant" -> affine.diagnostics.determinant,
      "affine_evaluations" -> affine.diagnostics.evaluations.toDouble,
      "accepted_steps" -> registration.diagnostics.acceptedSteps.toDouble,
      "attempted_steps" -> registration.diagnostics.attemptedSteps.toDouble,
      "topology_safe" -> (if guard.safe then 1.0 else 0.0),
      "forward_min_jacobian" -> guard.forward.quantiles.minimum.getOrElse(Double.NaN),
      "backward_min_jacobian" -> guard.backward.quantiles.minimum.getOrElse(Double.NaN),
      "forward_nonpositive_jacobians" -> guard.forward.nonPositive.toDouble,
      "backward_nonpositive_jacobians" -> guard.backward.nonPositive.toDouble,
      "forward_inverse_max_mm" -> guard.inverse.forwardThenBackward.maximumMm.getOrElse(Double.NaN),
      "backward_inverse_max_mm" -> guard.inverse.backwardThenForward.maximumMm.getOrElse(Double.NaN),
      "read_seconds" -> readSeconds,
      "resample_seconds" -> resampleSeconds,
      "affine_seconds" -> affineSeconds,
      "nonlinear_seconds" -> nonlinearSeconds,
      "output_seconds" -> outputSeconds,
      "total_seconds" -> totalSeconds
    )
    val text = values.map(_._1).mkString(",") + "\n" + values.map(_._2).mkString(",") + "\n"
    Files.writeString(path, text, StandardCharsets.UTF_8)

  private def writeAttempts(path: Path, diagnostics: RegistrationDiagnostics): Unit =
    val rows = Vector.newBuilder[String]
    rows += "shrink,attempt,accepted,current_value,candidate_value,actual_drop,predicted_drop,gain_ratio,rejection,damping_before,damping_after,maximum_velocity_mm,maximum_strain,local_safe,accumulated_safe"
    diagnostics.levels.foreach: level =>
      level.attempts.foreach: attempt =>
        rows += Vector(
          level.shrink,
          attempt.attempt,
          attempt.accepted,
          attempt.currentValue,
          attempt.candidateValue,
          attempt.actualDrop,
          attempt.predictedDrop,
          attempt.gainRatio,
          attempt.rejection.fold("")(_.toString),
          attempt.dampingBefore,
          attempt.dampingAfter,
          attempt.maximumVelocityMm,
          attempt.maximumStrain,
          attempt.localSafe,
          attempt.accumulatedSafe
        ).mkString(",")
    Files.writeString(path, rows.result().mkString("\n") + "\n", StandardCharsets.UTF_8)

  private def printSummary(
      metrics: Vector[(String, Double)],
      affine: AffineInitializationResult[Work, Fixed, Moving],
      registration: RegistrationResult[Fixed, Moving],
      guard: PairGuardReport,
      readSeconds: Double,
      resampleSeconds: Double,
      affineSeconds: Double,
      nonlinearSeconds: Double,
      outputSeconds: Double,
      totalSeconds: Double
  ): Unit =
    metrics.foreach((name, value) => println(s"metric|$name=$value"))
    println(s"affine|initial=${affine.diagnostics.initialValue}|final=${affine.diagnostics.finalValue}|overlap=${affine.diagnostics.overlap}|determinant=${affine.diagnostics.determinant}|evaluations=${affine.diagnostics.evaluations}")
    registration.diagnostics.levels.foreach: level =>
      println(s"level|shrink=${level.shrink}|initial=${level.initialValue}|final=${level.finalValue}|attempts=${level.trust.attempts}|accepted=${level.trust.acceptedSteps}|termination=${level.termination}")
    println(s"topology|safe=${guard.safe}|forwardMin=${guard.forward.quantiles.minimum}|backwardMin=${guard.backward.quantiles.minimum}|forwardFolds=${guard.forward.nonPositive}|backwardFolds=${guard.backward.nonPositive}|forwardInverse=${guard.inverse.forwardThenBackward.maximumMm}|backwardInverse=${guard.inverse.backwardThenForward.maximumMm}|reasons=${guard.reasons.mkString(";")}")
    println(s"timing|read=$readSeconds|resample=$resampleSeconds|affine=$affineSeconds|nonlinear=$nonlinearSeconds|output=$outputSeconds|total=$totalSeconds")

  private def elapsed(started: Long): Double =
    (System.nanoTime() - started).toDouble / 1e9
