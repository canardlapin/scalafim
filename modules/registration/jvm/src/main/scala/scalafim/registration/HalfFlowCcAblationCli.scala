package scalafim.registration

import scalafim.image.*
import scalafim.image.io.{ItkAffine, Nifti}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/** JVM admission and ablation runner for the HalfFlow-CC experiment.
  *
  * Commands are deliberately explicit so benchmark orchestration can record
  * the exact operation rather than depending on positional defaults.
  */
object HalfFlowCcAblationCli:
  private sealed trait Work
  private sealed trait Fixed
  private sealed trait Moving

  private enum ScheduleProfile(val id: String):
    case Frozen extends ScheduleProfile("frozen")
    case Budget2x extends ScheduleProfile("budget2x")
    case Softened extends ScheduleProfile("softened")
    case Shrink8 extends ScheduleProfile("shrink8")
    case Export150 extends ScheduleProfile("export150")

  private object ScheduleProfile:
    def parse(value: String): Either[String, ScheduleProfile] =
      ScheduleProfile.values.find(_.id == value).toRight(
        s"unknown schedule profile '$value'; expected ${ScheduleProfile.values.map(_.id).mkString(", ")}"
      )

  private final case class EvaluationMetrics(
      dice: Double,
      globalNcc: Double,
      maskedNcc: Double,
      validFraction: Double
  )

  def main(args: Array[String]): Unit =
    args.toVector match
      case Vector("inspect-affine", path) => inspectAffine(path)
      case Vector("make-affine-oracle", fixed, moving, affine, output) =>
        makeAffineOracle(Paths.get(fixed), Paths.get(moving), Paths.get(affine), Paths.get(output))
      case Vector("score-affine-oracle", expected, actual, valid) =>
        scoreAffineOracle(Paths.get(expected), Paths.get(actual), Paths.get(valid))
      case Vector("run-a", caseId, fixed, moving, fixedMask, movingMask, affine, output) =>
        runLane(
          "A",
          HalfFlowCcAction.SymmetricMidpoint,
          legacyInverseGate = false,
          standardizedCenter = false,
          caseId,
          Paths.get(fixed),
          Paths.get(moving),
          Paths.get(fixedMask),
          Paths.get(movingMask),
          Paths.get(affine),
          Paths.get(output)
        )
      case Vector("run-a-profile", profileText, caseId, fixed, moving, fixedMask, movingMask, affine, output) =>
        val profile = ScheduleProfile.parse(profileText).fold(fail, identity)
        runLane(
          s"A-${profile.id}",
          HalfFlowCcAction.SymmetricMidpoint,
          legacyInverseGate = false,
          standardizedCenter = false,
          caseId,
          Paths.get(fixed),
          Paths.get(moving),
          Paths.get(fixedMask),
          Paths.get(movingMask),
          Paths.get(affine),
          Paths.get(output),
          profile
        )
      case Vector("run-b", caseId, fixed, moving, fixedMask, movingMask, affine, output) =>
        runLane(
          "B",
          HalfFlowCcAction.SymmetricMidpoint,
          legacyInverseGate = true,
          standardizedCenter = false,
          caseId,
          Paths.get(fixed),
          Paths.get(moving),
          Paths.get(fixedMask),
          Paths.get(movingMask),
          Paths.get(affine),
          Paths.get(output)
        )
      case Vector("run-c", caseId, fixed, moving, fixedMask, movingMask, affine, output) =>
        runLane(
          "C",
          HalfFlowCcAction.FixedAnchor,
          legacyInverseGate = false,
          standardizedCenter = false,
          caseId,
          Paths.get(fixed),
          Paths.get(moving),
          Paths.get(fixedMask),
          Paths.get(movingMask),
          Paths.get(affine),
          Paths.get(output)
        )
      case Vector("run-d", caseId, fixed, moving, fixedMask, movingMask, affine, output) =>
        runLane(
          "D",
          HalfFlowCcAction.SymmetricMidpoint,
          legacyInverseGate = false,
          standardizedCenter = true,
          caseId,
          Paths.get(fixed),
          Paths.get(moving),
          Paths.get(fixedMask),
          Paths.get(movingMask),
          Paths.get(affine),
          Paths.get(output)
        )
      case Vector("run-e", caseId, fixed, moving, fixedMask, movingMask, affine, output) =>
        runLaneE(
          caseId,
          Paths.get(fixed),
          Paths.get(moving),
          Paths.get(fixedMask),
          Paths.get(movingMask),
          Paths.get(affine),
          Paths.get(output)
        )
      case _ =>
        Console.err.println(
          "usage: HalfFlowCcAblationCli " +
            "inspect-affine <itk-affine.mat|txt> | " +
            "make-affine-oracle <fixed-ref.nii> <moving-ref.nii> <affine.mat> <output-dir> | " +
            "score-affine-oracle <expected.nii> <actual.nii> <valid-mask.nii> | " +
            "run-a-profile <frozen|budget2x|softened|shrink8|export150> <case-id> <fixed.nii> <moving.nii> <fixed-mask.nii> <moving-mask.nii> <affine.mat> <output-dir> | " +
            "run-{a,b,c,d,e} <case-id> <fixed.nii> <moving.nii> <fixed-mask.nii> <moving-mask.nii> <affine.mat> <output-dir>"
        )
        sys.exit(2)

  private def inspectAffine(pathText: String): Unit =
    val path = Paths.get(pathText)
    ItkAffine.read(path) match
      case Left(error) =>
        Console.err.println(error.message)
        sys.exit(1)
      case Right(transform) =>
        println(s"itk-affine|path=${path.toAbsolutePath.normalize()}|parameters=${transform.parameters.length}|fixed=${transform.fixedParameters.length}")
        printMatrix("stored_lps", transform.storedLps.matrix)
        printMatrix("ants_pullback_ras", transform.antsPullbackRas.matrix)
        printMatrix("inverse_ras", transform.inverseRas.matrix)

  private def makeAffineOracle(fixedPath: Path, movingPath: Path, affinePath: Path, output: Path): Unit =
    Files.createDirectories(output)
    val fixed = Nifti.readHeader(fixedPath).space.spatialSpace
    val moving = Nifti.readHeader(movingPath).space.spatialSpace
    val transform = ItkAffine.read(affinePath).fold(error => fail(error.message), identity)
    val movingGrid = GridSpec.fromSpace(moving)
    val movingValues = NArrayUtil.ofSize[Double](movingGrid.nVoxels)
    var index = 0
    while index < movingGrid.nVoxels do
      movingValues(index) = linearWorldValue(worldAt(movingGrid, index))
      index += 1
    Nifti.writeVol(output.resolve("moving-linear.nii"), NeuroVol.fromLinear(movingValues, moving, "linear-world"))
    writeExpected(output, "ants-pullback", fixed, movingGrid, transform.antsPullbackRas.matrix)
    writeExpected(output, "inverse", fixed, movingGrid, transform.inverseRas.matrix)
    println(s"affine-oracle|output=${output.toAbsolutePath.normalize()}|fixed_voxels=${fixed.dims.take(3).product}|moving_voxels=${movingGrid.nVoxels}")

  private def writeExpected(
      output: Path,
      name: String,
      fixed: NeuroSpace,
      moving: GridSpec,
      matrix: DMat
  ): Unit =
    val fixedGrid = GridSpec.fromSpace(fixed)
    val values = NArrayUtil.ofSize[Double](fixedGrid.nVoxels)
    val valid = NArrayUtil.ofSize[Double](fixedGrid.nVoxels)
    var index = 0
    while index < fixedGrid.nVoxels do
      val sourceWorld = Affine.applyAffine(matrix, worldAt(fixedGrid, index))
      val sourceVoxel = moving.worldToVoxel(sourceWorld).fold(error => fail(error.message), identity)
      val inside =
        sourceVoxel(0) >= 1.0 && sourceVoxel(0) <= moving.shape.x - 2.0 &&
          sourceVoxel(1) >= 1.0 && sourceVoxel(1) <= moving.shape.y - 2.0 &&
          sourceVoxel(2) >= 1.0 && sourceVoxel(2) <= moving.shape.z - 2.0
      values(index) = linearWorldValue(sourceWorld)
      valid(index) = if inside then 1.0 else 0.0
      index += 1
    Nifti.writeVol(output.resolve(s"expected-$name-fixed.nii"), NeuroVol.fromLinear(values, fixed, s"expected-$name"))
    Nifti.writeVol(output.resolve(s"valid-$name-fixed.nii"), NeuroVol.fromLinear(valid, fixed, s"valid-$name"))

  private def scoreAffineOracle(expectedPath: Path, actualPath: Path, validPath: Path): Unit =
    val expected = Nifti.readVol(expectedPath)
    val actual = Nifti.readVol(actualPath)
    val valid = Nifti.readVol(validPath)
    require(expected.space.dims.take(3) == actual.space.dims.take(3), "expected and actual dimensions differ")
    require(expected.space.dims.take(3) == valid.space.dims.take(3), "expected and validity dimensions differ")
    var count = 0L
    var squared = 0.0
    var maximum = 0.0
    var index = 0
    while index < expected.values.data.length do
      if valid.values.data(index) > 0.5 && actual.values.data(index).isFinite then
        val error = math.abs(expected.values.data(index) - actual.values.data(index))
        squared += error * error
        maximum = math.max(maximum, error)
        count += 1
      index += 1
    require(count > 0, "affine oracle has no valid samples")
    println(
      s"affine-oracle-score|expected=${expectedPath.getFileName}|samples=$count|rms=${math.sqrt(squared / count.toDouble)}|max=$maximum"
    )

  private def runLane(
      lane: String,
      action: HalfFlowCcAction,
      legacyInverseGate: Boolean,
      standardizedCenter: Boolean,
      caseId: String,
      fixedPath: Path,
      movingPath: Path,
      fixedMaskPath: Path,
      movingMaskPath: Path,
      affinePath: Path,
      output: Path,
      profile: ScheduleProfile = ScheduleProfile.Frozen
  ): Unit =
    Files.createDirectories(output)
    val started = System.nanoTime()
    val fixedVolume = Nifti.readVol(fixedPath)
    val movingVolume = Nifti.readVol(movingPath)
    val fixedMaskVolume = Nifti.readVol(fixedMaskPath)
    val movingMaskVolume = Nifti.readVol(movingMaskPath)
    require(fixedMaskVolume.space == fixedVolume.space, "fixed mask grid differs from fixed image")
    require(movingMaskVolume.space == movingVolume.space, "moving mask grid differs from moving image")
    val readSeconds = elapsed(started)

    val fixedGrid = GridSpec.fromSpace(fixedVolume.space)
    val movingGrid = GridSpec.fromSpace(movingVolume.space)
    val workFrame = Frame[Work](SpatialDomainId(s"halfflow-cc-$caseId-work"), fixedGrid)
    val fixedFrame = Frame[Fixed](SpatialDomainId(s"halfflow-cc-$caseId-fixed"), fixedGrid)
    val movingFrame = Frame[Moving](SpatialDomainId(s"halfflow-cc-$caseId-moving"), movingGrid)
    val fixedMask = binaryMask(fixedMaskVolume)
    val fixed = RegistrationImage
      .make(fixedFrame, fixedVolume, FieldValidity.Mask(fixedMask))
      .fold(error => fail(error.message), identity)
    val moving = RegistrationImage
      .make(movingFrame, movingVolume)
      .fold(error => fail(error.message), identity)
    val supplied = ItkAffine.read(affinePath).fold(error => fail(error.message), identity).antsPullbackRas
    val initialization = AffineInitializer
      .supplied(fixed, moving, workFrame, supplied)
      .fold(error => fail(error.message), identity)
    val initial = ForwardMidpoint.fromLegacy(initialization.midpoint)
    val initialEvaluation = evaluate(
      "affine",
      fixedVolume,
      movingVolume,
      movingMaskVolume,
      fixedMask,
      initialization.affine.dense.forward,
      output
    )

    val optimizeStarted = System.nanoTime()
    val plan = g5Plan(action, standardizedCenter, profile)
    val optimization =
      if legacyInverseGate then
        HalfFlowCc.optimizeWithLegacyInverseGate(fixed, moving, initialization.midpoint, plan, legacyGuard)
      else HalfFlowCc.optimize(fixed, moving, initial, plan)
    val optimized = optimization.fold(error => fail(error.message), identity)
    val optimizeSeconds = elapsed(optimizeStarted)
    val geometry = ForwardGeometry.accumulated(optimized.state, plan.geometry)._2
    val exportStarted = System.nanoTime()
    val candidate = ForwardMidpointExporter
      .inspect(optimized.state, plan.exportConfig)
      .fold(error => fail(error.message), identity)
    val exported = ForwardMidpointExporter.admit(candidate, plan.exportConfig)
    val exportSeconds = elapsed(exportStarted)
    val finalEvaluation = Some(
      evaluate(
        "nonlinear-diagnostic",
        fixedVolume,
        movingVolume,
        movingMaskVolume,
        fixedMask,
        candidate.transform.forward,
        output
      )
    )
    val totalSeconds = elapsed(started)
    writeAttempts(output.resolve("attempts.tsv"), optimized.diagnostics)
    writeLaneSummary(
      output.resolve("summary.tsv"),
      lane,
      caseId,
      readSeconds,
      optimizeSeconds,
      exportSeconds,
      totalSeconds,
      initialEvaluation,
      finalEvaluation,
      geometry,
      optimized,
      candidate,
      exported
    )
    printLaneSummary(
      lane,
      caseId,
      readSeconds,
      optimizeSeconds,
      exportSeconds,
      totalSeconds,
      initialEvaluation,
      finalEvaluation,
      geometry,
      optimized,
      candidate,
      exported
    )

  /** Frozen multi-window, 3x3 HalfFlow-LM control with only affine estimation
    * replaced by the exact supplied affine required by G5.
    */
  private def runLaneE(
      caseId: String,
      fixedPath: Path,
      movingPath: Path,
      fixedMaskPath: Path,
      movingMaskPath: Path,
      affinePath: Path,
      output: Path
  ): Unit =
    Files.createDirectories(output)
    val started = System.nanoTime()
    val fixedVolume = Nifti.readVol(fixedPath)
    val movingVolume = Nifti.readVol(movingPath)
    val fixedMaskVolume = Nifti.readVol(fixedMaskPath)
    val movingMaskVolume = Nifti.readVol(movingMaskPath)
    require(fixedMaskVolume.space == fixedVolume.space, "fixed mask grid differs from fixed image")
    require(movingMaskVolume.space == movingVolume.space, "moving mask grid differs from moving image")
    val readSeconds = elapsed(started)
    val fixedGrid = GridSpec.fromSpace(fixedVolume.space)
    val movingGrid = GridSpec.fromSpace(movingVolume.space)
    val workFrame = Frame[Work](SpatialDomainId(s"halfflow-lm-$caseId-work"), fixedGrid)
    val fixedFrame = Frame[Fixed](SpatialDomainId(s"halfflow-lm-$caseId-fixed"), fixedGrid)
    val movingFrame = Frame[Moving](SpatialDomainId(s"halfflow-lm-$caseId-moving"), movingGrid)
    val fixedMask = binaryMask(fixedMaskVolume)
    val fixed = RegistrationImage
      .make(fixedFrame, fixedVolume, FieldValidity.Mask(fixedMask))
      .fold(error => fail(error.message), identity)
    val moving = RegistrationImage.make(movingFrame, movingVolume).fold(error => fail(error.message), identity)
    val supplied = ItkAffine.read(affinePath).fold(error => fail(error.message), identity).antsPullbackRas
    val initialization = AffineInitializer
      .supplied(fixed, moving, workFrame, supplied)
      .fold(error => fail(error.message), identity)
    val initialEvaluation = evaluate(
      "affine",
      fixedVolume,
      movingVolume,
      movingMaskVolume,
      fixedMask,
      initialization.affine.dense.forward,
      output
    )
    val optimizeStarted = System.nanoTime()
    val attemptedRegistration = HalfFlowLm.register(fixed, moving, initialization.midpoint, frozenLmPlan)
    val optimizeSeconds = elapsed(optimizeStarted)
    if attemptedRegistration.isLeft then
      val error = attemptedRegistration.swap.toOption.get
      val totalSeconds = elapsed(started)
      val values = Vector[(String, Any)](
        "case" -> caseId,
        "lane" -> "E",
        "status" -> "typed-failure",
        "failure" -> error.message,
        "read_seconds" -> readSeconds,
        "optimize_seconds" -> optimizeSeconds,
        "total_seconds" -> totalSeconds,
        "dice_affine" -> initialEvaluation.dice,
        "global_ncc_affine" -> initialEvaluation.globalNcc,
        "masked_ncc_affine" -> initialEvaluation.maskedNcc
      )
      Files.writeString(
        output.resolve("summary.tsv"),
        "metric\tvalue\n" + values.map((name, value) => s"$name\t$value").mkString("\n") + "\n",
        StandardCharsets.UTF_8
      )
      println(values.map((name, value) => s"$name=$value").mkString("lane-e|", "|", ""))
      return
    val registration = attemptedRegistration.toOption.get
    val finalEvaluation = evaluate(
      "nonlinear-diagnostic",
      fixedVolume,
      movingVolume,
      movingMaskVolume,
      fixedMask,
      registration.transform.forward,
      output
    )
    val totalSeconds = elapsed(started)
    writeFrozenAttempts(output.resolve("attempts.tsv"), registration.diagnostics)
    val values = Vector[(String, Any)](
      "case" -> caseId,
      "lane" -> "E",
      "read_seconds" -> readSeconds,
      "optimize_seconds" -> optimizeSeconds,
      "total_seconds" -> totalSeconds,
      "accepted" -> registration.diagnostics.acceptedSteps,
      "attempted" -> registration.diagnostics.attemptedSteps,
      "dice_affine" -> initialEvaluation.dice,
      "dice_nonlinear" -> finalEvaluation.dice,
      "global_ncc_affine" -> initialEvaluation.globalNcc,
      "global_ncc_nonlinear" -> finalEvaluation.globalNcc,
      "masked_ncc_affine" -> initialEvaluation.maskedNcc,
      "masked_ncc_nonlinear" -> finalEvaluation.maskedNcc,
      "forward_min_jacobian" -> registration.guard.forward.quantiles.minimum.getOrElse(Double.NaN),
      "backward_min_jacobian" -> registration.guard.backward.quantiles.minimum.getOrElse(Double.NaN),
      "state_nonpositive" -> (registration.guard.forward.nonPositive + registration.guard.backward.nonPositive),
      "inverse_max_mm" -> Vector(
        registration.guard.inverse.forwardThenBackward.maximumMm,
        registration.guard.inverse.backwardThenForward.maximumMm
      ).flatten.maxOption.getOrElse(Double.NaN)
    )
    Files.writeString(
      output.resolve("summary.tsv"),
      "metric\tvalue\n" + values.map((name, value) => s"$name\t$value").mkString("\n") + "\n",
      StandardCharsets.UTF_8
    )
    registration.diagnostics.levels.foreach: level =>
      println(
        s"level|shrink=${level.shrink}|initial=${level.initialValue}|final=${level.finalValue}|accepted=${level.trust.acceptedSteps}|attempts=${level.trust.attempts}|termination=${level.termination}"
      )
    println(values.map((name, value) => s"$name=$value").mkString("lane-e|", "|", ""))

  private def evaluate[A, B](
      name: String,
      fixed: NeuroVol[Double],
      moving: NeuroVol[Double],
      movingMask: NeuroVol[Double],
      fixedMask: narr.NArray[Boolean],
      pull: DensePull[A, B],
      output: Path
  ): EvaluationMetrics =
    val warped = DenseFieldKernels.pullScalar(
      moving,
      pull.sourceCoordinates,
      pull.validity,
      FieldValidity.All,
      outside = 0.0
    )
    val warpedMask = DenseFieldKernels.pullScalarNearest(
      movingMask,
      pull.sourceCoordinates,
      pull.validity,
      FieldValidity.All,
      outside = 0.0
    )
    Nifti.writeVol(output.resolve(s"$name-moving.nii"), warped.values)
    Nifti.writeVol(output.resolve(s"$name-moving-mask.nii"), warpedMask.values)
    val evaluationMask = binaryMask(warpedMask.values)
    val valid = warped.valid.values.data
    EvaluationMetrics(
      dice(fixedMask, evaluationMask),
      correlation(fixed, warped.values, valid, None),
      correlation(fixed, warped.values, valid, Some(fixedMask)),
      countTrue(valid).toDouble / valid.length.toDouble
    )

  private def writeAttempts(path: Path, diagnostics: HalfFlowCcDiagnostics): Unit =
    val lines = Vector.newBuilder[String]
    lines += "shrink\tattempt\taccepted\tcurrent_loss\tcandidate_loss\tmaximum_step_mm\tactive_fraction\tedge_active_fraction\tgeometry\tnumerical\tlegacy_inverse_safe\tlegacy_inverse_max_mm\tdamping\tstep_scale\tsquarings"
    diagnostics.levels.foreach { level =>
      level.trace.foreach { attempt =>
        val after = attempt.controlTransitions.lastOption.map(_.after)
        lines += Vector(
          level.shrink,
          attempt.attempt,
          attempt.accepted,
          attempt.currentLoss,
          attempt.candidateLoss,
          attempt.maximumStepMm,
          attempt.activeFraction,
          attempt.edgeActiveFraction,
          attempt.geometry,
          attempt.numerical,
          attempt.legacyInverseSafe.map(_.toString).getOrElse(""),
          attempt.legacyInverseMaximumMm.map(_.toString).getOrElse(""),
          after.map(_.damping).getOrElse(Double.NaN),
          after.map(_.stepScale).getOrElse(Double.NaN),
          after.map(_.squarings).getOrElse(-1)
        ).mkString("\t")
      }
    }
    Files.writeString(path, lines.result().mkString("\n") + "\n", StandardCharsets.UTF_8)

  private def writeFrozenAttempts(path: Path, diagnostics: RegistrationDiagnostics): Unit =
    val lines = Vector.newBuilder[String]
    lines += "shrink\tattempt\taccepted\tcurrent_value\tcandidate_value\trejection\tdamping_before\tdamping_after\tmaximum_velocity_mm\tmaximum_strain\tlocal_safe\taccumulated_safe"
    diagnostics.levels.foreach: level =>
      level.attempts.foreach: attempt =>
        lines += Vector(
          level.shrink,
          attempt.attempt,
          attempt.accepted,
          attempt.currentValue,
          attempt.candidateValue,
          attempt.rejection.map(_.toString).getOrElse(""),
          attempt.dampingBefore,
          attempt.dampingAfter,
          attempt.maximumVelocityMm,
          attempt.maximumStrain,
          attempt.localSafe,
          attempt.accumulatedSafe
        ).mkString("\t")
    Files.writeString(path, lines.result().mkString("\n") + "\n", StandardCharsets.UTF_8)

  private def writeLaneSummary[W, F, M](
      path: Path,
      lane: String,
      caseId: String,
      readSeconds: Double,
      optimizeSeconds: Double,
      exportSeconds: Double,
      totalSeconds: Double,
      initial: EvaluationMetrics,
      after: Option[EvaluationMetrics],
      geometry: Vector[ForwardJacobianReport],
      optimization: HalfFlowCcOptimization[W, F, M],
      candidate: ForwardMidpointExportCandidate[F, M],
      exported: Either[ForwardExportError, ForwardMidpointExport[F, M]]
  ): Unit =
    val values = laneSummaryValues(
      lane,
      caseId,
      readSeconds,
      optimizeSeconds,
      exportSeconds,
      totalSeconds,
      initial,
      after,
      geometry,
      optimization,
      candidate,
      exported
    )
    Files.writeString(
      path,
      "metric\tvalue\n" + values.map((name, value) => s"$name\t$value").mkString("\n") + "\n",
      StandardCharsets.UTF_8
    )

  private def printLaneSummary[W, F, M](
      lane: String,
      caseId: String,
      readSeconds: Double,
      optimizeSeconds: Double,
      exportSeconds: Double,
      totalSeconds: Double,
      initial: EvaluationMetrics,
      after: Option[EvaluationMetrics],
      geometry: Vector[ForwardJacobianReport],
      optimization: HalfFlowCcOptimization[W, F, M],
      candidate: ForwardMidpointExportCandidate[F, M],
      exported: Either[ForwardExportError, ForwardMidpointExport[F, M]]
  ): Unit =
    optimization.diagnostics.levels.foreach { level =>
      println(
        s"level|shrink=${level.shrink}|initial=${level.initialLoss}|final=${level.finalLoss}|accepted=${level.acceptedSteps}|attempts=${level.attempts}|termination=${level.termination}"
      )
    }
    println(
      laneSummaryValues(
        lane,
        caseId,
        readSeconds,
        optimizeSeconds,
        exportSeconds,
        totalSeconds,
        initial,
        after,
        geometry,
        optimization,
        candidate,
        exported
      ).map((name, value) => s"$name=$value").mkString(s"lane-${lane.toLowerCase}|", "|", "")
    )

  private def laneSummaryValues[W, F, M](
      lane: String,
      caseId: String,
      readSeconds: Double,
      optimizeSeconds: Double,
      exportSeconds: Double,
      totalSeconds: Double,
      initial: EvaluationMetrics,
      after: Option[EvaluationMetrics],
      geometry: Vector[ForwardJacobianReport],
      optimization: HalfFlowCcOptimization[W, F, M],
      candidate: ForwardMidpointExportCandidate[F, M],
      exported: Either[ForwardExportError, ForwardMidpointExport[F, M]]
  ): Vector[(String, Any)] =
    Vector[(String, Any)](
      "case" -> caseId,
      "lane" -> lane,
      "read_seconds" -> readSeconds,
      "optimize_seconds" -> optimizeSeconds,
      "export_seconds" -> exportSeconds,
      "total_seconds" -> totalSeconds,
      "accepted" -> optimization.diagnostics.acceptedSteps,
      "attempted" -> optimization.diagnostics.attempts,
      "loss_initial" -> optimization.diagnostics.levels.head.initialLoss,
      "loss_final" -> optimization.diagnostics.levels.last.finalLoss,
      "dice_affine" -> initial.dice,
      "dice_nonlinear" -> after.map(_.dice).getOrElse(Double.NaN),
      "global_ncc_affine" -> initial.globalNcc,
      "global_ncc_nonlinear" -> after.map(_.globalNcc).getOrElse(Double.NaN),
      "masked_ncc_affine" -> initial.maskedNcc,
      "masked_ncc_nonlinear" -> after.map(_.maskedNcc).getOrElse(Double.NaN),
      "valid_fraction_affine" -> initial.validFraction,
      "valid_fraction_nonlinear" -> after.map(_.validFraction).getOrElse(Double.NaN),
      "fixed_arm_min_jacobian" -> geometry.headOption.map(_.minimum).getOrElse(Double.NaN),
      "moving_arm_min_jacobian" -> geometry.drop(1).headOption.map(_.minimum).getOrElse(Double.NaN),
      "state_nonpositive" -> geometry.map(_.nonPositive).sum,
      "export_status" -> exported.fold(_.toString, _ => "verified"),
      "export_inverse_p99_mm" -> maximumExportP99(candidate),
      "export_inverse_max_mm" -> maximumExportError(candidate)
    )

  private def maximumExportP99[F, M](exported: ForwardMidpointExportCandidate[F, M]): Double =
    Vector(
      exported.fixedResidualInverse.forwardThenInverse.p99Mm,
      exported.fixedResidualInverse.inverseThenForward.p99Mm,
      exported.movingResidualInverse.forwardThenInverse.p99Mm,
      exported.movingResidualInverse.inverseThenForward.p99Mm
    ).max

  private def maximumExportError[F, M](exported: ForwardMidpointExportCandidate[F, M]): Double =
    Vector(
      exported.fixedResidualInverse.forwardThenInverse.maximumMm,
      exported.fixedResidualInverse.inverseThenForward.maximumMm,
      exported.movingResidualInverse.forwardThenInverse.maximumMm,
      exported.movingResidualInverse.inverseThenForward.maximumMm
    ).max

  private def binaryMask(volume: NeuroVol[Double]): narr.NArray[Boolean] =
    val out = NArrayUtil.ofSize[Boolean](volume.values.data.length)
    var index = 0
    while index < out.length do
      out(index) = volume.values.data(index).isFinite && volume.values.data(index) > 0.5
      index += 1
    out

  private def dice(left: narr.NArray[Boolean], right: narr.NArray[Boolean]): Double =
    require(left.length == right.length)
    var intersection = 0L
    var total = 0L
    var index = 0
    while index < left.length do
      if left(index) then total += 1
      if right(index) then total += 1
      if left(index) && right(index) then intersection += 1
      index += 1
    if total == 0 then 1.0 else 2.0 * intersection.toDouble / total.toDouble

  private def correlation(
      left: NeuroVol[Double],
      right: NeuroVol[Double],
      valid: narr.NArray[Boolean],
      mask: Option[narr.NArray[Boolean]]
  ): Double =
    require(left.values.data.length == right.values.data.length && valid.length == left.values.data.length)
    var count = 0L
    var sumLeft = 0.0
    var sumRight = 0.0
    var index = 0
    while index < valid.length do
      val include = valid(index) && mask.forall(_(index))
      if include then
        sumLeft += left.values.data(index)
        sumRight += right.values.data(index)
        count += 1
      index += 1
    if count < 2 then Double.NaN
    else
      val meanLeft = sumLeft / count.toDouble
      val meanRight = sumRight / count.toDouble
      var cross = 0.0
      var squareLeft = 0.0
      var squareRight = 0.0
      index = 0
      while index < valid.length do
        val include = valid(index) && mask.forall(_(index))
        if include then
          val dl = left.values.data(index) - meanLeft
          val dr = right.values.data(index) - meanRight
          cross += dl * dr
          squareLeft += dl * dl
          squareRight += dr * dr
        index += 1
      val denominator = math.sqrt(squareLeft * squareRight)
      if denominator > 0.0 then cross / denominator else Double.NaN

  private def countTrue(values: narr.NArray[Boolean]): Long =
    var count = 0L
    var index = 0
    while index < values.length do
      if values(index) then count += 1
      index += 1
    count

  private val legacyGuard = GuardConfig(
    minimumJacobian = 0.05,
    maximumInverseErrorMm = 0.50,
    maximumInverseErrorVox = 0.25,
    minimumValidFraction = 0.80,
    minimumInverseValidFraction = Some(0.60)
  )

  private val frozenLmPlan: HalfFlowPlan =
    HalfFlowPlan
      .make(
        Vector(
          frozenLmLevel(4, 4.0, 8.0, 2.5, 3.0, 10, 30),
          frozenLmLevel(2, 2.0, 5.0, 1.5, 2.0, 8, 24),
          frozenLmLevel(1, 0.0, 3.0, 0.8, 1.0, 6, 18)
        )
      )
      .fold(error => fail(error.message), identity)

  private def frozenLmLevel(
      shrink: Int,
      pyramidSigmaMm: Double,
      featureRadiusMm: Double,
      smoothLengthMm: Double,
      maximumDisplacementMm: Double,
      accepted: Int,
      attempts: Int
  ): HalfFlowLevel =
    val feature = T1FeatureConfig
      .make(
        Vector(1.0, 2.0, 3.0).map(scale => FeatureRadiusMm(scale * featureRadiusMm)),
        minimumValidWindowFraction = 0.60,
        minimumActiveVoxels = 64
      )
      .fold(error => fail(error.message), identity)
    val local = LocalLmConfig
      .make(damping = 1e-2, minimumActiveVoxels = 64, variant = LocalLmVariant.MultiChannel)
      .fold(error => fail(error.message), identity)
    val sobolev = SobolevConfig
      .make(
        SmoothLengthMm(smoothLengthMm),
        power = 2,
        relativeTolerance = 1e-6,
        maximumIterations = 80,
        maximumDisplacementMm = maximumDisplacementMm,
        maximumStrain = 0.25
      )
      .fold(error => fail(error.message), identity)
    val trust = TrustConfig
      .make(targetAcceptedSteps = accepted, maximumAttempts = attempts)
      .fold(error => fail(error.message), identity)
    HalfFlowLevel
      .make(
        shrink,
        pyramidSigmaMm,
        feature,
        local,
        sobolev,
        FlowConfig(maximumInitialDisplacementMm = 0.4, maximumInitialGradient = 0.2),
        legacyGuard,
        trust,
        minimumUsefulVelocityMm = 1e-5
      )
      .fold(error => fail(error.message), identity)

  private def g5Plan(
      action: HalfFlowCcAction,
      standardizedCenter: Boolean,
      profile: ScheduleProfile = ScheduleProfile.Frozen
  ): HalfFlowCcPlan =
    val cc = NeighborhoodCcConfig
      .make(
        radius = VoxelWindowRadius(2, 2, 2),
        minimumSupportFraction = 0.15,
        fullSupportFraction = 0.7,
        minimumVarianceFraction = 1e-7,
        fullVarianceFraction = 1e-5,
        denominatorEpsilonFraction = 1e-7
      )
      .fold(error => fail(error.message), identity)
    val levels = profile match
      case ScheduleProfile.Frozen =>
        Vector(
          level(4, 2.0, cc, smoothSigmaMm = 14.0, maximumStepMm = 1.5, accepted = 10, attempts = 30,
            metric = g5Metric(standardizedCenter, 8.0)),
          level(2, 1.0, cc, smoothSigmaMm = 10.0, maximumStepMm = 1.0, accepted = 8, attempts = 24,
            metric = g5Metric(standardizedCenter, 5.0)),
          level(1, 0.0, cc, smoothSigmaMm = 6.0, maximumStepMm = 0.6, accepted = 6, attempts = 18,
            metric = g5Metric(standardizedCenter, 3.0))
        )
      case ScheduleProfile.Budget2x | ScheduleProfile.Export150 =>
        Vector(
          level(4, 2.0, cc, smoothSigmaMm = 14.0, maximumStepMm = 1.5, accepted = 20, attempts = 60,
            metric = g5Metric(standardizedCenter, 8.0)),
          level(2, 1.0, cc, smoothSigmaMm = 10.0, maximumStepMm = 1.0, accepted = 16, attempts = 48,
            metric = g5Metric(standardizedCenter, 5.0)),
          level(1, 0.0, cc, smoothSigmaMm = 6.0, maximumStepMm = 0.6, accepted = 12, attempts = 36,
            metric = g5Metric(standardizedCenter, 3.0))
        )
      case ScheduleProfile.Softened =>
        Vector(
          level(4, 2.0, cc, smoothSigmaMm = 10.0, maximumStepMm = 1.5, accepted = 20, attempts = 60,
            metric = g5Metric(standardizedCenter, 8.0)),
          level(2, 1.0, cc, smoothSigmaMm = 7.0, maximumStepMm = 1.0, accepted = 16, attempts = 48,
            metric = g5Metric(standardizedCenter, 5.0)),
          level(1, 0.0, cc, smoothSigmaMm = 4.0, maximumStepMm = 0.6, accepted = 12, attempts = 36,
            metric = g5Metric(standardizedCenter, 3.0))
        )
      case ScheduleProfile.Shrink8 =>
        Vector(
          level(8, 4.0, cc, smoothSigmaMm = 18.0, maximumStepMm = 2.0, accepted = 12, attempts = 36,
            metric = g5Metric(standardizedCenter, 12.0)),
          level(4, 2.0, cc, smoothSigmaMm = 14.0, maximumStepMm = 1.5, accepted = 20, attempts = 60,
            metric = g5Metric(standardizedCenter, 8.0)),
          level(2, 1.0, cc, smoothSigmaMm = 10.0, maximumStepMm = 1.0, accepted = 16, attempts = 48,
            metric = g5Metric(standardizedCenter, 5.0)),
          level(1, 0.0, cc, smoothSigmaMm = 6.0, maximumStepMm = 0.6, accepted = 12, attempts = 36,
            metric = g5Metric(standardizedCenter, 3.0))
        )
    val control = HalfFlowCcControlConfig
      .make(
        initialDamping = 1e-4,
        minimumDamping = 1e-8,
        maximumDamping = 1.0,
        maximumObjectiveRetries = 6,
        maximumGeometryRetries = 8,
        maximumIntegrationRetries = 5
      )
      .fold(error => fail(error.message), identity)
    val inverse = ResidualInverseConfig
      .make(
        shrinks = Vector(4, 2, 1),
        iterationsPerLevel = if profile == ScheduleProfile.Export150 then 150 else 50,
        iterationToleranceMm = 1e-5,
        maximumInteriorErrorMm = 0.2,
        maximumInteriorErrorVox = 0.2,
        interiorMargin = 6
      )
      .fold(error => fail(error.message), identity)
    HalfFlowCcPlan
      .make(
        levels,
        supportSigmaMm = 1.5,
        minimumUsefulStepMm = 1e-6,
        maximumIntegrationInverseErrorMm = 0.03,
        control = control,
        exportConfig = inverse,
        action = action
      )
      .fold(error => fail(error.message), identity)

  private def level(
      shrink: Int,
      pyramidSigmaMm: Double,
      cc: NeighborhoodCcConfig,
      smoothSigmaMm: Double,
      maximumStepMm: Double,
      accepted: Int,
      attempts: Int,
      metric: HalfFlowCcMetric
  ): HalfFlowCcLevel =
    HalfFlowCcLevel
      .make(shrink, pyramidSigmaMm, cc, smoothSigmaMm, maximumStepMm, accepted, attempts, metric)
      .fold(error => fail(error.message), identity)

  private def g5Metric(standardizedCenter: Boolean, radiusMm: Double): HalfFlowCcMetric =
    if !standardizedCenter then HalfFlowCcMetric.TrueNeighborhoodCc
    else
      val feature = T1FeatureConfig
        .make(
          Vector(FeatureRadiusMm(radiusMm)),
          minimumValidWindowFraction = 0.60,
          minimumActiveVoxels = 64
        )
        .fold(error => fail(error.message), identity)
      HalfFlowCcMetric.StandardizedCenter(feature)

  private def elapsed(started: Long): Double =
    (System.nanoTime() - started).toDouble / 1e9

  private def worldAt(grid: GridSpec, index: Int): Vector[Double] =
    val x = index % grid.shape.x
    val yz = index / grid.shape.x
    val y = yz % grid.shape.y
    val z = yz / grid.shape.y
    grid.voxelToWorld(Vector(x.toDouble, y.toDouble, z.toDouble))

  private def linearWorldValue(world: Vector[Double]): Double =
    0.125 + 0.001 * world(0) + 0.002 * world(1) + 0.003 * world(2)

  private def fail(message: String): Nothing =
    throw new IllegalArgumentException(message)

  private def printMatrix(name: String, matrix: DMat): Unit =
    var row = 0
    while row < matrix.rows do
      val values = Vector.tabulate(matrix.cols)(column => f"${matrix(row, column)}%.17g")
      println(s"matrix|name=$name|row=$row|values=${values.mkString(",")}")
      row += 1
