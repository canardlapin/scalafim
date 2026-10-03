package scalafim.fmri.mvpa.pattern

import gale.backend.Backend.given
import gale.linalg.DMat
import multivar.core.SemanticSpace
import scalafim.fmri.mvpa.{AxisDigest, AxisRef, EvidenceIdentity, MultiResponse, Observations}
import scalafim.fmri.mvpa.analysis.*

enum TwoStagePatternFitError:
  case Admission(detail: String)
  case Structured(stage: String, error: StructuredPatternError)
  case Covariance(error: ResidualCovarianceError)
  case Unconverged(stage: String)
  case Resource(error: ObservationProductError)
  case FitAndResource(fit: TwoStagePatternFitError, resource: ObservationProductError)

/** Provider declarations cover all resident source/buffer bytes and peak
  * scratch. The default enforces owned numeric costs and records unknown
  * external costs; it does not assert a process-memory limit. */
final case class TwoStagePatternResources(
    budget: ResourceBudget = ResourceBudget(ResourceLimit.OwnedNumeric(Long.MaxValue), MaterializationPolicy.ForbidSourceCopy),
    route: ObservationProductRoute = ObservationProductRoute.Direct,
    observations: ObservationProviderCosts = ObservationProviderCosts(),
    responseSource: ResourceBound = ResourceBound.Unknown("response source/buffer bytes not declared"),
    responseScratch: ResourceBound = ResourceBound.Unknown("response scratch bytes not declared"),
    numericalScratch: ResourceBound = ResourceBound.Unknown("Gale/Multivar backend scratch bytes not declared"),
    resource: ObservationProductResource = TwoStagePatternResources.borrowed
)
object TwoStagePatternResources:
  /** Caller retains ownership of the evidence; only the adapter scope expires. */
  val borrowed: ObservationProductResource = new ObservationProductResource:
    def acquire(): Either[String, Unit] = Right(())
    def close(): Either[String, Unit] = Right(())

final case class TwoStagePatternResourceReceipt(
    admission: NumericResourceAdmission, observationWork: ObservationProductWork,
    binding: String, route: ObservationProductRoute, structuredFits: Int, covarianceFits: Int,
    structuredWorkspaceCellsUpperBound: Long, retainedFactorCells: Long
)

final case class TwoStagePatternFitPolicy(
    structured: StructuredPatternConfig,
    covariance: ResidualCovarianceFitPolicy,
    maximumResidualCells: Long
):
  require(maximumResidualCells >= 0L)

  private[mvpa] def identity: String = AxisDigest.sha256Hex: writer =>
    writer.string("two-stage-pattern-policy-v1")
    writer.intLE(covariance.rank)
    writer.string(java.lang.Long.toString(maximumResidualCells))
    val config = structured
    Vector(config.penalty.sparsity, config.penalty.supportTv, config.penalty.signedSmoothness, config.ridge,
      config.stationarityTolerance, config.objectiveTolerance, covariance.relativeFloor,
      covariance.tolerance, covariance.diagonalMomentTolerance, covariance.centeringTolerance).foreach(value => writer.string(java.lang.Double.toHexString(value)))
    writer.intLE(config.maximumOuterIterations)
    writer.intLE(config.inner.maxIterations)
    Vector(config.inner.tolerance.absolute, config.inner.tolerance.relative,
      config.inner.stepSafety, config.inner.extrapolation).foreach(value => writer.string(java.lang.Double.toHexString(value)))
    writer.string(java.lang.Long.toString(config.maximumWorkspaceCells))
    writer.string(java.lang.Long.toString(config.maximumOperatorColumns))
    writer.intLE(config.maximumTargetDimension)
    writer.intLE(covariance.maximumIterations)
    writer.intLE(covariance.sensitivityRanks.size)
    covariance.sensitivityRanks.foreach(writer.intLE)
    writer.string(covariance.convergence match
      case ConvergencePolicy.Refuse => "refuse"
      case ConvergencePolicy.Record => "record")

final case class TwoStagePatternFitWork(
    structuredFits: Int, covarianceFitCalls: Int, covarianceModels: Int, residualCells: Long,
    observationCells: Long, targetProjectionCells: Long
)

final case class TwoStagePatternFitResult[N, Q, R](
    pilot: StructuredPatternResult[N, Q, R], covarianceFit: ResidualCovarianceFit[N],
    finalFit: StructuredPatternResult[N, Q, R], residualReceipt: String,
    work: TwoStagePatternFitWork, resources: TwoStagePatternResourceReceipt
)

/** Training-only two-stage fixed-Psi adapter. It first obtains structured
  * factors under identity Psi, fits D + U U^T to the centered training
  * residuals, then reruns the same structured optimizer with that Psi. This
  * is not joint optimization of the mean and covariance. All inputs must
  * already be centered within the declared training scope; the serving
  * intercept and target prior are the learner adapter's responsibility.
  * maximumResidualCells bounds planned live cells at this adapter and the
  * covariance fitter allocation sites, excluding resident evidence, the
  * separately budgeted structured solver, provider scratch and objects.
  */
object TwoStagePatternFit:
  def fit[SK, NK, QK, RK](samples: AxisRef[SK], neural: AxisRef[NK], target: AxisRef[QK], components: AxisRef[RK])(
      observations: Observations[samples.Id, neural.Id], responses: MultiResponse[samples.Id, target.Id],
      support: SupportGraph, geometry: TargetGeometry, centering: CenteringPolicy,
      binding: TrainingBinding, lineage: Vector[String], replay: PatternReplay, policy: TwoStagePatternFitPolicy,
      resources: TwoStagePatternResources = TwoStagePatternResources()
  ): Either[TwoStagePatternFitError, TwoStagePatternFitResult[NK, QK, RK]] =
    val n = samples.size
    val p = neural.size
    val q = target.size
    val r = components.size
    val cells = BigInt(n) * p
    val ranks = policy.covariance.rank +: policy.covariance.sensitivityRanks
    val largest = ranks.max
    // Residual and its covariance-fit copy, projection and one streamed column.
    // Covariance own-code live allocations include every requested sensitivity
    // model and the largest EM/likelihood blocks. Provider scratch is excluded.
    def retained(h: Int): BigInt = 2 * BigInt(p) + BigInt(p) * h + 2 * (BigInt(p) + h) * h + h
    val fixed = 4 * cells + 3 * BigInt(p) + BigInt(n) * largest + largest + BigInt(largest) * p
    val transient = ranks.map: h =>
      val step = 3 * BigInt(n) * h + 3 * BigInt(p) * h + 6 * BigInt(h) * h
      val finish = 3 * BigInt(n) * h + 2 * BigInt(h) * h + p + BigInt(p) * h + (BigInt(p) + h) * h + 2 * (BigInt(p) + h) * math.min(n, ResidualCovariance.fallbackChunk)
      BigInt(p) * h + p + step.max(finish)
    val copies = fixed + ranks.map(retained).sum + transient.max + BigInt(ranks.size) * (policy.covariance.maximumIterations.toLong + 1L) + BigInt(n) * r + BigInt(p) * r + n + p
    val shapesAdmitted = admitAllocationShapes(n, p, q, r, largest)
    val centered = centering match
      case CenteringPolicy.CenteredBeforeFit(_, _) => true
      case _ => false
    val repeatable = replay match
      case PatternReplay.Repeatable(receipt) => receipt.trim.nonEmpty
      case _ => false
    def fitAdmitted(admittedObservations: Observations[samples.Id, neural.Id], admission: NumericResourceAdmission,
        product: PreparedObservationProduct[samples.Id, neural.Id], structuredWorkspace: BigInt,
        retainedFactors: BigInt, resourceBinding: String, psi0: ResidualCovariance[NK]): Either[TwoStagePatternFitError, TwoStagePatternFitResult[NK, QK, RK]] =
      product.fit(StructuredPatternOptimizer.fit(samples, neural, target, components)(admittedObservations, responses, psi0, support, geometry, centering, policy.structured, binding, lineage, replay)).left.map(TwoStagePatternFitError.Structured("pilot", _)).flatMap: pilot =>
        if pilot.stopping != StructuredPatternStopping.Converged || pilot.artifact.isEmpty then Left(TwoStagePatternFitError.Unconverged("pilot"))
        else trainingResiduals(admittedObservations, responses, pilot.factors.neuralByComponent, pilot.factors.targetByComponent).left.map(TwoStagePatternFitError.Admission.apply).flatMap: residual =>
          val receipt = residualIdentity(samples, neural, target, components, observations, responses, pilot, residual, binding, policy)
          product.fit(ResidualCovariance.fit(neural, residual, receipt, binding, policy.covariance)).left.map(TwoStagePatternFitError.Covariance.apply).flatMap: covariance =>
            if !covariance.receipt.converged then Left(TwoStagePatternFitError.Unconverged("covariance"))
            else product.fit(StructuredPatternOptimizer.fit(samples, neural, target, components)(admittedObservations, responses, covariance.covariance, support, geometry, centering, policy.structured, binding, lineage, replay, initial = Some(pilot.factors))).left.map(TwoStagePatternFitError.Structured("final", _)).flatMap: finalFit =>
              if finalFit.stopping != StructuredPatternStopping.Converged || finalFit.artifact.isEmpty then Left(TwoStagePatternFitError.Unconverged("final"))
              else Right(TwoStagePatternFitResult(pilot, covariance, finalFit, receipt, TwoStagePatternFitWork(2, 1, ranks.size, cells.longValue, cells.longValue, n.toLong * r),
                TwoStagePatternResourceReceipt(admission, product.work, resourceBinding, product.route, 2, 1, structuredWorkspace.toLong, retainedFactors.toLong)))

    if observations.sampleAxis != samples.descriptor || responses.sampleAxis != samples.descriptor || observations.neuralAxis != neural.descriptor || responses.featureAxis != target.descriptor then Left(TwoStagePatternFitError.Admission("declared training axes do not match observations/responses"))
    else if binding.declaredSampleAxis != samples.descriptor || !centered then Left(TwoStagePatternFitError.Admission("two-stage fitting requires this declared training axis and CenteredBeforeFit"))
    else if support.axis != neural.descriptor || components.size < 1 || components.size > math.min(p, q) then Left(TwoStagePatternFitError.Admission("support or component rank is incompatible"))
    else if !repeatable then Left(TwoStagePatternFitError.Admission("two-stage fitting requires repeatable training evidence"))
    else if ranks.exists(_ > ResidualCovariance.maximumIdentifiableRank(n, p)) then Left(TwoStagePatternFitError.Admission("noise rank exceeds training identifiable rank"))
    else if cells > policy.maximumResidualCells || copies > policy.maximumResidualCells || shapesAdmitted.isLeft || BigInt(p) + largest > Int.MaxValue then Left(TwoStagePatternFitError.Admission("residual materialization budget or Int capacity exceeded"))
    else
      val replayScope = replay match
        case PatternReplay.Repeatable(receipt) => ObservationReplay.Scoped(binding.source, receipt)
        case PatternReplay.SinglePass => ObservationReplay.OneShot
      val resourceBinding = AxisDigest.sha256Hex: writer =>
        writer.string("two-stage-resource-binding-v1")
        observations.identity.writeFramed(writer)
        responses.identity.writeFramed(writer)
        writer.string(binding.fingerprintDigest)
        writer.string(policy.identity)
      def combine(values: Vector[(String, ResourceBound)]): ResourceBound =
        val unknown = values.collect { case (name, ResourceBound.Unknown(reason)) => s"$name: $reason" }
        val invalid = values.collectFirst:
          case (_, bound @ ResourceBound.Known(bytes, receipt)) if bytes < 0 || receipt.trim.isEmpty => bound
          case (_, bound @ ResourceBound.Unknown(reason)) if reason.trim.isEmpty => bound
        if invalid.nonEmpty then invalid.get
        else if unknown.nonEmpty then ResourceBound.Unknown(unknown.mkString("; "))
        else ResourceBound.Known(values.collect { case (_, ResourceBound.Known(bytes, _)) => bytes }.sum,
          values.collect { case (name, ResourceBound.Known(_, receipt)) => s"$name=$receipt" }.mkString("; "))
      val h = BigInt(math.max(1, largest))
      val stored = 2 * BigInt(p) + BigInt(p) * h + 2 * (BigInt(p) + h) * h + h
      val precision = stored + 4 * (BigInt(p) + h) * r + BigInt(p) * r
      val structuredWorkspace = BigInt(128) * p * (r + 1L) + BigInt(64) * n * math.max(q, r) +
        BigInt(64) * q * q + BigInt(64) * support.edges.size + BigInt(16) * policy.structured.maximumOuterIterations + precision
      // Pilot + final factors remain live in the returned result. Covariance
      // models/history are already included in copies. Charge peak structured
      // workspace once; operation counts add across the two fits.
      val retainedFactors = 2 * (BigInt(p) * r + BigInt(q) * r + p)
      val productCosts = resources.observations.copy(applicationBuffers = ResourceBound.Known(
        8 * (BigInt(n) + p) * math.max(q, r), "two-stage admitted maximum RHS width and returned matrices"))
      val preflight =
        if q > policy.structured.maximumTargetDimension || structuredWorkspace > policy.structured.maximumWorkspaceCells ||
            BigInt(n) * q > Int.MaxValue || BigInt(q) * q > Int.MaxValue then
          Left(ObservationProductError.InvalidShape("structured target/workspace admission exceeded before source preparation"))
        else ObservationProduct.preflight(observations, replayScope, productCosts,
          resources.route, resources.budget, resourceBinding)
      val admitted = preflight.flatMap: product =>
          val footprint = product.footprint.copy(
            source = combine(Vector("observations" -> resources.observations.source, "responses" -> resources.responseSource)),
            workerScratch = combine(Vector("observations/application" -> product.footprint.workerScratch,
              "responses" -> resources.responseScratch, "numerical backend" -> resources.numericalScratch)),
            ownedWorkerBytes = product.footprint.ownedWorkerBytes + (copies + structuredWorkspace) * 8,
            retainedOutputBytes = (retainedFactors + BigInt(64) * policy.structured.maximumOuterIterations) * 8
          )
          ResourceAdmission.evaluateFootprint(footprint, product.sourceCopyBytes, resources.budget)
            .left.map(ObservationProductError.Resource.apply)
      admitted.left.map(TwoStagePatternFitError.Resource.apply).flatMap: admission =>
        ResidualCovariance.fromFactors(neural, Vector.fill(p)(1.0), DMat.zeros(p, 1))
          .left.map(TwoStagePatternFitError.Covariance.apply).flatMap: psi0 =>
            StructuredPatternOptimizer.admit(samples, neural, target, components)(observations, responses, psi0,
              support, geometry, centering, policy.structured, binding, lineage, replay)
              .left.map(TwoStagePatternFitError.Structured("pilot admission", _)).flatMap: _ =>
                var fitFailure: Option[TwoStagePatternFitError] = None
                val executed = ObservationProduct.withPrepared(samples, neural, observations, replayScope, resources.resource,
                  productCosts, resources.route, resources.budget, resourceBinding): product =>
                    fitAdmitted(product.observations, admission, product, structuredWorkspace, retainedFactors, resourceBinding, psi0)
                      .left.map: error =>
                        fitFailure = Some(error)
                        ObservationProductError.TaskFailure(error.toString)
                executed.left.map: error =>
                  (fitFailure, error) match
                    case (Some(fit), _: ObservationProductError.TaskAndCloseFailure) => TwoStagePatternFitError.FitAndResource(fit, error)
                    case (Some(fit), _: ObservationProductError.TaskFailure) => fit
                    case _ => TwoStagePatternFitError.Resource(error)



  /** Shape-only check includes the final noise-rank precision products. No
    * large fixtures or model allocation are needed to test these boundaries. */
  private[pattern] def admitAllocationShapes(n: Int, p: Int, q: Int, r: Int, noiseRank: Int): Either[TwoStagePatternFitError, Unit] =
    val h = math.max(1, noiseRank)
    val shapes = Vector(BigInt(n) * p, BigInt(n) * q, BigInt(q) * q,
      BigInt(p) * (r + 1L), BigInt(n) * r, BigInt(p) * r,
      BigInt(n) * h, BigInt(p) * h, (BigInt(p) + h) * h,
      (BigInt(p) + h) * r, (BigInt(p) + h) * math.min(n, ResidualCovariance.fallbackChunk), BigInt(h) * h)
    if Vector(n, p, q, r, noiseRank).exists(_ < 0) || BigInt(p) + h > Int.MaxValue || shapes.exists(_ > Int.MaxValue) then
      Left(TwoStagePatternFitError.Admission("numeric allocation shape exceeds Int capacity"))
    else Right(())

  /** Stream X one neural column at a time and project Y directly through C.
    * This helper is used only after fit's shape/replay/workspace admission.
    */
  private[pattern] def trainingResiduals[S <: SemanticSpace, N <: SemanticSpace, Q <: SemanticSpace](
      x: Observations[S, N], y: MultiResponse[S, Q], a: DMat, c: DMat
  ): Either[String, DMat] =
    if a.rows != x.neuralAxis.size || c.rows != y.featureAxis.size || a.cols != c.cols || x.sampleAxis != y.sampleAxis then Left("residual factor/axis shape mismatch")
    else y.targets(c).left.map(_.toString).flatMap: scores =>
      val n = x.sampleAxis.size
      val p = x.neuralAxis.size
      val data = new Array[Double](n * p)
      var column = 0
      var failure: Option[String] = None
      while column < p && failure.isEmpty do
        x.patterns(DMat.tabulate(p, 1)((row, _) => if row == column then 1.0 else 0.0)) match
          case Left(error) => failure = Some(error.toString)
          case Right(values) =>
            var row = 0
            while row < n && failure.isEmpty do
              var prediction = 0.0
              var component = 0
              while component < a.cols do
                prediction += scores(row, component) * a(column, component)
                component += 1
              val residual = values(row, 0) - prediction
              if !residual.isFinite then failure = Some("nonfinite training residual")
              else data(row * p + column) = residual
              row += 1
        column += 1
      failure match
        case Some(error) => Left(error)
        case None => Right(DMat.dense(n, p, data.toIndexedSeq))

  private def residualIdentity[SK, NK, QK, RK](samples: AxisRef[SK], neural: AxisRef[NK], target: AxisRef[QK], components: AxisRef[RK], observations: Observations[samples.Id, neural.Id], responses: MultiResponse[samples.Id, target.Id], pilot: StructuredPatternResult[NK, QK, RK], residual: DMat, binding: TrainingBinding, policy: TwoStagePatternFitPolicy): String =
    AxisDigest.sha256Hex: writer =>
      writer.string("two-stage-pattern-residual-v2")
      writer.string(samples.descriptor.stableKey); writer.string(neural.descriptor.stableKey)
      writer.string(target.descriptor.stableKey); writer.string(components.descriptor.stableKey)
      writer.string(binding.declaredSampleAxis.stableKey); writer.string(binding.source); writer.string(binding.fingerprintDigest)
      writer.intLE(policy.covariance.rank); writer.string(java.lang.Long.toString(policy.maximumResidualCells))
      observations.identity.writeFramed(writer)
      responses.identity.writeFramed(writer)
      Vector(pilot.factors.neuralByComponent, pilot.factors.targetByComponent, residual).foreach: matrix =>
        writer.intLE(matrix.rows); writer.intLE(matrix.cols)
        var row = 0
        while row < matrix.rows do
          var col = 0
          while col < matrix.cols do
            writer.string(java.lang.Double.toHexString(matrix(row, col)))
            col += 1
          row += 1
      writer.string(policy.identity)
