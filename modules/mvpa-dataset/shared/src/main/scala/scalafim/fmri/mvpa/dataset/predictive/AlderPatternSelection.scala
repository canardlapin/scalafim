package scalafim.fmri.mvpa.dataset.predictive

import alder.data.{FixedCoverage, Resample4sResampler, Schema}
import alder.kernel.*
import alder.metrics.*
import alder.tune.{GridStrategy, PositiveInt, Search, Space, CrossValidatedResult, FoldEvaluationError}
import cats.Id
import cats.data.EitherT
import cats.kernel.CommutativeMonoid
import gale.backend.Backend.given
import gale.linalg.DMat
import multivar.core.{SemanticSpace, SpaceRole, ValueId, ValueIdentity}
import resample4s.core.{Coverage, DigestAlgorithm, Labels, UnitKey}
import resample4s.designs.FixedPartitions
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.pattern.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}

enum PatternSelectionTask:
  case Regression(targets: AxisRef[String])
  /** Coding is declared and unweighted zero-sum; priors are estimated only
    * from each fit's training rows. Raw targets contain one declared code.
    */
  case Classification(conditions: AxisRef[String], targets: AxisRef[String], contrast: DMat, codes: Vector[Double])
  def targetAxis: AxisRef[String] = this match
    case Regression(axis) => axis
    case Classification(_, axis, _, _) => axis

final case class PatternCandidate(rank: Int, policy: TwoStagePatternFitPolicy):
  require(rank > 0)

trait PatternInnerValidation:
  def build(samples: AxisRef[String]): Either[EvidenceError, ValidationDesign[samples.Id, String, Coverage.ExactOnce]]

enum PatternSelection:
  case Fixed(candidate: PatternCandidate)
  case Nested(candidates: Vector[PatternCandidate], inner: PatternInnerValidation)

/** Planned own-adapter cells; resident admitted rows, provider numerical
  * scratch, objects and cumulative work are separate from this peak bound.
  */
final case class PatternSelectionBudget(maximumLearnerCalls: Long, maximumTrainingCells: Long, maximumRetainedCells: Long):
  require(maximumLearnerCalls > 0 && maximumTrainingCells >= 0 && maximumRetainedCells >= 0)

final case class PatternSelectionWork(plannedLearnerCalls: Long, actualLearnerCalls: Long,
    successfulLearnerCalls: Long, plannedStructuredFits: Long, plannedCovarianceFitCalls: Long,
    retainedCellsUpperBound: Long, route: String)

enum PatternHeldOutPrediction:
  case Continuous(values: Vector[Double])
  case Categorical(posterior: ClassPosterior)

final case class PatternOofRow(stableKey: String, unit: UnitKey, observed: Vector[Double], predicted: PatternHeldOutPrediction)
final case class PatternOuterFit(unit: UnitKey, trainingStableKeys: Vector[String], selected: PatternCandidate,
    model: PatternFittedModel, audit: Audit, innerSearch: Option[CrossValidatedResult[PatternCandidate, ?, RootMeanSquaredError]])
final case class PatternHeldOutAssessment(taskLoss: Double, loss: String, samples: Int,
    regression: Option[RegressionPooledAssessment], classificationAccuracy: Option[Double])
final case class AlderPatternSelectionResult(rows: Vector[PatternOofRow], fits: Vector[PatternOuterFit],
    assessment: PatternHeldOutAssessment, work: PatternSelectionWork,
    validationReceipt: resample4s.core.PlanReceipt)

enum AlderPatternSelectionError:
  case Admission(detail: String)
  case Evidence(error: EvidenceError)
  case Fit(error: TwoStagePatternFitError)
  case Artifact(error: PatternArtifactError)
  case Prediction(error: PatternPredictionError)
  case Search(detail: String)
  case Metric(error: MetricError)
  case InvalidTarget(stableKey: String)
  case MissingClass(key: String)
  case Coverage(stableKey: String)

/** Rich model of one actual Alder fit. No assessment/tuning rows or learner
  * closure are retained. Continuous predictions restore the training target
  * mean; categorical serving uses meanX - A C^T meanH as its neural intercept.
  */
final class PatternFittedModel private[predictive] (
    val fitted: TwoStagePatternFitResult[String, String, String],
    val prediction: PatternPrediction[String, String, String],
    val neuralMean: Vector[Double], val targetMean: Vector[Double],
    val task: PatternSelectionTask, val trainingContentIdentity: String,
    val servingIdentity: String, private val stage: StagePath
) extends Pipe[Array[Double], AlderPatternSelectionError, PatternHeldOutPrediction]:
  def run(input: Array[Double]): Either[Failure[AlderPatternSelectionError], PatternHeldOutPrediction] =
    val result = AxisValues(prediction.factors.neuralAxis, input.toVector)
      .left.map(AlderPatternSelectionError.Artifact.apply).flatMap: values =>
        task match
          case PatternSelectionTask.Regression(_) => prediction.decode(values).left.map(AlderPatternSelectionError.Prediction.apply)
            .map(value => PatternHeldOutPrediction.Continuous(value.values.values.zip(targetMean).map(_ + _)))
          case PatternSelectionTask.Classification(_, _, _, _) => prediction.classify(values).left.map(AlderPatternSelectionError.Prediction.apply)
            .map(PatternHeldOutPrediction.Categorical.apply)
    result.left.map(stage.failure)

/** Actual native Alder search and fit lifecycle; no duplicate tuning engine.
  * Selection minimizes equal-fold mean row task loss, not reconstruction and
  * not pooled inner-row loss. Failed folds invalidate a candidate. The current
  * bounded route materializes centered training X and residuals; it is not an
  * end-to-end matrix-free covariance fit. No full-population refit is implied.
  */
object AlderPatternSelection:
  private type Row[M] = Example[Array[Double], Array[Double], M]
  private final case class PreparedOuter[M](unit: UnitKey, train: NonEmptyData[Use.Train, Row[M]],
      assessment: Data[Use.Test, Row[M]], trainingAxis: AxisRef[String],
      inner: Option[ValidationDesign[?, String, Coverage.ExactOnce]], trainingKeys: Vector[String])

  def crossValidate[S <: SemanticSpace, K, M](rows: AlderMaterializedRows[M],
      design: ValidationDesign[S, K, Coverage.ExactOnce], neural: AxisRef[String], support: SupportGraph,
      task: PatternSelectionTask, selection: PatternSelection, budget: PatternSelectionBudget,
      predictionPolicy: PatternPredictionPolicy = PatternPredictionPolicy.strict
  )(using DigestAlgorithm): Either[AlderPatternSelectionError, AlderPatternSelectionResult] =
    val candidates = selection match
      case PatternSelection.Fixed(candidate) => Vector(candidate)
      case PatternSelection.Nested(values, _) => values
    for
      _ <- NativeAxisMapping.verify(design.samples.descriptor, rows.mapping, rows.mapping.declaredSource)
        .left.map(error => AlderPatternSelectionError.Admission(error.toString))
      _ <- if rows.root.ids == rows.mapping.nativeIds && support.axis == neural.descriptor && candidates.nonEmpty then Right(())
        else Left(AlderPatternSelectionError.Admission("population, neural support or candidate family mismatch"))
      _ <- rows.nativeReadReceipt match
        case Some(read) if read.observationsIdentity.columns != neural.descriptor =>
          Left(AlderPatternSelectionError.Admission("native neural evidence axis does not match the declared fit axis"))
        case Some(read) if (task match
          case PatternSelectionTask.Regression(target) => read.targetsIdentity.columns != target.descriptor
          case _ => false) =>
          Left(AlderPatternSelectionError.Admission("native regression target evidence axis does not match the declared fit axis"))
        case _ => Right(())
      _ <- admitTaskBudget(task, budget)
      _ <- admitTask(task)
      prepared <- prepare(rows, design, selection)
      work <- admitWork(prepared, neural, task, candidates, budget)
      result <- evaluate(rows, design, neural, support, task, candidates, prepared, work, predictionPolicy)
    yield result

  private def admitTaskBudget(task: PatternSelectionTask, budget: PatternSelectionBudget): Either[AlderPatternSelectionError, Unit] =
    val q = BigInt(task.targetAxis.size)
    val classes = task match
      case PatternSelectionTask.Classification(conditions, _, _, _) => BigInt(conditions.size)
      case _ => BigInt(0)
    val cells = 32 * q * q + 32 * classes
    if q * q > Int.MaxValue || cells > budget.maximumTrainingCells then Left(AlderPatternSelectionError.Admission("target validation workspace exceeds budget or Int capacity"))
    else Right(())

  private def admitTask(task: PatternSelectionTask): Either[AlderPatternSelectionError, Unit] = task match
    case PatternSelectionTask.Regression(targets) =>
      if targets.size > 0 then Right(()) else Left(AlderPatternSelectionError.Admission("empty continuous targets"))
    case PatternSelectionTask.Classification(conditions, targets, contrast, codes) =>
      if codes.size != conditions.size || codes.distinct.size != codes.size || codes.exists(!_.isFinite) then Left(AlderPatternSelectionError.Admission("invalid class coding"))
      else for
        prior <- AxisValues(conditions, Vector.fill(conditions.size)(1.0 / conditions.size)).left.map(AlderPatternSelectionError.Artifact.apply)
        _ <- TargetGeometry.categorical(conditions, targets, contrast, prior).left.map(AlderPatternSelectionError.Artifact.apply)
      yield ()

  private def prepare[S <: SemanticSpace, K, M](rows: AlderMaterializedRows[M],
      design: ValidationDesign[S, K, Coverage.ExactOnce], selection: PatternSelection
  ): Either[AlderPatternSelectionError, Vector[PreparedOuter[M]]] =
    design.keys.foldLeft[Either[AlderPatternSelectionError, Vector[PreparedOuter[M]]]](Right(Vector.empty)): (acc, key) =>
      for
        prepared <- acc
        unit <- design.at(key).left.map(AlderPatternSelectionError.Evidence.apply)
        ids = unit.analysis.ordinals.toVector.map(rows.mapping.nativeIds)
        test = unit.assessment.ordinals.toVector.map(rows.mapping.nativeIds)
        split <- rows.fixedHoldout(ids, test, FixedCoverage.DeclaredSubset).left.map(error => AlderPatternSelectionError.Admission(error.toString))
        keys = unit.analysis.ordinals.toVector.map(index => rows.mapping.entriesByOrdinal(index).stableKey)
        axis <- AxisRef.fromStableKeys("pattern-training", SpaceRole.Samples, keys, "alder-fit", split.train.fingerprint.digest, "raw", Vector("native-alder-training"))
          .left.map(AlderPatternSelectionError.Evidence.apply)
        inner <- selection match
          case PatternSelection.Fixed(_) => Right(None)
          case PatternSelection.Nested(_, builder) => builder.build(axis).left.map(AlderPatternSelectionError.Evidence.apply).map(Some(_))
      yield prepared :+ PreparedOuter(unit.key, split.train, split.test.data, axis, inner, keys)

  private def admitWork[M](prepared: Vector[PreparedOuter[M]], neural: AxisRef[String], task: PatternSelectionTask,
      candidates: Vector[PatternCandidate], budget: PatternSelectionBudget
  ): Either[AlderPatternSelectionError, PatternSelectionWork] =
    var calls = BigInt(0)
    var retained = BigInt(0)
    var error: Option[AlderPatternSelectionError] = None
    def check(n: Int, candidate: PatternCandidate): Unit =
      val p = BigInt(neural.size); val q = BigInt(task.targetAxis.size); val r = BigInt(candidate.rank)
      val h = BigInt((candidate.policy.covariance.rank +: candidate.policy.covariance.sensitivityRanks).max)
      val materialized = 32 * BigInt(n) * (p + q) + 16 * (p + h) * h + 32 * p * r + 32 * q * q
      if n < 2 || r > p.min(q) || h > ResidualCovariance.maximumIdentifiableRank(n, neural.size) || q > candidate.policy.structured.maximumTargetDimension then
        error = Some(AlderPatternSelectionError.Admission("candidate rank or target dimension is inadmissible on a training population"))
      else if materialized > budget.maximumTrainingCells || Vector(BigInt(n) * p, BigInt(n) * q, p * r, q * q, (p + h) * h).exists(_ > Int.MaxValue) then
        error = Some(AlderPatternSelectionError.Admission("training materialization budget or Int capacity refusal"))
    prepared.foreach: outer =>
      candidates.foreach(check(outer.trainingAxis.size, _))
      calls += 1
      val largestRank = candidates.map(_.rank).max
      val largestNoise = candidates.map(c => (c.policy.covariance.rank +: c.policy.covariance.sensitivityRanks).max).max
      // Outer retains pilot/final factors, covariance/history, serving heads.
      retained += 128 * (BigInt(neural.size) + task.targetAxis.size) * largestRank + 64 * (BigInt(neural.size) + largestNoise) * largestNoise +
        32 * BigInt(task.targetAxis.size) * task.targetAxis.size + 64 * candidates.map(_.policy.structured.maximumOuterIterations).max +
        candidates.map(c => BigInt(c.policy.covariance.maximumIterations) * (c.policy.covariance.sensitivityRanks.size + 1L)).max +
        (task match
          case PatternSelectionTask.Classification(conditions, _, _, _) => 16 * BigInt(conditions.size)
          case _ => BigInt(0))
      outer.inner.foreach: inner =>
        if inner.samples.descriptor != outer.trainingAxis.descriptor then error = Some(AlderPatternSelectionError.Admission("inner design is not bound to this outer training population"))
        inner.keys.foreach: key =>
          inner.at(key) match
            case Left(value) => error = Some(AlderPatternSelectionError.Evidence(value))
            case Right(unit) =>
              val test = unit.assessment.ordinals.toVector
              val complement = (0 until outer.trainingAxis.size).filterNot(test.toSet).toVector
              if unit.analysis.ordinals.toVector != complement || test != test.sorted then error = Some(AlderPatternSelectionError.Admission("native exact inner partition requires the declared complement; purged/ordered analysis cannot be replaced"))
              candidates.foreach(check(unit.analysis.child.size, _))
        calls += BigInt(candidates.size) * inner.keys.size
    val predictionWidth = task match
      case PatternSelectionTask.Classification(conditions, _, _, _) => 2L * conditions.size + 1L
      case PatternSelectionTask.Regression(target) => 2L * target.size
    retained += prepared.foldLeft(BigInt(0))((sum, outer) => sum + outer.assessment.size) * (predictionWidth + 16L)
    if error.nonEmpty then Left(error.get)
    else if calls > budget.maximumLearnerCalls || calls * 2 > Long.MaxValue || retained > budget.maximumRetainedCells then
      Left(AlderPatternSelectionError.Admission("full fixed/nested fit or retained artifact budget refusal"))
    else Right(PatternSelectionWork(calls.longValue, 0L, 0L, (2 * calls).longValue, calls.longValue,
      retained.longValue, if prepared.exists(_.inner.nonEmpty) then "nested-native-alder-search" else "fixed-hyperparameters"))

  private def evaluate[S <: SemanticSpace, K, M](rows: AlderMaterializedRows[M], design: ValidationDesign[S, K, Coverage.ExactOnce],
      neural: AxisRef[String], support: SupportGraph, task: PatternSelectionTask, candidates: Vector[PatternCandidate],
      prepared: Vector[PreparedOuter[M]], work: PatternSelectionWork, predictionPolicy: PatternPredictionPolicy
  )(using DigestAlgorithm): Either[AlderPatternSelectionError, AlderPatternSelectionResult] =
    given Schema[Array[Double]] with
      val descriptor = "scalafim.pattern-neural-array:" + neural.descriptor.stableKey
      val fingerprint = new SchemaFingerprint(FingerprintPolicy.ContentDigest("scalafim-neural-axis-sha256-v1"),
        AxisDigest.sha256Hex(writer => writer.string(neural.descriptor.coordinateSignature.value)))
    val metric = taskMetric[M](task)
    val oof = Array.fill[Option[PatternOofRow]](design.samples.size)(None)
    val fits = Vector.newBuilder[PatternOuterFit]
    val ordinal: Map[Long, Int] = rows.mapping.nativeIds.zipWithIndex.toMap
    var calls = 0L
    var successful = 0L
    var failure: Option[AlderPatternSelectionError] = None
    def learner(candidate: PatternCandidate) = new PatternLearner[M](rows.mapping, neural, support, task, candidate, predictionPolicy,
      () => calls += 1, () => successful += 1)
    prepared.foreach: outer =>
      if failure.isEmpty then
        val plan = PlanFingerprint(AxisDigest.sha256Hex: writer =>
          writer.string("scalafim.pattern-selection.v2"); writer.string(outer.train.fingerprint.digest)
          writer.string(outer.trainingAxis.descriptor.stableKey); writer.intLE(outer.unit.repeat); writer.intLE(outer.unit.fold)
          writer.string(taskIdentity(task)); writer.intLE(candidates.size)
          candidates.foreach: candidate =>
            writer.intLE(candidate.rank); writer.string(candidate.policy.identity)
          writer.string(support.axis.stableKey); writer.string(support.weightUnits)
          support.topology match
            case SupportTopology.SurfaceTriangles(id) => writer.string("surface"); writer.string(id)
            case SupportTopology.VolumeFaceNeighbours(id) => writer.string("volume"); writer.string(id)
            case SupportTopology.Declared(id) => writer.string("declared"); writer.string(id)
          writer.intLE(support.edges.size)
          support.edges.foreach: edge =>
            writer.intLE(edge.left); writer.intLE(edge.right); writer.string(java.lang.Double.toHexString(edge.weight))
          outer.inner match
            case None => writer.string("fixed")
            case Some(inner) =>
              writer.string("nested"); writer.string(inner.receipt.algorithm.value)
              Vector(inner.receipt.design, inner.receipt.assignment).foreach: digest =>
                writer.string(digest.algorithm.value)
                val bytes = digest.value.toIArray
                writer.intLE(bytes.length); bytes.foreach(byte => writer.intLE(byte & 0xff))
              writer.string(java.lang.Long.toUnsignedString(inner.receipt.seed.value)))
        val context = FitContext.root(Seed(design.receipt.seed.value), plan, SchemaFingerprint("scalafim.pattern-selection.v1"), NumericMode.Deterministic)
        val selected: Either[AlderPatternSelectionError, (PatternCandidate, Option[CrossValidatedResult[PatternCandidate, FoldEvaluationError[Any, Any], RootMeanSquaredError]])] = outer.inner match
          case None => Right((candidates.head, None))
          case Some(inner) =>
            val assignment = inner.keys.zipWithIndex.foldLeft[Either[AlderPatternSelectionError, Array[Int]]](Right(Array.fill(outer.trainingAxis.size)(-1))): (acc, entry) =>
              for
                values <- acc
                unit <- inner.at(entry._1).left.map(AlderPatternSelectionError.Evidence.apply)
              yield
                unit.assessment.ordinals.iterator.foreach(i => values(i) = entry._2)
                values
            val nativeIds = outer.train.data.foldRows(Vector.empty[Long])((out, id, _) => out :+ id.value)
            val result = for
              assigned <- assignment
              labels <- Labels.retained(IArray.unsafeFromArray(assigned)).left.map(error => AlderPatternSelectionError.Admission(error.toString))
              fixed <- FixedPartitions.once(labels).left.map(error => AlderPatternSelectionError.Admission(error.toString))
              resampler <- Resample4sResampler.fromDesignForPopulation[Row[M]](fixed, outer.train.fingerprint, nativeIds).left.map(error => AlderPatternSelectionError.Admission(error.toString))
              search <- Search.crossValidatedGridSync(Space.choice(candidates.head, candidates.tail*), GridStrategy(PositiveInt.one), resampler,
                learner, metric, (score: RootMeanSquaredError) => score.value * score.value, Seed(design.receipt.seed.value), plan).run(outer.train)
                .left.map(error => AlderPatternSelectionError.Search(error.toString))
            yield (search.best, Some(search))
            result
        selected match
          case Left(error) => failure = Some(error)
          case Right((candidate, search)) => learner(candidate).fit(outer.train)(using context).value match
            case Left(error) => failure = Some(error.cause)
            case Right(trained) =>
              fits += PatternOuterFit(outer.unit, outer.trainingKeys, candidate, trained.artifact, trained.audit, search)
              outer.assessment.foreachRow: (id, row) =>
                if failure.isEmpty then trained.artifact.run(row.input) match
                  case Left(error) => failure = Some(error.cause)
                  case Right(predicted) =>
                    val index = ordinal(id.value)
                    val key = rows.mapping.entriesByOrdinal(index).stableKey
                    if oof(index).nonEmpty then failure = Some(AlderPatternSelectionError.Coverage(key))
                    else oof(index) = Some(PatternOofRow(key, outer.unit, row.target.toVector, predicted))
    failure match
      case Some(error) => Left(error)
      case None if oof.exists(_.isEmpty) => Left(AlderPatternSelectionError.Coverage(rows.mapping.entriesByOrdinal(oof.indexWhere(_.isEmpty)).stableKey))
      case None =>
        val complete: Vector[PatternOofRow] = oof.toVector.flatten
        taskMetric[Unit](task).evaluate(complete.map(row => Scored(row.observed.toArray, row.predicted, ())))
          .left.map(AlderPatternSelectionError.Metric.apply).map: score =>
            val loss = score.value * score.value
            val regression = task match
              case PatternSelectionTask.Regression(target) => Some(RegressionAssessment.pooled(Vector.tabulate(target.size): col =>
                complete.map(_.observed(col)) -> complete.map(_.predicted match
                  case PatternHeldOutPrediction.Continuous(values) => values(col)
                  case _ => Double.NaN)))
              case _ => None
            AlderPatternSelectionResult(complete, fits.result(), PatternHeldOutAssessment(loss, lossName(task), complete.size,
              regression, if regression.isEmpty then Some(1.0 - loss) else None),
              work.copy(actualLearnerCalls = calls, successfulLearnerCalls = successful), design.receipt)

  private def lossName(task: PatternSelectionTask): String = task match
    case PatternSelectionTask.Regression(_) => "uniform-coordinate mean squared error"
    case _ => "misclassification fraction"

  /** Reuses Alder's exact RMSE accumulator for scalar sqrt(row loss). No
    * numerical accumulator is reimplemented. Squaring the finished score is
    * the declared row task loss; finite validation remains native Alder's.
    */
  private def taskMetric[M](task: PatternSelectionTask): ObjectiveMetric[Scored[Array[Double], PatternHeldOutPrediction, M], RootMeanSquaredError] =
    val native = RegressionMetrics.rmse[M]
    new ObjectiveMetric[Scored[Array[Double], PatternHeldOutPrediction, M], RootMeanSquaredError]:
      type Acc = native.Acc
      given accumulator: CommutativeMonoid[Acc] = native.accumulator
      val direction = ObjectiveDirection.Minimize
      val descriptor = MetricDescriptor(MetricId("scalafim-pattern-root-task-loss"), MetricVersion("1"),
        AuditValue.record("loss" -> AuditValue.text(lossName(task)), "task-identity" -> AuditValue.text(taskIdentity(task))),
        MetricNumericPolicy.Reproducible, Some(ObjectiveDescriptor(direction, "square-root-row-loss-native-rmse-v1")))
      def auditScore(score: RootMeanSquaredError): AuditValue = AuditValue.decimal(score.value)
      def observe(row: Scored[Array[Double], PatternHeldOutPrediction, M]): Acc =
        val loss = (task, row.prediction) match
          case (PatternSelectionTask.Regression(target), PatternHeldOutPrediction.Continuous(values)) if row.truth.length == target.size && values.size == target.size =>
            row.truth.toVector.zip(values).map((actual, predicted) => math.pow(actual - predicted, 2)).sum / target.size
          case (PatternSelectionTask.Classification(conditions, _, _, codes), PatternHeldOutPrediction.Categorical(posterior)) if row.truth.length == 1 && posterior.keys == conditions.toRecord.stableKeys =>
            val actual = codes.indexOf(row.truth(0))
            if actual < 0 || posterior.probabilities.isEmpty then Double.NaN
            else if posterior.probabilities.indices.maxBy(posterior.probabilities) == actual then 0.0 else 1.0
          case _ => Double.NaN
        native.observe(Scored(0.0, math.sqrt(loss), row.meta))
      def finish(acc: Acc): Either[MetricError, RootMeanSquaredError] = native.finish(acc)

  private final class PatternLearner[M](mapping: NativeAxisMapping, neural: AxisRef[String], support: SupportGraph,
      task: PatternSelectionTask, candidate: PatternCandidate, predictionPolicy: PatternPredictionPolicy,
      attempted: () => Unit, succeeded: () => Unit
  ) extends Learner[Id, Array[Double], Array[Double], M, PatternHeldOutPrediction]:
    type FitError = AlderPatternSelectionError
    type RunError = AlderPatternSelectionError
    type Model = PatternFittedModel
    def fit[U <: Use.Fit](data: NonEmptyData[U, Row[M]])(using context: FitContext): FitResult[Id, FitError, Trained[Model]] =
      attempted()
      fitted(data, context) match
        case Left(error) => EitherT.leftT(context.stagePath.failure(error))
        case Right(model) =>
          succeeded()
          val descriptor = ComponentDescriptor(ComponentId("scalafim.two-stage-pattern"), ComponentVersion("1"),
            AuditValue.record("numerical-identity" -> AuditValue.text(model.prediction.numericalIdentity),
              "training-content" -> AuditValue.text(model.trainingContentIdentity),
              "serving-identity" -> AuditValue.text(model.servingIdentity),
              "residual-receipt" -> AuditValue.text(model.fitted.residualReceipt), "rank" -> AuditValue.integer(candidate.rank.toLong)),
            BackendFingerprint("gale", "pinned", AuditValue.record()))
          EitherT.rightT(context.complete(model, data, descriptor))

    private def fitted[U <: Use.Fit](data: NonEmptyData[U, Row[M]], context: FitContext): Either[FitError, Model] =
      val records = data.data.foldRows(Vector.empty[(Long, Row[M])])((out, id, row) => out :+ (id.value -> row))
      val keys = mapping.entriesByOrdinal.map(entry => entry.nativeId -> entry.stableKey).toMap
      val target = task.targetAxis
      val n = records.size
      val contentIdentity = AxisDigest.sha256Hex: writer =>
        writer.string("pattern-training-content-v1")
        writer.string(taskIdentity(task)); writer.intLE(records.size)
        records.foreach: (id, row) =>
          writer.string(java.lang.Long.toString(id))
          Vector(row.input, row.target).foreach: values =>
            writer.intLE(values.length)
            values.foreach(value => writer.string(java.lang.Double.toHexString(value)))
      if records.exists((id, row) => !keys.contains(id) || row.input.length != neural.size || row.input.exists(!_.isFinite)) then Left(AlderPatternSelectionError.Admission("invalid training neural rows"))
      else
        for
          samples <- AxisRef.fromStableKeys("pattern-training", SpaceRole.Samples, records.map((id, _) => keys(id)), "alder-fit", data.fingerprint.digest, "raw", Vector("native-alder-training")).left.map(AlderPatternSelectionError.Evidence.apply)
          components <- AxisRef.fromStableKeys("pattern-component", SpaceRole.Latent, Vector.tabulate(candidate.rank)(i => s"component-$i"), "structured", "none", "raw", Vector("structured-fit")).left.map(AlderPatternSelectionError.Evidence.apply)
          y <- targetRows(records, task)
          meansX = Vector.tabulate(neural.size)(col => records.map(_._2.input(col) / n).sum)
          meansY = Vector.tabulate(target.size)(col => (0 until n).map(row => y(row, col) / n).sum)
          centeredX = DMat.tabulate(n, neural.size)((row, col) => records(row)._2.input(col) - meansX(col))
          centeredY = DMat.tabulate(n, target.size)((row, col) => y(row, col) - meansY(col))
          geometry <- targetGeometry(task, records)
          observations <- Observations.fromDense(samples, neural, centeredX, ValueIdentity.source(ValueId.unsafe("training-x-" + contentIdentity)), source("pattern-x")).left.map(AlderPatternSelectionError.Evidence.apply)
          responses <- MultiResponse.fromDense(samples, target, centeredY, ValueIdentity.source(ValueId.unsafe("training-y-" + contentIdentity)), source("pattern-y")).left.map(AlderPatternSelectionError.Evidence.apply)
          binding <- TrainingBinding(samples.descriptor, "native-alder-fit", data.fingerprint.digest).left.map(AlderPatternSelectionError.Artifact.apply)
          fit <- TwoStagePatternFit.fit(samples, neural, target, components)(observations, responses, support, geometry,
            CenteringPolicy.CenteredBeforeFit("training-neural-mean:" + contentIdentity, "training-target-mean:" + contentIdentity),
            binding, Vector("native-alder-training:" + data.fingerprint.digest, "training-content:" + contentIdentity), PatternReplay.Repeatable("owned-centered-training:" + contentIdentity), candidate.policy).left.map(AlderPatternSelectionError.Fit.apply)
          prior <- task match
            case PatternSelectionTask.Regression(_) =>
              val sigma = (centeredY.t * centeredY) * (1.0 / n)
              TargetPriorCovariance(target, sigma, ValueIdentity.source(ValueId.unsafe("training-target-prior-" + contentIdentity)), "empirical-centered-training-targets", predictionPolicy).left.map(AlderPatternSelectionError.Prediction.apply).map(Some(_))
            case _ => Right(None)
          // Categorical means are evaluated against the uncentered codebook.
          offset = task match
            case PatternSelectionTask.Regression(_) => meansX
            case _ =>
              val shift = fit.finalFit.factors.neuralByComponent * (fit.finalFit.factors.targetByComponent.t * DMat.dense(target.size, 1, meansY))
              meansX.zipWithIndex.map((value, row) => value - shift(row, 0))
          intercept <- AxisValues(neural, offset).left.map(AlderPatternSelectionError.Artifact.apply)
          old <- fit.finalFit.artifact.toRight(AlderPatternSelectionError.Admission("converged final fit lacks artifact"))
          artifact <- PatternArtifact(fit.finalFit.factors, geometry, CenteringPolicy.ExplicitIntercept(intercept, "training-target-mean:" + contentIdentity),
            old.degenerateTarget, old.residualCovariance, binding, old.trainingLineage, old.diagnostics).left.map(AlderPatternSelectionError.Artifact.apply)
          prediction <- PatternPrediction.fromArtifact(neural, target, components, artifact, fit.covarianceFit.covariance, prior, predictionPolicy).left.map(AlderPatternSelectionError.Prediction.apply)
        yield
          val serving = AxisDigest.sha256Hex: writer =>
            writer.string("pattern-serving-v1"); writer.string(contentIdentity)
            writer.string(taskIdentity(task)); writer.string(prediction.numericalIdentity)
            writer.intLE(meansY.size)
            meansY.foreach(value => writer.string(java.lang.Double.toHexString(value)))
          new PatternFittedModel(fit, prediction, meansX, meansY, task, contentIdentity, serving, context.stagePath)

  private def taskIdentity(task: PatternSelectionTask): String = AxisDigest.sha256Hex: writer =>
    writer.string("pattern-selection-task-v1")
    writer.string(task.targetAxis.descriptor.stableKey)
    task match
      case PatternSelectionTask.Regression(_) => writer.string("regression")
      case PatternSelectionTask.Classification(conditions, _, contrast, codes) =>
        writer.string("classification"); writer.string(conditions.descriptor.stableKey)
        writer.intLE(codes.size); codes.foreach(value => writer.string(java.lang.Double.toHexString(value)))
        writer.intLE(contrast.rows); writer.intLE(contrast.cols)
        var row = 0
        while row < contrast.rows do
          var col = 0
          while col < contrast.cols do
            writer.string(java.lang.Double.toHexString(contrast(row, col)))
            col += 1
          row += 1

  private def targetRows[M](records: Vector[(Long, Row[M])], task: PatternSelectionTask): Either[AlderPatternSelectionError, DMat] = task match
    case PatternSelectionTask.Regression(target) =>
      if records.exists((_, row) => row.target.length != target.size || row.target.exists(!_.isFinite)) then Left(AlderPatternSelectionError.Admission("invalid continuous training target rows"))
      else Right(DMat.tabulate(records.size, target.size)((row, col) => records(row)._2.target(col)))
    case PatternSelectionTask.Classification(_, target, contrast, codes) =>
      if records.exists((_, row) => row.target.length != 1 || !codes.contains(row.target(0))) then Left(AlderPatternSelectionError.Admission("unknown training class code"))
      else Right(DMat.tabulate(records.size, target.size)((row, col) => contrast(codes.indexOf(records(row)._2.target(0)), col)))

  private def targetGeometry[M](task: PatternSelectionTask, records: Vector[(Long, Row[M])]): Either[AlderPatternSelectionError, TargetGeometry] = task match
    case PatternSelectionTask.Regression(target) =>
      AxisValues(target, Vector.fill(target.size)(1.0)).left.map(AlderPatternSelectionError.Artifact.apply).flatMap(unit =>
        TargetGeometry.continuous(target, unit, unit, Vector("all" -> unit)).left.map(AlderPatternSelectionError.Artifact.apply))
    case PatternSelectionTask.Classification(conditions, target, contrast, codes) =>
      val counts = codes.map(code => records.count(_._2.target(0) == code))
      counts.indexOf(0) match
        case missing if missing >= 0 => Left(AlderPatternSelectionError.MissingClass(conditions.toRecord.stableKeys(missing)))
        case _ =>
          for
            priors <- AxisValues(conditions, counts.map(_.toDouble / records.size)).left.map(AlderPatternSelectionError.Artifact.apply)
            geometry <- TargetGeometry.categorical(conditions, target, contrast, priors).left.map(AlderPatternSelectionError.Artifact.apply)
          yield geometry

  private def source(name: String): EvidenceSource =
    val id = SourceId.unsafe(name)
    EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(name + "-root"), id)).toOption.get
