package scalafim.fmri.mvpa.pattern

import gale.linalg.DMat
import multivar.core.SemanticSpace
import resample4s.core.Injection
import scalafim.fmri.mvpa.{AxisDescriptor, AxisDigest, AxisMember, AxisRef, Column, EvidenceIdentity, ReindexingIdentity, ReindexingLeg}

/** Declares the provenance assumption for numbers supplied at the local input
  * boundary. The head retains no observation reader or preprocessing callback.
  */
enum LocalPredictionPreparation:
  case AlreadyPreparedLocalCoordinates(receipt: String)
  case RequiresOutsideMeasurements(reason: String)

enum LocalPredictionError:
  case ForeignParent
  case EmptySelection
  case InvalidPreparation(reason: String)
  case Budget(requiredCells: BigInt, allowedCells: Long)
  case Covariance(error: ResidualCovarianceError)
  case Prediction(error: PatternPredictionError)

final case class LocalPredictionReceipt(
    identity: String, parentPrediction: String, restriction: ReindexingIdentity,
    localAxis: AxisDescriptor, preparation: LocalPredictionPreparation,
    additionalWorkspaceCells: Long, inheritedTaskRank: Int
)

enum LocalAssessmentError:
  case ScopeMismatch(boundary: String)
  case AssessmentReuse(role: String, ordinals: Vector[Int])
  case InvalidReduction
  case InvalidUnits
  case InvalidBudget
  case Budget(requiredCells: BigInt, allowedCells: Long)
  case Prediction(error: PatternPredictionError)
  case NonFiniteLoss(row: Int)

final case class LocalEvaluationUnit(key: String, rows: Int, meanSquaredLoss: Double)
final case class LocalAssessmentResult(
    identity: String, prediction: LocalPredictionReceipt, assessment: ReindexingIdentity,
    targetReduction: Vector[Double], units: Vector[LocalEvaluationUnit], meanSquaredLoss: Double
)

/** Marginal observation prediction under the inherited global task subspace.
  * This neither refits the task model nor measures maximal achievable local
  * information. Its input methods require only the bound local coordinates.
  */
final class LocalPatternPrediction[L, Q, R] private[pattern] (
    val localAxis: AxisRef[L], val targetAxis: AxisRef[Q], val componentAxis: AxisRef[R],
    val receipt: LocalPredictionReceipt, private val heads: PatternPrediction[L, Q, R]
):
  val rawFilters: RawNeuralFilters[L, R] = heads.rawFilters
  val gram: DMat = heads.gram
  val work: PatternPredictionWork = heads.work
  val limitation: String = "Prediction inherits the fitted global task subspace; ROI-only dimensions omitted by that rank are not recovered."
  def rawScores(values: AxisValues[L]): Either[PatternPredictionError, RawComponentScores[R]] = heads.rawScores(values)
  def calibratedScores(values: AxisValues[L]): Either[PatternPredictionError, CalibratedComponentScores[R]] = heads.calibratedScores(values)
  def posteriorScores(values: AxisValues[L]): Either[PatternPredictionError, PosteriorComponentScores[R]] = heads.posteriorScores(values)
  def decode(values: AxisValues[L]): Either[PatternPredictionError, PosteriorTargetMean[Q]] = heads.decode(values)
  def classify(values: AxisValues[L]): Either[PatternPredictionError, ClassPosterior] = heads.classify(values)
  def encode(values: AxisValues[Q]): Either[PatternPredictionError, AxisValues[L]] = heads.encode(values)

  /** Squared loss averages the declared target coordinates within each row,
    * then rows within each identified evaluation unit, then units equally.
    * Scope checks use ordinals in one common parent, never display labels.
    * The training binding and selection/tuning scopes are declarations; this
    * method cannot authenticate unrecorded analyst access to assessment data.
    */
  def assess[P <: SemanticSpace, K](training: ReindexingLeg[P, K, Injection],
      assessment: ReindexingLeg[P, K, Injection],
      usedForSelection: Vector[ReindexingLeg[P, K, Injection]],
      usedForTuning: Vector[ReindexingLeg[P, K, Injection]]
  )(observations: Column[assessment.Child, AxisValues[L]],
      targets: Column[assessment.Child, AxisValues[Q]],
      evaluationUnits: Column[assessment.Child, String], reduction: AxisValues[Q],
      maximumWorkspaceCells: Long
  ): Either[LocalAssessmentError, LocalAssessmentResult] =
    val n = assessment.child.size
    val cells = BigInt(n) * (localAxis.size + targetAxis.size + 8 * BigInt(componentAxis.size) + 16)
    val roles = Vector("training" -> Vector(training), "selection" -> usedForSelection, "tuning" -> usedForTuning)
    if training.child.descriptor != heads.artifact.trainingBinding.declaredSampleAxis then Left(LocalAssessmentError.ScopeMismatch("training binding"))
    else if roles.exists(_._2.exists(_.parentAxis != assessment.parentAxis)) then Left(LocalAssessmentError.ScopeMismatch("scope parent"))
    else if n == 0 || observations.rowAxis != assessment.child.descriptor || targets.rowAxis != assessment.child.descriptor || evaluationUnits.rowAxis != assessment.child.descriptor then Left(LocalAssessmentError.ScopeMismatch("assessment rows"))
    else if maximumWorkspaceCells < 0 then Left(LocalAssessmentError.InvalidBudget)
    else if cells > maximumWorkspaceCells || cells > Int.MaxValue then Left(LocalAssessmentError.Budget(cells, maximumWorkspaceCells))
    else if reduction.axis.descriptor != targetAxis.descriptor || reduction.values.exists(_ < 0.0) || !reduction.values.exists(_ > 0.0) then Left(LocalAssessmentError.InvalidReduction)
    else if evaluationUnits.values.exists(_.trim.isEmpty) then Left(LocalAssessmentError.InvalidUnits)
    else
      val held = assessment.ordinals.iterator.toSet
      val overlap = roles.iterator.flatMap: (role, scopes) =>
        scopes.iterator.map(scope => role -> scope.ordinals.iterator.filter(held).toVector)
      overlap.find(_._2.nonEmpty) match
        case Some((role, ordinals)) => Left(LocalAssessmentError.AssessmentReuse(role, ordinals))
        case None =>
          val scale = reduction.values.max
          val scaled = reduction.values.map(_ / scale)
          val weights = scaled.map(_ / scaled.sum)
          if reduction.values.zip(weights).exists((original, normalized) => original > 0.0 && normalized == 0.0) then return Left(LocalAssessmentError.InvalidReduction)
          val accumulators = scala.collection.mutable.LinkedHashMap.empty[String, (Int, Double)]
          var row = 0
          var failure: Option[LocalAssessmentError] = None
          while row < n && failure.isEmpty do
            val actual = targets.values(row)
            if actual.axis.descriptor != targetAxis.descriptor then failure = Some(LocalAssessmentError.ScopeMismatch("target coordinates"))
            else heads.decode(observations.values(row)) match
              case Left(error) => failure = Some(LocalAssessmentError.Prediction(error))
              case Right(predicted) =>
                var loss = 0.0
                var coordinate = 0
                while coordinate < targetAxis.size do
                  if weights(coordinate) > 0.0 then
                    val difference = predicted.values.values(coordinate) - actual.values(coordinate)
                    loss += weights(coordinate) * difference * difference
                  coordinate += 1
                if !loss.isFinite then failure = Some(LocalAssessmentError.NonFiniteLoss(row))
                else
                  val key = evaluationUnits.values(row)
                  val (count, mean) = accumulators.getOrElse(key, (0, 0.0))
                  accumulators.update(key, (count + 1, mean + (loss - mean) / (count + 1)))
            row += 1
          failure.toLeft(()).map: _ =>
            val units = accumulators.iterator.map((key, value) => LocalEvaluationUnit(key, value._1, value._2)).toVector
            var mean = 0.0
            units.zipWithIndex.foreach((unit, index) => mean += (unit.meanSquaredLoss - mean) / (index + 1))
            val identity = AxisDigest.sha256Hex: writer =>
              writer.string("scalafim.local-assessment.v1")
              writer.string(receipt.identity)
              def scope(value: ReindexingLeg[P, K, Injection]): Unit =
                writer.string(value.parentAxis.stableKey)
                writer.string(value.child.descriptor.stableKey)
                writer.intLE(value.child.size)
                value.ordinals.iterator.foreach(writer.intLE)
              scope(training)
              scope(assessment)
              roles.foreach: (role, scopes) =>
                writer.string(role)
                writer.intLE(scopes.size)
                scopes.foreach(scope)
              EvidenceIdentity.writeValues(writer, observations.valueIdentity)
              EvidenceIdentity.writeValues(writer, targets.valueIdentity)
              EvidenceIdentity.writeValues(writer, evaluationUnits.valueIdentity)
              writer.string("weighted target squared loss; equal mean within units; equal mean across units")
              weights.foreach(value => writer.string(java.lang.Double.toHexString(value)))
              writer.intLE(n)
              for index <- 0 until n do
                writer.string(evaluationUnits.values(index))
                observations.values(index).values.foreach(value => writer.string(java.lang.Double.toHexString(value)))
                targets.values(index).values.foreach(value => writer.string(java.lang.Double.toHexString(value)))
            LocalAssessmentResult(identity, receipt, assessment.identity, weights, units, mean)

object LocalPatternPrediction:
  /** Only a checked hard injection is admitted. General measurements require a
    * separately admitted covariance capability and are not approximated here.
    * Source descriptors also permit restored equivalent parent witnesses.
    */
  def derive[N, Q, R, P <: SemanticSpace, K](parent: PatternPrediction[N, Q, R],
      selection: ReindexingLeg[P, K, Injection], preparation: LocalPredictionPreparation,
      policy: PatternPredictionPolicy = PatternPredictionPolicy.strict
  ): Either[LocalPredictionError, LocalPatternPrediction[AxisMember[K], Q, R]] =
    val local = selection.child
    val m = BigInt(local.size)
    val q = BigInt(parent.factors.targetAxis.size)
    val r = BigInt(parent.factors.componentAxis.size)
    val h = BigInt(parent.covariance.rank)
    val classes = parent.artifact.target match
      case TargetGeometry.Categorical(value) => BigInt(value.conditions.size)
      case _ => BigInt(0)
    // Conservative adapter plan: includes covariance construction, retained
    // factors and head construction, excluding already resident parent data,
    // provider scratch and object overhead.
    val cells = 64 * m * r + 64 * q * r + 64 * r * r + 16 * q * q +
      64 * classes * (r + 1) + 64 * (m + h) * h + 16 * (m + h) * r + 16 * m * h + 16 * m
    val capacityValid = Vector(m * h, m * r, q * q, q * r, r * r, (m + h) * h, (m + h) * r, m + h).forall(_ <= Int.MaxValue)
    val preparationReceipt = preparation match
      case LocalPredictionPreparation.AlreadyPreparedLocalCoordinates(receipt) if receipt.trim.nonEmpty => Right(receipt)
      case LocalPredictionPreparation.AlreadyPreparedLocalCoordinates(_) => Left(LocalPredictionError.InvalidPreparation("local coordinate preparation receipt is required"))
      case LocalPredictionPreparation.RequiresOutsideMeasurements(reason) => Left(LocalPredictionError.InvalidPreparation(s"strict local input cannot import outside measurements: $reason"))
    if selection.parentAxis != parent.factors.neuralAxis.descriptor then Left(LocalPredictionError.ForeignParent)
    else if local.size == 0 then Left(LocalPredictionError.EmptySelection)
    else if cells > policy.maximumWorkspaceCells || !capacityValid then Left(LocalPredictionError.Budget(cells, policy.maximumWorkspaceCells))
    else
      for
        declaredPreparation <- preparationReceipt
        covariance <- parent.covariance.restrict(selection).left.map(LocalPredictionError.Covariance.apply)
        ordinals = selection.ordinals.toVector
        a = DMat.tabulate(local.size, parent.factors.componentAxis.size)((row, col) => parent.factors.neuralByComponent(ordinals(row), col))
        factors <- PatternPredictionFactors.checked(local, parent.factors.targetAxis, parent.factors.componentAxis, a, parent.factors.targetByComponent)
          .left.map(LocalPredictionError.Prediction.apply)
        centering <- parent.effectiveCentering match
          case value: CenteringPolicy.CenteredBeforeFit => Right(value)
          case CenteringPolicy.ExplicitIntercept(offset, receipt) =>
            AxisValues(local, ordinals.map(offset.values)).left.map(error => LocalPredictionError.Prediction(PatternPredictionError.Artifact(error)))
              .map(values => CenteringPolicy.ExplicitIntercept(values, receipt))
        heads <- PatternPrediction.buildHeads(parent.artifact, factors, covariance, parent.priorCovariance, policy, centering)
          .left.map(LocalPredictionError.Prediction.apply)
      yield
        val identity = AxisDigest.sha256Hex: writer =>
          writer.string("scalafim.local-pattern-prediction.v1")
          writer.string(parent.numericalIdentity)
          writer.string(heads.numericalIdentity)
          writer.string(local.descriptor.stableKey)
          writer.string(declaredPreparation)
          writer.string("hard-ROI marginal covariance; no cropped whole-brain precision")
          writer.intLE(ordinals.size)
          ordinals.foreach(writer.intLE)
          writer.string(cells.toString)
        val receipt = LocalPredictionReceipt(identity, parent.numericalIdentity, selection.identity, local.descriptor,
          preparation, cells.toLong, parent.factors.componentAxis.size)
        new LocalPatternPrediction(local, parent.factors.targetAxis, parent.factors.componentAxis, receipt, heads)
