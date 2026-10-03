package scalafim.fmri.mvpa.pattern

import multivar.core.SemanticSpace
import multivar.family.spectral.SubspaceAgreement
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.analysis.*
import scala.util.control.NonFatal

/** Refit seam receives the actual selected discovery rows on both sides.
  * The numerical owner remains responsible for its fit/resource contract. */
trait PatternBlockRefit[N <: SemanticSpace, Q <: SemanticSpace]:
  def fit[K](samples: AxisRef[K])(
      observations: Observations[samples.Id, N], targets: MultiResponse[samples.Id, Q], binding: TrainingBinding
  ): Either[String, PatternArtifact]

enum PatternStabilityError:
  case AxisMismatch(field: String)
  case Exposure(detail: String)
  case Budget(detail: String)
  case Unit(detail: String)
  case Refit(detail: String)
  case Comparison(detail: String)

enum PatternStabilityOutcome:
  case Measured(brain: SubspaceAgreement, target: SubspaceAgreement, fixedComponentOrderComparable: Boolean)
  case Failed(error: PatternStabilityError)

final case class PatternTrainingStabilityUnit(
    unitAddress: String, heldOutGroup: String, analysisOrdinals: Vector[Int],
    outcome: PatternStabilityOutcome
)

final class PatternTrainingStabilityResult private[pattern] (
    val discoveryIdentity: String, val designReceipt: LeaveOneGroupOutReceipt,
    val units: Vector[PatternTrainingStabilityUnit], val exposure: EvidenceExposure
):
  def allMeasured: Boolean = units.forall(_.outcome match
    case PatternStabilityOutcome.Measured(_, _, _) => true
    case PatternStabilityOutcome.Failed(_) => false)

final case class PatternStabilityBudget(maximumRefits: Int = 64, maximumComparisonElements: Long = 4000000L):
  require(maximumRefits > 0 && maximumComparisonElements >= 0L)

object PatternTrainingStability:
  /** Descriptive leave-one-training-group-out sensitivity, not a confidence
    * interval, selection frequency p-value or source-identification result.
    * No favorable rotation/permutation is chosen after seeing confirmation.
    * Fixed-axis cosines ignore signs and preserve declared component order;
    * differing component descriptors make coordinate comparisons unavailable.
    */
  def run[S <: SemanticSpace, N <: SemanticSpace, Q <: SemanticSpace, K, G](
      discovery: DiscoverySnapshot[?, ?], training: LeaveOneGroupOutDesign[S, K, G],
      observations: Observations[S, N], targets: MultiResponse[S, Q], exposure: EvidenceExposure,
      refitter: PatternBlockRefit[N, Q], budget: PatternStabilityBudget = PatternStabilityBudget()
  ): Either[PatternStabilityError, PatternTrainingStabilityResult] =
    val baseline = discovery.artifact.factors
    if training.samples.descriptor != discovery.samples.rows.descriptor || observations.sampleAxis != training.samples.descriptor || targets.sampleAxis != training.samples.descriptor then
      Left(PatternStabilityError.AxisMismatch("actual discovery training rows; confirmation cannot be used for stability"))
    else if observations.neuralAxis != baseline.neuralAxis.descriptor || targets.featureAxis != baseline.targetAxis.descriptor then
      Left(PatternStabilityError.AxisMismatch("discovery neural/target coordinates"))
    else if exposure.reference.evidenceIdentity != discovery.identity || exposure.initialScope != ExposureScope.Training || exposure.events.exists(_.request.scope.canOverlap(ExposureScope.Holdout)) then
      Left(PatternStabilityError.Exposure("training snapshot identity or possible confirmation exposure"))
    else if training.keys.size > budget.maximumRefits then Left(PatternStabilityError.Budget(s"${training.keys.size} declared refits exceed ${budget.maximumRefits}"))
    else
      val output = Vector.newBuilder[PatternTrainingStabilityUnit]
      var currentExposure = exposure
      training.keys.foreach: key =>
        training.at(key) match
          case Left(error) => output += PatternTrainingStabilityUnit(key.toString, "unavailable", Vector.empty, PatternStabilityOutcome.Failed(PatternStabilityError.Unit(error.toString)))
          case Right(unit) =>
            val x = observations.reindex(unit.analysis)
            val y = targets.reindex(unit.analysis)
            val digest = AxisDigest.sha256Hex: writer =>
              writer.string("scalafim.pattern-training-block.v1"); writer.string(discovery.identity)
              writer.string(unit.identity.toString); writer.string(x.identity.toString); writer.string(y.identity.toString)
            val binding = TrainingBinding(unit.analysis.child.descriptor, s"training stability ${unit.heldOut.stableKey}", digest)
            val cells = BigInt(x.rows) * (BigInt(x.columns) + y.columns)
            val request = ExposureRequest(ExposurePurpose.ModelSelection, ExposureActorRole.Analyst, ExposureScope.Training,
              ExposurePayload.Payload, ExposureAssurance.Declared, cells.toLong)
            val attempt = for
              b <- binding.left.map(error => error.toString)
              permit <- ExposureControl.permit(currentExposure, request).left.map(error => error.toString)
            yield ExposureControl.read(currentExposure, permit, request)(refitter.fit(unit.analysis.child)(x, y, b))
            val outcome = attempt match
              case Left(detail) => PatternStabilityOutcome.Failed(PatternStabilityError.Refit(detail))
              case Right(ExposureAttempt.Refused(error, record)) =>
                currentExposure = record
                PatternStabilityOutcome.Failed(PatternStabilityError.Exposure(error.toString))
              case Right(ExposureAttempt.Failed(detail, record)) =>
                currentExposure = record
                PatternStabilityOutcome.Failed(PatternStabilityError.Refit(detail))
              case Right(ExposureAttempt.Completed(artifact, record)) =>
                currentExposure = record
                if artifact.trainingBinding.declaredSampleAxis != unit.analysis.child.descriptor || artifact.factors.neuralAxis.descriptor != observations.neuralAxis || artifact.factors.targetAxis.descriptor != targets.featureAxis then
                  PatternStabilityOutcome.Failed(PatternStabilityError.AxisMismatch("returned refit must bind the actual selected rows and original feature coordinates"))
                else
                  try
                    val compared = for
                      brain <- SubspaceAgreement.compare(baseline.neuralByComponent, artifact.factors.neuralByComponent, maximumElements = budget.maximumComparisonElements)
                      target <- SubspaceAgreement.compare(baseline.targetByComponent, artifact.factors.targetByComponent, maximumElements = budget.maximumComparisonElements)
                    yield (brain, target)
                    compared match
                      case Left(error) => PatternStabilityOutcome.Failed(PatternStabilityError.Comparison(error.message))
                      case Right((brain, target)) =>
                        val ordered = baseline.componentAxis.descriptor == artifact.factors.componentAxis.descriptor
                        PatternStabilityOutcome.Measured(
                          if ordered then brain else brain.copy(fixedAxisAbsoluteCosines = None),
                          if ordered then target else target.copy(fixedAxisAbsoluteCosines = None), ordered)
                  catch
                    case NonFatal(error) => PatternStabilityOutcome.Failed(PatternStabilityError.Comparison(Option(error.getMessage).getOrElse(error.getClass.getName)))
            output += PatternTrainingStabilityUnit(key.toString, unit.heldOut.stableKey, unit.analysis.ordinals.toVector, outcome)
      Right(new PatternTrainingStabilityResult(discovery.identity, training.receipt, output.result(), currentExposure))
