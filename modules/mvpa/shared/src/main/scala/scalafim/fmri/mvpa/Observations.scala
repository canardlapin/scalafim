package scalafim.fmri.mvpa

import multivar.core.SemanticSpace

enum ObservationsError:
  case Identity(error: ScientificIdentityError)
  case InvalidSamplePurpose(actual: AxisPurpose)
  case InvalidNeuralPurpose(actual: AxisPurpose)

  def message: String =
    this match
      case Identity(error)              => error.message
      case InvalidSamplePurpose(actual) =>
        s"observations require a '${AxisPurpose.Samples.value}' row axis, obtained '${actual.value}'"
      case InvalidNeuralPurpose(actual) =>
        s"observations require a '${AxisPurpose.NeuralFeatures.value}' column axis, obtained '${actual.value}'"

/** An identified observation-by-neural table independent of targets, learners, validation, or a particular numerical
  * representation.
  */
final class Observations[
    Samples <: SemanticSpace,
    NeuralSpace <: SemanticSpace,
    NeuralCoordinate
] private (
    val evidence: EvidenceTable[
      Samples,
      NeuralSpace,
      SampleId,
      NeuralCoordinate
    ],
    val sampleAxisName: ScientificAxisName,
    val neuralAxisName: ScientificAxisName,
    val identity: ScientificSourceIdentity
) extends ScientificSource:
  override type Neural = NeuralSpace
  override type NeuralKey = NeuralCoordinate

  def samples: AxisRef.Aux[SampleId, Samples] = evidence.rows

  override def neuralAxis: AxisRef.Aux[NeuralCoordinate, NeuralSpace] =
    evidence.columns

object Observations:
  private val Protocol = "scalafim-mvpa-observations/v1"

  def apply[
      S <: SemanticSpace,
      N <: SemanticSpace,
      K
  ](
      evidence: EvidenceTable[S, N, SampleId, K],
      sampleAxisName: ScientificAxisName = ScientificAxisName.unsafe("samples"),
      neuralAxisName: ScientificAxisName = ScientificAxisName.unsafe("neural")
  ): Either[ObservationsError, Observations[S, N, K]] =
    if evidence.rows.identity.purpose != AxisPurpose.Samples then
      Left(ObservationsError.InvalidSamplePurpose(evidence.rows.identity.purpose))
    else if evidence.columns.identity.purpose != AxisPurpose.NeuralFeatures then
      Left(ObservationsError.InvalidNeuralPurpose(evidence.columns.identity.purpose))
    else
      ScientificSourceIdentity(
        ScientificSourceKind.unsafe("observations"),
        Vector(
          ScientificSourceAxis(sampleAxisName, evidence.rows.identity),
          ScientificSourceAxis(neuralAxisName, evidence.columns.identity)
        ),
        Vector(
          "evidence-value" -> evidence.table.valueIdentity.stableKey,
          "protocol" -> Protocol
        )
      ).left
        .map(ObservationsError.Identity.apply)
        .map: identity =>
          new Observations(
            evidence,
            sampleAxisName,
            neuralAxisName,
            identity
          )
