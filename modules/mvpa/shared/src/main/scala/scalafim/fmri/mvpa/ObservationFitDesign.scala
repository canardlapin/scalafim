package scalafim.fmri.mvpa

import multivar.core.SemanticSpace
import resample4s.core.DesignError
import resample4s.core.IndexSpace
import resample4s.core.Selection

enum ObservationFitScope:
  case EntireTable
  case DeclaredTrainingSubset

  def label: String =
    this match
      case EntireTable            => "entire-table"
      case DeclaredTrainingSubset => "declared-training-subset"

/** An exact set of observation rows authorized to fit an unsupervised global decomposition. This is not validation and
  * carries no assessment role.
  */
final class ObservationFitDesign[S <: SemanticSpace] private (
    val samples: AxisRef.Aux[SampleId, S],
    val selection: ReindexingLeg[S, SampleId, SampleId, Selection],
    val sampleAxisName: ScientificAxisName,
    val scope: ObservationFitScope,
    val identity: DesignIdentity,
    val referencedAxes: Vector[DesignAxisReference]
) extends EvidenceDesign:
  def fitSamples: AxisRef.Aux[SampleId, selection.child.Id] =
    selection.child

object ObservationFitDesign:
  private val Protocol = "scalafim-mvpa-observation-fit/v1"

  def entireTable[S <: SemanticSpace](
      samples: AxisRef.Aux[SampleId, S],
      sampleAxisName: ScientificAxisName = ScientificAxisName.unsafe("samples")
  ): Either[ObservationFitDesignError, ObservationFitDesign[S]] =
    for
      space <- IndexSpace.of(samples.size).left.map(ObservationFitDesignError.Resampling.apply)
      selected <- Selection
        .from(IArray.unsafeFromArray(Array.tabulate(samples.size)(index => index)), space)
        .left
        .map(ObservationFitDesignError.Resampling.apply)
      leg <- ReindexingLeg
        .selection(samples, selected)
        .left
        .map(ObservationFitDesignError.Axis.apply)
      design <- build(samples, leg, sampleAxisName, ObservationFitScope.EntireTable)
    yield design

  def trainingSubset[S <: SemanticSpace](
      samples: AxisRef.Aux[SampleId, S],
      selection: ReindexingLeg[S, SampleId, SampleId, Selection],
      sampleAxisName: ScientificAxisName = ScientificAxisName.unsafe("samples")
  ): Either[ObservationFitDesignError, ObservationFitDesign[S]] =
    build(
      samples,
      selection,
      sampleAxisName,
      ObservationFitScope.DeclaredTrainingSubset
    )

  private def build[S <: SemanticSpace](
      samples: AxisRef.Aux[SampleId, S],
      selection: ReindexingLeg[S, SampleId, SampleId, Selection],
      sampleAxisName: ScientificAxisName,
      scope: ObservationFitScope
  ): Either[ObservationFitDesignError, ObservationFitDesign[S]] =
    if samples.identity != selection.parentIdentity then
      Left(
        ObservationFitDesignError.SampleAxisMismatch(
          samples.identity.fingerprint,
          selection.parentIdentity.fingerprint
        )
      )
    else if !(samples.evidence eq selection.parentEvidence) then Left(ObservationFitDesignError.SampleWitnessMismatch)
    else if selection.size == 0 then Left(ObservationFitDesignError.EmptyFit)
    else
      DesignIdentity(
        DesignKind.unsafe("observation-fit"),
        Vector(
          "fit-rows" -> selection.child.identity.fingerprint.value,
          "protocol" -> Protocol,
          "sample-axis" -> sampleAxisName.value,
          "sample-space" -> samples.identity.fingerprint.value,
          "scope" -> scope.label
        )
      ).left
        .map(ObservationFitDesignError.Identity.apply)
        .map: identity =>
          new ObservationFitDesign(
            samples,
            selection,
            sampleAxisName,
            scope,
            identity,
            Vector(DesignAxisReference(sampleAxisName, samples.identity))
          )

enum ObservationFitDesignError:
  case Identity(error: ScientificIdentityError)
  case Axis(error: AxisRefError)
  case Resampling(error: DesignError)
  case EmptyFit
  case SampleAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case SampleWitnessMismatch

  def message: String =
    this match
      case Identity(error)                      => error.message
      case Axis(error)                          => error.message
      case Resampling(error)                    => error.message
      case EmptyFit                             => "observation fit requires at least one row"
      case SampleAxisMismatch(expected, actual) =>
        s"observation fit selection belongs to ${actual.value}, expected ${expected.value}"
      case SampleWitnessMismatch =>
        "observation fit selection and table use different nominal sample witnesses"
