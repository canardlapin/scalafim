package scalafim.fmri.workflow

import scalafim.estimates.{EstimandId, ObservationId, PinnedUnit, ProductId}
import scalafim.fmri.group.{GroupEstimateInput, GroupMarginalUncertainty}

private def workflowIdentifier(label: String, value: String): Either[WorkflowError, String] =
  WorkflowValidation.identifier(label, value)

opaque type WorkflowId = String

object WorkflowId:
  def apply(value: String): Either[WorkflowError, WorkflowId] =
    workflowIdentifier("workflow id", value).map(valid => valid: WorkflowId)

  def unsafe(value: String): WorkflowId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: WorkflowId)
    inline def value: String = id

opaque type FirstLevelUnitId = String

object FirstLevelUnitId:
  def apply(value: String): Either[WorkflowError, FirstLevelUnitId] =
    workflowIdentifier("first-level unit id", value).map(valid => valid: FirstLevelUnitId)

  def unsafe(value: String): FirstLevelUnitId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: FirstLevelUnitId)
    inline def value: String = id

opaque type SubjectJobId = String

object SubjectJobId:
  def apply(value: String): Either[WorkflowError, SubjectJobId] =
    workflowIdentifier("subject job id", value).map(valid => valid: SubjectJobId)

  def unsafe(value: String): SubjectJobId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  def from(workflowId: WorkflowId, unitId: FirstLevelUnitId): SubjectJobId =
    unsafe(s"${workflowId.value}.${unitId.value}")

  extension (id: SubjectJobId)
    inline def value: String = id

opaque type GroupWorkflowId = String

object GroupWorkflowId:
  def apply(value: String): Either[WorkflowError, GroupWorkflowId] =
    workflowIdentifier("group workflow id", value).map(valid => valid: GroupWorkflowId)

  def unsafe(value: String): GroupWorkflowId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: GroupWorkflowId)
    inline def value: String = id

opaque type ResultBundleId = String

object ResultBundleId:
  def apply(value: String): Either[WorkflowError, ResultBundleId] =
    workflowIdentifier("result bundle id", value).map(valid => valid: ResultBundleId)

  def unsafe(value: String): ResultBundleId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: ResultBundleId)
    inline def value: String = id

opaque type OutputFormatId = String

object OutputFormatId:
  /** Canonical estimate-set NIfTI, whose manifest and product axes are owned
    * by the estimates contract rather than a legacy BIDS result facade.
    */
  val CoreNifti: OutputFormatId = unsafe("core-nifti")

  def apply(value: String): Either[WorkflowError, OutputFormatId] =
    workflowIdentifier("output format id", value).map(valid => valid: OutputFormatId)

  def unsafe(value: String): OutputFormatId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: OutputFormatId)
    inline def value: String = id

opaque type WorkflowContrastId = String

object WorkflowContrastId:
  def apply(value: String): Either[WorkflowError, WorkflowContrastId] =
    workflowIdentifier("contrast id", value).map(valid => valid: WorkflowContrastId)

  def unsafe(value: String): WorkflowContrastId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: WorkflowContrastId)
    inline def value: String = id

opaque type CoefficientName = String

object CoefficientName:
  def apply(value: String): Either[WorkflowError, CoefficientName] =
    WorkflowValidation.label("coefficient name", value).map(valid => valid: CoefficientName)

  def unsafe(value: String): CoefficientName =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (name: CoefficientName)
    inline def value: String = name

opaque type ArtifactLocation = String

object ArtifactLocation:
  def apply(value: String): Either[WorkflowError, ArtifactLocation] =
    val clean = value.trim
    if clean.isEmpty then Left(WorkflowError.InvalidValue("artifact location", value, "must be non-empty"))
    else if clean.exists(character => character == '\n' || character == '\r' || character == '\u0000') then
      Left(WorkflowError.InvalidValue("artifact location", value, "must not contain control separators"))
    else Right(clean)

  def unsafe(value: String): ArtifactLocation =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (location: ArtifactLocation)
    inline def value: String = location

    def resolve(segments: String*): ArtifactLocation =
      val base = location.reverse.dropWhile(_ == '/').reverse
      val suffix = segments.iterator.map(_.stripPrefix("/").stripSuffix("/")).filter(_.nonEmpty).mkString("/")
      if suffix.isEmpty then location else unsafe(s"$base/$suffix")

sealed trait BidsProjectResource
sealed trait BoldImageResource
sealed trait EventsTableResource
sealed trait ConfoundsTableResource
sealed trait MaskImageResource

final case class WorkflowArtifactRef[+A] private[workflow] (location: ArtifactLocation)

object WorkflowArtifactRef:
  def apply[A](location: String): Either[WorkflowError, WorkflowArtifactRef[A]] =
    ArtifactLocation(location).map(new WorkflowArtifactRef[A](_))

  def unsafe[A](location: String): WorkflowArtifactRef[A] =
    apply[A](location).fold(error => throw new IllegalArgumentException(error.message), identity)

enum EstimateMapLayout:
  case BackendDefault
  case BundledMaps
  case IndividualNamedMaps

/** A destination reservation, not a completed result.  It deliberately
  * contains no reader-facing identity: only `SealedEstimateReference` can
  * cross from execution into a group consumer.
  */
final case class PlannedEstimateOutput private (
    id: ResultBundleId,
    location: ArtifactLocation,
    format: OutputFormatId,
    layout: EstimateMapLayout
)

object PlannedEstimateOutput:
  def make(
      id: ResultBundleId,
      location: ArtifactLocation,
      format: OutputFormatId,
      layout: EstimateMapLayout
  ): PlannedEstimateOutput =
    PlannedEstimateOutput(id, location, format, layout)

final case class ResultOutputPolicy(
    root: ArtifactLocation,
    format: OutputFormatId = OutputFormatId.CoreNifti,
    layout: EstimateMapLayout = EstimateMapLayout.BundledMaps
):
  def firstLevel(workflowId: WorkflowId, unitId: FirstLevelUnitId): PlannedEstimateOutput =
    val id = ResultBundleId.unsafe(s"${workflowId.value}.${unitId.value}")
    PlannedEstimateOutput.make(id, root.resolve("first-level", unitId.value), format, layout)

  def group(workflowId: WorkflowId, groupId: GroupWorkflowId): PlannedEstimateOutput =
    val id = ResultBundleId.unsafe(s"${workflowId.value}.${groupId.value}")
    PlannedEstimateOutput.make(id, root.resolve("group", groupId.value), format, layout)

/** Product and observation axes selected for a group handoff. */
final case class EstimateProductSelection private (
    observation: ObservationId,
    effect: ProductId,
    uncertainty: Option[GroupMarginalUncertainty],
    estimands: Vector[EstimandId]
):
  require(estimands.nonEmpty && estimands.distinct.size == estimands.size,
    "a group handoff must select nonempty unique estimands")

object EstimateProductSelection:
  def make(
      observation: ObservationId,
      effect: ProductId,
      uncertainty: Option[GroupMarginalUncertainty],
      estimands: Vector[EstimandId]
  ): Either[WorkflowError, EstimateProductSelection] =
    if estimands.isEmpty then Left(WorkflowError.InvalidOutput("group handoff must select at least one estimand"))
    else if estimands.distinct.size != estimands.size then Left(WorkflowError.InvalidOutput("group handoff estimands must be unique"))
    else Right(new EstimateProductSelection(observation, effect, uncertainty, estimands))

/** Immutable execution output. `PinnedUnit.manifest` includes the content
  * digest and byte length; no mutable output location or format appears here.
  */
final case class SealedEstimateReference private (
    pinned: PinnedUnit,
    selection: EstimateProductSelection
):
  def groupInput: GroupEstimateInput =
    GroupEstimateInput(pinned, selection.observation, selection.effect, selection.uncertainty)

object SealedEstimateReference:
  def make(pinned: PinnedUnit, selection: EstimateProductSelection): Either[WorkflowError, SealedEstimateReference] =
    if pinned.manifest.path.trim.isEmpty || pinned.manifest.bytes <= 0L then
      Left(WorkflowError.InvalidOutput("sealed estimate manifest must have a path and positive byte length"))
    else Right(new SealedEstimateReference(pinned, selection))
