package scalafim.fmri.workflow

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
  val BidsNifti: OutputFormatId = unsafe("bids-nifti")

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
sealed trait ResultBundleResource

final case class WorkflowArtifactRef[+A] private[workflow] (location: ArtifactLocation)

object WorkflowArtifactRef:
  def apply[A](location: String): Either[WorkflowError, WorkflowArtifactRef[A]] =
    ArtifactLocation(location).map(new WorkflowArtifactRef[A](_))

  def unsafe[A](location: String): WorkflowArtifactRef[A] =
    apply[A](location).fold(error => throw new IllegalArgumentException(error.message), identity)

enum ResultMapLayout:
  case BackendDefault
  case BundledMaps
  case IndividualNamedMaps

final case class ResultBundleRef private (
    id: ResultBundleId,
    artifact: WorkflowArtifactRef[ResultBundleResource],
    format: OutputFormatId,
    layout: ResultMapLayout
)

object ResultBundleRef:
  def make(
      id: ResultBundleId,
      location: ArtifactLocation,
      format: OutputFormatId,
      layout: ResultMapLayout
  ): ResultBundleRef =
    ResultBundleRef(id, new WorkflowArtifactRef[ResultBundleResource](location), format, layout)

final case class ResultOutputPolicy(
    root: ArtifactLocation,
    format: OutputFormatId = OutputFormatId.BidsNifti,
    layout: ResultMapLayout = ResultMapLayout.BundledMaps
):
  def firstLevel(workflowId: WorkflowId, unitId: FirstLevelUnitId): ResultBundleRef =
    val id = ResultBundleId.unsafe(s"${workflowId.value}.${unitId.value}")
    ResultBundleRef.make(id, root.resolve("first-level", unitId.value), format, layout)

  def group(workflowId: WorkflowId, groupId: GroupWorkflowId): ResultBundleRef =
    val id = ResultBundleId.unsafe(s"${workflowId.value}.${groupId.value}")
    ResultBundleRef.make(id, root.resolve("group", groupId.value), format, layout)
