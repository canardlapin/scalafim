package scalafim.dataset

private def checkedId(value: String, label: String): String =
  val out = value.trim
  require(out.nonEmpty, s"$label must be non-empty")
  out

private def checkedIdEither(value: String, label: String): Either[DatasetError, String] =
  val out = value.trim
  if out.nonEmpty then Right(out)
  else Left(DatasetError.InvalidLabel(label, value, "must be non-empty"))

opaque type DatasetId = String

object DatasetId:
  def make(value: String): Either[DatasetError, DatasetId] =
    checkedIdEither(value, "DatasetId")

  def apply(value: String): DatasetId = checkedId(value, "DatasetId")

  extension (id: DatasetId)
    def value: String = id

opaque type SubjectId = String

object SubjectId:
  def make(value: String): Either[DatasetError, SubjectId] =
    checkedIdEither(value, "SubjectId")

  def apply(value: String): SubjectId = checkedId(value, "SubjectId")

  extension (id: SubjectId)
    def value: String = id

opaque type SessionId = String

object SessionId:
  def make(value: String): Either[DatasetError, SessionId] =
    checkedIdEither(value, "SessionId")

  def apply(value: String): SessionId = checkedId(value, "SessionId")

  extension (id: SessionId)
    def value: String = id

opaque type TaskId = String

object TaskId:
  def make(value: String): Either[DatasetError, TaskId] =
    checkedIdEither(value, "TaskId")

  def apply(value: String): TaskId = checkedId(value, "TaskId")

  extension (id: TaskId)
    def value: String = id

opaque type SpaceId = String

object SpaceId:
  def make(value: String): Either[DatasetError, SpaceId] =
    checkedIdEither(value, "SpaceId")

  def apply(value: String): SpaceId = checkedId(value, "SpaceId")

  extension (id: SpaceId)
    def value: String = id

opaque type DatasetFieldId = String

object DatasetFieldId:
  def make(value: String): Either[DatasetError, DatasetFieldId] =
    checkedIdEither(value, "DatasetFieldId")

  def apply(value: String): DatasetFieldId = checkedId(value, "DatasetFieldId")

  inline def unsafe(value: String): DatasetFieldId = value

  extension (id: DatasetFieldId)
    def value: String = id

opaque type ConditionLabel = String

object ConditionLabel:
  def make(value: String): Either[DatasetError, ConditionLabel] =
    checkedIdEither(value, "ConditionLabel")

  def apply(value: String): ConditionLabel = checkedId(value, "ConditionLabel")

  inline def unsafe(value: String): ConditionLabel = value

  extension (label: ConditionLabel)
    def value: String = label

opaque type RunId = String

object RunId:
  def make(value: String): Either[DatasetError, RunId] =
    checkedIdEither(value, "RunId")

  def apply(value: String): RunId = checkedId(value, "RunId")

  extension (id: RunId)
    def value: String = id
