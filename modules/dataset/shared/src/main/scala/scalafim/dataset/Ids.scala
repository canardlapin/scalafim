package scalafim.dataset

private def checkedId(value: String, label: String): String =
  val out = value.trim
  require(out.nonEmpty, s"$label must be non-empty")
  out

opaque type DatasetId = String

object DatasetId:
  def apply(value: String): DatasetId = checkedId(value, "DatasetId")

  extension (id: DatasetId)
    def value: String = id

opaque type SubjectId = String

object SubjectId:
  def apply(value: String): SubjectId = checkedId(value, "SubjectId")

  extension (id: SubjectId)
    def value: String = id

opaque type SessionId = String

object SessionId:
  def apply(value: String): SessionId = checkedId(value, "SessionId")

  extension (id: SessionId)
    def value: String = id

opaque type RunId = String

object RunId:
  def apply(value: String): RunId = checkedId(value, "RunId")

  extension (id: RunId)
    def value: String = id
