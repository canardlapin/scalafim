package scalafim.dataset

final case class DatasetMetadata(values: Map[String, String] = Map.empty):
  def get(key: String): Option[String] = values.get(key)
  def updated(key: String, value: String): DatasetMetadata =
    copy(values = values.updated(key, value))

object DatasetMetadata:
  val Empty: DatasetMetadata = DatasetMetadata()

final case class DatasetEvents(rows: Vector[Map[String, String]] = Vector.empty):
  def nrows: Int = rows.length
  def isEmpty: Boolean = rows.isEmpty

object DatasetEvents:
  val Empty: DatasetEvents = DatasetEvents()
