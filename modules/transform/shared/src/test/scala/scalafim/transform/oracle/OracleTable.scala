package scalafim.transform.oracle

/** A tab-separated oracle table written by `tools/transform/oracle_common.write_table`: one header row, then rows of
  * full-precision numbers (Python `repr`, which round-trips exactly through Scala's `toDouble`).
  */
final case class OracleTable(header: Vector[String], rows: Vector[Vector[Double]]):
  def column(name: String): Vector[Double] =
    val index = header.indexOf(name)
    require(index >= 0, s"oracle table has no column '$name' (columns: ${header.mkString(", ")})")
    rows.map(_(index))

  def columns(names: String*): Vector[Vector[Double]] =
    val indices = names.map: name =>
      val index = header.indexOf(name)
      require(index >= 0, s"oracle table has no column '$name'")
      index
    rows.map(row => indices.map(row).toVector)

object OracleTable:
  def parse(text: String): OracleTable =
    val lines = text.linesIterator.map(_.trim).filter(line => line.nonEmpty && !line.startsWith("#")).toVector
    require(lines.nonEmpty, "empty oracle table")
    val header = lines.head.split('\t').toVector
    val rows = lines.tail.map: line =>
      val cells = line.split('\t').toVector
      require(cells.size == header.size, s"row has ${cells.size} cells, header has ${header.size}: $line")
      cells.map(_.toDouble)
    OracleTable(header, rows)

  def load(path: String): OracleTable =
    parse(OracleFixtures.text(path))
