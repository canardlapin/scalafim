package scalafim.transform.oracle

/** A tab-separated oracle table written by `tools/transform/oracle_common.write_table`: one header row, then rows of
  * full-precision numbers (Python `repr`, which round-trips exactly through Scala's `toDouble`). A first column named
  * `key` holds a string key per row.
  */
final case class OracleTable(header: Vector[String], keys: Vector[String], rows: Vector[Vector[Double]]):
  def index(name: String): Int =
    val i = header.indexOf(name)
    require(i >= 0, s"oracle table has no column '$name' (columns: ${header.mkString(", ")})")
    i

  def column(name: String): Vector[Double] =
    val i = index(name)
    rows.map(_(i))

  def columns(names: String*): Vector[Vector[Double]] =
    val indices = names.map(index)
    rows.map(row => indices.map(row).toVector)

  /** `count` consecutive columns starting at `first`, e.g. the 16 entries of a row-major 4x4 named `m00`..`m33`. */
  def block(row: Vector[Double], first: String, count: Int): Vector[Double] =
    val start = index(first)
    row.slice(start, start + count)

  def keyed: Vector[(String, Vector[Double])] =
    keys.zip(rows)

object OracleTable:
  def parse(text: String): OracleTable =
    val lines = text.linesIterator.map(_.stripLineEnd).filter(line => line.trim.nonEmpty && !line.startsWith("#")).toVector
    require(lines.nonEmpty, "empty oracle table")
    val fullHeader = lines.head.split('\t').toVector
    val hasKey = fullHeader.headOption.contains("key")
    val header = if hasKey then fullHeader.tail else fullHeader
    val parsed = lines.tail.map: line =>
      val cells = line.split('\t').toVector
      require(cells.size == fullHeader.size, s"row has ${cells.size} cells, header has ${fullHeader.size}: $line")
      if hasKey then cells.head -> cells.tail.map(_.toDouble) else "" -> cells.map(_.toDouble)
    OracleTable(header, parsed.map(_._1), parsed.map(_._2))

  def load(path: String): OracleTable =
    parse(OracleFixtures.text(path))
