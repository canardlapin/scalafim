package scalafim.fmri.design.data

import scalafim.fmri.design.{ColumnId, DesignError}
import scalafim.fmri.hrf.Hrf

import scala.collection.immutable.VectorMap

enum Column:
  case Doubles(values: Vector[Double])
  case Ints(values: Vector[Int])
  case Strings(values: Vector[String])
  case Bools(values: Vector[Boolean])
  case DoubleLists(values: Vector[Vector[Double]])
  case Hrfs(values: Vector[Hrf])

  def size: Int =
    this match
      case Doubles(v) => v.length
      case Ints(v)    => v.length
      case Strings(v) => v.length
      case Bools(v)   => v.length
      case DoubleLists(v) => v.length
      case Hrfs(v)    => v.length

  def typeName: String =
    this match
      case Doubles(_)     => "numeric"
      case Ints(_)        => "integer"
      case Strings(_)     => "string"
      case Bools(_)       => "boolean"
      case DoubleLists(_) => "numeric-list"
      case Hrfs(_)        => "HRF"

/** How a [[Column]] is read as a `Vector[A]`.
  *
  * One instance per element type, in place of a per-type family of accessors:
  * the `Ints`-widen-to-`Doubles` rule lives here once rather than being
  * restated by every accessor that admits it.
  */
trait ColumnType[A]:
  /** How this element type is named in a [[DesignError.InvalidColumnType]]. */
  def typeName: String

  def extract(column: Column): Option[Vector[A]]

object ColumnType:
  given ColumnType[Double] with
    val typeName: String = "numeric"
    def extract(column: Column): Option[Vector[Double]] =
      column match
        case Column.Doubles(v) => Some(v)
        case Column.Ints(v)    => Some(v.map(_.toDouble))
        case _                 => None

  given ColumnType[Int] with
    val typeName: String = "integer"
    def extract(column: Column): Option[Vector[Int]] =
      column match
        case Column.Ints(v) => Some(v)
        case _              => None

  given ColumnType[String] with
    val typeName: String = "string"
    def extract(column: Column): Option[Vector[String]] =
      column match
        case Column.Strings(v) => Some(v)
        case _                 => None

  given ColumnType[Boolean] with
    val typeName: String = "boolean"
    def extract(column: Column): Option[Vector[Boolean]] =
      column match
        case Column.Bools(v) => Some(v)
        case _               => None

  given ColumnType[Vector[Double]] with
    val typeName: String = "numeric-list"
    def extract(column: Column): Option[Vector[Vector[Double]]] =
      column match
        case Column.DoubleLists(v) => Some(v)
        case _                     => None

  given ColumnType[Hrf] with
    val typeName: String = "HRF"
    def extract(column: Column): Option[Vector[Hrf]] =
      column match
        case Column.Hrfs(v) => Some(v)
        case _              => None

final case class DataTable private (nrows: Int, columns: VectorMap[String, Column]):
  require(nrows >= 0, "`nrows` must be >= 0")
  columns.foreach { case (k, col) =>
    require(col.size == nrows, s"Column '$k' has length ${col.size} but expected $nrows")
  }

  def names: Vector[String] = columns.keys.toVector

  def contains(id: ColumnId): Boolean =
    columns.contains(id.value)

  /** The column `id` names, or [[DesignError.MissingColumn]]. */
  def column(id: ColumnId): Either[DesignError, Column] =
    columns.get(id.value).toRight(DesignError.MissingColumn(id.value))

  /** The column `id` names, read as `Vector[A]`.
    *
    * The only fallible column accessor. It reports a missing column and a
    * mistyped column as the [[DesignError]] cases that exist for them, and
    * there is no throwing twin a caller could reach for by accident and lose
    * that distinction to a stringly catch-all.
    */
  def get[A](id: ColumnId)(using ct: ColumnType[A]): Either[DesignError, Vector[A]] =
    column(id).flatMap { col =>
      ct.extract(col).toRight(DesignError.InvalidColumnType(id.value, ct.typeName, col.typeName))
    }

  def filterRows(keep: Seq[Boolean]): DataTable =
    require(keep.length == nrows, s"keep mask has length ${keep.length} but expected $nrows")
    val idx = keep.iterator.zipWithIndex.collect { case (true, i) => i }.toVector
    DataTable(
      idx.length,
      columns.map { case (k, col) =>
        val out: Column =
          col match
            case Column.Doubles(v) => Column.Doubles(idx.map(v))
            case Column.Ints(v)    => Column.Ints(idx.map(v))
            case Column.Strings(v) => Column.Strings(idx.map(v))
            case Column.Bools(v)   => Column.Bools(idx.map(v))
            case Column.DoubleLists(v) => Column.DoubleLists(idx.map(v))
            case Column.Hrfs(v)    => Column.Hrfs(idx.map(v))
        k -> out
      }
    )

object DataTable:
  def apply(nrows: Int, columns: IterableOnce[(String, Column)]): DataTable =
    DataTable(nrows, VectorMap.from(columns))

  def fromColumns(cols: (String, Column)*): DataTable =
    require(cols.nonEmpty, "DataTable.fromColumns requires at least one column")
    val n = cols.head._2.size
    DataTable(n, cols)
