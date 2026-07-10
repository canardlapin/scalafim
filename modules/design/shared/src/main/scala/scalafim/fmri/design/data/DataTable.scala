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

final case class DataTable private (nrows: Int, columns: VectorMap[String, Column]):
  require(nrows >= 0, "`nrows` must be >= 0")
  columns.foreach { case (k, col) =>
    require(col.size == nrows, s"Column '$k' has length ${col.size} but expected $nrows")
  }

  def names: Vector[String] = columns.keys.toVector

  def contains(name: String): Boolean =
    columns.contains(name)

  def column(name: String): Column =
    columns.getOrElse(name, throw new IllegalArgumentException(s"Unknown column: '$name'"))

  def columnEither(name: String): Either[DesignError, Column] =
    columns.get(name).toRight(DesignError.MissingColumn(name))

  def columnById(id: ColumnId): Either[DesignError, Column] =
    columnEither(id.value)

  def doubles(name: String): Vector[Double] =
    column(name) match
      case Column.Doubles(v) => v
      case Column.Ints(v)    => v.map(_.toDouble)
      case other             => throw new IllegalArgumentException(s"Column '$name' is not numeric: $other")

  def doublesEither(name: String): Either[DesignError, Vector[Double]] =
    columnEither(name).flatMap {
      case Column.Doubles(v) => Right(v)
      case Column.Ints(v)    => Right(v.map(_.toDouble))
      case other             => Left(DesignError.InvalidColumnType(name, "numeric", other.typeName))
    }

  def doublesById(id: ColumnId): Either[DesignError, Vector[Double]] =
    doublesEither(id.value)

  def ints(name: String): Vector[Int] =
    column(name) match
      case Column.Ints(v) => v
      case other          => throw new IllegalArgumentException(s"Column '$name' is not Int: $other")

  def intsEither(name: String): Either[DesignError, Vector[Int]] =
    columnEither(name).flatMap {
      case Column.Ints(v) => Right(v)
      case other          => Left(DesignError.InvalidColumnType(name, "integer", other.typeName))
    }

  def intsById(id: ColumnId): Either[DesignError, Vector[Int]] =
    intsEither(id.value)

  def strings(name: String): Vector[String] =
    column(name) match
      case Column.Strings(v) => v
      case other             => throw new IllegalArgumentException(s"Column '$name' is not String: $other")

  def stringsEither(name: String): Either[DesignError, Vector[String]] =
    columnEither(name).flatMap {
      case Column.Strings(v) => Right(v)
      case other             => Left(DesignError.InvalidColumnType(name, "string", other.typeName))
    }

  def stringsById(id: ColumnId): Either[DesignError, Vector[String]] =
    stringsEither(id.value)

  def bools(name: String): Vector[Boolean] =
    column(name) match
      case Column.Bools(v) => v
      case other           => throw new IllegalArgumentException(s"Column '$name' is not Boolean: $other")

  def boolsEither(name: String): Either[DesignError, Vector[Boolean]] =
    columnEither(name).flatMap {
      case Column.Bools(v) => Right(v)
      case other           => Left(DesignError.InvalidColumnType(name, "boolean", other.typeName))
    }

  def boolsById(id: ColumnId): Either[DesignError, Vector[Boolean]] =
    boolsEither(id.value)

  def hrfs(name: String): Vector[Hrf] =
    column(name) match
      case Column.Hrfs(v) => v
      case other          => throw new IllegalArgumentException(s"Column '$name' is not HRF: $other")

  def hrfsEither(name: String): Either[DesignError, Vector[Hrf]] =
    columnEither(name).flatMap {
      case Column.Hrfs(v) => Right(v)
      case other          => Left(DesignError.InvalidColumnType(name, "HRF", other.typeName))
    }

  def hrfsById(id: ColumnId): Either[DesignError, Vector[Hrf]] =
    hrfsEither(id.value)

  def doubleLists(name: String): Vector[Vector[Double]] =
    column(name) match
      case Column.DoubleLists(v) => v
      case other                 => throw new IllegalArgumentException(s"Column '$name' is not a list of numeric vectors: $other")

  def doubleListsEither(name: String): Either[DesignError, Vector[Vector[Double]]] =
    columnEither(name).flatMap {
      case Column.DoubleLists(v) => Right(v)
      case other                 => Left(DesignError.InvalidColumnType(name, "numeric-list", other.typeName))
    }

  def doubleListsById(id: ColumnId): Either[DesignError, Vector[Vector[Double]]] =
    doubleListsEither(id.value)

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
