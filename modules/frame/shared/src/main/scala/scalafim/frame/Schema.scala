package scalafim.frame

import scala.NamedTuple

opaque type ColumnId = String

object ColumnId:
  private[frame] def derived(name: String): ColumnId = s"column:$name"

  extension (id: ColumnId)
    def value: String = id

opaque type ExprId = String

object ExprId:
  private[frame] def derived(description: String): ExprId = s"expr:$description"

  extension (id: ExprId)
    def value: String = id

opaque type SourceId = String

object SourceId:
  private[frame] def unsafe(value: String): SourceId = value

  extension (id: SourceId)
    def value: String = id

enum DataType:
  case Bool
  case Int32
  case Int64
  case Float32
  case Float64
  case Utf8
  case Timestamp(unit: TimeUnit)

enum TimeUnit:
  case Second
  case Millisecond
  case Microsecond
  case Nanosecond

opaque type TimestampMicros = Long

object TimestampMicros:
  def apply(value: Long): TimestampMicros = value

  extension (value: TimestampMicros)
    def toLong: Long = value

final case class Field private[frame] (
    id: ColumnId,
    name: String,
    dataType: DataType,
    nullable: Boolean
)

enum SchemaError:
  case EmptyFieldName(index: Int)
  case DuplicateFieldName(name: String)

  def message: String = this match
    case EmptyFieldName(index) => s"field at index $index has an empty name"
    case DuplicateFieldName(name) => s"field '$name' occurs more than once"

final class Schema private (val fields: Vector[Field]):
  def size: Int = fields.size

  def field(name: String): Option[Field] = fields.find(_.name == name)

  override def equals(other: Any): Boolean = other match
    case that: Schema => fields == that.fields
    case _ => false

  override def hashCode(): Int = fields.hashCode()

  override def toString: String =
    fields
      .map: field =>
        val suffix = if field.nullable then "?" else ""
        s"${field.name}: ${field.dataType}$suffix"
      .mkString("Schema(", ", ", ")")

object Schema:
  def apply(fields: Vector[Field]): Either[SchemaError, Schema] =
    fields.zipWithIndex.find(_._1.name.trim.isEmpty) match
      case Some((_, index)) => Left(SchemaError.EmptyFieldName(index))
      case None =>
        fields
          .groupMapReduce(_.name)(_ => 1)(_ + _)
          .collectFirst { case (name, count) if count > 1 => name } match
          case Some(name) => Left(SchemaError.DuplicateFieldName(name))
          case None => Right(new Schema(fields))

  private[frame] def unsafe(fields: Vector[Field]): Schema =
    apply(fields).fold(error => throw new IllegalArgumentException(error.message), identity)

trait ColumnType[A]:
  def dataType: DataType
  def nullable: Boolean
  private[frame] def literal(value: A): LiteralValue

object ColumnType:
  given ColumnType[Boolean] with
    val dataType = DataType.Bool
    val nullable = false
    private[frame] def literal(value: Boolean) = LiteralValue.Bool(value)

  given ColumnType[Int] with
    val dataType = DataType.Int32
    val nullable = false
    private[frame] def literal(value: Int) = LiteralValue.Int32(value)

  given ColumnType[Long] with
    val dataType = DataType.Int64
    val nullable = false
    private[frame] def literal(value: Long) = LiteralValue.Int64(value)

  given ColumnType[Float] with
    val dataType = DataType.Float32
    val nullable = false
    private[frame] def literal(value: Float) = LiteralValue.Float32(value)

  given ColumnType[Double] with
    val dataType = DataType.Float64
    val nullable = false
    private[frame] def literal(value: Double) = LiteralValue.Float64(value)

  given ColumnType[String] with
    val dataType = DataType.Utf8
    val nullable = false
    private[frame] def literal(value: String) = LiteralValue.Utf8(value)

  given ColumnType[TimestampMicros] with
    val dataType = DataType.Timestamp(TimeUnit.Microsecond)
    val nullable = false
    private[frame] def literal(value: TimestampMicros) =
      LiteralValue.Timestamp(value.toLong, TimeUnit.Microsecond)

  given [A](using value: ColumnType[A]): ColumnType[Option[A]] with
    val dataType = value.dataType
    val nullable = true
    private[frame] def literal(input: Option[A]) = input match
      case Some(actual) => value.literal(actual)
      case None => LiteralValue.Null(value.dataType)

private[frame] trait SchemaFields[Names <: Tuple, Values <: Tuple]:
  def fields: Vector[Field]

private[frame] object SchemaFields:
  given SchemaFields[EmptyTuple, EmptyTuple] with
    val fields = Vector.empty

  given [
      Name <: String & Singleton,
      Names <: Tuple,
      Value,
      Values <: Tuple
  ](using
      name: ValueOf[Name],
      value: ColumnType[Value],
      tail: SchemaFields[Names, Values]
  ): SchemaFields[Name *: Names, Value *: Values] with
    val fields = Field(
      ColumnId.derived(name.value),
      name.value,
      value.dataType,
      value.nullable
    ) +: tail.fields

trait SchemaDescriptor[S <: NamedTuple.AnyNamedTuple]:
  def schema: Schema

object SchemaDescriptor:
  given derived[S <: NamedTuple.AnyNamedTuple](using
      fields: SchemaFields[NamedTuple.Names[S], NamedTuple.DropNames[S]]
  ): SchemaDescriptor[S] with
    val schema = Schema.unsafe(fields.fields)

private[frame] trait ColumnAt[
    Names <: Tuple,
    Values <: Tuple,
    Name <: String
]:
  def index: Int

private[frame] object ColumnAt:
  given head[
      Name <: String,
      Names <: Tuple,
      Value,
      Values <: Tuple
  ]: ColumnAt[Name *: Names, Value *: Values, Name] with
    val index = 0

  given tail[
      Head <: String,
      Names <: Tuple,
      Value,
      Values <: Tuple,
      Name <: String
  ](using
      notSame: scala.util.NotGiven[Head =:= Name],
      next: ColumnAt[Names, Values, Name]
  ): ColumnAt[Head *: Names, Value *: Values, Name] with
    val index = next.index + 1

type FieldType[
    Names <: Tuple,
    Values <: Tuple,
    Name <: String
] = (Names, Values) match
  case (Name *: names, value *: values) => value
  case (_ *: names, _ *: values) => FieldType[names, values, Name]

type SchemaFieldType[S <: NamedTuple.AnyNamedTuple, Name <: String] =
  FieldType[NamedTuple.Names[S], NamedTuple.DropNames[S], Name]

type SelectionNames[Expressions <: Tuple] <: Tuple = Expressions match
  case EmptyTuple => EmptyTuple
  case NamedExpr[name, value] *: tail => name *: SelectionNames[tail]

type SelectionValues[Expressions <: Tuple] <: Tuple = Expressions match
  case EmptyTuple => EmptyTuple
  case NamedExpr[name, value] *: tail => value *: SelectionValues[tail]

type SelectedSchema[Expressions <: Tuple] = NamedTuple.NamedTuple[
  SelectionNames[Expressions],
  SelectionValues[Expressions]
]

type AppendedSchema[S <: NamedTuple.AnyNamedTuple, Name <: String, Value] = NamedTuple.NamedTuple[
  Tuple.Concat[NamedTuple.Names[S], Name *: EmptyTuple],
  Tuple.Concat[NamedTuple.DropNames[S], Value *: EmptyTuple]
]

type ConcatSchema[
    Left <: NamedTuple.AnyNamedTuple,
    Right <: NamedTuple.AnyNamedTuple
] = NamedTuple.NamedTuple[
  Tuple.Concat[NamedTuple.Names[Left], NamedTuple.Names[Right]],
  Tuple.Concat[NamedTuple.DropNames[Left], NamedTuple.DropNames[Right]]
]

type Nullable[A] = A match
  case Option[value] => Option[value]
  case _ => Option[A]

type NullableTuple[Values <: Tuple] <: Tuple = Values match
  case EmptyTuple => EmptyTuple
  case head *: tail => Nullable[head] *: NullableTuple[tail]

type LeftJoinSchema[
    Left <: NamedTuple.AnyNamedTuple,
    Right <: NamedTuple.AnyNamedTuple
] = NamedTuple.NamedTuple[
  Tuple.Concat[NamedTuple.Names[Left], NamedTuple.Names[Right]],
  Tuple.Concat[NamedTuple.DropNames[Left], NullableTuple[NamedTuple.DropNames[Right]]]
]

type RemoveFieldNames[
    Names <: Tuple,
    Values <: Tuple,
    Name <: String
] <: Tuple = (Names, Values) match
  case (Name *: names, _ *: values) => names
  case (head *: names, _ *: values) =>
    head *: RemoveFieldNames[names, values, Name]

type RemoveFieldValues[
    Names <: Tuple,
    Values <: Tuple,
    Name <: String
] <: Tuple = (Names, Values) match
  case (Name *: names, _ *: values) => values
  case (_ *: names, head *: values) =>
    head *: RemoveFieldValues[names, values, Name]

type UsingJoinSchema[
    Left <: NamedTuple.AnyNamedTuple,
    Right <: NamedTuple.AnyNamedTuple,
    Name <: String
] = NamedTuple.NamedTuple[
  Tuple.Concat[
    NamedTuple.Names[Left],
    RemoveFieldNames[NamedTuple.Names[Right], NamedTuple.DropNames[Right], Name]
  ],
  Tuple.Concat[
    NamedTuple.DropNames[Left],
    RemoveFieldValues[NamedTuple.Names[Right], NamedTuple.DropNames[Right], Name]
  ]
]

type LeftUsingJoinSchema[
    Left <: NamedTuple.AnyNamedTuple,
    Right <: NamedTuple.AnyNamedTuple,
    Name <: String
] = NamedTuple.NamedTuple[
  Tuple.Concat[
    NamedTuple.Names[Left],
    RemoveFieldNames[NamedTuple.Names[Right], NamedTuple.DropNames[Right], Name]
  ],
  Tuple.Concat[
    NamedTuple.DropNames[Left],
    NullableTuple[
      RemoveFieldValues[NamedTuple.Names[Right], NamedTuple.DropNames[Right], Name]
    ]
  ]
]

private[frame] trait DisjointNames[
    LeftNames <: Tuple,
    RightNames <: Tuple,
    RightValues <: Tuple
]

private[frame] object DisjointNames:
  given [RightNames <: Tuple, RightValues <: Tuple]:
      DisjointNames[EmptyTuple, RightNames, RightValues] with {}

  given [
      Head <: String,
      Tail <: Tuple,
      RightNames <: Tuple,
      RightValues <: Tuple
  ](using
      absent: scala.util.NotGiven[ColumnAt[RightNames, RightValues, Head]],
      next: DisjointNames[Tail, RightNames, RightValues]
  ): DisjointNames[Head *: Tail, RightNames, RightValues] with {}
