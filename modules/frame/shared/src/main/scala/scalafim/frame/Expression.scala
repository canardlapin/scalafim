package scalafim.frame

import scala.NamedTuple

private[frame] enum InputRef:
  case Current
  case Left
  case Right

  def qualifier: String = this match
    case Current => "current"
    case Left => "left"
    case Right => "right"

enum UnaryOperator:
  case IsNull
  case IsTrue
  case Negate

enum BinaryOperator:
  case Equal
  case NullSafeEqual
  case NotEqual
  case LessThan
  case LessThanOrEqual
  case GreaterThan
  case GreaterThanOrEqual
  case Add
  case Subtract
  case Multiply
  case Divide
  case And
  case Or

enum LiteralValue:
  case Null(dataType: DataType)
  case Bool(value: Boolean)
  case Int32(value: Int)
  case Int64(value: Long)
  case Float32(value: Float)
  case Float64(value: Double)
  case Utf8(value: String)
  case Timestamp(value: Long, unit: TimeUnit)

private[frame] enum ExprNode:
  case Column(input: InputRef, id: ColumnId, name: String, index: Int)
  case Literal(value: LiteralValue)
  case Unary(operator: UnaryOperator, input: ResolvedExpr)
  case Binary(operator: BinaryOperator, left: ResolvedExpr, right: ResolvedExpr)

private[frame] final case class ResolvedExpr(
    id: ExprId,
    dataType: DataType,
    nullable: Boolean,
    node: ExprNode
)

type ComparisonResult[A] = A match
  case Option[value] => Option[Boolean]
  case _ => Boolean

final class Expr[A] private[frame] (private[frame] val resolved: ResolvedExpr):
  def id: ExprId = resolved.id
  def dataType: DataType = resolved.dataType
  def nullable: Boolean = resolved.nullable

  def as[Name <: String & Singleton](name: Name): NamedExpr[Name, A] =
    NamedExpr(name, this)

  def ===(other: Expr[A]): Expr[ComparisonResult[A]] =
    Expr.comparison(BinaryOperator.Equal, this, other)

  def =!=(other: Expr[A]): Expr[ComparisonResult[A]] =
    Expr.comparison(BinaryOperator.NotEqual, this, other)

  def nullSafeEq(other: Expr[A]): Expr[Boolean] =
    Expr.nullSafeComparison(this, other)

  def <(other: Expr[A])(using Ordering[A]): Expr[ComparisonResult[A]] =
    Expr.comparison(BinaryOperator.LessThan, this, other)

  def <=(other: Expr[A])(using Ordering[A]): Expr[ComparisonResult[A]] =
    Expr.comparison(BinaryOperator.LessThanOrEqual, this, other)

  def >(other: Expr[A])(using Ordering[A]): Expr[ComparisonResult[A]] =
    Expr.comparison(BinaryOperator.GreaterThan, this, other)

  def >=(other: Expr[A])(using Ordering[A]): Expr[ComparisonResult[A]] =
    Expr.comparison(BinaryOperator.GreaterThanOrEqual, this, other)

  def +(other: Expr[A])(using Numeric[A]): Expr[A] =
    Expr.sameType(BinaryOperator.Add, this, other)

  def -(other: Expr[A])(using Numeric[A]): Expr[A] =
    Expr.sameType(BinaryOperator.Subtract, this, other)

  def *(other: Expr[A])(using Numeric[A]): Expr[A] =
    Expr.sameType(BinaryOperator.Multiply, this, other)

  def /(other: Expr[A])(using Fractional[A]): Expr[A] =
    Expr.sameType(BinaryOperator.Divide, this, other)

  def isNull: Expr[Boolean] =
    Expr.unary(UnaryOperator.IsNull, this, DataType.Bool, nullable = false)

object Expr:
  def literal[A](value: A)(using columnType: ColumnType[A]): Expr[A] =
    val literal = columnType.literal(value)
    val id = ExprId.derived(s"literal:$literal")
    new Expr(ResolvedExpr(id, columnType.dataType, columnType.nullable, ExprNode.Literal(literal)))

  private def binaryId(operator: BinaryOperator, left: ResolvedExpr, right: ResolvedExpr): ExprId =
    ExprId.derived(s"$operator(${left.id.value},${right.id.value})")

  private[frame] def unary[A, B](
      operator: UnaryOperator,
      input: Expr[A],
      dataType: DataType,
      nullable: Boolean
  ): Expr[B] =
    val resolved = input.resolved
    new Expr(
      ResolvedExpr(
        ExprId.derived(s"$operator(${resolved.id.value})"),
        dataType,
        nullable,
        ExprNode.Unary(operator, resolved)
      )
    )

  private def comparison[A](
      operator: BinaryOperator,
      left: Expr[A],
      right: Expr[A]
  ): Expr[ComparisonResult[A]] =
    new Expr(
      ResolvedExpr(
        binaryId(operator, left.resolved, right.resolved),
        DataType.Bool,
        left.nullable || right.nullable,
        ExprNode.Binary(operator, left.resolved, right.resolved)
      )
    )

  private def sameType[A](operator: BinaryOperator, left: Expr[A], right: Expr[A]): Expr[A] =
    new Expr(
      ResolvedExpr(
        binaryId(operator, left.resolved, right.resolved),
        left.dataType,
        left.nullable || right.nullable,
        ExprNode.Binary(operator, left.resolved, right.resolved)
      )
    )

  private def nullSafeComparison[A](left: Expr[A], right: Expr[A]): Expr[Boolean] =
    new Expr(
      ResolvedExpr(
        binaryId(BinaryOperator.NullSafeEqual, left.resolved, right.resolved),
        DataType.Bool,
        nullable = false,
        ExprNode.Binary(BinaryOperator.NullSafeEqual, left.resolved, right.resolved)
      )
    )

  private[frame] def booleanBinary(
      operator: BinaryOperator,
      left: Expr[Boolean],
      right: Expr[Boolean]
  ): Expr[Boolean] =
    new Expr(
      ResolvedExpr(
        binaryId(operator, left.resolved, right.resolved),
        DataType.Bool,
        nullable = false,
        ExprNode.Binary(operator, left.resolved, right.resolved)
      )
    )

extension (expression: Expr[Boolean])
  def &&(other: Expr[Boolean]): Expr[Boolean] =
    Expr.booleanBinary(BinaryOperator.And, expression, other)

  def ||(other: Expr[Boolean]): Expr[Boolean] =
    Expr.booleanBinary(BinaryOperator.Or, expression, other)

extension (expression: Expr[Option[Boolean]])
  def isTrue: Expr[Boolean] =
    Expr.unary(UnaryOperator.IsTrue, expression, DataType.Bool, nullable = false)

final case class NamedExpr[Name <: String, A](name: Name, expression: Expr[A])

private[frame] final case class NamedExpression(name: String, expression: ResolvedExpr)

private[frame] trait ExpressionSelection[Expressions <: Tuple]:
  def expressions(value: Expressions): Vector[NamedExpression]

private[frame] object ExpressionSelection:
  given ExpressionSelection[EmptyTuple] with
    def expressions(value: EmptyTuple): Vector[NamedExpression] = Vector.empty

  given [
      Name <: String,
      Value,
      Tail <: Tuple
  ](using tail: ExpressionSelection[Tail]): ExpressionSelection[NamedExpr[Name, Value] *: Tail] with
    def expressions(value: NamedExpr[Name, Value] *: Tail): Vector[NamedExpression] =
      NamedExpression(value.head.name, value.head.expression.resolved) +: tail.expressions(value.tail)

final class Scope[S <: NamedTuple.AnyNamedTuple] private[frame] (
    input: InputRef,
    schema: Schema
):
  def col[Name <: String & Singleton](name: Name)(using
      at: ColumnAt[NamedTuple.Names[S], NamedTuple.DropNames[S], Name],
      columnType: ColumnType[SchemaFieldType[S, Name]]
  ): Expr[SchemaFieldType[S, Name]] =
    val field = schema.fields(at.index)
    val resolved = ResolvedExpr(
      ExprId.derived(s"column:${input.qualifier}:${field.id.value}"),
      columnType.dataType,
      columnType.nullable,
      ExprNode.Column(input, field.id, field.name, at.index)
    )
    new Expr(resolved)

object Scope:
  private[frame] def current[S <: NamedTuple.AnyNamedTuple](schema: Schema): Scope[S] =
    new Scope(InputRef.Current, schema)

  private[frame] def left[S <: NamedTuple.AnyNamedTuple](schema: Schema): Scope[S] =
    new Scope(InputRef.Left, schema)

  private[frame] def right[S <: NamedTuple.AnyNamedTuple](schema: Schema): Scope[S] =
    new Scope(InputRef.Right, schema)

private[frame] enum AggregateNode:
  case Count
  case Sum(input: ResolvedExpr)
  case Mean(input: ResolvedExpr)
  case Variance(input: ResolvedExpr)
  case Min(input: ResolvedExpr)
  case Max(input: ResolvedExpr)

private[frame] final case class ResolvedAggregate(
    dataType: DataType,
    nullable: Boolean,
    node: AggregateNode
)

final class AggregateExpr[A] private[frame] (private[frame] val resolved: ResolvedAggregate):
  def as[Name <: String & Singleton](name: Name): NamedAggregate[Name, A] =
    NamedAggregate(name, this)

final case class NamedAggregate[Name <: String, A](name: Name, expression: AggregateExpr[A])

object Aggregate:
  val count: AggregateExpr[Long] =
    new AggregateExpr(ResolvedAggregate(DataType.Int64, nullable = false, AggregateNode.Count))

  def sum[A](expression: Expr[A])(using NumericColumn[A]): AggregateExpr[A] =
    new AggregateExpr(ResolvedAggregate(expression.dataType, expression.nullable, AggregateNode.Sum(expression.resolved)))

  def mean[A](expression: Expr[A])(using NumericColumn[A]): AggregateExpr[MeanResult[A]] =
    new AggregateExpr(
      ResolvedAggregate(DataType.Float64, expression.nullable, AggregateNode.Mean(expression.resolved))
    )

  def variance[A](expression: Expr[A])(using NumericColumn[A]): AggregateExpr[MeanResult[A]] =
    new AggregateExpr(
      ResolvedAggregate(DataType.Float64, expression.nullable, AggregateNode.Variance(expression.resolved))
    )

  def min[A](expression: Expr[A])(using Ordering[A]): AggregateExpr[A] =
    new AggregateExpr(ResolvedAggregate(expression.dataType, expression.nullable, AggregateNode.Min(expression.resolved)))

  def max[A](expression: Expr[A])(using Ordering[A]): AggregateExpr[A] =
    new AggregateExpr(ResolvedAggregate(expression.dataType, expression.nullable, AggregateNode.Max(expression.resolved)))

trait NumericColumn[A]

object NumericColumn:
  given NumericColumn[Int] with {}
  given NumericColumn[Long] with {}
  given NumericColumn[Float] with {}
  given NumericColumn[Double] with {}
  given [A](using NumericColumn[A]): NumericColumn[Option[A]] with {}

type MeanResult[A] = A match
  case Option[value] => Option[Double]
  case _ => Double

private[frame] final case class NamedAggregateExpression(
    name: String,
    expression: ResolvedAggregate
)

private[frame] trait AggregateSelection[Expressions <: Tuple]:
  def expressions(value: Expressions): Vector[NamedAggregateExpression]

private[frame] object AggregateSelection:
  given AggregateSelection[EmptyTuple] with
    def expressions(value: EmptyTuple): Vector[NamedAggregateExpression] = Vector.empty

  given [
      Name <: String,
      Value,
      Tail <: Tuple
  ](using tail: AggregateSelection[Tail]): AggregateSelection[NamedAggregate[Name, Value] *: Tail] with
    def expressions(value: NamedAggregate[Name, Value] *: Tail): Vector[NamedAggregateExpression] =
      NamedAggregateExpression(value.head.name, value.head.expression.resolved) +: tail.expressions(value.tail)

type AggregateNames[Expressions <: Tuple] <: Tuple = Expressions match
  case EmptyTuple => EmptyTuple
  case NamedAggregate[name, value] *: tail => name *: AggregateNames[tail]

type AggregateValues[Expressions <: Tuple] <: Tuple = Expressions match
  case EmptyTuple => EmptyTuple
  case NamedAggregate[name, value] *: tail => value *: AggregateValues[tail]

type AggregateSchema[Expressions <: Tuple] = NamedTuple.NamedTuple[
  AggregateNames[Expressions],
  AggregateValues[Expressions]
]
