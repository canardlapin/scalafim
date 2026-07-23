package scalafim.frame

import scala.NamedTuple
import scala.util.NotGiven

enum BindingIssue:
  case FieldCount(expected: Int, actual: Int)
  case FieldName(index: Int, expected: String, actual: String)
  case FieldType(index: Int, expected: DataType, actual: DataType)
  case FieldNullability(index: Int, expected: Boolean, actual: Boolean)

  def message: String = this match
    case FieldCount(expected, actual) => s"expected $expected fields but found $actual"
    case FieldName(index, expected, actual) =>
      s"field $index is named '$actual'; expected '$expected'"
    case FieldType(index, expected, actual) =>
      s"field $index has type $actual; expected $expected"
    case FieldNullability(index, expected, actual) =>
      s"field $index nullable=$actual; expected nullable=$expected"

enum FrameError:
  case InvalidSourceId(id: String)
  case InvalidSourceName(name: String)
  case InvalidSchema(error: SchemaError)
  case SchemaMismatch(issues: Vector[BindingIssue])
  case ColumnNotFound(name: String)
  case ColumnCollision(name: String)
  case InvalidColumnReference(name: String, index: Int)
  case ExpressionType(expected: DataType, actual: DataType)
  case NullablePredicate(id: ExprId)
  case InvalidExpressionScope(id: ExprId)
  case InvalidLimit(count: Int)
  case EmptySort
  case EmptyJoinKeys
  case DuplicateJoinKey(name: String)
  case JoinKeyType(name: String, left: DataType, right: DataType)

  def message: String = this match
    case InvalidSourceId(id) => s"source id '$id' is empty"
    case InvalidSourceName(name) => s"source name '$name' is empty"
    case InvalidSchema(error) => error.message
    case SchemaMismatch(issues) => issues.map(_.message).mkString("schema mismatch: ", "; ", "")
    case ColumnNotFound(name) => s"column '$name' does not exist"
    case ColumnCollision(name) => s"column '$name' occurs on both join sides"
    case InvalidColumnReference(name, index) =>
      s"column reference '$name' at index $index is not valid in this scope"
    case ExpressionType(expected, actual) =>
      s"expression has type $actual; expected $expected"
    case NullablePredicate(id) => s"predicate ${id.value} is nullable; make it total before filtering"
    case InvalidExpressionScope(id) => s"expression ${id.value} references a different input scope"
    case InvalidLimit(count) => s"limit must be non-negative, found $count"
    case EmptySort => "sort requires at least one expression"
    case EmptyJoinKeys => "using join requires at least one key"
    case DuplicateJoinKey(name) => s"using join key '$name' occurs more than once"
    case JoinKeyType(name, left, right) =>
      s"using join key '$name' has incompatible types $left and $right"

private object ExpressionValidation:
  private def validateColumn(
      input: InputRef,
      id: ColumnId,
      name: String,
      index: Int,
      expectedInput: InputRef,
      schema: Schema
  ): Either[FrameError, Unit] =
    if input != expectedInput then Left(FrameError.InvalidExpressionScope(ExprId.derived(name)))
    else
      schema.fields.lift(index) match
        case Some(field) if field.id == id && field.name == name => Right(())
        case _ => Left(FrameError.InvalidColumnReference(name, index))

  private def loop(
      expression: ResolvedExpr,
      expectedInput: InputRef,
      schema: Schema
  ): Either[FrameError, Unit] = expression.node match
    case ExprNode.Column(input, id, name, index) =>
      validateColumn(input, id, name, index, expectedInput, schema)
    case ExprNode.Literal(_) => Right(())
    case ExprNode.Unary(_, input) => loop(input, expectedInput, schema)
    case ExprNode.Binary(_, left, right) =>
      loop(left, expectedInput, schema).flatMap(_ => loop(right, expectedInput, schema))

  def current(expression: ResolvedExpr, schema: Schema): Either[FrameError, Unit] =
    loop(expression, InputRef.Current, schema)

  def join(
      expression: ResolvedExpr,
      left: Schema,
      right: Schema
  ): Either[FrameError, Unit] =
    def joined(current: ResolvedExpr): Either[FrameError, Unit] = current.node match
      case ExprNode.Column(InputRef.Left, id, name, index) =>
        validateColumn(InputRef.Left, id, name, index, InputRef.Left, left)
      case ExprNode.Column(InputRef.Right, id, name, index) =>
        validateColumn(InputRef.Right, id, name, index, InputRef.Right, right)
      case ExprNode.Column(_, _, _, _) => Left(FrameError.InvalidExpressionScope(current.id))
      case ExprNode.Literal(_) => Right(())
      case ExprNode.Unary(_, input) => joined(input)
      case ExprNode.Binary(_, lhs, rhs) => joined(lhs).flatMap(_ => joined(rhs))
    joined(expression)

final class DynamicExpr private[frame] (private[frame] val resolved: ResolvedExpr):
  def id: ExprId = resolved.id
  def dataType: DataType = resolved.dataType
  def nullable: Boolean = resolved.nullable

  def ===(other: DynamicExpr): Either[FrameError, DynamicExpr] =
    if dataType != other.dataType then Left(FrameError.ExpressionType(dataType, other.dataType))
    else
      val combined = ResolvedExpr(
        ExprId.derived(s"Equal(${id.value},${other.id.value})"),
        DataType.Bool,
        nullable || other.nullable,
        ExprNode.Binary(BinaryOperator.Equal, resolved, other.resolved)
      )
      Right(new DynamicExpr(combined))

  def isTrue: Either[FrameError, DynamicExpr] =
    if dataType != DataType.Bool then Left(FrameError.ExpressionType(DataType.Bool, dataType))
    else
      Right:
        new DynamicExpr(
          ResolvedExpr(
            ExprId.derived(s"IsTrue(${id.value})"),
            DataType.Bool,
            nullable = false,
            ExprNode.Unary(UnaryOperator.IsTrue, resolved)
          )
        )

object DynamicExpr:
  def literal(value: LiteralValue): DynamicExpr =
    val (dataType, nullable) = value match
      case LiteralValue.Null(dataType) => (dataType, true)
      case LiteralValue.Bool(_) => (DataType.Bool, false)
      case LiteralValue.Int32(_) => (DataType.Int32, false)
      case LiteralValue.Int64(_) => (DataType.Int64, false)
      case LiteralValue.Float32(_) => (DataType.Float32, false)
      case LiteralValue.Float64(_) => (DataType.Float64, false)
      case LiteralValue.Utf8(_) => (DataType.Utf8, false)
      case LiteralValue.Timestamp(_, unit) => (DataType.Timestamp(unit), false)
    new DynamicExpr(
      ResolvedExpr(
        ExprId.derived(s"literal:$value"),
        dataType,
        nullable,
        ExprNode.Literal(value)
      )
    )

final class DynamicAggregate private[frame] (
    private[frame] val resolved: ResolvedAggregate
)

object DynamicAggregate:
  val count: DynamicAggregate =
    new DynamicAggregate(
      ResolvedAggregate(DataType.Int64, nullable = false, AggregateNode.Count)
    )

  def sum(expression: DynamicExpr): Either[FrameError, DynamicAggregate] =
    numeric(expression).map: _ =>
      new DynamicAggregate(
        ResolvedAggregate(
          expression.dataType,
          expression.nullable,
          AggregateNode.Sum(expression.resolved)
        )
      )

  def mean(expression: DynamicExpr): Either[FrameError, DynamicAggregate] =
    numeric(expression).map: _ =>
      new DynamicAggregate(
        ResolvedAggregate(
          DataType.Float64,
          expression.nullable,
          AggregateNode.Mean(expression.resolved)
        )
      )

  def variance(expression: DynamicExpr): Either[FrameError, DynamicAggregate] =
    numeric(expression).map: _ =>
      new DynamicAggregate(
        ResolvedAggregate(
          DataType.Float64,
          expression.nullable,
          AggregateNode.Variance(expression.resolved)
        )
      )

  def min(expression: DynamicExpr): DynamicAggregate =
    new DynamicAggregate(
      ResolvedAggregate(
        expression.dataType,
        expression.nullable,
        AggregateNode.Min(expression.resolved)
      )
    )

  def max(expression: DynamicExpr): DynamicAggregate =
    new DynamicAggregate(
      ResolvedAggregate(
        expression.dataType,
        expression.nullable,
        AggregateNode.Max(expression.resolved)
      )
    )

  private def numeric(expression: DynamicExpr): Either[FrameError, Unit] =
    expression.dataType match
      case DataType.Int32 | DataType.Int64 | DataType.Float32 | DataType.Float64 =>
        Right(())
      case other => Left(FrameError.ExpressionType(DataType.Float64, other))

final class DynamicScope private[frame] (
    schema: Schema,
    input: InputRef
):
  def col(name: String): Either[FrameError, DynamicExpr] =
    val index = schema.fields.indexWhere(_.name == name)
    if index < 0 then Left(FrameError.ColumnNotFound(name))
    else
      val field = schema.fields(index)
      Right:
        new DynamicExpr(
          ResolvedExpr(
            ExprId.derived(s"column:${input.qualifier}:${field.id.value}"),
            field.dataType,
            field.nullable,
            ExprNode.Column(input, field.id, field.name, index)
          )
        )

final class DynamicFrame private[frame] (
    val plan: LogicalPlan,
    val schema: Schema
):
  def col(name: String): Either[FrameError, DynamicExpr] =
    new DynamicScope(schema, InputRef.Current).col(name)

  def filter(predicate: DynamicExpr): Either[FrameError, DynamicFrame] =
    if predicate.dataType != DataType.Bool then
      Left(FrameError.ExpressionType(DataType.Bool, predicate.dataType))
    else if predicate.nullable then Left(FrameError.NullablePredicate(predicate.id))
    else
      ExpressionValidation.current(predicate.resolved, schema).map: _ =>
        new DynamicFrame(LogicalPlan.Filter(plan, predicate.resolved, schema), schema)

  def select(expressions: (String, DynamicExpr)*): Either[FrameError, DynamicFrame] =
    val names = expressions.map(_._1).toVector
    names.groupMapReduce(identity)(_ => 1)(_ + _).collectFirst:
      case (name, count) if count > 1 => name
    match
      case Some(name) => Left(FrameError.ColumnCollision(name))
      case None =>
        val validated = expressions.foldLeft[Either[FrameError, Unit]](Right(())):
          case (result, (_, expression)) =>
            result.flatMap(_ => ExpressionValidation.current(expression.resolved, schema))
        validated.flatMap: _ =>
          Schema(
            expressions.toVector.map: (name, expression) =>
              Field(ColumnId.derived(name), name, expression.dataType, expression.nullable)
          ).left.map(FrameError.InvalidSchema.apply).map: output =>
            val selected = expressions.toVector.map((name, expression) => NamedExpression(name, expression.resolved))
            new DynamicFrame(LogicalPlan.Project(plan, selected, output), output)

  def withColumn(name: String, expression: DynamicExpr): Either[FrameError, DynamicFrame] =
    if schema.field(name).nonEmpty then Left(FrameError.ColumnCollision(name))
    else
      ExpressionValidation.current(expression.resolved, schema).flatMap: _ =>
        val retained = schema.fields.zipWithIndex.map: (field, index) =>
          NamedExpression(
            field.name,
            ResolvedExpr(
              ExprId.derived(s"column:current:${field.id.value}"),
              field.dataType,
              field.nullable,
              ExprNode.Column(InputRef.Current, field.id, field.name, index)
            )
          )
        Schema(
          schema.fields :+ Field(
            ColumnId.derived(name),
            name,
            expression.dataType,
            expression.nullable
          )
        ).left.map(FrameError.InvalidSchema.apply).map: output =>
          new DynamicFrame(
            LogicalPlan.Project(
              plan,
              retained :+ NamedExpression(name, expression.resolved),
              output
            ),
            output
          )

  def groupBy(expressions: (String, DynamicExpr)*): Either[FrameError, DynamicGroupedFrame] =
    val names = expressions.map(_._1).toVector
    names.groupMapReduce(identity)(_ => 1)(_ + _).collectFirst:
      case (name, count) if count > 1 => name
    match
      case Some(name) => Left(FrameError.ColumnCollision(name))
      case None =>
        val validated = expressions.foldLeft[Either[FrameError, Unit]](Right(())):
          case (result, (_, expression)) =>
            result.flatMap(_ => ExpressionValidation.current(expression.resolved, schema))
        validated.map: _ =>
          new DynamicGroupedFrame(
            this,
            expressions.toVector.map: (name, expression) =>
              NamedExpression(name, expression.resolved)
          )

  def innerJoin(right: DynamicFrame)(
      condition: (DynamicScope, DynamicScope) => Either[FrameError, DynamicExpr]
  ): Either[FrameError, DynamicFrame] =
    join(right, JoinKind.Inner, condition)

  def leftJoin(right: DynamicFrame)(
      condition: (DynamicScope, DynamicScope) => Either[FrameError, DynamicExpr]
  ): Either[FrameError, DynamicFrame] =
    join(right, JoinKind.LeftOuter, condition)

  def innerJoinUsing(
      right: DynamicFrame,
      first: String,
      rest: String*
  ): Either[FrameError, DynamicFrame] =
    JoinPlanning.using(this, right, JoinKind.Inner, (first +: rest).toVector)

  def leftJoinUsing(
      right: DynamicFrame,
      first: String,
      rest: String*
  ): Either[FrameError, DynamicFrame] =
    JoinPlanning.using(this, right, JoinKind.LeftOuter, (first +: rest).toVector)

  private def join(
      right: DynamicFrame,
      kind: JoinKind,
      condition: (DynamicScope, DynamicScope) => Either[FrameError, DynamicExpr]
  ): Either[FrameError, DynamicFrame] =
    schema.fields.map(_.name).find(name => right.schema.field(name).nonEmpty) match
      case Some(name) => Left(FrameError.ColumnCollision(name))
      case None =>
        condition(
          new DynamicScope(schema, InputRef.Left),
          new DynamicScope(right.schema, InputRef.Right)
        ).flatMap: expression =>
          if expression.dataType != DataType.Bool then
            Left(FrameError.ExpressionType(DataType.Bool, expression.dataType))
          else if expression.nullable then Left(FrameError.NullablePredicate(expression.id))
          else
            ExpressionValidation.join(expression.resolved, schema, right.schema).flatMap: _ =>
              val rightFields = right.schema.fields.map: field =>
                if kind == JoinKind.LeftOuter then field.copy(nullable = true) else field
              Schema(schema.fields ++ rightFields)
                .left.map(FrameError.InvalidSchema.apply)
                .map: output =>
                  new DynamicFrame(
                    LogicalPlan.Join(plan, right.plan, kind, expression.resolved, output),
                    output
                  )

  def sortBy(
      expression: DynamicExpr,
      direction: SortDirection = SortDirection.Ascending,
      nulls: NullPlacement = NullPlacement.Last
  ): Either[FrameError, DynamicFrame] =
    ExpressionValidation.current(expression.resolved, schema).map: _ =>
      new DynamicFrame(
        LogicalPlan.Sort(
          plan,
          Vector(SortExpression(expression.resolved, direction, nulls)),
          schema
        ),
        schema
      )

  def limit(count: Int): Either[FrameError, DynamicFrame] =
    if count < 0 then Left(FrameError.InvalidLimit(count))
    else Right(new DynamicFrame(LogicalPlan.Limit(plan, count, schema), schema))

  def typed[S <: NamedTuple.AnyNamedTuple](using descriptor: SchemaDescriptor[S]): Either[FrameError, Frame[S]] =
    val expected = descriptor.schema
    val countIssues =
      if expected.size == schema.size then Vector.empty
      else Vector(BindingIssue.FieldCount(expected.size, schema.size))
    val fieldIssues = expected.fields.zip(schema.fields).zipWithIndex.flatMap:
      case ((expectedField, actualField), index) =>
        Vector(
          Option.when(expectedField.name != actualField.name):
            BindingIssue.FieldName(index, expectedField.name, actualField.name),
          Option.when(expectedField.dataType != actualField.dataType):
            BindingIssue.FieldType(index, expectedField.dataType, actualField.dataType),
          Option.when(expectedField.nullable != actualField.nullable):
            BindingIssue.FieldNullability(index, expectedField.nullable, actualField.nullable)
        ).flatten
    val issues = countIssues ++ fieldIssues
    if issues.isEmpty then Right(new Frame(plan, schema))
    else Left(FrameError.SchemaMismatch(issues))

  def explain: String = LogicalPlan.explain(plan)

final class DynamicGroupedFrame private[frame] (
    input: DynamicFrame,
    keys: Vector[NamedExpression]
):
  def aggregate(
      expressions: (String, DynamicAggregate)*
  ): Either[FrameError, DynamicFrame] =
    val names = keys.map(_.name) ++ expressions.map(_._1)
    names.groupMapReduce(identity)(_ => 1)(_ + _).collectFirst:
      case (name, count) if count > 1 => name
    match
      case Some(name) => Left(FrameError.ColumnCollision(name))
      case None =>
        val fields =
          keys.map: key =>
            Field(
              ColumnId.derived(key.name),
              key.name,
              key.expression.dataType,
              key.expression.nullable
            )
          ++ expressions.toVector.map: (name, aggregate) =>
            Field(
              ColumnId.derived(name),
              name,
              aggregate.resolved.dataType,
              aggregate.resolved.nullable
            )
        Schema(fields)
          .left.map(FrameError.InvalidSchema.apply)
          .map: output =>
            new DynamicFrame(
              LogicalPlan.Aggregate(
                input.plan,
                keys,
                expressions.toVector.map: (name, aggregate) =>
                  NamedAggregateExpression(name, aggregate.resolved),
                output
              ),
              output
            )

object DynamicFrame:
  def scan(reference: SourceRef, fields: Vector[Field]): Either[FrameError, DynamicFrame] =
    Schema(fields)
      .left.map(FrameError.InvalidSchema.apply)
      .map: schema =>
        new DynamicFrame(LogicalPlan.Source(reference, schema), schema)

  def source(name: String, fields: Vector[Field]): Either[FrameError, DynamicFrame] =
    SourceRef.scan(name, name).flatMap(scan(_, fields))

  def field(name: String, dataType: DataType, nullable: Boolean = false): Field =
    Field(ColumnId.derived(name), name, dataType, nullable)

final class Frame[S <: NamedTuple.AnyNamedTuple] private[frame] (
    val plan: LogicalPlan,
    val schema: Schema
):
  def select[Expressions <: Tuple](
      expressions: Scope[S] => Expressions
  )(using
      selection: ExpressionSelection[Expressions],
      output: SchemaDescriptor[SelectedSchema[Expressions]]
  ): Frame[SelectedSchema[Expressions]] =
    val selected = selection.expressions(expressions(Scope.current(schema)))
    new Frame[SelectedSchema[Expressions]](
      LogicalPlan.Project(plan, selected, output.schema),
      output.schema
    )

  def withColumn[Name <: String & Singleton, Value](
      name: Name
  )(
      expression: Scope[S] => Expr[Value]
  )(using
      absent: NotGiven[ColumnAt[NamedTuple.Names[S], NamedTuple.DropNames[S], Name]],
      output: SchemaDescriptor[AppendedSchema[S, Name, Value]]
  ): Frame[AppendedSchema[S, Name, Value]] =
    val scope = Scope.current[S](schema)
    val retained = schema.fields.zipWithIndex.map: (field, index) =>
      NamedExpression(
        field.name,
        ResolvedExpr(
          ExprId.derived(s"column:current:${field.id.value}"),
          field.dataType,
          field.nullable,
          ExprNode.Column(InputRef.Current, field.id, field.name, index)
        )
      )
    val appended = retained :+ NamedExpression(name, expression(scope).resolved)
    new Frame[AppendedSchema[S, Name, Value]](
      LogicalPlan.Project(plan, appended, output.schema),
      output.schema
    )

  def filter(predicate: Scope[S] => Expr[Boolean]): Frame[S] =
    val resolved = predicate(Scope.current(schema)).resolved
    new Frame(LogicalPlan.Filter(plan, resolved, schema), schema)

  def innerJoin[Right <: NamedTuple.AnyNamedTuple](right: Frame[Right])(
      condition: (Scope[S], Scope[Right]) => Expr[Boolean]
  )(using
      disjoint: DisjointNames[
        NamedTuple.Names[S],
        NamedTuple.Names[Right],
        NamedTuple.DropNames[Right]
      ],
      output: SchemaDescriptor[ConcatSchema[S, Right]]
  ): Frame[ConcatSchema[S, Right]] =
    val on = condition(Scope.left(schema), Scope.right(right.schema))
    new Frame[ConcatSchema[S, Right]](
      LogicalPlan.Join(plan, right.plan, JoinKind.Inner, on.resolved, output.schema),
      output.schema
    )

  def leftJoin[Right <: NamedTuple.AnyNamedTuple](right: Frame[Right])(
      condition: (Scope[S], Scope[Right]) => Expr[Boolean]
  )(using
      disjoint: DisjointNames[
        NamedTuple.Names[S],
        NamedTuple.Names[Right],
        NamedTuple.DropNames[Right]
      ],
      output: SchemaDescriptor[LeftJoinSchema[S, Right]]
  ): Frame[LeftJoinSchema[S, Right]] =
    val on = condition(Scope.left(schema), Scope.right(right.schema))
    new Frame[LeftJoinSchema[S, Right]](
      LogicalPlan.Join(plan, right.plan, JoinKind.LeftOuter, on.resolved, output.schema),
      output.schema
    )

  def innerJoinUsing[
      Right <: NamedTuple.AnyNamedTuple,
      Name <: String & Singleton
  ](right: Frame[Right], name: Name)(using
      leftAt: ColumnAt[NamedTuple.Names[S], NamedTuple.DropNames[S], Name],
      rightAt: ColumnAt[NamedTuple.Names[Right], NamedTuple.DropNames[Right], Name],
      same: SchemaFieldType[S, Name] =:= SchemaFieldType[Right, Name],
      disjoint: DisjointNames[
        NamedTuple.Names[S],
        RemoveFieldNames[NamedTuple.Names[Right], NamedTuple.DropNames[Right], Name],
        RemoveFieldValues[NamedTuple.Names[Right], NamedTuple.DropNames[Right], Name]
      ],
      output: SchemaDescriptor[UsingJoinSchema[S, Right, Name]]
  ): Frame[UsingJoinSchema[S, Right, Name]] =
    val dynamic = JoinPlanning
      .using(this.dynamic, right.dynamic, JoinKind.Inner, Vector(name))
      .fold(error => throw new IllegalStateException(error.message), identity)
    new Frame(dynamic.plan, output.schema)

  def leftJoinUsing[
      Right <: NamedTuple.AnyNamedTuple,
      Name <: String & Singleton
  ](right: Frame[Right], name: Name)(using
      leftAt: ColumnAt[NamedTuple.Names[S], NamedTuple.DropNames[S], Name],
      rightAt: ColumnAt[NamedTuple.Names[Right], NamedTuple.DropNames[Right], Name],
      same: SchemaFieldType[S, Name] =:= SchemaFieldType[Right, Name],
      disjoint: DisjointNames[
        NamedTuple.Names[S],
        RemoveFieldNames[NamedTuple.Names[Right], NamedTuple.DropNames[Right], Name],
        RemoveFieldValues[NamedTuple.Names[Right], NamedTuple.DropNames[Right], Name]
      ],
      output: SchemaDescriptor[LeftUsingJoinSchema[S, Right, Name]]
  ): Frame[LeftUsingJoinSchema[S, Right, Name]] =
    val dynamic = JoinPlanning
      .using(this.dynamic, right.dynamic, JoinKind.LeftOuter, Vector(name))
      .fold(error => throw new IllegalStateException(error.message), identity)
    new Frame(dynamic.plan, output.schema)

  def groupBy[Keys <: Tuple](
      keys: Scope[S] => Keys
  )(using selection: ExpressionSelection[Keys]): GroupedFrame[S, Keys] =
    val selected = keys(Scope.current(schema))
    new GroupedFrame(this, selection.expressions(selected))

  def sortBy(first: Scope[S] => Expr[?], rest: (Scope[S] => Expr[?])*): Frame[S] =
    sortBy(SortDirection.Ascending, NullPlacement.Last)(first, rest*)

  def sortBy(
      direction: SortDirection,
      nulls: NullPlacement
  )(
      first: Scope[S] => Expr[?],
      rest: (Scope[S] => Expr[?])*
  ): Frame[S] =
    val scope = Scope.current[S](schema)
    val expressions = (first +: rest)
      .map(build => SortExpression(build(scope).resolved, direction, nulls))
      .toVector
    new Frame(LogicalPlan.Sort(plan, expressions, schema), schema)

  def limit(count: Int): Either[FrameError, Frame[S]] =
    if count < 0 then Left(FrameError.InvalidLimit(count))
    else Right(new Frame(LogicalPlan.Limit(plan, count, schema), schema))

  def dynamic: DynamicFrame = new DynamicFrame(plan, schema)

  def normalized: (Frame[S], NormalizationReceipt) =
    val result = PlanNormalizer.normalize(plan)
    (new Frame(result.plan, schema), result.receipt)

  def explain: String = LogicalPlan.explain(plan)

object Frame:
  def scan[S <: NamedTuple.AnyNamedTuple](reference: SourceRef)(using
      descriptor: SchemaDescriptor[S]
  ): Frame[S] =
    val schema = descriptor.schema
    new Frame(LogicalPlan.Source(reference, schema), schema)

  def values[S <: NamedTuple.AnyNamedTuple](reference: SourceRef)(using
      descriptor: SchemaDescriptor[S]
  ): Either[FrameError, Frame[S]] =
    if reference.kind != SourceKind.Values then
      Left(FrameError.InvalidSourceId(s"${reference.id.value} is not a values reference"))
    else Right(scan(reference))

  def source[S <: NamedTuple.AnyNamedTuple](name: String)(using
      descriptor: SchemaDescriptor[S]
  ): Either[FrameError, Frame[S]] =
    SourceRef.scan(name, name).map(scan(_))

private object JoinPlanning:
  def using(
      left: DynamicFrame,
      right: DynamicFrame,
      kind: JoinKind,
      keys: Vector[String]
  ): Either[FrameError, DynamicFrame] =
    if keys.isEmpty then Left(FrameError.EmptyJoinKeys)
    else
      keys.groupMapReduce(identity)(_ => 1)(_ + _).collectFirst:
        case (name, count) if count > 1 => name
      match
        case Some(name) => Left(FrameError.DuplicateJoinKey(name))
        case None =>
          val keyFields = keys.foldLeft[Either[FrameError, Vector[(Field, Field)]]](Right(Vector.empty)):
            case (result, name) =>
              result.flatMap: fields =>
                (left.schema.field(name), right.schema.field(name)) match
                  case (None, _) => Left(FrameError.ColumnNotFound(name))
                  case (_, None) => Left(FrameError.ColumnNotFound(name))
                  case (Some(lhs), Some(rhs)) if lhs.dataType != rhs.dataType =>
                    Left(FrameError.JoinKeyType(name, lhs.dataType, rhs.dataType))
                  case (Some(lhs), Some(rhs)) => Right(fields :+ (lhs, rhs))
          keyFields.flatMap: resolvedKeys =>
            left.schema.fields.map(_.name).find: name =>
              !keys.contains(name) && right.schema.field(name).nonEmpty
            match
              case Some(name) => Left(FrameError.ColumnCollision(name))
              case None =>
                val renamedRight = right.schema.fields.zipWithIndex.map: (field, index) =>
                  val renamed =
                    if keys.contains(field.name) then field.copy(
                      id = ColumnId.derived(s"using-right-$index-${field.name}"),
                      name = s"__frame_using_right_${index}_${field.name}"
                    )
                    else field
                  if kind == JoinKind.LeftOuter then renamed.copy(nullable = true)
                  else renamed
                val joinedSchema = Schema.unsafe(left.schema.fields ++ renamedRight)
                val comparisons = resolvedKeys.map: (lhs, rhs) =>
                  val leftIndex = left.schema.fields.indexWhere(_.id == lhs.id)
                  val rightIndex = right.schema.fields.indexWhere(_.id == rhs.id)
                  val leftExpr = ResolvedExpr(
                    ExprId.derived(s"column:left:${lhs.id.value}"),
                    lhs.dataType,
                    lhs.nullable,
                    ExprNode.Column(InputRef.Left, lhs.id, lhs.name, leftIndex)
                  )
                  val rightExpr = ResolvedExpr(
                    ExprId.derived(s"column:right:${rhs.id.value}"),
                    rhs.dataType,
                    rhs.nullable,
                    ExprNode.Column(InputRef.Right, rhs.id, rhs.name, rightIndex)
                  )
                  val equal = ResolvedExpr(
                    ExprId.derived(s"Equal(${leftExpr.id.value},${rightExpr.id.value})"),
                    DataType.Bool,
                    lhs.nullable || rhs.nullable,
                    ExprNode.Binary(BinaryOperator.Equal, leftExpr, rightExpr)
                  )
                  ResolvedExpr(
                    ExprId.derived(s"IsTrue(${equal.id.value})"),
                    DataType.Bool,
                    nullable = false,
                    ExprNode.Unary(UnaryOperator.IsTrue, equal)
                  )
                val condition = comparisons.tail.foldLeft(comparisons.head): (acc, next) =>
                  ResolvedExpr(
                    ExprId.derived(s"And(${acc.id.value},${next.id.value})"),
                    DataType.Bool,
                    nullable = false,
                    ExprNode.Binary(BinaryOperator.And, acc, next)
                  )
                val joined = LogicalPlan.Join(
                  left.plan,
                  right.plan,
                  kind,
                  condition,
                  joinedSchema
                )
                val retainedIndexes =
                  left.schema.fields.indices ++
                    right.schema.fields.indices
                      .filter(index => !keys.contains(right.schema.fields(index).name))
                      .map(_ + left.schema.size)
                val outputFields = retainedIndexes.toVector.map: index =>
                  val field = joinedSchema.fields(index)
                  if kind == JoinKind.LeftOuter && index >= left.schema.size then
                    field.copy(nullable = true)
                  else field
                Schema(outputFields)
                  .left.map(FrameError.InvalidSchema.apply)
                  .map: output =>
                    val selected = retainedIndexes.toVector.zip(output.fields).map:
                      case (index, field) =>
                        val input = joinedSchema.fields(index)
                        NamedExpression(
                          field.name,
                          ResolvedExpr(
                            ExprId.derived(s"column:current:${input.id.value}"),
                            input.dataType,
                            field.nullable,
                            ExprNode.Column(InputRef.Current, input.id, input.name, index)
                          )
                        )
                    new DynamicFrame(LogicalPlan.Project(joined, selected, output), output)

final class GroupedFrame[
    S <: NamedTuple.AnyNamedTuple,
    Keys <: Tuple
] private[frame] (
    input: Frame[S],
    keys: Vector[NamedExpression]
):
  def aggregate[Aggregates <: Tuple](
      expressions: Scope[S] => Aggregates
  )(using
      selection: AggregateSelection[Aggregates],
      output: SchemaDescriptor[ConcatSchema[SelectedSchema[Keys], AggregateSchema[Aggregates]]]
  ): Frame[ConcatSchema[SelectedSchema[Keys], AggregateSchema[Aggregates]]] =
    val aggregates = selection.expressions(expressions(Scope.current(input.schema)))
    new Frame[ConcatSchema[SelectedSchema[Keys], AggregateSchema[Aggregates]]](
      LogicalPlan.Aggregate(input.plan, keys, aggregates, output.schema),
      output.schema
    )
