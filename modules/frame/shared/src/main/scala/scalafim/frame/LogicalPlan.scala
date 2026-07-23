package scalafim.frame

enum JoinKind:
  case Inner
  case LeftOuter

enum SortDirection:
  case Ascending
  case Descending

enum NullPlacement:
  case First
  case Last

enum SourceKind:
  case Scan
  case Values

enum OrderGuarantee:
  case Unspecified
  case Stable
  case Sorted(keys: Vector[ExprId])

final case class SourceRef private (
    id: SourceId,
    displayName: String,
    kind: SourceKind,
    order: OrderGuarantee
)

object SourceRef:
  def scan(
      id: String,
      displayName: String,
      order: OrderGuarantee = OrderGuarantee.Unspecified
  ): Either[FrameError, SourceRef] =
    create(id, displayName, SourceKind.Scan, order)

  def values(id: String, displayName: String): Either[FrameError, SourceRef] =
    create(id, displayName, SourceKind.Values, OrderGuarantee.Stable)

  private def create(
      id: String,
      displayName: String,
      kind: SourceKind,
      order: OrderGuarantee
  ): Either[FrameError, SourceRef] =
    if id.trim.isEmpty then Left(FrameError.InvalidSourceId(id))
    else if displayName.trim.isEmpty then Left(FrameError.InvalidSourceName(displayName))
    else Right(SourceRef(SourceId.unsafe(id), displayName, kind, order))

private[frame] final case class SortExpression(
    expression: ResolvedExpr,
    direction: SortDirection,
    nulls: NullPlacement
)

sealed trait LogicalPlan:
  def output: Schema
  def children: Vector[LogicalPlan]
  def nodeName: String
  def order: OrderGuarantee

object LogicalPlan:
  private[frame] final case class Source(
      reference: SourceRef,
      output: Schema
  ) extends LogicalPlan:
    val children = Vector.empty
    val nodeName = reference.kind.toString
    val order = reference.order

  private[frame] final case class Project(
      input: LogicalPlan,
      expressions: Vector[NamedExpression],
      output: Schema
  ) extends LogicalPlan:
    val children = Vector(input)
    val nodeName = "Project"
    val order = input.order

  private[frame] final case class Filter(
      input: LogicalPlan,
      predicate: ResolvedExpr,
      output: Schema
  ) extends LogicalPlan:
    val children = Vector(input)
    val nodeName = "Filter"
    val order = input.order

  private[frame] final case class Join(
      left: LogicalPlan,
      right: LogicalPlan,
      kind: JoinKind,
      condition: ResolvedExpr,
      output: Schema
  ) extends LogicalPlan:
    val children = Vector(left, right)
    val nodeName = "Join"
    val order = OrderGuarantee.Unspecified

  private[frame] final case class Aggregate(
      input: LogicalPlan,
      keys: Vector[NamedExpression],
      aggregates: Vector[NamedAggregateExpression],
      output: Schema
  ) extends LogicalPlan:
    val children = Vector(input)
    val nodeName = "Aggregate"
    val order = OrderGuarantee.Unspecified

  private[frame] final case class Sort(
      input: LogicalPlan,
      sortExpressions: Vector[SortExpression],
      output: Schema
  ) extends LogicalPlan:
    val children = Vector(input)
    val nodeName = "Sort"
    val order = OrderGuarantee.Sorted(sortExpressions.map(_.expression.id))

  private[frame] final case class Limit(
      input: LogicalPlan,
      count: Int,
      output: Schema
  ) extends LogicalPlan:
    val children = Vector(input)
    val nodeName = "Limit"
    val order = input.order

  def explain(plan: LogicalPlan): String =
    def loop(current: LogicalPlan, depth: Int, builder: StringBuilder): Unit =
      val indent = "  " * depth
      current match
        case Source(reference, schema) =>
          builder.append(
            s"${indent}${reference.kind}[${reference.displayName}; id=${reference.id.value}] ${schema.toString}\n"
          )
        case Project(input, expressions, schema) =>
          builder.append(
            s"${indent}Project[${expressions.map(_.name).mkString(", ")}] ${schema.toString}\n"
          )
          loop(input, depth + 1, builder)
        case Filter(input, predicate, schema) =>
          builder.append(s"${indent}Filter[${predicate.id.value}] ${schema.toString}\n")
          loop(input, depth + 1, builder)
        case Join(left, right, kind, condition, schema) =>
          builder.append(s"${indent}Join[$kind; ${condition.id.value}] ${schema.toString}\n")
          loop(left, depth + 1, builder)
          loop(right, depth + 1, builder)
        case Aggregate(input, keys, aggregates, schema) =>
          val names = (keys.map(_.name) ++ aggregates.map(_.name)).mkString(", ")
          builder.append(s"${indent}Aggregate[$names] ${schema.toString}\n")
          loop(input, depth + 1, builder)
        case Sort(input, order, schema) =>
          val ids = order
            .map(item => s"${item.expression.id.value}:${item.direction}:${item.nulls}")
            .mkString(", ")
          builder.append(s"${indent}Sort[$ids] ${schema.toString}\n")
          loop(input, depth + 1, builder)
        case Limit(input, count, schema) =>
          builder.append(s"${indent}Limit[$count] ${schema.toString}\n")
          loop(input, depth + 1, builder)

    val builder = new StringBuilder
    loop(plan, 0, builder)
    builder.result().stripSuffix("\n")
