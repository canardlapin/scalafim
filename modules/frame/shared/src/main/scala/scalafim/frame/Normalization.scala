package scalafim.frame

enum NormalizationRule:
  case FuseFilters
  case FuseProjects
  case RemoveIdentityProject
  case CollapseLimits
  case PushLimitThroughProject
  case PushFilterThroughProject

final case class NormalizationReceipt(
    rules: Vector[NormalizationRule],
    originalExplain: String,
    normalizedExplain: String
):
  def changed: Boolean = rules.nonEmpty

final case class NormalizedPlan(
    plan: LogicalPlan,
    receipt: NormalizationReceipt
)

object PlanNormalizer:
  def normalize(plan: LogicalPlan): NormalizedPlan =
    val original = LogicalPlan.explain(plan)
    val (normalized, rules) = loop(plan)
    NormalizedPlan(
      normalized,
      NormalizationReceipt(rules, original, LogicalPlan.explain(normalized))
    )

  private def loop(plan: LogicalPlan): (LogicalPlan, Vector[NormalizationRule]) =
    val (childrenNormalized, childRules) = plan match
      case source: LogicalPlan.Source => (source, Vector.empty)
      case LogicalPlan.Project(input, expressions, output) =>
        val (normalized, rules) = loop(input)
        (LogicalPlan.Project(normalized, expressions, output), rules)
      case LogicalPlan.Filter(input, predicate, output) =>
        val (normalized, rules) = loop(input)
        (LogicalPlan.Filter(normalized, predicate, output), rules)
      case LogicalPlan.Limit(input, count, output) =>
        val (normalized, rules) = loop(input)
        (LogicalPlan.Limit(normalized, count, output), rules)
      case LogicalPlan.Aggregate(input, keys, aggregates, output) =>
        val (normalized, rules) = loop(input)
        (LogicalPlan.Aggregate(normalized, keys, aggregates, output), rules)
      case LogicalPlan.Sort(input, order, output) =>
        val (normalized, rules) = loop(input)
        (LogicalPlan.Sort(normalized, order, output), rules)
      case LogicalPlan.Join(left, right, kind, condition, output) =>
        val (normalizedLeft, leftRules) = loop(left)
        val (normalizedRight, rightRules) = loop(right)
        (
          LogicalPlan.Join(normalizedLeft, normalizedRight, kind, condition, output),
          leftRules ++ rightRules
        )

    rewrite(childrenNormalized) match
      case Some((rewritten, rule)) =>
        val (fixedPoint, moreRules) = loop(rewritten)
        (fixedPoint, (childRules :+ rule) ++ moreRules)
      case None => (childrenNormalized, childRules)

  private def rewrite(
      plan: LogicalPlan
  ): Option[(LogicalPlan, NormalizationRule)] = plan match
    case LogicalPlan.Filter(
          LogicalPlan.Filter(input, first, _),
          second,
          output
        ) if isTotal(second) =>
      Some((
        LogicalPlan.Filter(input, and(first, second), output),
        NormalizationRule.FuseFilters
      ))
    case LogicalPlan.Project(input, expressions, output)
        if isIdentity(input.output, expressions, output) =>
      Some((input, NormalizationRule.RemoveIdentityProject))
    case LogicalPlan.Project(
          LogicalPlan.Project(input, inner, _),
          outer,
          output
        ) if inner.forall(named => isTotal(named.expression)) =>
      substituteAll(outer, inner).map: fused =>
        (LogicalPlan.Project(input, fused, output), NormalizationRule.FuseProjects)
    case LogicalPlan.Filter(
          LogicalPlan.Project(input, expressions, projectOutput),
          predicate,
          output
        ) if expressions.forall(named => isTotal(named.expression)) =>
      substitute(predicate, expressions).map: pushed =>
        (
          LogicalPlan.Project(
            LogicalPlan.Filter(input, pushed, input.output),
            expressions,
            projectOutput
          ),
          NormalizationRule.PushFilterThroughProject
        )
    case LogicalPlan.Limit(LogicalPlan.Limit(input, first, _), second, output) =>
      Some((
        LogicalPlan.Limit(input, math.min(first, second), output),
        NormalizationRule.CollapseLimits
      ))
    case LogicalPlan.Limit(
          LogicalPlan.Project(input, expressions, projectOutput),
          count,
          output
        ) if expressions.forall(named => isTotal(named.expression)) =>
      Some((
        LogicalPlan.Project(
          LogicalPlan.Limit(input, count, input.output),
          expressions,
          projectOutput
        ),
        NormalizationRule.PushLimitThroughProject
      ))
    case _ => None

  private def and(left: ResolvedExpr, right: ResolvedExpr): ResolvedExpr =
    ResolvedExpr(
      ExprId.derived(s"And(${left.id.value},${right.id.value})"),
      DataType.Bool,
      nullable = left.nullable || right.nullable,
      ExprNode.Binary(BinaryOperator.And, left, right)
    )

  private def isTotal(expression: ResolvedExpr): Boolean = expression.node match
    case ExprNode.Column(_, _, _, _) | ExprNode.Literal(_) => true
    case ExprNode.Unary(UnaryOperator.IsNull | UnaryOperator.IsTrue, input) =>
      isTotal(input)
    case ExprNode.Unary(UnaryOperator.Negate, input) =>
      isFloating(input.dataType) && isTotal(input)
    case ExprNode.Binary(operator, left, right) =>
      val operatorIsTotal = operator match
        case BinaryOperator.Add | BinaryOperator.Subtract |
            BinaryOperator.Multiply | BinaryOperator.Divide =>
          isFloating(left.dataType) && left.dataType == right.dataType
        case BinaryOperator.Equal | BinaryOperator.NullSafeEqual |
            BinaryOperator.NotEqual | BinaryOperator.LessThan |
            BinaryOperator.LessThanOrEqual | BinaryOperator.GreaterThan |
            BinaryOperator.GreaterThanOrEqual | BinaryOperator.And |
            BinaryOperator.Or =>
          true
      operatorIsTotal && isTotal(left) && isTotal(right)

  private def isFloating(dataType: DataType): Boolean = dataType match
    case DataType.Float32 | DataType.Float64 => true
    case _ => false

  private def isIdentity(
      input: Schema,
      expressions: Vector[NamedExpression],
      output: Schema
  ): Boolean =
    input == output &&
      expressions.length == input.size &&
      expressions.zipWithIndex.forall: (named, index) =>
        named.name == input.fields(index).name &&
          (named.expression.node match
            case ExprNode.Column(InputRef.Current, id, name, actualIndex) =>
              id == input.fields(index).id &&
                name == input.fields(index).name &&
                actualIndex == index
            case _ => false)

  private def substituteAll(
      outer: Vector[NamedExpression],
      inner: Vector[NamedExpression]
  ): Option[Vector[NamedExpression]] =
    val rewritten = outer.map: named =>
      substitute(named.expression, inner).map(NamedExpression(named.name, _))
    if rewritten.forall(_.nonEmpty) then Some(rewritten.flatten)
    else None

  private def substitute(
      expression: ResolvedExpr,
      inputs: Vector[NamedExpression]
  ): Option[ResolvedExpr] = expression.node match
    case ExprNode.Column(InputRef.Current, _, _, index) => inputs.lift(index).map(_.expression)
    case ExprNode.Column(_, _, _, _) => None
    case ExprNode.Literal(_) => Some(expression)
    case ExprNode.Unary(operator, input) =>
      substitute(input, inputs).map: rewritten =>
        expression.copy(
          id = ExprId.derived(s"$operator(${rewritten.id.value})"),
          node = ExprNode.Unary(operator, rewritten)
        )
    case ExprNode.Binary(operator, left, right) =>
      for
        lhs <- substitute(left, inputs)
        rhs <- substitute(right, inputs)
      yield expression.copy(
        id = ExprId.derived(s"$operator(${lhs.id.value},${rhs.id.value})"),
        node = ExprNode.Binary(operator, lhs, rhs)
      )
