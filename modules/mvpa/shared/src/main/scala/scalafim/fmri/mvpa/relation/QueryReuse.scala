package scalafim.fmri.mvpa.relation

import gale.backend.Backend.given
import gale.linalg.DMat
import multivar.core.{Lin, Primal, SemanticSpace, ValueIdentity}
import scalafim.fmri.mvpa.{AxisDescriptor, EvidenceError, EvidenceIdentity}

/** Open, method-owned names; these are local nodes in a numerical program,
  * not a global cache or an estimand registry.
  */
opaque type QueryProductId = String
object QueryProductId:
  def apply(value: String): QueryProductId =
    require(value.nonEmpty && value == value.trim, "query product id must be non-empty and trimmed")
    value
  extension (value: QueryProductId) inline def text: String = value

/** Exact declarations, rather than a locator or a display label. Identity is
  * supplied by the evidence provider; this metadata check does not hash or
  * authenticate an operator's payload.
  */
enum QueryDependency:
  case RelationValues(effects: AxisDescriptor, neural: AxisDescriptor, values: ValueIdentity, origins: RelationOrigins, estimability: Vector[EffectEstimability])
  case Metric(left: AxisDescriptor, right: AxisDescriptor, values: ValueIdentity)
  case Target(identity: EvidenceIdentity)
  case Scope(name: String, revision: String)
  case Parameter(name: String, value: String)
  case RandomStream(name: String, revision: String)
  case Unknown(name: String, reason: String)

object QueryDependency:
  def relation[E <: SemanticSpace, N <: SemanticSpace](value: Relation[E, N]): QueryDependency =
    RelationValues(value.effectAxis, value.neuralAxis, value.estimate.valueIdentity, value.origins, value.estimability)

  def metric[L <: SemanticSpace, R <: SemanticSpace](value: CrossClosure[L, R], left: AxisDescriptor, right: AxisDescriptor): QueryDependency =
    Metric(left, right, value.valueIdentity)

/** A product's exact dependency law. Parents reference only preceding nodes,
  * making cycles impossible in a successfully constructed program. Geometry
  * and RSA comparison/multiplicity can therefore invalidate independently.
  */
final case class QueryNode(
    id: QueryProductId,
    parents: Vector[QueryProductId],
    dependencies: Vector[QueryDependency],
    fidelity: String,
    implementation: String
):
  require(fidelity.nonEmpty && fidelity == fidelity.trim, "query fidelity must be explicit")
  require(implementation.nonEmpty && implementation == implementation.trim, "query implementation must be explicit")
  require(parents.distinct.length == parents.length, "query parents must be unique")

final case class QueryReuseBudget(maximumNodes: Int, maximumDependencies: Int, maximumRetainedCells: Long, maximumExplanationEntries: Long = 4096L):
  require(maximumNodes > 0 && maximumDependencies > 0 && maximumRetainedCells >= 0L && maximumExplanationEntries > 0L, "query reuse budget must be bounded and non-negative")

enum QueryReuseDecision:
  case Admitted
  case Rejected(paths: Vector[Vector[QueryProductId]], reasons: Vector[String])
  case Unknown(paths: Vector[Vector[QueryProductId]], reasons: Vector[String])

final class QueryReuseProgram private[relation] (val nodes: Vector[QueryNode], val explanationUpperBound: Long):
  val nodeCount: Int = nodes.length
  val dependencyCount: Long = nodes.foldLeft(0L)((count, node) => count + node.dependencies.length + node.parents.length)

  private[relation] def within(budget: QueryReuseBudget): Boolean =
    nodeCount <= budget.maximumNodes && dependencyCount <= budget.maximumDependencies && explanationUpperBound <= budget.maximumExplanationEntries
  /** Walk in dependency order, recording the causal path from changed inputs.
    * No numerical callbacks or payload reads occur during revalidation.
    */
  def assess(previous: QueryReuseProgram): Vector[(QueryProductId, QueryReuseDecision)] =
    val old = previous.nodes.map(node => node.id -> node).toMap
    val outcomes = scala.collection.mutable.Map.empty[QueryProductId, QueryReuseDecision]
    nodes.foreach: node =>
      val changed = Vector.newBuilder[String]
      val unknown = Vector.newBuilder[String]
      old.get(node.id) match
        case None => changed += "product was not retained"
        case Some(before) =>
          if before.parents != node.parents then changed += "ordered parent dependencies changed"
          if before.dependencies != node.dependencies then changed += "exact value, metric, scope, parameters or randomness changed"
          if before.fidelity != node.fidelity then changed += "numerical fidelity changed"
          if before.implementation != node.implementation then changed += "implementation binding changed"
      node.dependencies.foreach:
        case QueryDependency.Unknown(name, reason) => unknown += s"$name: $reason"
        case QueryDependency.Scope(name, revision) if name.trim.isEmpty || revision.trim.isEmpty => unknown += "scope revision is unspecified"
        case QueryDependency.Parameter(name, value) if name.trim.isEmpty || value.trim.isEmpty => unknown += "parameter is unspecified"
        case QueryDependency.RandomStream(name, revision) if name.trim.isEmpty || revision.trim.isEmpty => unknown += "random stream is unspecified"
        case _ => ()
      val rejectedParents = node.parents.flatMap: parent =>
        outcomes(parent) match
          case QueryReuseDecision.Rejected(paths, reasons) => Some((paths.take(1).map(_ :+ node.id), reasons.take(1)))
          case _ => None
      val unknownParents = node.parents.flatMap: parent =>
        outcomes(parent) match
          case QueryReuseDecision.Unknown(paths, reasons) => Some((paths.take(1).map(_ :+ node.id), reasons.take(1)))
          case _ => None
      val changedReasons = changed.result()
      val unknownReasons = unknown.result()
      outcomes(node.id) =
        if changedReasons.nonEmpty || rejectedParents.nonEmpty then
          QueryReuseDecision.Rejected(
            (if changedReasons.nonEmpty then Vector(Vector(node.id)) else Vector.empty) ++ rejectedParents.flatMap(_._1),
            changedReasons ++ rejectedParents.flatMap(_._2)
          )
        else if unknownReasons.nonEmpty || unknownParents.nonEmpty then
          QueryReuseDecision.Unknown(
            (if unknownReasons.nonEmpty then Vector(Vector(node.id)) else Vector.empty) ++ unknownParents.flatMap(_._1),
            unknownReasons ++ unknownParents.flatMap(_._2)
          )
        else QueryReuseDecision.Admitted
    nodes.map(node => node.id -> outcomes(node.id))

object QueryReuseProgram:
  def apply(nodes: Vector[QueryNode], budget: QueryReuseBudget): Either[EvidenceError, QueryReuseProgram] =
    if nodes.length > budget.maximumNodes || nodes.foldLeft(0L)((count, node) => count + node.dependencies.length + node.parents.length) > budget.maximumDependencies then
      Left(EvidenceError.InvalidSource("query dependency program exceeds metadata budget"))
    else if nodes.map(_.id).distinct.length != nodes.length then
      Left(EvidenceError.InvalidSource("query product ids must be unique"))
    else
      val preceding = scala.collection.mutable.Map.empty[QueryProductId, Int]
      var maximumDepth = 0
      var failure: Option[EvidenceError] = None
      nodes.foreach: node =>
        if node.parents.exists(parent => !preceding.contains(parent)) then
          failure = Some(EvidenceError.InvalidSource("query parents must reference preceding products"))
        val depth = 1 + node.parents.flatMap(preceding.get).foldLeft(0)(math.max)
        preceding(node.id) = depth
        maximumDepth = math.max(maximumDepth, depth)
      // At most one representative path/reason per direct parent plus each
      // local dependency reason. Include each output id, one local path entry,
      // and up to four local comparison reasons, even for input-free nodes.
      // Bound all logical id/path/reason entries before producing them.
      val dependencies = nodes.foldLeft(0L)((count, node) => count + node.dependencies.length + node.parents.length)
      val explanation = 6L * nodes.length + dependencies * (maximumDepth.toLong + 2L)
      if explanation > budget.maximumExplanationEntries then
        Left(EvidenceError.InvalidSource("query explanation exceeds its entry budget"))
      else failure.toLeft(new QueryReuseProgram(nodes, explanation))

/** Immutable retained output. Its method-owned type is preserved. Reuse is
  * revalidated at the use boundary; refusal never runs the callback.
  */
final class RetainedQuery[A] private[relation] (
    val product: QueryProductId, val program: QueryReuseProgram, val retainedCells: Long, private val value: A
):
  def use[B](current: QueryReuseProgram, budget: QueryReuseBudget)(read: A => B): Either[QueryReuseDecision, B] =
    if retainedCells > budget.maximumRetainedCells || !program.within(budget) || !current.within(budget) then
      Left(QueryReuseDecision.Rejected(Vector(Vector(product)), Vector("retained output or metadata exceeds current budget")))
    else current.assess(program).find(_._1 == product).map(_._2) match
      case Some(QueryReuseDecision.Admitted) => Right(read(value))
      case Some(refusal) => Left(refusal)
      case None => Left(QueryReuseDecision.Rejected(Vector(Vector(product)), Vector("product is absent from the requested program")))

object RetainedQuery:
  def apply[A](product: QueryProductId, program: QueryReuseProgram, value: A, retainedCells: Long, budget: QueryReuseBudget): Either[EvidenceError, RetainedQuery[A]] =
    if !program.within(budget) then Left(EvidenceError.InvalidSource("retained query dependency program exceeds its budget"))
    else if !program.nodes.exists(_.id == product) then Left(EvidenceError.InvalidSource("retained product is not in the dependency program"))
    else if retainedCells < 0L || retainedCells > budget.maximumRetainedCells then Left(EvidenceError.InvalidSource("retained query output exceeds its budget"))
    else Right(new RetainedQuery(product, program, retainedCells, value))

final case class ContractionBudget(maximumWorkspaceCells: Long, maximumOperatorColumns: Int, maximumScalarProducts: Long = 1000000L):
  require(maximumWorkspaceCells >= 0L && maximumOperatorColumns > 0 && maximumScalarProducts > 0L, "contraction budget must be explicit")

/** Admitted rank-factor law H = U V^T. The factor maps are U^T and V^T,
  * sharing a nominal component space. No full effect-space form is allocated.
  */
object LowRankRelationContraction:
  def scalar[EL <: SemanticSpace, NL <: SemanticSpace, ER <: SemanticSpace, NR <: SemanticSpace, Q <: SemanticSpace](
      pair: RelationPair[EL, NL, ER, NR],
      leftFactor: Lin[Primal[EL], Primal[Q]],
      rightFactor: Lin[Primal[ER], Primal[Q]],
      metric: CrossClosure[NL, NR],
      budget: ContractionBudget
  ): Either[EvidenceError, ScalarRelationStatistic] =
    val rank = leftFactor.rows
    // Conservative live-vector accounting includes intermediate effect and
    // neural vectors; no neural-by-neural matrix is admitted.
    val cells = 2L * (rank.toLong + pair.left.effectAxis.size + pair.right.effectAxis.size + pair.left.neuralAxis.size + pair.right.neuralAxis.size)
    if cells > budget.maximumWorkspaceCells then Left(EvidenceError.InvalidSource("low-rank contraction exceeds workspace budget"))
    else if pair.left.origins.access == RelationAccess.OneShot || pair.right.origins.access == RelationAccess.OneShot then
      Left(EvidenceError.InvalidSource("low-rank contraction requires owned replay"))
    else
      val left = pair.left.estimate.andThen(leftFactor)
      val right = pair.right.estimate.andThen(rightFactor)
      val contraction = right.star.andThen(metric).andThen(left)
      var total = 0.0
      var correction = 0.0
      var index = 0
      var failure: Option[EvidenceError] = None
      while index < rank && failure.isEmpty do
        contraction(DMat.tabulate(rank, 1)((row, _) => if row == index then 1.0 else 0.0)) match
          case Left(error) => failure = Some(EvidenceError.SemanticFailure(error))
          case Right(value) =>
            val next = value(index, 0)
            val sum = total + next
            correction += (if math.abs(total) >= math.abs(next) then (total - sum) + next else (next - sum) + total)
            total = sum
        index += 1
      val result = total + correction
      failure.toLeft(ScalarRelationStatistic(result)).flatMap: statistic =>
        if statistic.value.isFinite then Right(statistic) else Left(EvidenceError.InvalidSource("low-rank contraction is non-finite"))

/** Equal-weight, all-distinct bilinear sum. Cancellation-prone coordinates
  * use direct ordered products instead of subtracting nearly equal totals.
  * This law has no application to weighted/missing edges or feature-additive
  * dense-metric caches. Inputs are already admitted sufficient statistics.
  */
object AllDistinctProducts:
  def sum(left: Vector[Vector[Double]], right: Vector[Vector[Double]], budget: ContractionBudget): Either[EvidenceError, Double] =
    if left.length < 2 || left.length != right.length || left.map(_.length).distinct.length != 1 || right.exists(_.length != left.head.length) then
      Left(EvidenceError.InvalidSource("all-distinct statistics require at least two matched rectangular partitions"))
    else if left.length.toLong * left.head.length * 2L > budget.maximumWorkspaceCells then
      Left(EvidenceError.InvalidSource("all-distinct statistics exceed workspace budget"))
    else if BigInt(left.length) * left.length * left.head.length + left.head.length > budget.maximumScalarProducts then
      Left(EvidenceError.InvalidSource("all-distinct rewrite or cancellation fallback exceeds scalar-product budget"))
    else if left.exists(_.exists(value => !value.isFinite)) || right.exists(_.exists(value => !value.isFinite)) then
      Left(EvidenceError.InvalidSource("all-distinct statistics must be finite"))
    else
      var total = 0.0
      var correction = 0.0
      def add(value: Double): Unit =
        val next = total + value
        correction += (if math.abs(total) >= math.abs(value) then (total - next) + value else (value - next) + total)
        total = next
      var globalScale = 0.0
      var unsafeRewrite = false
      var column = 0
      while column < left.head.length do
        var sumLeft = 0.0
        var leftCorrection = 0.0
        var sumRight = 0.0
        var rightCorrection = 0.0
        var self = 0.0
        var selfCorrection = 0.0
        var absolute = 0.0
        var absoluteLeft = 0.0
        var absoluteRight = 0.0
        var row = 0
        while row < left.length do
          val a = left(row)(column)
          val b = right(row)(column)
          val nextLeft = sumLeft + a
          leftCorrection += (if math.abs(sumLeft) >= math.abs(a) then (sumLeft - nextLeft) + a else (a - nextLeft) + sumLeft)
          sumLeft = nextLeft
          val nextRight = sumRight + b
          rightCorrection += (if math.abs(sumRight) >= math.abs(b) then (sumRight - nextRight) + b else (b - nextRight) + sumRight)
          sumRight = nextRight
          val diagonal = a * b
          val nextSelf = self + diagonal
          selfCorrection += (if math.abs(self) >= math.abs(diagonal) then (self - nextSelf) + diagonal else (diagonal - nextSelf) + self)
          self = nextSelf
          absolute += math.abs(diagonal)
          absoluteLeft += math.abs(a)
          absoluteRight += math.abs(b)
          row += 1
        val product = (sumLeft + leftCorrection) * (sumRight + rightCorrection)
        val difference = product - (self + selfCorrection)
        val scale = absoluteLeft * absoluteRight + absolute
        globalScale += scale
        if !difference.isFinite || !scale.isFinite || math.abs(difference) <= 1e-8 * scale then unsafeRewrite = true
        add(difference)
        column += 1
      val tentative = total + correction
      if unsafeRewrite || !tentative.isFinite || !globalScale.isFinite || math.abs(tentative) <= 1e-8 * globalScale then
        // One global fallback avoids losing terms between different feature
        // coordinates and performs no duplicate per-coordinate fallback.
        total = 0.0
        correction = 0.0
        var row = 0
        while row < left.length do
          var other = 0
          while other < right.length do
            if row != other then
              column = 0
              while column < left.head.length do
                add(left(row)(column) * right(other)(column))
                column += 1
            other += 1
          row += 1
      val result = total + correction
      if result.isFinite then Right(result) else Left(EvidenceError.InvalidSource("all-distinct contraction overflowed"))
