package scalafim.frame

import scala.NamedTuple
import scala.collection.mutable.ArrayBuffer

enum TriBool:
  case True
  case False
  case Unknown

enum ExecutionError:
  case Storage(error: StorageError)
  case MissingSource(id: SourceId)
  case SourceSchema(expected: Schema, actual: Schema)
  case ExpressionType(id: ExprId, expected: DataType, actual: ScalarValue)
  case PredicateType(id: ExprId, actual: ScalarValue)
  case IntegerOverflow(id: ExprId, operator: BinaryOperator)
  case DivisionByZero(id: ExprId)
  case UnsupportedNode(node: String)
  case InvalidLiteral(value: LiteralValue)

  def message: String = this match
    case Storage(error) => error.message
    case MissingSource(id) => s"no reference source is bound for '${id.value}'"
    case SourceSchema(expected, actual) =>
      s"source schema $actual does not match resolved schema $expected"
    case ExpressionType(id, expected, actual) =>
      s"expression ${id.value} expected $expected but evaluated to $actual"
    case PredicateType(id, actual) =>
      s"predicate ${id.value} evaluated to $actual instead of boolean or null"
    case IntegerOverflow(id, operator) =>
      s"integer overflow in ${operator.toString} for expression ${id.value}"
    case DivisionByZero(id) => s"division by zero in expression ${id.value}"
    case UnsupportedNode(node) => s"reference interpreter does not implement $node"
    case InvalidLiteral(value) => s"literal $value is not valid"

trait ExecutionCursor:
  def nextBatch(): Either[ExecutionError, Option[RecordBatch]]
  def close(): Unit

final case class ExecutionShape(
    streaming: Boolean,
    blockingNodes: Vector[String],
    estimatedRows: Option[Long]
)

final class ReferenceSources private (
    private val entries: Map[String, ReferenceSources.Entry]
):
  def bind[S <: NamedTuple.AnyNamedTuple](
      reference: SourceRef,
      table: Table[S]
  ): ReferenceSources =
    new ReferenceSources(entries.updated(reference.id.value, ReferenceSources.Entry(table.schema, table.batches)))

  private[frame] def open(
      reference: SourceRef,
      expected: Schema
  ): Either[ExecutionError, ExecutionCursor] =
    entries.get(reference.id.value) match
      case None => Left(ExecutionError.MissingSource(reference.id))
      case Some(entry) if entry.schema != expected =>
        Left(ExecutionError.SourceSchema(expected, entry.schema))
      case Some(entry) =>
        val retained = ArrayBuffer.empty[RecordBatch]
        var index = 0
        var error: Option[StorageError] = None
        while index < entry.batches.length && error.isEmpty do
          entry.batches(index).slice(0, entry.batches(index).rowCount) match
            case Right(batch) => retained += batch
            case Left(value) => error = Some(value)
          index += 1
        error match
          case Some(value) =>
            retained.foreach(_.close())
            Left(ExecutionError.Storage(value))
          case None =>
            Right:
              new ExecutionCursor:
                private var position = 0
                private var closed = false

                def nextBatch(): Either[ExecutionError, Option[RecordBatch]] = synchronized:
                  if closed then Left(ExecutionError.Storage(StorageError.SourceClosed))
                  else if position >= retained.length then Right(None)
                  else
                    val batch = retained(position)
                    position += 1
                    Right(Some(batch))

                def close(): Unit = synchronized:
                  if !closed then
                    closed = true
                    while position < retained.length do
                      retained(position).close()
                      position += 1

object ReferenceSources:
  private final case class Entry(schema: Schema, batches: Vector[RecordBatch])

  val empty: ReferenceSources = new ReferenceSources(Map.empty)

final class ReferenceExecution private[frame] (
    val logicalPlan: LogicalPlan,
    val shape: ExecutionShape,
    private val openCursor: () => Either[ExecutionError, ExecutionCursor]
):
  def open(): Either[ExecutionError, ExecutionCursor] = openCursor()

  def physicalExplain: String =
    val mode = if shape.streaming then "streaming" else "blocking"
    val blockers =
      if shape.blockingNodes.isEmpty then "none"
      else shape.blockingNodes.mkString(",")
    s"ReferenceExecution(mode=$mode, blocking=$blockers, estimatedRows=${shape.estimatedRows.getOrElse("unknown")}, fallback=none)"

  def collect[S <: NamedTuple.AnyNamedTuple](using
      descriptor: SchemaDescriptor[S]
  ): Either[ExecutionError, Table[S]] =
    open().flatMap: cursor =>
      val batches = ArrayBuffer.empty[RecordBatch]
      try
        var done = false
        var error: Option[ExecutionError] = None
        while !done && error.isEmpty do
          cursor.nextBatch() match
            case Right(Some(batch)) => batches += batch
            case Right(None) => done = true
            case Left(value) => error = Some(value)
        error match
          case Some(value) =>
            batches.foreach(_.close())
            Left(value)
          case None =>
            Table[S](batches.toVector) match
              case Right(table) => Right(table)
              case Left(value) =>
                batches.foreach(_.close())
                Left(ExecutionError.Storage(value))
      finally cursor.close()

object ReferenceInterpreter:
  def prepare(
      plan: LogicalPlan,
      sources: ReferenceSources
  ): ReferenceExecution =
    val blockers = blockingNodes(plan)
    new ReferenceExecution(
      plan,
      ExecutionShape(blockers.isEmpty, blockers, estimatedRows(plan)),
      () => open(plan, sources)
    )

  private def blockingNodes(plan: LogicalPlan): Vector[String] = plan match
    case _: LogicalPlan.Aggregate => Vector("Aggregate") ++ plan.children.flatMap(blockingNodes)
    case _: LogicalPlan.Join => Vector("Join") ++ plan.children.flatMap(blockingNodes)
    case _: LogicalPlan.Sort => Vector("Sort") ++ plan.children.flatMap(blockingNodes)
    case _ => plan.children.flatMap(blockingNodes)

  private def estimatedRows(plan: LogicalPlan): Option[Long] = plan match
    case LogicalPlan.Limit(_, count, _) => Some(count.toLong)
    case _ => None

  private def open(
      plan: LogicalPlan,
      sources: ReferenceSources
  ): Either[ExecutionError, ExecutionCursor] = plan match
    case LogicalPlan.Source(reference, schema) => sources.open(reference, schema)
    case LogicalPlan.Project(input, expressions, schema) =>
      open(input, sources).map(new ProjectCursor(_, expressions, schema))
    case LogicalPlan.Filter(input, predicate, schema) =>
      open(input, sources).map(new FilterCursor(_, predicate, schema))
    case LogicalPlan.Limit(input, count, _) =>
      open(input, sources).map(new LimitCursor(_, count))
    case LogicalPlan.Aggregate(input, keys, aggregates, schema) =>
      open(input, sources).map(new AggregateCursor(_, keys, aggregates, schema))
    case LogicalPlan.Join(left, right, kind, condition, schema) =>
      open(left, sources).flatMap: leftCursor =>
        open(right, sources) match
          case Right(rightCursor) =>
            Right(new JoinCursor(leftCursor, rightCursor, kind, condition, schema))
          case Left(error) =>
            leftCursor.close()
            Left(error)
    case LogicalPlan.Sort(input, order, schema) =>
      open(input, sources).map(new SortCursor(_, order, schema))

  private final class ProjectCursor(
      input: ExecutionCursor,
      expressions: Vector[NamedExpression],
      schema: Schema
  ) extends ExecutionCursor:
    private var closed = false

    def nextBatch(): Either[ExecutionError, Option[RecordBatch]] =
      if closed then Left(ExecutionError.Storage(StorageError.SourceClosed))
      else
        input.nextBatch().flatMap:
          case None => Right(None)
          case Some(batch) =>
            val columns = expressions.map: expression =>
              val values = new Array[ScalarValue](batch.rowCount)
              var row = 0
              var error: Option[ExecutionError] = None
              while row < batch.rowCount && error.isEmpty do
                evaluate(expression.expression, EvalContext.current(batch), row) match
                  case Right(value) => values(row) = value
                  case Left(value) => error = Some(value)
                row += 1
              error match
                case Some(value) => Left(value)
                case None => Right(values)
            sequence(columns).flatMap(buildBatch(schema, _)) match
              case Right(output) =>
                batch.close()
                Right(Some(output))
              case Left(error) =>
                batch.close()
                Left(error)

    def close(): Unit =
      if !closed then
        closed = true
        input.close()

  private final class FilterCursor(
      input: ExecutionCursor,
      predicate: ResolvedExpr,
      schema: Schema
  ) extends ExecutionCursor:
    private var closed = false

    def nextBatch(): Either[ExecutionError, Option[RecordBatch]] =
      if closed then Left(ExecutionError.Storage(StorageError.SourceClosed))
      else
        input.nextBatch().flatMap:
          case None => Right(None)
          case Some(batch) =>
            val retained = ArrayBuffer.empty[Int]
            var row = 0
            var error: Option[ExecutionError] = None
            while row < batch.rowCount && error.isEmpty do
              evaluate(predicate, EvalContext.current(batch), row) match
                case Right(ScalarValue.Bool(true)) => retained += row
                case Right(ScalarValue.Bool(false) | ScalarValue.Null) => ()
                case Right(other) => error = Some(ExecutionError.PredicateType(predicate.id, other))
                case Left(value) => error = Some(value)
              row += 1
            val result = error match
              case Some(value) => Left(value)
              case None =>
                val columns = batch.columns.map: column =>
                  val values = new Array[ScalarValue](retained.length)
                  var outputRow = 0
                  var columnError: Option[ExecutionError] = None
                  while outputRow < retained.length && columnError.isEmpty do
                    column.scalar(retained(outputRow)) match
                      case Right(value) => values(outputRow) = value
                      case Left(value) => columnError = Some(ExecutionError.Storage(value))
                    outputRow += 1
                  columnError match
                    case Some(value) => Left(value)
                    case None => Right(values)
                sequence(columns).flatMap(buildBatch(schema, _)).map(Some(_))
            batch.close()
            result

    def close(): Unit =
      if !closed then
        closed = true
        input.close()

  private final class LimitCursor(input: ExecutionCursor, requested: Int) extends ExecutionCursor:
    private var remaining = requested
    private var closed = false

    def nextBatch(): Either[ExecutionError, Option[RecordBatch]] =
      if closed then Left(ExecutionError.Storage(StorageError.SourceClosed))
      else if remaining == 0 then Right(None)
      else
        input.nextBatch().flatMap:
          case None => Right(None)
          case Some(batch) if batch.rowCount <= remaining =>
            remaining -= batch.rowCount
            Right(Some(batch))
          case Some(batch) =>
            val result = batch.slice(0, remaining).left.map(ExecutionError.Storage.apply)
            remaining = 0
            batch.close()
            result.map(Some(_))

    def close(): Unit =
      if !closed then
        closed = true
        input.close()

  private final class AggregateCursor(
      input: ExecutionCursor,
      keys: Vector[NamedExpression],
      aggregates: Vector[NamedAggregateExpression],
      schema: Schema
  ) extends ExecutionCursor:
    private var emitted = false
    private var closed = false

    def nextBatch(): Either[ExecutionError, Option[RecordBatch]] =
      if closed then Left(ExecutionError.Storage(StorageError.SourceClosed))
      else if emitted then Right(None)
      else
        emitted = true
        drain(input).flatMap: batches =>
          val groups = scala.collection.mutable.LinkedHashMap.empty[Vector[KeyAtom], GroupRows]
          var batchIndex = 0
          var error: Option[ExecutionError] = None
          while batchIndex < batches.length && error.isEmpty do
            val batch = batches(batchIndex)
            var row = 0
            while row < batch.rowCount && error.isEmpty do
              val keyValues = new Array[ScalarValue](keys.length)
              var keyIndex = 0
              while keyIndex < keys.length && error.isEmpty do
                evaluate(keys(keyIndex).expression, EvalContext.current(batch), row) match
                  case Right(value) => keyValues(keyIndex) = value
                  case Left(value) => error = Some(value)
                keyIndex += 1
              if error.isEmpty then
                val key = keyValues.toVector.map(KeyAtom.fromScalar)
                val rows = groups.getOrElseUpdate(key, GroupRows(keyValues, ArrayBuffer.empty))
                rows.rows += RowRef(batch, row, 0L)
              row += 1
            batchIndex += 1

          val result = error match
            case Some(value) => Left(value)
            case None =>
              if keys.isEmpty && groups.isEmpty then
                groups.put(Vector.empty, GroupRows(Array.empty, ArrayBuffer.empty))
              val outputRows = ArrayBuffer.empty[Array[ScalarValue]]
              val iterator = groups.valuesIterator
              while iterator.hasNext && error.isEmpty do
                val group = iterator.next()
                val output = new Array[ScalarValue](keys.length + aggregates.length)
                Array.copy(group.keys, 0, output, 0, group.keys.length)
                var aggregateIndex = 0
                while aggregateIndex < aggregates.length && error.isEmpty do
                  aggregateValue(aggregates(aggregateIndex).expression, group.rows.toVector) match
                    case Right(value) => output(keys.length + aggregateIndex) = value
                    case Left(value) => error = Some(value)
                  aggregateIndex += 1
                outputRows += output
              error match
                case Some(value) => Left(value)
                case None => buildRows(schema, outputRows.toVector).map(Some(_))
          batches.foreach(_.close())
          result

    def close(): Unit =
      if !closed then
        closed = true
        input.close()

  private final class JoinCursor(
      leftInput: ExecutionCursor,
      rightInput: ExecutionCursor,
      kind: JoinKind,
      condition: ResolvedExpr,
      schema: Schema
  ) extends ExecutionCursor:
    private var emitted = false
    private var closed = false

    def nextBatch(): Either[ExecutionError, Option[RecordBatch]] =
      if closed then Left(ExecutionError.Storage(StorageError.SourceClosed))
      else if emitted then Right(None)
      else
        emitted = true
        drain(leftInput).flatMap: leftBatches =>
          drain(rightInput) match
            case Left(error) =>
              leftBatches.foreach(_.close())
              Left(error)
            case Right(rightBatches) =>
              val outputRows = ArrayBuffer.empty[Array[ScalarValue]]
              var error: Option[ExecutionError] = None
              var leftBatchIndex = 0
              while leftBatchIndex < leftBatches.length && error.isEmpty do
                val leftBatch = leftBatches(leftBatchIndex)
                var leftRow = 0
                while leftRow < leftBatch.rowCount && error.isEmpty do
                  var matched = false
                  var rightBatchIndex = 0
                  while rightBatchIndex < rightBatches.length && error.isEmpty do
                    val rightBatch = rightBatches(rightBatchIndex)
                    var rightRow = 0
                    while rightRow < rightBatch.rowCount && error.isEmpty do
                      evaluateJoin(condition, leftBatch, leftRow, rightBatch, rightRow) match
                        case Right(ScalarValue.Bool(true)) =>
                          matched = true
                          joinedRow(leftBatch, leftRow, rightBatch, rightRow) match
                            case Right(row) => outputRows += row
                            case Left(value) => error = Some(value)
                        case Right(ScalarValue.Bool(false) | ScalarValue.Null) => ()
                        case Right(other) => error = Some(ExecutionError.PredicateType(condition.id, other))
                        case Left(value) => error = Some(value)
                      rightRow += 1
                    rightBatchIndex += 1
                  if !matched && kind == JoinKind.LeftOuter && error.isEmpty then
                    leftOnlyRow(leftBatch, leftRow, schema.size - leftBatch.columns.size) match
                      case Right(row) => outputRows += row
                      case Left(value) => error = Some(value)
                  leftRow += 1
                leftBatchIndex += 1

              val result = error match
                case Some(value) => Left(value)
                case None => buildRows(schema, outputRows.toVector).map(Some(_))
              leftBatches.foreach(_.close())
              rightBatches.foreach(_.close())
              result

    def close(): Unit =
      if !closed then
        closed = true
        leftInput.close()
        rightInput.close()

  private final class SortCursor(
      input: ExecutionCursor,
      order: Vector[SortExpression],
      schema: Schema
  ) extends ExecutionCursor:
    private var emitted = false
    private var closed = false

    def nextBatch(): Either[ExecutionError, Option[RecordBatch]] =
      if closed then Left(ExecutionError.Storage(StorageError.SourceClosed))
      else if emitted then Right(None)
      else
        emitted = true
        drain(input).flatMap: batches =>
          val rows = ArrayBuffer.empty[SortableRow]
          var ordinal = 0L
          var batchIndex = 0
          var error: Option[ExecutionError] = None
          while batchIndex < batches.length && error.isEmpty do
            val batch = batches(batchIndex)
            var row = 0
            while row < batch.rowCount && error.isEmpty do
              val keys = new Array[ScalarValue](order.length)
              var keyIndex = 0
              while keyIndex < order.length && error.isEmpty do
                evaluate(order(keyIndex).expression, EvalContext.current(batch), row) match
                  case Right(value) => keys(keyIndex) = value
                  case Left(value) => error = Some(value)
                keyIndex += 1
              if error.isEmpty then rows += SortableRow(RowRef(batch, row, ordinal), keys)
              ordinal += 1
              row += 1
            batchIndex += 1

          val result = error match
            case Some(value) => Left(value)
            case None =>
              var comparisonError: Option[ExecutionError] = None
              val sorted = rows.toVector.sortWith: (left, right) =>
                compareSortRows(left, right, order) match
                  case Right(value) => value < 0
                  case Left(value) =>
                    comparisonError = Some(value)
                    false
              comparisonError match
                case Some(value) => Left(value)
                case None =>
                  val outputRows = sorted.map(row => readRow(row.reference.batch, row.reference.row))
                  sequence(outputRows).flatMap(buildRows(schema, _)).map(Some(_))
          batches.foreach(_.close())
          result

    def close(): Unit =
      if !closed then
        closed = true
        input.close()

  private final case class RowRef(batch: RecordBatch, row: Int, ordinal: Long)
  private final case class GroupRows(keys: Array[ScalarValue], rows: ArrayBuffer[RowRef])
  private final case class SortableRow(reference: RowRef, keys: Array[ScalarValue])

  private enum KeyAtom:
    case Null
    case Bool(value: Boolean)
    case Int32(value: Int)
    case Int64(value: Long)
    case Float32(bits: Int)
    case Float64(bits: Long)
    case Utf8(value: String)
    case Timestamp(value: Long, unit: TimeUnit)

  private object KeyAtom:
    def fromScalar(value: ScalarValue): KeyAtom = value match
      case ScalarValue.Null => KeyAtom.Null
      case ScalarValue.Bool(actual) => KeyAtom.Bool(actual)
      case ScalarValue.Int32(actual) => KeyAtom.Int32(actual)
      case ScalarValue.Int64(actual) => KeyAtom.Int64(actual)
      case ScalarValue.Float32(actual) =>
        val normalized = if actual == 0.0f then 0.0f else actual
        KeyAtom.Float32(java.lang.Float.floatToIntBits(normalized))
      case ScalarValue.Float64(actual) =>
        val normalized = if actual == 0.0 then 0.0 else actual
        KeyAtom.Float64(java.lang.Double.doubleToLongBits(normalized))
      case ScalarValue.Utf8(actual) => KeyAtom.Utf8(actual)
      case ScalarValue.Timestamp(actual, unit) => KeyAtom.Timestamp(actual, unit)

  private def drain(cursor: ExecutionCursor): Either[ExecutionError, Vector[RecordBatch]] =
    val batches = ArrayBuffer.empty[RecordBatch]
    var done = false
    var error: Option[ExecutionError] = None
    while !done && error.isEmpty do
      cursor.nextBatch() match
        case Right(Some(batch)) => batches += batch
        case Right(None) => done = true
        case Left(value) => error = Some(value)
    cursor.close()
    error match
      case Some(value) =>
        batches.foreach(_.close())
        Left(value)
      case None => Right(batches.toVector)

  private def aggregateValue(
      aggregate: ResolvedAggregate,
      rows: Vector[RowRef]
  ): Either[ExecutionError, ScalarValue] = aggregate.node match
    case AggregateNode.Count => Right(ScalarValue.Int64(rows.length.toLong))
    case AggregateNode.Sum(input) =>
      var total: ScalarValue = ScalarValue.Null
      var index = 0
      var error: Option[ExecutionError] = None
      while index < rows.length && error.isEmpty do
        val row = rows(index)
        evaluate(input, EvalContext.current(row.batch), row.row) match
          case Right(ScalarValue.Null) => ()
          case Right(value) if total == ScalarValue.Null => total = value
          case Right(value) =>
            arithmetic(input.id, BinaryOperator.Add, total, value) match
              case Right(result) => total = result
              case Left(value) => error = Some(value)
          case Left(value) => error = Some(value)
        index += 1
      error.toLeft(total)
    case AggregateNode.Mean(input) =>
      numericMoments(input, rows).map: moments =>
        if moments.count == 0 then ScalarValue.Null
        else ScalarValue.Float64(moments.mean)
    case AggregateNode.Variance(input) =>
      numericMoments(input, rows).map: moments =>
        if moments.count == 0 then ScalarValue.Null
        else ScalarValue.Float64(moments.m2 / moments.count.toDouble)
    case AggregateNode.Min(input) => extremum(input, rows, minimum = true)
    case AggregateNode.Max(input) => extremum(input, rows, minimum = false)

  private final case class Moments(count: Long, mean: Double, m2: Double)

  private def numericMoments(
      input: ResolvedExpr,
      rows: Vector[RowRef]
  ): Either[ExecutionError, Moments] =
    var count = 0L
    var mean = 0.0
    var m2 = 0.0
    var index = 0
    var error: Option[ExecutionError] = None
    while index < rows.length && error.isEmpty do
      val row = rows(index)
      evaluate(input, EvalContext.current(row.batch), row.row) match
        case Right(ScalarValue.Null) => ()
        case Right(value) =>
          scalarDouble(input.id, value) match
            case Right(number) =>
              count += 1
              val delta = number - mean
              mean += delta / count.toDouble
              m2 += delta * (number - mean)
            case Left(value) => error = Some(value)
        case Left(value) => error = Some(value)
      index += 1
    error match
      case Some(value) => Left(value)
      case None => Right(Moments(count, mean, m2))

  private def scalarDouble(
      id: ExprId,
      value: ScalarValue
  ): Either[ExecutionError, Double] = value match
    case ScalarValue.Int32(actual) => Right(actual.toDouble)
    case ScalarValue.Int64(actual) => Right(actual.toDouble)
    case ScalarValue.Float32(actual) => Right(actual.toDouble)
    case ScalarValue.Float64(actual) => Right(actual)
    case other => Left(ExecutionError.ExpressionType(id, DataType.Float64, other))

  private def extremum(
      input: ResolvedExpr,
      rows: Vector[RowRef],
      minimum: Boolean
  ): Either[ExecutionError, ScalarValue] =
    var selected: ScalarValue = ScalarValue.Null
    var index = 0
    var error: Option[ExecutionError] = None
    while index < rows.length && error.isEmpty do
      val row = rows(index)
      evaluate(input, EvalContext.current(row.batch), row.row) match
        case Right(ScalarValue.Null) => ()
        case Right(value) if selected == ScalarValue.Null => selected = value
        case Right(value) =>
          compareValues(input.id, value, selected) match
            case Right(comparison) if (minimum && comparison < 0) || (!minimum && comparison > 0) =>
              selected = value
            case Right(_) => ()
            case Left(value) => error = Some(value)
        case Left(value) => error = Some(value)
      index += 1
    error.toLeft(selected)

  private def joinedRow(
      left: RecordBatch,
      leftRow: Int,
      right: RecordBatch,
      rightRow: Int
  ): Either[ExecutionError, Array[ScalarValue]] =
    val output = new Array[ScalarValue](left.columns.size + right.columns.size)
    var index = 0
    var error: Option[ExecutionError] = None
    while index < left.columns.size && error.isEmpty do
      left.columns(index).scalar(leftRow) match
        case Right(value) => output(index) = value
        case Left(value) => error = Some(ExecutionError.Storage(value))
      index += 1
    var rightIndex = 0
    while rightIndex < right.columns.size && error.isEmpty do
      right.columns(rightIndex).scalar(rightRow) match
        case Right(value) => output(left.columns.size + rightIndex) = value
        case Left(value) => error = Some(ExecutionError.Storage(value))
      rightIndex += 1
    error match
      case Some(value) => Left(value)
      case None => Right(output)

  private def leftOnlyRow(
      left: RecordBatch,
      leftRow: Int,
      rightColumns: Int
  ): Either[ExecutionError, Array[ScalarValue]] =
    val output = new Array[ScalarValue](left.columns.size + rightColumns)
    var index = 0
    var error: Option[ExecutionError] = None
    while index < left.columns.size && error.isEmpty do
      left.columns(index).scalar(leftRow) match
        case Right(value) => output(index) = value
        case Left(value) => error = Some(ExecutionError.Storage(value))
      index += 1
    while index < output.length do
      output(index) = ScalarValue.Null
      index += 1
    error match
      case Some(value) => Left(value)
      case None => Right(output)

  private def readRow(
      batch: RecordBatch,
      row: Int
  ): Either[ExecutionError, Array[ScalarValue]] =
    val output = new Array[ScalarValue](batch.columns.size)
    var index = 0
    var error: Option[ExecutionError] = None
    while index < batch.columns.size && error.isEmpty do
      batch.columns(index).scalar(row) match
        case Right(value) => output(index) = value
        case Left(value) => error = Some(ExecutionError.Storage(value))
      index += 1
    error match
      case Some(value) => Left(value)
      case None => Right(output)

  private def buildRows(
      schema: Schema,
      rows: Vector[Array[ScalarValue]]
  ): Either[ExecutionError, RecordBatch] =
    val columns = Vector.tabulate(schema.size): column =>
      val values = new Array[ScalarValue](rows.length)
      var row = 0
      while row < rows.length do
        values(row) = rows(row)(column)
        row += 1
      values
    buildBatch(schema, columns)

  private def compareSortRows(
      left: SortableRow,
      right: SortableRow,
      order: Vector[SortExpression]
  ): Either[ExecutionError, Int] =
    var index = 0
    var result = 0
    var error: Option[ExecutionError] = None
    while index < order.length && result == 0 && error.isEmpty do
      val leftValue = left.keys(index)
      val rightValue = right.keys(index)
      val item = order(index)
      val comparison = (leftValue, rightValue) match
        case (ScalarValue.Null, ScalarValue.Null) => Right(0)
        case (ScalarValue.Null, _) =>
          Right(if item.nulls == NullPlacement.First then -1 else 1)
        case (_, ScalarValue.Null) =>
          Right(if item.nulls == NullPlacement.First then 1 else -1)
        case _ => compareValues(item.expression.id, leftValue, rightValue)
      comparison match
        case Right(value) =>
          result = if item.direction == SortDirection.Ascending then value else -value
        case Left(value) => error = Some(value)
      index += 1
    error match
      case Some(value) => Left(value)
      case None if result != 0 => Right(result)
      case None => Right(left.reference.ordinal.compare(right.reference.ordinal))

  private def evaluateJoin(
      expression: ResolvedExpr,
      left: RecordBatch,
      leftRow: Int,
      right: RecordBatch,
      rightRow: Int
  ): Either[ExecutionError, ScalarValue] = expression.node match
    case ExprNode.Column(InputRef.Left, _, _, index) =>
      left.columns(index).scalar(leftRow).left.map(ExecutionError.Storage.apply)
    case ExprNode.Column(InputRef.Right, _, _, index) =>
      right.columns(index).scalar(rightRow).left.map(ExecutionError.Storage.apply)
    case ExprNode.Column(InputRef.Current, _, _, _) =>
      Left(ExecutionError.UnsupportedNode("current expression inside join"))
    case ExprNode.Literal(value) => literal(value)
    case ExprNode.Unary(operator, input) =>
      evaluateJoin(input, left, leftRow, right, rightRow)
        .flatMap(unary(expression.id, operator, _))
    case ExprNode.Binary(operator, lhs, rhs) =>
      evaluateJoin(lhs, left, leftRow, right, rightRow).flatMap: leftValue =>
        evaluateJoin(rhs, left, leftRow, right, rightRow)
          .flatMap(rightValue => binary(expression.id, operator, leftValue, rightValue))

  private final case class EvalContext(
      current: Option[RecordBatch],
      left: Option[RecordBatch],
      right: Option[RecordBatch]
  )

  private object EvalContext:
    def current(batch: RecordBatch): EvalContext = EvalContext(Some(batch), None, None)

  private def evaluate(
      expression: ResolvedExpr,
      context: EvalContext,
      row: Int
  ): Either[ExecutionError, ScalarValue] = expression.node match
    case ExprNode.Column(input, _, _, index) =>
      val batch = input match
        case InputRef.Current => context.current
        case InputRef.Left => context.left
        case InputRef.Right => context.right
      batch match
        case None => Left(ExecutionError.UnsupportedNode(s"${input.qualifier} expression scope"))
        case Some(value) => value.columns(index).scalar(row).left.map(ExecutionError.Storage.apply)
    case ExprNode.Literal(value) => literal(value)
    case ExprNode.Unary(operator, input) =>
      evaluate(input, context, row).flatMap(unary(expression.id, operator, _))
    case ExprNode.Binary(operator, left, right) =>
      evaluate(left, context, row).flatMap: lhs =>
        evaluate(right, context, row).flatMap(rhs => binary(expression.id, operator, lhs, rhs))

  private def literal(value: LiteralValue): Either[ExecutionError, ScalarValue] = value match
    case LiteralValue.Null(_) => Right(ScalarValue.Null)
    case LiteralValue.Bool(value) => Right(ScalarValue.Bool(value))
    case LiteralValue.Int32(value) => Right(ScalarValue.Int32(value))
    case LiteralValue.Int64(value) => Right(ScalarValue.Int64(value))
    case LiteralValue.Float32(value) => Right(ScalarValue.Float32(value))
    case LiteralValue.Float64(value) => Right(ScalarValue.Float64(value))
    case LiteralValue.Utf8(value) => Right(ScalarValue.Utf8(value))
    case LiteralValue.Timestamp(value, unit) => Right(ScalarValue.Timestamp(value, unit))

  private def unary(
      id: ExprId,
      operator: UnaryOperator,
      input: ScalarValue
  ): Either[ExecutionError, ScalarValue] = operator match
    case UnaryOperator.IsNull => Right(ScalarValue.Bool(input == ScalarValue.Null))
    case UnaryOperator.IsTrue => Right(ScalarValue.Bool(input == ScalarValue.Bool(true)))
    case UnaryOperator.Negate => input match
      case ScalarValue.Null => Right(ScalarValue.Null)
      case ScalarValue.Int32(value) if value == Int.MinValue =>
        Left(ExecutionError.IntegerOverflow(id, BinaryOperator.Subtract))
      case ScalarValue.Int32(value) => Right(ScalarValue.Int32(-value))
      case ScalarValue.Int64(value) if value == Long.MinValue =>
        Left(ExecutionError.IntegerOverflow(id, BinaryOperator.Subtract))
      case ScalarValue.Int64(value) => Right(ScalarValue.Int64(-value))
      case ScalarValue.Float32(value) => Right(ScalarValue.Float32(-value))
      case ScalarValue.Float64(value) => Right(ScalarValue.Float64(-value))
      case other => Left(ExecutionError.ExpressionType(id, DataType.Float64, other))

  private def binary(
      id: ExprId,
      operator: BinaryOperator,
      left: ScalarValue,
      right: ScalarValue
  ): Either[ExecutionError, ScalarValue] = operator match
    case BinaryOperator.NullSafeEqual => Right(ScalarValue.Bool(nullSafeEqual(left, right)))
    case BinaryOperator.And => Right(fromTri(and(toTri(left), toTri(right))))
    case BinaryOperator.Or => Right(fromTri(or(toTri(left), toTri(right))))
    case _ if left == ScalarValue.Null || right == ScalarValue.Null => Right(ScalarValue.Null)
    case BinaryOperator.Equal => Right(ScalarValue.Bool(equalValues(left, right)))
    case BinaryOperator.NotEqual => Right(ScalarValue.Bool(!equalValues(left, right)))
    case BinaryOperator.LessThan => compareValues(id, left, right).map(value => ScalarValue.Bool(value < 0))
    case BinaryOperator.LessThanOrEqual => compareValues(id, left, right).map(value => ScalarValue.Bool(value <= 0))
    case BinaryOperator.GreaterThan => compareValues(id, left, right).map(value => ScalarValue.Bool(value > 0))
    case BinaryOperator.GreaterThanOrEqual => compareValues(id, left, right).map(value => ScalarValue.Bool(value >= 0))
    case BinaryOperator.Add => arithmetic(id, operator, left, right)
    case BinaryOperator.Subtract => arithmetic(id, operator, left, right)
    case BinaryOperator.Multiply => arithmetic(id, operator, left, right)
    case BinaryOperator.Divide => arithmetic(id, operator, left, right)

  private def toTri(value: ScalarValue): TriBool = value match
    case ScalarValue.Bool(true) => TriBool.True
    case ScalarValue.Bool(false) => TriBool.False
    case _ => TriBool.Unknown

  private def fromTri(value: TriBool): ScalarValue = value match
    case TriBool.True => ScalarValue.Bool(true)
    case TriBool.False => ScalarValue.Bool(false)
    case TriBool.Unknown => ScalarValue.Null

  private def and(left: TriBool, right: TriBool): TriBool = (left, right) match
    case (TriBool.False, _) | (_, TriBool.False) => TriBool.False
    case (TriBool.True, TriBool.True) => TriBool.True
    case _ => TriBool.Unknown

  private def or(left: TriBool, right: TriBool): TriBool = (left, right) match
    case (TriBool.True, _) | (_, TriBool.True) => TriBool.True
    case (TriBool.False, TriBool.False) => TriBool.False
    case _ => TriBool.Unknown

  private def nullSafeEqual(left: ScalarValue, right: ScalarValue): Boolean =
    if left == ScalarValue.Null then right == ScalarValue.Null
    else if right == ScalarValue.Null then false
    else equalValues(left, right)

  private def equalValues(left: ScalarValue, right: ScalarValue): Boolean = (left, right) match
    case (ScalarValue.Float32(a), ScalarValue.Float32(b)) => !a.isNaN && !b.isNaN && a == b
    case (ScalarValue.Float64(a), ScalarValue.Float64(b)) => !a.isNaN && !b.isNaN && a == b
    case _ => left == right

  private def compareValues(
      id: ExprId,
      left: ScalarValue,
      right: ScalarValue
  ): Either[ExecutionError, Int] = (left, right) match
    case (ScalarValue.Bool(a), ScalarValue.Bool(b)) => Right(a.compare(b))
    case (ScalarValue.Int32(a), ScalarValue.Int32(b)) => Right(a.compare(b))
    case (ScalarValue.Int64(a), ScalarValue.Int64(b)) => Right(a.compare(b))
    case (ScalarValue.Float32(a), ScalarValue.Float32(b)) => Right(compareFloat(a.toDouble, b.toDouble))
    case (ScalarValue.Float64(a), ScalarValue.Float64(b)) => Right(compareFloat(a, b))
    case (ScalarValue.Utf8(a), ScalarValue.Utf8(b)) => Right(compareUtf8(a, b))
    case (ScalarValue.Timestamp(a, unitA), ScalarValue.Timestamp(b, unitB)) if unitA == unitB =>
      Right(a.compare(b))
    case _ => Left(ExecutionError.ExpressionType(id, scalarType(left), right))

  private def compareFloat(left: Double, right: Double): Int =
    if left.isNaN then if right.isNaN then 0 else 1
    else if right.isNaN then -1
    else left.compare(right)

  private def compareUtf8(left: String, right: String): Int =
    val leftBytes = left.getBytes("UTF-8")
    val rightBytes = right.getBytes("UTF-8")
    val limit = Math.min(leftBytes.length, rightBytes.length)
    var index = 0
    while index < limit do
      val compared = (leftBytes(index) & 0xff).compare(rightBytes(index) & 0xff)
      if compared != 0 then return compared
      index += 1
    leftBytes.length.compare(rightBytes.length)

  private def arithmetic(
      id: ExprId,
      operator: BinaryOperator,
      left: ScalarValue,
      right: ScalarValue
  ): Either[ExecutionError, ScalarValue] = (left, right) match
    case (ScalarValue.Int32(a), ScalarValue.Int32(b)) =>
      if operator == BinaryOperator.Divide && b == 0 then Left(ExecutionError.DivisionByZero(id))
      else
        val result = operator match
          case BinaryOperator.Add => BigInt(a) + BigInt(b)
          case BinaryOperator.Subtract => BigInt(a) - BigInt(b)
          case BinaryOperator.Multiply => BigInt(a) * BigInt(b)
          case BinaryOperator.Divide => BigInt(a) / BigInt(b)
          case _ => BigInt(0)
        if !result.isValidInt then Left(ExecutionError.IntegerOverflow(id, operator))
        else Right(ScalarValue.Int32(result.toInt))
    case (ScalarValue.Int64(a), ScalarValue.Int64(b)) =>
      if operator == BinaryOperator.Divide && b == 0L then Left(ExecutionError.DivisionByZero(id))
      else
        val result = operator match
          case BinaryOperator.Add => BigInt(a) + BigInt(b)
          case BinaryOperator.Subtract => BigInt(a) - BigInt(b)
          case BinaryOperator.Multiply => BigInt(a) * BigInt(b)
          case BinaryOperator.Divide => BigInt(a) / BigInt(b)
          case _ => BigInt(0)
        if !result.isValidLong then Left(ExecutionError.IntegerOverflow(id, operator))
        else Right(ScalarValue.Int64(result.toLong))
    case (ScalarValue.Float32(a), ScalarValue.Float32(b)) =>
      Right(ScalarValue.Float32(floatOperation(operator, a.toDouble, b.toDouble).toFloat))
    case (ScalarValue.Float64(a), ScalarValue.Float64(b)) =>
      Right(ScalarValue.Float64(floatOperation(operator, a, b)))
    case _ => Left(ExecutionError.ExpressionType(id, scalarType(left), right))

  private def floatOperation(operator: BinaryOperator, left: Double, right: Double): Double = operator match
    case BinaryOperator.Add => left + right
    case BinaryOperator.Subtract => left - right
    case BinaryOperator.Multiply => left * right
    case BinaryOperator.Divide => left / right
    case _ => Double.NaN

  private def scalarType(value: ScalarValue): DataType = value match
    case ScalarValue.Null => DataType.Utf8
    case ScalarValue.Bool(_) => DataType.Bool
    case ScalarValue.Int32(_) => DataType.Int32
    case ScalarValue.Int64(_) => DataType.Int64
    case ScalarValue.Float32(_) => DataType.Float32
    case ScalarValue.Float64(_) => DataType.Float64
    case ScalarValue.Utf8(_) => DataType.Utf8
    case ScalarValue.Timestamp(_, unit) => DataType.Timestamp(unit)

  private def sequence[A](
      values: Vector[Either[ExecutionError, A]]
  ): Either[ExecutionError, Vector[A]] =
    val output = ArrayBuffer.empty[A]
    var index = 0
    var error: Option[ExecutionError] = None
    while index < values.length && error.isEmpty do
      values(index) match
        case Right(value) => output += value
        case Left(value) => error = Some(value)
      index += 1
    error match
      case Some(value) => Left(value)
      case None => Right(output.toVector)

  private def buildBatch(
      schema: Schema,
      columns: Vector[Array[ScalarValue]]
  ): Either[ExecutionError, RecordBatch] =
    val built = ArrayBuffer.empty[ColumnArray]
    var index = 0
    var error: Option[ExecutionError] = None
    while index < schema.fields.length && error.isEmpty do
      buildColumn(schema.fields(index), columns(index)) match
        case Right(column) => built += column
        case Left(value) => error = Some(value)
      index += 1
    error match
      case Some(value) =>
        built.foreach(_.close())
        Left(value)
      case None =>
        RecordBatch(schema, built.toVector).left.map(ExecutionError.Storage.apply)

  private def buildColumn(
      field: Field,
      values: Array[ScalarValue]
  ): Either[ExecutionError, ColumnArray] =
    val valid = values.map(_ != ScalarValue.Null)
    def mismatch(actual: ScalarValue) =
      Left(ExecutionError.ExpressionType(ExprId.derived(field.name), field.dataType, actual))
    field.dataType match
      case DataType.Bool =>
        val output = new Array[Boolean](values.length)
        var index = 0
        while index < values.length do
          values(index) match
            case ScalarValue.Bool(value) => output(index) = value
            case ScalarValue.Null => ()
            case other => return mismatch(other)
          index += 1
        ColumnArray.bool(output, valid).left.map(ExecutionError.Storage.apply)
      case DataType.Int32 =>
        val output = new Array[Int](values.length)
        var index = 0
        while index < values.length do
          values(index) match
            case ScalarValue.Int32(value) => output(index) = value
            case ScalarValue.Null => ()
            case other => return mismatch(other)
          index += 1
        ColumnArray.int32(output, valid).left.map(ExecutionError.Storage.apply)
      case DataType.Int64 =>
        val output = new Array[Long](values.length)
        var index = 0
        while index < values.length do
          values(index) match
            case ScalarValue.Int64(value) => output(index) = value
            case ScalarValue.Null => ()
            case other => return mismatch(other)
          index += 1
        ColumnArray.int64(output, valid).left.map(ExecutionError.Storage.apply)
      case DataType.Float32 =>
        val output = new Array[Float](values.length)
        var index = 0
        while index < values.length do
          values(index) match
            case ScalarValue.Float32(value) => output(index) = value
            case ScalarValue.Null => ()
            case other => return mismatch(other)
          index += 1
        ColumnArray.float32(output, valid).left.map(ExecutionError.Storage.apply)
      case DataType.Float64 =>
        val output = new Array[Double](values.length)
        var index = 0
        while index < values.length do
          values(index) match
            case ScalarValue.Float64(value) => output(index) = value
            case ScalarValue.Null => ()
            case other => return mismatch(other)
          index += 1
        ColumnArray.float64(output, valid).left.map(ExecutionError.Storage.apply)
      case DataType.Utf8 =>
        val output = new Array[String](values.length)
        var index = 0
        while index < values.length do
          values(index) match
            case ScalarValue.Utf8(value) => output(index) = value
            case ScalarValue.Null => output(index) = ""
            case other => return mismatch(other)
          index += 1
        ColumnArray.utf8(output, valid).left.map(ExecutionError.Storage.apply)
      case DataType.Timestamp(unit) =>
        val output = new Array[Long](values.length)
        var index = 0
        while index < values.length do
          values(index) match
            case ScalarValue.Timestamp(value, actualUnit) if actualUnit == unit => output(index) = value
            case ScalarValue.Null => ()
            case other => return mismatch(other)
          index += 1
        ColumnArray.timestamp(output, unit, valid).left.map(ExecutionError.Storage.apply)
