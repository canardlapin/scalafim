package scalafim.pipeline

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

final case class ParallelPolicy private (maxConcurrency: Int):
  require(maxConcurrency > 0, "maxConcurrency must be positive")

object ParallelPolicy:
  val unbounded: ParallelPolicy =
    ParallelPolicy(Int.MaxValue)

  def bounded(maxConcurrency: Int): Either[PipelineError, ParallelPolicy] =
    if maxConcurrency > 0 then Right(ParallelPolicy(maxConcurrency))
    else Left(PipelineError.InvalidGraph(s"maxConcurrency must be positive, got $maxConcurrency"))

  def unsafe(maxConcurrency: Int): ParallelPolicy =
    bounded(maxConcurrency).fold(error => throw new IllegalArgumentException(error.message), identity)

object FuturePipelineRunner:
  def run(
      graph: PipelineGraph,
      context: RunContext = RunContext.empty,
      policy: ParallelPolicy = ParallelPolicy.unbounded
  )(using ExecutionContext): Future[PipelineRun] =
    graph.executionPlan match
      case Left(error) =>
        Future.successful(
          PipelineRun(
            graphId = graph.id,
            status = PipelineStatus.Failed,
            values = ArtifactTable.empty,
            receipts = Vector.empty,
            error = Some(error)
          )
        )
      case Right(plan) =>
        runPlan(plan, context, policy)

  def runPlan(
      plan: ExecutionPlan,
      context: RunContext = RunContext.empty,
      policy: ParallelPolicy = ParallelPolicy.unbounded
  )(using ExecutionContext): Future[PipelineRun] =
    runStages(
      graphId = plan.graph.id,
      stages = plan.stages,
      table = ArtifactTable.empty,
      failedOrSkipped = Set.empty,
      firstError = None,
      receipts = Vector.empty,
      context = context,
      policy = policy
    )

  private final case class NodeResult(
      node: PipelineNode,
      result: Either[PipelineError, StoredArtifact]
  )

  private final case class StageState(
      table: ArtifactTable,
      failedOrSkipped: Set[NodeId],
      firstError: Option[PipelineError],
      receipts: Vector[StepReceipt]
  )

  private final case class StageItem(
      node: PipelineNode,
      skippedReceipt: Option[StepReceipt]
  )

  private def runStages(
      graphId: PipelineId,
      stages: Vector[PipelineStage],
      table: ArtifactTable,
      failedOrSkipped: Set[NodeId],
      firstError: Option[PipelineError],
      receipts: Vector[StepReceipt],
      context: RunContext,
      policy: ParallelPolicy
  )(using ExecutionContext): Future[PipelineRun] =
    if stages.isEmpty then
      val finalStatus =
        if firstError.isEmpty && failedOrSkipped.isEmpty then PipelineStatus.Succeeded
        else PipelineStatus.Failed
      Future.successful(
        PipelineRun(
          graphId = graphId,
          status = finalStatus,
          values = table,
          receipts = receipts,
          error = firstError
        )
      )
    else
      val stage = stages.head
      val remaining = stages.tail
      runStage(stage, table, failedOrSkipped, firstError, receipts, context, policy).flatMap { state =>
        runStages(
          graphId = graphId,
          stages = remaining,
          table = state.table,
          failedOrSkipped = state.failedOrSkipped,
          firstError = state.firstError,
          receipts = state.receipts,
          context = context,
          policy = policy
        )
      }

  private def runStage(
      stage: PipelineStage,
      table: ArtifactTable,
      failedOrSkipped: Set[NodeId],
      firstError: Option[PipelineError],
      receipts: Vector[StepReceipt],
      context: RunContext,
      policy: ParallelPolicy
  )(using ExecutionContext): Future[StageState] =
    val items =
      stage.nodes.map { node =>
        val skippedReceipt =
          node.dependencies.find(ref => failedOrSkipped.contains(ref.nodeId)).map { dep =>
            receipt(
              node,
              PipelineStatus.Skipped,
              Some(s"dependency '${dep.nodeId.value}' did not succeed")
            )
          }
        StageItem(node, skippedReceipt)
      }
    val skippedIds =
      failedOrSkipped ++ items.collect { case StageItem(node, Some(_)) => node.id }
    val runnable =
      items.collect { case StageItem(node, None) => node }

    runBounded(runnable, policy) { node =>
      Future(executeNode(node, table, context))
    }.map { results =>
      val byNodeId = results.map(result => result.node.id -> result).toMap
      mergeStageResults(
        table = table,
        failedOrSkipped = skippedIds,
        firstError = firstError,
        receipts = receipts,
        items = items,
        results = byNodeId
      )
    }

  private def runBounded[A, B](
      values: Vector[A],
      policy: ParallelPolicy
  )(
      f: A => Future[B]
  )(using ExecutionContext): Future[Vector[B]] =
    if values.isEmpty then Future.successful(Vector.empty)
    else
      val chunkSize = math.min(policy.maxConcurrency, values.length)
      val chunks = values.grouped(chunkSize).toVector
      chunks.foldLeft(Future.successful(Vector.empty[B])) { (acc, chunk) =>
        acc.flatMap { collected =>
          Future.sequence(chunk.map(f)).map(results => collected ++ results)
        }
      }

  private def executeNode(
      node: PipelineNode,
      table: ArtifactTable,
      context: RunContext
  ): NodeResult =
    val result =
      try node.execute(table, context)
      catch
        case NonFatal(t) =>
          val reason = Option(t.getMessage).filter(_.nonEmpty).getOrElse(t.toString)
          node.description.step match
            case Some(step) => Left(PipelineError.StepFailed(node.id, step.stepId, reason))
            case None       => Left(PipelineError.InvalidGraph(reason))
    NodeResult(node, result)

  private def mergeStageResults(
      table: ArtifactTable,
      failedOrSkipped: Set[NodeId],
      firstError: Option[PipelineError],
      receipts: Vector[StepReceipt],
      items: Vector[StageItem],
      results: Map[NodeId, NodeResult]
  ): StageState =
    var nextTable = table
    var nextFailedOrSkipped = failedOrSkipped
    var nextFirstError = firstError
    val nextReceipts = Vector.newBuilder[StepReceipt]
    nextReceipts ++= receipts

    items.foreach {
      case StageItem(_, Some(skippedReceipt)) =>
        nextReceipts += skippedReceipt

      case StageItem(node, None) =>
        results(node.id).result match
          case Right(stored) =>
            nextTable.putStored(node.output, stored) match
              case Right(updated) =>
                nextTable = updated
                nextReceipts += receipt(node, PipelineStatus.Succeeded, None)
              case Left(error) =>
                if nextFirstError.isEmpty then nextFirstError = Some(error)
                nextFailedOrSkipped = nextFailedOrSkipped + node.id
                nextReceipts += receipt(node, PipelineStatus.Failed, Some(error.message))
          case Left(error) =>
            if nextFirstError.isEmpty then nextFirstError = Some(error)
            nextFailedOrSkipped = nextFailedOrSkipped + node.id
            nextReceipts += receipt(node, PipelineStatus.Failed, Some(error.message))
    }

    StageState(
      table = nextTable,
      failedOrSkipped = nextFailedOrSkipped,
      firstError = nextFirstError,
      receipts = nextReceipts.result()
    )

  private def receipt(
      node: PipelineNode,
      status: PipelineStatus,
      message: Option[String]
  ): StepReceipt =
    StepReceipt(
      nodeId = node.id,
      status = status,
      dependencies = node.dependencies.map(_.nodeId).distinct,
      outputKind = node.output.kind.label,
      stepId = node.description.step.map(_.stepId),
      message = message
    )
