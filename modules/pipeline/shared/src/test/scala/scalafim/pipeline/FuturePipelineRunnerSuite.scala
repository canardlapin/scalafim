package scalafim.pipeline

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class FuturePipelineRunnerSuite extends munit.FunSuite:
  private val intKind = ArtifactKind.unsafe[Int]("int")
  private val stringKind = ArtifactKind.unsafe[String]("string")

  private object AddOne extends PipelineStep[Int, Int]:
    override val id: StepId = StepId.unsafe("add-one")
    override val outputKind: ArtifactKind[Int] = intKind

    override def run(input: Int, context: RunContext): Either[PipelineError, Int] =
      Right(input + 1)

  private object TimesTen extends PipelineStep[Int, Int]:
    override val id: StepId = StepId.unsafe("times-ten")
    override val outputKind: ArtifactKind[Int] = intKind

    override def run(input: Int, context: RunContext): Either[PipelineError, Int] =
      Right(input * 10)

  private object Sum extends PipelineStep[(Int, Int), Int]:
    override val id: StepId = StepId.unsafe("sum")
    override val outputKind: ArtifactKind[Int] = intKind

    override def run(input: (Int, Int), context: RunContext): Either[PipelineError, Int] =
      Right(input._1 + input._2)

  private object Stringify extends PipelineStep[Int, String]:
    override val id: StepId = StepId.unsafe("stringify")
    override val outputKind: ArtifactKind[String] = stringKind

    override def run(input: Int, context: RunContext): Either[PipelineError, String] =
      Right(s"value=$input")

  private object Fail extends PipelineStep[Int, Int]:
    override val id: StepId = StepId.unsafe("fail")
    override val outputKind: ArtifactKind[Int] = intKind

    override def run(input: Int, context: RunContext): Either[PipelineError, Int] =
      Left(PipelineError.InvalidGraph("parallel failure"))

  private object Throw extends PipelineStep[Int, Int]:
    override val id: StepId = StepId.unsafe("throw")
    override val outputKind: ArtifactKind[Int] = intKind

    override def run(input: Int, context: RunContext): Either[PipelineError, Int] =
      throw new IllegalStateException("thrown failure")

  private def branchedGraph: (PipelineGraph, ArtifactRef[Int], ArtifactRef[String]) =
    val base = PipelineGraph.empty(PipelineId.unsafe("parallel"))
    val (g1, input) = base.addInput(NodeId.unsafe("x"), intKind).toOption.get
    val (g2, a) = g1.addStep(NodeId.unsafe("a"), AddOne, input.expr).toOption.get
    val (g3, b) = g2.addStep(NodeId.unsafe("b"), TimesTen, input.expr).toOption.get
    val (g4, sum) = g3.addStep(NodeId.unsafe("sum"), Sum, a.expr.zip(b.expr)).toOption.get
    val (g5, rendered) = g4.addStep(NodeId.unsafe("render"), Stringify, sum.expr).toOption.get
    val g6 = g5.addOutput(PortName.unsafe("rendered"), rendered).toOption.get
    (g6, input, rendered)

  test("future runner preserves stage barriers and deterministic receipt order") {
    val (graph, input, rendered) = branchedGraph
    val context = RunContext.empty.withInput(input, 3)

    FuturePipelineRunner.run(graph, context).map { run =>
      assert(run.succeeded)
      assertEquals(run.get(rendered).toOption.get, "value=34")
      assertEquals(
        run.receipts.map(_.nodeId.value),
        Vector("x", "a", "b", "sum", "render")
      )
      assertEquals(
        graph.executionPlan.toOption.get.stages.map(_.nodeIds.map(_.value)),
        Vector(Vector("x"), Vector("a", "b"), Vector("sum"), Vector("render"))
      )
    }
  }

  test("future runner matches the local runner surface apart from interpreter metadata") {
    val (graph, input, rendered) = branchedGraph
    val context =
      RunContext.empty
        .withMetadata("subject", "sub-01")
        .withInput(input, 2)
    val local = LocalPipelineRunner.run(graph, context)

    FuturePipelineRunner.run(graph, context, ParallelPolicy.unsafe(1)).map { future =>
      assertEquals(future.status, local.status)
      assertEquals(future.output("rendered", stringKind), local.output("rendered", stringKind))
      assertEquals(future.get(rendered), local.get(rendered))
      assertEquals(future.receipts, local.receipts)
      assertEquals(future.outputs, local.outputs)
      assertEquals(future.nodeDescriptions, local.nodeDescriptions)
      assertEquals(future.userMetadata, local.userMetadata)
      assertEquals(future.trace.nodes.map(_.status), local.trace.nodes.map(_.status))
      assertEquals(future.runner.name, "future")
      assertEquals(local.runner.name, "local")
    }
  }

  test("local, bounded future, and unbounded future agree across an input family") {
    Future.sequence {
      Vector(-2, 0, 1, 7).map { value =>
        val (graph, input, rendered) = branchedGraph
        val context = RunContext.empty.withInput(input, value)
        val local = LocalPipelineRunner.run(graph, context)

        for
          unbounded <- FuturePipelineRunner.run(graph, context)
          bounded <- FuturePipelineRunner.run(graph, context, ParallelPolicy.unsafe(1))
        yield
          assertEquals(unbounded.output("rendered", stringKind), local.output("rendered", stringKind))
          assertEquals(bounded.output("rendered", stringKind), local.output("rendered", stringKind))
          assertEquals(unbounded.get(rendered), local.get(rendered))
          assertEquals(bounded.get(rendered), local.get(rendered))
          assertEquals(unbounded.receipts, local.receipts)
          assertEquals(bounded.receipts, local.receipts)
          assertEquals(unbounded.trace.nodes.map(_.status), local.trace.nodes.map(_.status))
          assertEquals(bounded.trace.nodes.map(_.status), local.trace.nodes.map(_.status))
      }
    }.map(_ => ())
  }

  test("bounded and unbounded future runners produce the same run surface") {
    val (graph, input, rendered) = branchedGraph
    val context = RunContext.empty.withInput(input, 4)

    val unbounded = FuturePipelineRunner.run(graph, context)
    val bounded = FuturePipelineRunner.run(graph, context, ParallelPolicy.unsafe(1))

    for
      a <- unbounded
      b <- bounded
    yield
      assertEquals(a.status, b.status)
      assertEquals(a.get(rendered), b.get(rendered))
      assertEquals(a.receipts, b.receipts)
  }

  test("future runner records failures and skips later descendants without stopping independent nodes") {
    val base = PipelineGraph.empty(PipelineId.unsafe("future-failure"))
    val (g1, input) = base.addInput(NodeId.unsafe("x"), intKind).toOption.get
    val (g2, failed) = g1.addStep(NodeId.unsafe("bad"), Fail, input.expr).toOption.get
    val (g3, independent) = g2.addStep(NodeId.unsafe("independent"), Stringify, PipelineExpr.const(9, "nine")).toOption.get
    val (g4, downstream) = g3.addStep(NodeId.unsafe("downstream"), AddOne, failed.expr).toOption.get

    FuturePipelineRunner.run(g4, RunContext.empty.withInput(input, 2)).map { run =>
      val byNode = run.receipts.map(receipt => receipt.nodeId.value -> receipt).toMap

      assertEquals(run.status, PipelineStatus.Failed)
      assert(run.error.exists(_.message.contains("parallel failure")))
      assertEquals(byNode("x").status, PipelineStatus.Succeeded)
      assertEquals(byNode("bad").status, PipelineStatus.Failed)
      assertEquals(byNode("independent").status, PipelineStatus.Succeeded)
      assertEquals(byNode("downstream").status, PipelineStatus.Skipped)
      assertEquals(run.get(independent).toOption.get, "value=9")
      assert(run.get(downstream).isLeft)
    }
  }

  test("future runner keeps receipt order for mixed runnable and skipped nodes in one stage") {
    val base = PipelineGraph.empty(PipelineId.unsafe("mixed-stage"))
    val (g1, input) = base.addInput(NodeId.unsafe("x"), intKind).toOption.get
    val (g2, failed) = g1.addStep(NodeId.unsafe("bad"), Fail, input.expr).toOption.get
    val (g3, good) = g2.addStep(NodeId.unsafe("good"), AddOne, input.expr).toOption.get
    val (g4, goodChild) = g3.addStep(NodeId.unsafe("good-child"), AddOne, good.expr).toOption.get
    val (g5, badChild) = g4.addStep(NodeId.unsafe("bad-child"), AddOne, failed.expr).toOption.get

    FuturePipelineRunner.run(g5, RunContext.empty.withInput(input, 1)).map { run =>
      assertEquals(
        g5.executionPlan.toOption.get.stages.map(_.nodeIds.map(_.value)),
        Vector(Vector("x"), Vector("bad", "good"), Vector("good-child", "bad-child"))
      )
      assertEquals(
        run.receipts.map(_.nodeId.value),
        Vector("x", "bad", "good", "good-child", "bad-child")
      )
      assertEquals(run.get(goodChild).toOption.get, 3)
      assert(run.get(badChild).isLeft)
    }
  }

  test("future runner records missing inputs and skips dependent nodes") {
    val base = PipelineGraph.empty(PipelineId.unsafe("missing-input"))
    val (g1, input) = base.addInput(NodeId.unsafe("x"), intKind).toOption.get
    val (graph, out) = g1.addStep(NodeId.unsafe("child"), AddOne, input.expr).toOption.get

    FuturePipelineRunner.run(graph).map { run =>
      val byNode = run.receipts.map(receipt => receipt.nodeId.value -> receipt).toMap

      assertEquals(run.status, PipelineStatus.Failed)
      assertEquals(run.error, Some(PipelineError.MissingPipelineInput(input.nodeId)))
      assertEquals(byNode("x").status, PipelineStatus.Failed)
      assertEquals(byNode("child").status, PipelineStatus.Skipped)
      assert(byNode("child").message.exists(_.contains("dependency 'x'")))
      assert(run.get(out).isLeft)
    }
  }

  test("future runner attributes thrown step exceptions to the step node") {
    val base = PipelineGraph.empty(PipelineId.unsafe("thrown-step"))
    val (g1, input) = base.addInput(NodeId.unsafe("x"), intKind).toOption.get
    val (graph, out) = g1.addStep(NodeId.unsafe("explode"), Throw, input.expr).toOption.get

    FuturePipelineRunner.run(graph, RunContext.empty.withInput(input, 1)).map { run =>
      val thrown = run.receipts.find(_.nodeId.value == "explode").get

      assertEquals(run.status, PipelineStatus.Failed)
      assertEquals(thrown.status, PipelineStatus.Failed)
      assert(thrown.message.exists(_.contains("step 'throw' at node 'explode' failed: thrown failure")))
      assert(run.error.exists(_.message.contains("step 'throw' at node 'explode'")))
      assert(run.get(out).isLeft)
    }
  }

  test("future runner repeated runs with different contexts do not leak values") {
    val (graph, input, _) = branchedGraph

    for
      first <- FuturePipelineRunner.run(graph, RunContext.empty.withInput(input, 1))
      second <- FuturePipelineRunner.run(graph, RunContext.empty.withInput(input, 5))
    yield
      assertEquals(first.output("rendered", stringKind).toOption.get, "value=12")
      assertEquals(second.output("rendered", stringKind).toOption.get, "value=56")
      assertEquals(first.receipts.map(_.status), second.receipts.map(_.status))
  }

  test("parallel policy validates concurrency") {
    assertEquals(ParallelPolicy.bounded(2).toOption.get.maxConcurrency, 2)
    assert(ParallelPolicy.bounded(0).swap.toOption.get.message.contains("positive"))
  }
