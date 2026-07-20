package scalafim.pipeline

import scala.concurrent.ExecutionContext.Implicits.global

class PipelineProvenanceSuite extends munit.FunSuite:
  private val intKind = ArtifactKind.unsafe[Int]("int")

  private object AddOne extends PipelineStep[Int, Int]:
    override val id: StepId = StepId.unsafe("add-one")
    override val outputKind: ArtifactKind[Int] = intKind

    override def run(input: Int, context: RunContext): Either[PipelineError, Int] =
      Right(input + 1)

  private object Fail extends PipelineStep[Int, Int]:
    override val id: StepId = StepId.unsafe("fail")
    override val outputKind: ArtifactKind[Int] = intKind

    override def run(input: Int, context: RunContext): Either[PipelineError, Int] =
      Left(PipelineError.InvalidGraph("boom"))

  private def successGraph: (PipelineGraph, ArtifactRef[Int]) =
    val built =
      for
        b0 <- PipelineBuilder("trace-success")
        input <- b0.input("x", intKind, Some("raw-x"))
        out <- input.builder.step("add", AddOne, input.ref, Some("x-plus-one"))
        graph <- out.builder.output("answer", out.ref).map(_.build)
      yield (graph, input.ref)
    built.toOption.get

  test("local run trace exposes deterministic structured provenance and text rendering") {
    val (graph, input) = successGraph
    val context =
      RunContext.empty
        .withMetadata("z", "last")
        .withMetadata("a", "first")
        .withInput(input, 4)
    val run = LocalPipelineRunner.run(graph, context)
    val trace = run.trace

    assert(run.succeeded)
    assertEquals(trace.graphId.value, "trace-success")
    assertEquals(trace.runner, RunnerMetadata.local)
    assertEquals(trace.userMetadata, Vector("a" -> "first", "z" -> "last"))
    assertEquals(trace.outputs.map(_.name.value), Vector("answer"))
    assertEquals(
      trace.renderLines,
      Vector(
        "pipeline trace-success status=succeeded",
        "runner local",
        "metadata a=first",
        "metadata z=last",
        "output answer node=add kind=int label=x-plus-one",
        "node x kind=input status=succeeded deps=- output=int step=- label=raw-x",
        "node add kind=step status=succeeded deps=x output=int step=add-one label=x-plus-one"
      )
    )
  }

  test("run trace records failed and skipped nodes") {
    val built =
      for
        b0 <- PipelineBuilder("trace-failure")
        input <- b0.input("x", intKind)
        bad <- input.builder.step("bad", Fail, input.ref)
        downstream <- bad.builder.step("downstream", AddOne, bad.ref)
      yield (downstream.builder.build, input.ref)
    val (graph, input) = built.toOption.get
    val run = LocalPipelineRunner.run(graph, RunContext.empty.withInput(input, 1))
    val byId = run.trace.nodes.map(node => node.nodeId.value -> node).toMap

    assertEquals(run.status, PipelineStatus.Failed)
    assertEquals(byId("x").status, PipelineStatus.Succeeded)
    assertEquals(byId("bad").status, PipelineStatus.Failed)
    assertEquals(byId("downstream").status, PipelineStatus.Skipped)
    assert(byId("bad").message.exists(_.contains("boom")))
    assert(byId("downstream").message.exists(_.contains("dependency 'bad'")))
    assert(run.trace.error.exists(_.contains("boom")))
  }

  test("future run trace records interpreter and policy metadata") {
    val (graph, input) = successGraph
    val policy = ParallelPolicy.unsafe(2)

    FuturePipelineRunner.run(graph, RunContext.empty.withInput(input, 9), policy).map { run =>
      val trace = run.trace

      assert(run.succeeded)
      assertEquals(run.output("answer", intKind).toOption.get, 10)
      assertEquals(trace.runner, RunnerMetadata.future(policy))
      assertEquals(trace.renderLines.take(2), Vector("pipeline trace-success status=succeeded", "runner future maxConcurrency=2"))
    }
  }
