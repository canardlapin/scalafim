package scalafim.pipeline

class PipelineGraphSuite extends munit.FunSuite:
  private val intKind = ArtifactKind.unsafe[Int]("int")
  private val stringKind = ArtifactKind.unsafe[String]("string")

  private object DoubleStep extends PipelineStep[Int, Int]:
    override val id: StepId = StepId.unsafe("double")
    override val outputKind: ArtifactKind[Int] = intKind

    override def run(input: Int, context: RunContext): Either[PipelineError, Int] =
      Right(input * 2)

  private object SumStep extends PipelineStep[(Int, Int), Int]:
    override val id: StepId = StepId.unsafe("sum")
    override val outputKind: ArtifactKind[Int] = intKind

    override def run(input: (Int, Int), context: RunContext): Either[PipelineError, Int] =
      Right(input._1 + input._2)

  private object IntToStringStep extends PipelineStep[Int, String]:
    override val id: StepId = StepId.unsafe("stringify")
    override val outputKind: ArtifactKind[String] = stringKind

    override def run(input: Int, context: RunContext): Either[PipelineError, String] =
      Right(input.toString)

  private object FailingStep extends PipelineStep[Int, Int]:
    override val id: StepId = StepId.unsafe("fail")
    override val outputKind: ArtifactKind[Int] = intKind

    override def run(input: Int, context: RunContext): Either[PipelineError, Int] =
      Left(PipelineError.InvalidGraph("boom"))

  private def emptyGraph: PipelineGraph =
    PipelineGraph.empty(PipelineId.unsafe("toy"))

  test("graph stages inputs before dependent steps and executes typed artifacts") {
    val (g1, input) = emptyGraph.addInput(NodeId.unsafe("x"), intKind).toOption.get
    val (g2, doubled) = g1.addStep(NodeId.unsafe("double-x"), DoubleStep, input.expr).toOption.get
    val g3 = g2.addOutput(PortName.unsafe("answer"), doubled).toOption.get

    val plan = g3.executionPlan.toOption.get
    assertEquals(plan.stages.map(_.nodeIds.map(_.value)), Vector(Vector("x"), Vector("double-x")))

    val context = RunContext.empty.withInput(input, 21)
    val run = LocalPipelineRunner.run(g3, context)

    assert(run.succeeded)
    assertEquals(run.get(doubled).toOption.get, 42)
    assertEquals(run.receipts.map(_.status), Vector(PipelineStatus.Succeeded, PipelineStatus.Succeeded))
    assertEquals(g3.outputs.map(_.name.value), Vector("answer"))
  }

  test("zip expressions build typed multi-input step calls") {
    val (g1, a) = emptyGraph.addInput(NodeId.unsafe("a"), intKind).toOption.get
    val (g2, b) = g1.addInput(NodeId.unsafe("b"), intKind).toOption.get
    val (g3, sum) = g2.addStep(NodeId.unsafe("sum"), SumStep, a.expr.zip(b.expr)).toOption.get

    val run =
      LocalPipelineRunner.run(
        g3,
        RunContext.empty
          .withInput(a, 10)
          .withInput(b, 7)
      )

    assert(run.succeeded)
    assertEquals(run.get(sum).toOption.get, 17)
    assertEquals(g3.executionPlan.toOption.get.stages.map(_.nodeIds.map(_.value)), Vector(Vector("a", "b"), Vector("sum")))
  }

  test("mapped expressions are evaluated inside the receiving step") {
    val (g1, input) = emptyGraph.addInput(NodeId.unsafe("x"), intKind).toOption.get
    val normalized = input.expr.map("add-one-before-step")(_ + 1)
    val (g2, doubled) = g1.addStep(NodeId.unsafe("double-shifted"), DoubleStep, normalized).toOption.get

    val run = LocalPipelineRunner.run(g2, RunContext.empty.withInput(input, 4))

    assert(run.succeeded)
    assertEquals(run.get(doubled).toOption.get, 10)
  }

  test("constant expressions can seed pure steps without external inputs") {
    val (graph, rendered) =
      emptyGraph
        .addStep(NodeId.unsafe("render"), IntToStringStep, PipelineExpr.const(12, "literal-12"))
        .toOption
        .get

    val run = LocalPipelineRunner.run(graph)

    assert(run.succeeded)
    assertEquals(run.get(rendered).toOption.get, "12")
    assertEquals(graph.executionPlan.toOption.get.stages.map(_.nodeIds.map(_.value)), Vector(Vector("render")))
  }

  test("duplicate node and output ids are rejected") {
    val (g1, input) = emptyGraph.addInput(NodeId.unsafe("x"), intKind).toOption.get
    val duplicateNode = g1.addInput(NodeId.unsafe("x"), intKind).swap.toOption.get
    assertEquals(duplicateNode.message, "duplicate pipeline node 'x'")

    val g2 = g1.addOutput(PortName.unsafe("value"), input).toOption.get
    val duplicateOutput = g2.addOutput(PortName.unsafe("value"), input).swap.toOption.get
    assertEquals(duplicateOutput.message, "duplicate pipeline output 'value'")
  }

  test("execution plan validation detects cycles in low-level graphs") {
    val aRef = ArtifactRef[Int](NodeId.unsafe("a"), intKind, None)
    val bRef = ArtifactRef[Int](NodeId.unsafe("b"), intKind, None)
    val aNode = StepNode[Int, Int](NodeId.unsafe("a"), DoubleStep, bRef.expr, None)
    val bNode = StepNode[Int, Int](NodeId.unsafe("b"), DoubleStep, aRef.expr, None)
    val graph = PipelineGraph.unchecked(PipelineId.unsafe("cycle"), Vector(aNode, bNode))

    val error = graph.executionPlan.swap.toOption.get

    assert(error.message.contains("cycle"))
    assert(error.message.contains("a"))
    assert(error.message.contains("b"))
  }

  test("step failure records receipts and skips dependent nodes") {
    val (g1, input) = emptyGraph.addInput(NodeId.unsafe("x"), intKind).toOption.get
    val (g2, failed) = g1.addStep(NodeId.unsafe("bad"), FailingStep, input.expr).toOption.get
    val (g3, independent) = g2.addStep(NodeId.unsafe("independent"), IntToStringStep, PipelineExpr.const(8, "eight")).toOption.get
    val (g4, downstream) = g3.addStep(NodeId.unsafe("downstream"), DoubleStep, failed.expr).toOption.get

    val run = LocalPipelineRunner.run(g4, RunContext.empty.withInput(input, 2))
    val byNode = run.receipts.map(receipt => receipt.nodeId.value -> receipt).toMap

    assertEquals(run.status, PipelineStatus.Failed)
    assert(run.error.exists(_.message.contains("boom")))
    assertEquals(byNode("x").status, PipelineStatus.Succeeded)
    assertEquals(byNode("bad").status, PipelineStatus.Failed)
    assertEquals(byNode("independent").status, PipelineStatus.Succeeded)
    assertEquals(byNode("downstream").status, PipelineStatus.Skipped)
    assertEquals(run.get(independent).toOption.get, "8")
    assert(run.get(downstream).isLeft)
  }

  test("expression failures are surfaced as node failures") {
    val (g1, input) = emptyGraph.addInput(NodeId.unsafe("x"), intKind).toOption.get
    val exploding: PipelineExpr[Int] = input.expr.map("explode") { _ =>
      throw new IllegalStateException("no value")
    }
    val (g2, out) = g1.addStep(NodeId.unsafe("double"), DoubleStep, exploding).toOption.get

    val run = LocalPipelineRunner.run(g2, RunContext.empty.withInput(input, 1))

    assertEquals(run.status, PipelineStatus.Failed)
    assert(run.error.exists(_.message.contains("explode")))
    assert(run.get(out).isLeft)
  }
