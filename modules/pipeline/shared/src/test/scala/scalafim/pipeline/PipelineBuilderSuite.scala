package scalafim.pipeline

class PipelineBuilderSuite extends munit.FunSuite:
  private val intKind = ArtifactKind.unsafe[Int]("int")
  private val stringKind = ArtifactKind.unsafe[String]("string")

  private object DoubleStep extends PipelineStep[Int, Int]:
    override val id: StepId = StepId.unsafe("double")
    override val outputKind: ArtifactKind[Int] = intKind

    override def run(input: Int, context: RunContext): Either[PipelineError, Int] =
      Right(input * 2)

  private object AddPairStep extends PipelineStep[(Int, Int), Int]:
    override val id: StepId = StepId.unsafe("add-pair")
    override val outputKind: ArtifactKind[Int] = intKind

    override def run(input: (Int, Int), context: RunContext): Either[PipelineError, Int] =
      Right(input._1 + input._2)

  private object RenderStep extends PipelineStep[Int, String]:
    override val id: StepId = StepId.unsafe("render-int")
    override val outputKind: ArtifactKind[String] = stringKind

    override def run(input: Int, context: RunContext): Either[PipelineError, String] =
      Right(s"value=$input")

  test("builder composes typed inputs, refs, expressions, constants, and named outputs") {
    val built =
      for
        b0 <- PipelineBuilder("builder-toy")
        x <- b0.input("x", intKind)
        y <- x.builder.input("y", intKind)
        doubled <- y.builder.step("double-x", DoubleStep, x.ref)
        biased <- doubled.builder.step(
          "bias-double",
          AddPairStep,
          doubled.expr.zip(PipelineExpr.const(3, "bias"))
        )
        total <- biased.builder.step("sum-with-y", AddPairStep, biased.expr.zip(y.expr))
        rendered <- total.builder.step("render", RenderStep, total.ref)
        graph <- rendered.builder.output("answer", rendered.ref).map(_.build)
      yield (graph, x.ref, y.ref, rendered.ref)

    val (graph, xRef, yRef, answerRef) = built.toOption.get
    val run =
      LocalPipelineRunner.run(
        graph,
        RunContext.empty
          .withInput(xRef, 4)
          .withInput(yRef, 5)
      )

    assert(run.succeeded)
    assertEquals(run.get(answerRef).toOption.get, "value=16")
    assertEquals(graph.outputs.map(_.name.value), Vector("answer"))
    assertEquals(
      graph.executionPlan.toOption.get.stages.map(_.nodeIds.map(_.value)),
      Vector(Vector("x", "y"), Vector("double-x"), Vector("bias-double"), Vector("sum-with-y"), Vector("render"))
    )
  }

  test("builder validates string ids before changing the graph") {
    val error = PipelineBuilder("bad id!").swap.toOption.get

    assertEquals(error.message, "invalid pipeline id 'bad id!': may contain only letters, digits, '.', '_', and '-'")
  }

  test("builder preserves graph validation for duplicate nodes") {
    val duplicate =
      for
        b0 <- PipelineBuilder("dupe")
        first <- b0.input("x", intKind)
        second <- first.builder.input("x", intKind)
      yield second

    val error = duplicate.swap.toOption.get
    assertEquals(error.message, "duplicate pipeline node 'x'")
  }

  test("builder remains reusable after a rejected authoring call") {
    val base = PipelineBuilder("reusable").toOption.get
    val rejected = base.input("bad id!", intKind)

    val built =
      for
        input <- base.input("x", intKind)
        doubled <- input.builder.step("double-x", DoubleStep, input.ref)
      yield (doubled.builder.build, input.ref, doubled.ref)

    val (graph, input, doubled) = built.toOption.get
    val run = LocalPipelineRunner.run(graph, RunContext.empty.withInput(input, 6))

    assert(rejected.isLeft)
    assert(run.succeeded)
    assertEquals(run.get(doubled).toOption.get, 12)
    assertEquals(base.build.nodes.length, 0)
  }
