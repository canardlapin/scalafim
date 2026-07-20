package scalafim.pipeline

class PipelineOutputSuite extends munit.FunSuite:
  private val intKind = ArtifactKind.unsafe[Int]("int")
  private val stringKind = ArtifactKind.unsafe[String]("string")
  private val otherIntKind = ArtifactKind.unsafe[Int]("other-int")

  private object DoubleStep extends PipelineStep[Int, Int]:
    override val id: StepId = StepId.unsafe("double")
    override val outputKind: ArtifactKind[Int] = intKind

    override def run(input: Int, context: RunContext): Either[PipelineError, Int] =
      Right(input * 2)

  private def runWithOutput: PipelineRun =
    val built =
      for
        b0 <- PipelineBuilder("outputs")
        input <- b0.input("x", intKind)
        doubled <- input.builder.step("double-x", DoubleStep, input.ref)
        graph <- doubled.builder.output("answer", doubled.ref).map(_.build)
      yield (graph, input.ref)

    val (graph, input) = built.toOption.get
    LocalPipelineRunner.run(graph, RunContext.empty.withInput(input, 21))

  test("run reads successful named output by PortName with the expected kind") {
    val run = runWithOutput

    assert(run.succeeded)
    assertEquals(run.output(PortName.unsafe("answer"), intKind).toOption.get, 42)
    assertEquals(run.outputRef(PortName.unsafe("answer")).toOption.get.kind.label, "int")
  }

  test("run reads named output by string convenience method") {
    val run = runWithOutput

    assertEquals(run.output("answer", intKind).toOption.get, 42)
    assertEquals(run.outputRef("answer").toOption.get.nodeId.value, "double-x")
  }

  test("run reports missing named outputs with a structured error") {
    val error = runWithOutput.output(PortName.unsafe("missing"), intKind).swap.toOption.get

    assertEquals(error, PipelineError.MissingOutput(PortName.unsafe("missing")))
    assertEquals(error.message, "pipeline output 'missing' is not declared")
  }

  test("string output lookup validates the output name before lookup") {
    val error = runWithOutput.output("bad name!", intKind).swap.toOption.get

    assertEquals(error.message, "invalid port name 'bad name!': may contain only letters, digits, '.', '_', and '-'")
  }

  test("run checks declared output kind before reading the artifact table") {
    val stringError = runWithOutput.output(PortName.unsafe("answer"), stringKind).swap.toOption.get
    val otherIntError = runWithOutput.output("answer", otherIntKind).swap.toOption.get

    assert(stringError.message.contains("artifact 'double-x'"))
    assert(stringError.message.contains("expected 'string["))
    assert(stringError.message.contains("has kind 'int["))
    assert(otherIntError.message.contains("artifact 'double-x'"))
    assert(otherIntError.message.contains("expected 'other-int["))
    assert(otherIntError.message.contains("has kind 'int["))
  }
