package scalafim.pipeline

class PipelineSoundnessSuite extends munit.FunSuite:
  private val scalarIntKind = ArtifactKind.unsafe[Int]("scalar")
  private val scalarStringKind = ArtifactKind.unsafe[String]("scalar")
  private val textKind = ArtifactKind.unsafe[String]("text")

  private object DoubleStep extends PipelineStep[Int, Int]:
    override val id: StepId = StepId.unsafe("double")
    override val outputKind: ArtifactKind[Int] = scalarIntKind

    override def run(input: Int, context: RunContext): Either[PipelineError, Int] =
      Right(input * 2)

  private object StringIdentityStep extends PipelineStep[String, String]:
    override val id: StepId = StepId.unsafe("string-identity")
    override val outputKind: ArtifactKind[String] = textKind

    override def run(input: String, context: RunContext): Either[PipelineError, String] =
      Right(input)

  private def scalarRun: (PipelineRun, ArtifactRef[Int]) =
    val built =
      for
        b0 <- PipelineBuilder("kind-soundness")
        input <- b0.input("x", scalarIntKind)
        doubled <- input.builder.step("double-x", DoubleStep, input.ref)
        graph <- doubled.builder.output("answer", doubled.ref).map(_.build)
      yield (graph, input.ref, doubled.ref)

    val (graph, input, output) = built.toOption.get
    (LocalPipelineRunner.run(graph, RunContext.empty.withInput(input, 21)), output)

  test("same-label artifact kinds with different Scala types do not compare equal") {
    assertEquals(scalarIntKind.label, scalarStringKind.label)
    assert(scalarIntKind != scalarStringKind)
    assert(scalarIntKind.diagnosticName.startsWith("scalar["))
    assert(scalarStringKind.diagnosticName.startsWith("scalar["))
    assert(scalarIntKind.diagnosticName != scalarStringKind.diagnosticName)
  }

  test("named output access rejects same-label different-type kinds before casting") {
    val (run, _) = scalarRun
    val error = run.output("answer", scalarStringKind).swap.toOption.get

    error match
      case PipelineError.ArtifactKindMismatch(nodeId, expected, actual) =>
        assertEquals(nodeId.value, "double-x")
        assert(expected.startsWith("scalar["))
        assert(actual.startsWith("scalar["))
        assert(expected != actual)
      case other =>
        fail(s"expected artifact kind mismatch, got ${other.message}")
  }

  test("artifact table containment is kind-aware") {
    val (run, output) = scalarRun
    val wrongRef = ArtifactRef[String](output.nodeId, scalarStringKind, output.label)

    assert(run.values.contains(output))
    assert(!run.values.contains(wrongRef))
    assert(run.values.get(wrongRef).isLeft)
  }

  test("graph construction reports kind mismatches for known dependency nodes") {
    val base = PipelineGraph.empty(PipelineId.unsafe("dependency-kind"))
    val (g1, input) = base.addInput(NodeId.unsafe("x"), scalarIntKind).toOption.get
    val wrongRef = ArtifactRef[String](input.nodeId, scalarStringKind, input.label)
    val error = g1.addStep(NodeId.unsafe("bad"), StringIdentityStep, wrongRef.expr).swap.toOption.get

    error match
      case PipelineError.ArtifactKindMismatch(nodeId, expected, actual) =>
        assertEquals(nodeId.value, "x")
        assert(expected.startsWith("scalar["))
        assert(actual.startsWith("scalar["))
        assert(expected != actual)
      case other =>
        fail(s"expected artifact kind mismatch, got ${other.message}")
  }
