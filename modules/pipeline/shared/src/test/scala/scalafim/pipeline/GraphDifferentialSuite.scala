package scalafim.pipeline

class GraphDifferentialSuite extends munit.FunSuite:
  private val intKind = ArtifactKind.unsafe[Int]("int")

  private object CopyStep extends PipelineStep[Int, Int]:
    val id: StepId = StepId.unsafe("copy")
    val outputKind: ArtifactKind[Int] = intKind
    def run(input: Int, context: RunContext): Either[PipelineError, Int] = Right(input)

  private object SumStep extends PipelineStep[(Int, Int), Int]:
    val id: StepId = StepId.unsafe("sum")
    val outputKind: ArtifactKind[Int] = intKind
    def run(input: (Int, Int), context: RunContext): Either[PipelineError, Int] = Right(input._1 + input._2)

  test("Dag-delegated layers match the legacy insertion-stable staging oracle"):
    val base = PipelineGraph.empty(PipelineId.unsafe("differential"))
    val (g1, a) = base.addInput(NodeId.unsafe("a"), intKind).toOption.get
    val (g2, b) = g1.addInput(NodeId.unsafe("b"), intKind).toOption.get
    val (g3, c) = g2.addStep(NodeId.unsafe("c"), CopyStep, a.expr).toOption.get
    val (g4, d) = g3.addStep(NodeId.unsafe("d"), SumStep, a.expr.zip(b.expr)).toOption.get
    val (graph, _) = g4.addStep(NodeId.unsafe("e"), SumStep, c.expr.zip(d.expr)).toOption.get

    val pipelineLayers = graph.executionPlan.toOption.get.stages.map(_.nodeIds)
    val oracleLayers = stagingOracle(graph.nodes)

    assertEquals(pipelineLayers, oracleLayers)

  test("repeated expression dependencies collapse to one simple graph edge"):
    val base = PipelineGraph.empty(PipelineId.unsafe("duplicate-dependency"))
    val (g1, a) = base.addInput(NodeId.unsafe("a"), intKind).toOption.get
    val (graph, _) = g1.addStep(NodeId.unsafe("sum"), SumStep, a.expr.zip(a.expr)).toOption.get

    assertEquals(graph.nodes(1).dependencies.map(_.nodeId), Vector(a.nodeId, a.nodeId))
    assertEquals(graph.executionPlan.toOption.get.stages.map(_.nodeIds), Vector(Vector(a.nodeId), Vector(NodeId.unsafe("sum"))))

  test("cycle diagnostics preserve every blocked node after maximal staging"):
    val aRef = ArtifactRef[Int](NodeId.unsafe("a"), intKind, None)
    val bRef = ArtifactRef[Int](NodeId.unsafe("b"), intKind, None)
    val a = StepNode[Int, Int](NodeId.unsafe("a"), CopyStep, bRef.expr, None)
    val b = StepNode[Int, Int](NodeId.unsafe("b"), CopyStep, aRef.expr, None)
    val c = StepNode[Int, Int](NodeId.unsafe("c"), CopyStep, aRef.expr, None)
    val ready = InputNode[Int](NodeId.unsafe("ready"), intKind, None)
    val cyclic = PipelineGraph.unchecked(PipelineId.unsafe("cycle-differential"), Vector(ready, a, b, c))

    val remaining = cyclic.executionPlan.left.toOption.get match
      case PipelineError.CyclicGraph(ids) => ids
      case other                          => fail(s"expected pipeline cycle, got $other")

    assertEquals(remaining.map(_.value), Vector("a", "b", "c"))

  test("self-dependencies remain cycles despite loopless graph topology"):
    val selfRef = ArtifactRef[Int](NodeId.unsafe("self"), intKind, None)
    val self = StepNode[Int, Int](NodeId.unsafe("self"), CopyStep, selfRef.expr, None)
    val downstream = StepNode[Int, Int](NodeId.unsafe("downstream"), CopyStep, selfRef.expr, None)
    val graph = PipelineGraph.unchecked(PipelineId.unsafe("self-cycle"), Vector(self, downstream))

    assertEquals(
      graph.executionPlan.left.toOption,
      Some(PipelineError.CyclicGraph(Vector(NodeId.unsafe("self"), NodeId.unsafe("downstream"))))
    )

  private def stagingOracle(nodes: Vector[PipelineNode]): Vector[Vector[NodeId]] =
    var remaining = nodes
    var available = Set.empty[NodeId]
    val layers = Vector.newBuilder[Vector[NodeId]]
    while remaining.nonEmpty do
      val ready = remaining.filter(node => node.dependencies.forall(reference => available(reference.nodeId)))
      require(ready.nonEmpty, "oracle expects an acyclic graph")
      val ids = ready.map(_.id)
      layers += ids
      available = available ++ ids
      val readySet = ids.toSet
      remaining = remaining.filterNot(node => readySet(node.id))
    layers.result()
