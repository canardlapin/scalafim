package scalafim.connectivity

class ConnectivityLocusSuite extends munit.FunSuite:

  private val provenance =
    NodeAxisProvenance.unsafeDeclared("atlas", Some("v1"))

  private def axis(ids: String*): NodeAxis =
    val specs =
      ids.map: value =>
        val id = NodeId.unsafe(value)
        NodeSpec(id, value.toUpperCase)
    NodeAxis.unsafe(specs, provenance)

  test("node finite spaces preserve VertexBasis order and semantic identity"):
    val nodes = axis("a", "b", "c")
    val locus = nodes.locus
    val point = locus.pointFor(NodeId.unsafe("b")).get

    assertEquals(point.value, 1)
    assertEquals(locus.nodeAt(point).id.value, "b")
    assertEquals(
      locus.space.points.map(locus.nodeAt).map(_.id.value).toVector,
      Vector("a", "b", "c")
    )
    assert(
      !nodes.locus.space.sameRuntimeOwnerAs(
        axis("x", "y", "z").locus.space
      )
    )

  test("edge finite spaces retain topology and vectorization order"):
    val nodes = axis("a", "b", "c")
    val native =
      EdgeSpace
        .undirected(nodes, VectorizationOrder.ScalaNative)
        .toOption
        .get
    val ariadne =
      EdgeSpace
        .undirected(nodes, VectorizationOrder.AriadneCompatible)
        .toOption
        .get
    val first = native.locus.space.pointOption(0).get

    assertEquals(native.locus.edgeAt(first), native.edge(0))
    assert(!native.locus.space.sameRuntimeOwnerAs(ariadne.locus.space))
    assertEquals(
      native.locus.space.points.map(native.locus.edgeAt).toVector,
      native.edges
    )

  test("edge masks expose region semantics without losing edge ordering"):
    val nodes = axis("a", "b", "c", "d")
    val native =
      EdgeSpace
        .undirected(nodes, VectorizationOrder.ScalaNative)
        .toOption
        .get
    val ariadne =
      EdgeSpace
        .undirected(nodes, VectorizationOrder.AriadneCompatible)
        .toOption
        .get
    val left =
      EdgeMask
        .fromIndices(
          native,
          Vector(
            EdgeSpaceIx.from(0, native).toOption.get,
            EdgeSpaceIx.from(2, native).toOption.get
          )
        )
        .toOption
        .get
    val right =
      EdgeMask
        .fromIndices(
          native,
          Vector(
            EdgeSpaceIx.from(2, native).toOption.get,
            EdgeSpaceIx.from(3, native).toOption.get
          )
        )
        .toOption
        .get

    assertEquals(
      left.region.value.ordinalsInDomainOrder.toVector,
      Vector(0, 2)
    )
    assertEquals(
      left.union(right).toOption.get.selectedIndices.map(_.value),
      Vector(0, 2, 3)
    )
    assertEquals(
      left.intersect(right).toOption.get.selectedIndices.map(_.value),
      Vector(2)
    )
    assertEquals(
      left.diff(right).toOption.get.selectedIndices.map(_.value),
      Vector(0)
    )
    assertEquals(
      left.complement.selectedIndices.map(_.value),
      Vector(1, 3, 4, 5)
    )

    val foreign =
      EdgeMask
        .from(ariadne, Vector.fill(ariadne.size)(true))
        .toOption
        .get
    assert(left.union(foreign).isLeft)
