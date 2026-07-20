package scalafim.graph

class VertexBasisSuite extends munit.FunSuite:
  private def basis(entries: (String, String)*): VertexBasis[String, String] =
    VertexBasis.from(entries).toOption.get

  test("basis keys have dense local coordinates and round-trip through lookup"):
    val values = basis("a" -> "A", "b" -> "B", "c" -> "C")

    assertEquals(values.size, 3)
    assertEquals(values.indices.map(_.toInt), Vector(0, 1, 2))
    values.indices.foreach: index =>
      assertEquals(values.indexOf(values.keyAt(index)), Some(index))
      assertEquals(values.entryAt(index), values.keyAt(index) -> values.valueAt(index))

  test("basis construction rejects duplicate stable keys"):
    VertexBasis.from(Vector("a" -> 1, "b" -> 2, "a" -> 3)) match
      case Left(BasisError.DuplicateKey("a", 0, 2)) => ()
      case other                                     => fail(s"unexpected result: $other")

  test("compatibility distinguishes key order, key set, and metadata"):
    val original = basis("a" -> "A", "b" -> "B")
    val relabeled = basis("a" -> "alpha", "b" -> "beta")
    val reordered = basis("b" -> "B", "a" -> "A")

    assert(original.sameKeyOrderAs(relabeled))
    assert(original.sameKeySetAs(relabeled))
    assert(!original.sameMetadataAs(relabeled))
    assert(!original.sameKeyOrderAs(reordered))
    assert(original.sameKeySetAs(reordered))

  test("permutations are invertible and compose in coordinate order"):
    val abc = basis("a" -> "A", "b" -> "B", "c" -> "C")
    val cab = basis("c" -> "C", "a" -> "A", "b" -> "B")
    val bca = basis("b" -> "B", "c" -> "C", "a" -> "A")
    val first = abc.permutationTo(cab).toOption.get
    val second = cab.permutationTo(bca).toOption.get
    val composed = first.andThen(second).toOption.get

    assertEquals(first.targetIndicesBySource.map(_.toInt), Vector(1, 2, 0))
    assertEquals(first.inverse.targetIndicesBySource.map(_.toInt), Vector(2, 0, 1))
    assertEquals(composed.targetIndicesBySource.map(_.toInt), Vector(2, 0, 1))
    abc.indices.foreach: index =>
      assertEquals(first.inverse.targetOf(first.targetOf(index)), index)

  test("permutation composition is associative"):
    val abcd = basis("a" -> "A", "b" -> "B", "c" -> "C", "d" -> "D")
    val bcda = basis("b" -> "B", "c" -> "C", "d" -> "D", "a" -> "A")
    val dcba = basis("d" -> "D", "c" -> "C", "b" -> "B", "a" -> "A")
    val cadb = basis("c" -> "C", "a" -> "A", "d" -> "D", "b" -> "B")
    val p = abcd.permutationTo(bcda).toOption.get
    val q = bcda.permutationTo(dcba).toOption.get
    val r = dcba.permutationTo(cadb).toOption.get
    val left = p.andThen(q).flatMap(_.andThen(r)).toOption.get
    val right = q.andThen(r).flatMap(p.andThen).toOption.get

    assertEquals(left, right)

  test("permutation construction rejects different key sets"):
    val left = basis("a" -> "A", "b" -> "B")
    val right = basis("b" -> "B", "c" -> "C")

    left.permutationTo(right) match
      case Left(BasisError.KeySetMismatch(Vector("a"), Vector("c"))) => ()
      case other                                                       => fail(s"unexpected result: $other")

  test("induced bases preserve source order and carry both directions of the mapping"):
    val source = basis("a" -> "A", "b" -> "B", "c" -> "C", "d" -> "D")
    val induced = source.inducedByKeys(Vector("d", "b")).toOption.get

    assertEquals(induced.basis.keys, Vector("b", "d"))
    assertEquals(induced.sourceIndicesByInduced.map(_.toInt), Vector(1, 3))
    assertEquals(induced.sourceOf(VertexIx.unsafe(0)).toInt, 1)
    assertEquals(induced.inducedOf(VertexIx.unsafe(3)).map(_.toInt), Some(1))
    assertEquals(induced.inducedOf(VertexIx.unsafe(2)), None)

  test("induced and reindexed selections reject duplicates and unknown keys"):
    val source = basis("a" -> "A", "b" -> "B")

    assertEquals(source.inducedByKeys(Vector("a", "a")).left.toOption, Some(BasisError.DuplicateSelection("a")))
    assertEquals(source.inducedByKeys(Vector("c")).left.toOption, Some(BasisError.UnknownKey("c")))
    assert(source.reindex(Vector("a")).isLeft)
    assert(source.reindex(Vector("a", "c")).isLeft)

  test("reindexing returns the reordered basis and an invertible source mapping"):
    val source = basis("a" -> "A", "b" -> "B", "c" -> "C")
    val result = source.reindex(Vector("c", "a", "b")).toOption.get

    assertEquals(result.basis.keys, Vector("c", "a", "b"))
    assertEquals(result.permutation.targetIndicesBySource.map(_.toInt), Vector(1, 2, 0))
    source.indices.foreach: index =>
      assertEquals(result.permutation.inverse.targetOf(result.permutation.targetOf(index)), index)

  test("basis equality is structural and order-sensitive"):
    val left = basis("a" -> "A", "b" -> "B")
    val equal = basis("a" -> "A", "b" -> "B")
    val reordered = basis("b" -> "B", "a" -> "A")

    assertEquals(left, equal)
    assertEquals(left.hashCode, equal.hashCode)
    assertNotEquals(left, reordered)

  test("empty bases are valid values"):
    val empty = VertexBasis.from(Vector.empty[(String, Int)]).toOption.get

    assertEquals(empty.size, 0)
    assertEquals(empty.indices, Vector.empty)
    assertEquals(empty.keys, Vector.empty)
