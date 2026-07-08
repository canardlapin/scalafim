package scalafim.multivar

class MultivarCoreSuite extends munit.FunSuite:

  test("opaque identifiers and dimensions validate construction boundaries") {
    assertEquals(SpaceId(" observed.space ").toOption.map(_.value), Some("observed.space"))
    assert(SpaceId("bad space").isLeft)
    assert(Dimension(0).isLeft)
    assert(Dimension(-1).isLeft)
    assertEquals(Dimension(3).toOption.map(_.value), Some(3))
    assertEquals(ComponentCount(2).toOption.map(_.value), Some(2))
    assert(ComponentCount(0).isLeft)
  }

  test("MvSpace carries a typed role and positive dimension") {
    val observed = MvSpace.of("voxels", SpaceRole.Observed, 4).toOption.get

    assertEquals(observed.id.value, "voxels")
    assertEquals(observed.role, SpaceRole.Observed)
    assertEquals(observed.size, 4)
    assert(MvSpace.of("bad id", SpaceRole.Latent, 2).isLeft)
    assert(MvSpace.of("latent", SpaceRole.Latent, 0).isLeft)
  }

  test("IndexSet preserves order and rejects empty duplicate or out-of-bounds indices") {
    val set = IndexSet.from(Vector(2, 0, 3), axis = IndexAxis.Feature, limit = Some(4)).toOption.get

    assertEquals(set.indices, Vector(2, 0, 3))
    assertEquals(set.length, 3)
    assert(set.contains(0))
    assert(IndexSet.from(Vector.empty[Int], axis = IndexAxis.Feature).isLeft)
    assert(IndexSet.from(Vector(0, 0), axis = IndexAxis.Feature).isLeft)
    assert(IndexSet.from(Vector(4), axis = IndexAxis.Feature, limit = Some(4)).isLeft)
  }

  test("BlockPartition is a full disjoint ordered partition of feature columns") {
    val total = Dimension(5).toOption.get
    val left = BlockSpec(BlockId("left").toOption.get, IndexSet.from(Vector(0, 2), IndexAxis.Feature).toOption.get)
    val right = BlockSpec(BlockId("right").toOption.get, IndexSet.from(Vector(1, 3, 4), IndexAxis.Feature).toOption.get)

    val partition = BlockPartition.from(total, Vector(left, right)).toOption.get

    assertEquals(partition.blockCount, 2)
    assertEquals(partition.blockFor(3).map(_.id.value), Some("right"))
    assertEquals(partition.blockIndexFor(2), Some(0))
    assertEquals(partition.columns.indices, Vector(0, 2, 1, 3, 4))
  }

  test("BlockPartition rejects missing duplicate and repeated columns") {
    val total = Dimension(3).toOption.get
    val one = BlockSpec(BlockId("a").toOption.get, IndexSet.from(Vector(0), IndexAxis.Feature).toOption.get)
    val two = BlockSpec(BlockId("b").toOption.get, IndexSet.from(Vector(2), IndexAxis.Feature).toOption.get)
    val duplicateId = BlockSpec(BlockId("a").toOption.get, IndexSet.from(Vector(1), IndexAxis.Feature).toOption.get)
    val repeatedColumn = BlockSpec(BlockId("c").toOption.get, IndexSet.from(Vector(0, 1), IndexAxis.Feature).toOption.get)

    assert(BlockPartition.from(total, Vector(one, two)).swap.toOption.exists(_.message.contains("does not cover")))
    assert(BlockPartition.from(total, Vector(one, duplicateId, two)).swap.toOption.exists(_.message.contains("duplicate block")))
    assert(BlockPartition.from(total, Vector(one, repeatedColumn, two)).swap.toOption.exists(_.message.contains("duplicate index")))
  }

