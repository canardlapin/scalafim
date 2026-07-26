package scalafim.multivar
package core

import scalafim.multivar.core.*

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

  test("IndexSet rejects negative indices and supports contiguous construction") {
    IndexSet.from(Vector(-1), axis = IndexAxis.Feature) match
      case Left(MultivarError.IndexOutOfBounds(IndexAxis.Feature, -1, 0)) => ()
      case other => fail(s"expected negative-index rejection, got $other")

    val contiguous = IndexSet.contiguous(3, IndexAxis.Feature).toOption.get
    assertEquals(contiguous.indices, Vector(0, 1, 2))
    assertEquals(contiguous.axis, IndexAxis.Feature)
    assert(IndexSet.contiguous(0).isLeft)
  }

  test("IndexSet reports the known limit when rejecting negative indices") {
    IndexSet.from(Vector(-2), axis = IndexAxis.Row, limit = Some(7)) match
      case Left(MultivarError.IndexOutOfBounds(IndexAxis.Row, -2, 7)) => ()
      case other => fail(s"expected negative-index rejection with the real limit, got $other")
  }

  test("IndexSet requireWithin enforces the limit and preserves the set") {
    val set = IndexSet.from(Vector(2, 0, 3), axis = IndexAxis.Feature).toOption.get

    assertEquals(set.requireWithin(4).toOption.get.indices, Vector(2, 0, 3))
    set.requireWithin(3) match
      case Left(MultivarError.IndexOutOfBounds(IndexAxis.Feature, 3, 3)) => ()
      case other => fail(s"expected out-of-bounds rejection, got $other")
  }

  test("IndexSet has structural equality and fast membership") {
    val left = IndexSet.from(Vector(2, 0, 3), axis = IndexAxis.Feature).toOption.get
    val right = IndexSet.from(Vector(2, 0, 3), axis = IndexAxis.Feature).toOption.get
    val otherAxis = IndexSet.from(Vector(2, 0, 3), axis = IndexAxis.Column).toOption.get
    val otherOrder = IndexSet.from(Vector(0, 2, 3), axis = IndexAxis.Feature).toOption.get

    assertEquals(left, right)
    assertEquals(left.hashCode, right.hashCode)
    assertNotEquals(left, otherAxis)
    assertNotEquals(left, otherOrder)

    assert(left.contains(0))
    assert(left.contains(3))
    assert(!left.contains(1))
    assert(!left.contains(-1))
    assert(!left.contains(4))
  }

  test("BlockPartition rejects out-of-bounds block columns and misses gracefully") {
    val total = Dimension(3).toOption.get
    val inBounds = BlockSpec(BlockId("a").toOption.get, IndexSet.from(Vector(0, 1, 2), IndexAxis.Feature).toOption.get)
    val outOfBounds = BlockSpec(BlockId("b").toOption.get, IndexSet.from(Vector(0, 5), IndexAxis.Feature).toOption.get)

    BlockPartition.from(total, Vector(outOfBounds)) match
      case Left(MultivarError.IndexOutOfBounds(IndexAxis.Column, 5, 3)) => ()
      case other => fail(s"expected out-of-bounds rejection, got $other")

    val partition = BlockPartition.from(total, Vector(inBounds)).toOption.get
    assertEquals(partition.blockFor(7), None)
    assertEquals(partition.blockIndexFor(7), None)
  }

  test("BlockSpec localColumns always stamps the feature axis and validates membership") {
    val block = BlockSpec(BlockId("mid").toOption.get, IndexSet.from(Vector(1, 3, 4), IndexAxis.Column).toOption.get)

    val locals = block.localColumns(IndexSet.from(Vector(3, 1), IndexAxis.Column).toOption.get).toOption.get
    assertEquals(locals.axis, IndexAxis.Feature)
    assertEquals(locals.indices, Vector(1, 0))

    assert(block.localColumns(IndexSet.from(Vector(2), IndexAxis.Column).toOption.get).isLeft)
    assert(block.localColumns(IndexSet.from(Vector(1), IndexAxis.Row).toOption.get).isLeft)
  }
