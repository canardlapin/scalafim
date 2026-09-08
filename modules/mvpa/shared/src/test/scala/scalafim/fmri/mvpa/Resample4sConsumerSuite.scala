package scalafim.fmri.mvpa

import resample4s.core.*
import resample4s.designs.*

class Resample4sConsumerSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private def labels(values: Int*): Labels =
    right(Labels.dense(IArray.unsafeFromArray(values.toArray)))

  private def ordinals(value: Reindexing): Vector[Int] =
    val result = Vector.newBuilder[Int]
    value.foreachIndex(result += _)
    result.result()

  test("an exact-once compiled plan exposes ordered selection roles"):
    val space = right(IndexSpace.of(8))
    val compiled: Compiled[Split[Selection], Coverage.ExactOnce] =
      right(KFold.ordered(4).compile(space, Seed.fromLong(41L)))
    val assessments = compiled.plan.materialize.flatMap: (_, split) =>
      assertEquals(
        right(split.analysis.intersection(split.assessment)).domain,
        0
      )
      ordinals(split.assessment)

    assertEquals(compiled.plan.shape, right(PlanShape.of(1, 4)))
    assertEquals(assessments.sorted, Vector.range(0, 8))
    assertEquals(ordinals(compiled.plan.first.assessment), Vector(0, 4))

  test("grouped and fixed exact designs execute as a ScalaFIM consumer"):
    val space = right(IndexSpace.of(8))
    val groups = labels(10, 10, 20, 20, 30, 30, 40, 40)
    val grouped: Compiled[Split[Selection], Coverage.ExactOnce] =
      right(KFold.grouped(4, groups).compile(space, Seed.fromLong(52L)))
    val assessmentFold = Array.fill(space.size)(-1)
    grouped.plan.iterator.foreach: (key, split) =>
      split.assessment.foreachIndex(row => assessmentFold(row) = key.fold)

    assertEquals(assessmentFold(0), assessmentFold(1))
    assertEquals(assessmentFold(2), assessmentFold(3))
    assertEquals(assessmentFold(4), assessmentFold(5))
    assertEquals(assessmentFold(6), assessmentFold(7))

    val assignments = labels(0, 0, 1, 1, 2, 2, 3, 3)
    val fixed: Compiled[Split[Selection], Coverage.ExactOnce] =
      right(
        right(FixedPartitions.once(assignments))
          .compile(space, Seed.fromLong(99L))
      )
    assertEquals(
      fixed.plan.materialize.map((key, split) => key.fold -> ordinals(split.assessment)),
      Vector(
        0 -> Vector(0, 1),
        1 -> Vector(2, 3),
        2 -> Vector(4, 5),
        3 -> Vector(6, 7)
      )
    )

  test("nested inner selections reconstruct into the source population"):
    val space = right(IndexSpace.of(12))
    val nested: Compiled[NestedFold, Coverage.ExactOnce] =
      right(
        NestedCrossValidation(outerFolds = 3, innerFolds = 2)
          .compile(space, Seed.fromLong(63L))
      )

    nested.plan.iterator.foreach: (_, fold) =>
      assertEquals(fold.inner.shape, right(PlanShape.of(1, 2)))
      fold.inner.iterator.foreach: (_, inner) =>
        assertEquals(inner.analysis.codomain, space.size)
        assertEquals(inner.assessment.codomain, space.size)
        assertEquals(
          right(inner.analysis.union(inner.assessment)),
          fold.outer.analysis
        )
        assertEquals(
          right(inner.assessment.intersection(fold.outer.assessment)).domain,
          0
        )

  test("permutation plans and receipts replay from the pinned API"):
    given DigestAlgorithm = DigestAlgorithm.fnv1a64
    val space = right(IndexSpace.of(7))
    val seed = Seed.fromLong(74L)
    val design = PermutationDesign(times = 3)
    val compiled: Compiled[Permutation, Coverage] =
      right(design.compile(space, seed))
    val population = right(Summary.of("scalafim/sample-axis", 7007L))
    val receipt = right(compiled.receipt(population))

    assertEquals(compiled.plan.shape, right(PlanShape.of(3, 1)))
    compiled.plan.iterator.foreach: (_, permutation) =>
      assertEquals(ordinals(permutation).sorted, Vector.range(0, space.size))
    assertEquals(receipt.seed.value, seed.value)
    assertEquals(receipt.verify(design, space, population), Right(()))
