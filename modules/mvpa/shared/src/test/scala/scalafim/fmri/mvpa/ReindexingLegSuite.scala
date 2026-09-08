package scalafim.fmri.mvpa

import multivar.core.OperatorRepresentation
import resample4s.core.Draw
import resample4s.core.IndexSpace
import resample4s.core.Injection
import resample4s.core.Permutation
import resample4s.core.Selection

class ReindexingLegSuite extends munit.FunSuite:

  private val sampleIds =
    Vector("sample-a", "sample-b", "sample-c").map(SampleId.unsafe)

  private val axis =
    AxisRef
      .create(
        AxisId.unsafe("samples"),
        AxisPurpose.Samples,
        sampleIds,
        CoordinateBasis.unsafe("trial-table"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("fixture", "v1")
      )
      .toOption
      .get

  private val space =
    IndexSpace.of(axis.size).toOption.get

  private def indices(values: Int*): IArray[Int] =
    IArray.unsafeFromArray(values.toArray)

  test("selection, injection, draw, and permutation retain their concrete semantics"):
    val selection = Selection.from(indices(0, 2), space).toOption.get
    val injection = Injection.from(indices(2, 0), space).toOption.get
    val draw = Draw.from(indices(2, 0, 2, 1), space).toOption.get
    val permutation = Permutation.from(indices(2, 0, 1)).toOption.get

    val selected = ReindexingLeg.selection(axis, selection).toOption.get
    val injected = ReindexingLeg.injection(axis, injection).toOption.get
    val drawn = ReindexingLeg.draw(axis, draw).toOption.get
    val permuted = ReindexingLeg.permutation(axis, permutation).toOption.get

    assertEquals(selected.reindexing, selection)
    assertEquals(injected.reindexing, injection)
    assertEquals(drawn.reindexing, draw)
    assertEquals(permuted.reindexing, permutation)
    assertEquals(selected.child.keys.map(_.value), Vector("sample-a", "sample-c"))
    assertEquals(injected.child.keys.map(_.value), Vector("sample-c", "sample-a"))
    assertEquals(permuted.child.keys.map(_.value), Vector("sample-c", "sample-a", "sample-b"))

    assertEquals(
      drawn.child.keys.map(value => (value.source.value, value.sourcePosition, value.occurrence, value.drawPosition)),
      Vector(
        ("sample-c", 2, 0, 0),
        ("sample-a", 0, 0, 1),
        ("sample-c", 2, 1, 2),
        ("sample-b", 1, 0, 3)
      )
    )
    assertEquals(drawn.child.identity.orderedKeys.distinct.length, 4)

  test("reindexing kind and exact mapping participate in the child identity"):
    val sameMapping = indices(0, 2)
    val selected = ReindexingLeg
      .selection(axis, Selection.from(sameMapping, space).toOption.get)
      .toOption
      .get
    val injected = ReindexingLeg
      .injection(axis, Injection.from(sameMapping, space).toOption.get)
      .toOption
      .get
    val reversed = ReindexingLeg
      .injection(axis, Injection.from(indices(2, 0), space).toOption.get)
      .toOption
      .get

    assertEquals(selected.child.keys, injected.child.keys)
    assertNotEquals(selected.child.identity.fingerprint, injected.child.identity.fingerprint)
    assertNotEquals(injected.child.identity.fingerprint, reversed.child.identity.fingerprint)

  test("the typed Multivar leg is the sparse one-hot restriction operator"):
    val injection = Injection.from(indices(2, 0), space).toOption.get
    val relation = ReindexingLeg.injection(axis, injection).toOption.get
    val input = GaleTestMatrix.fromRows(
      Vector(Vector(10.0), Vector(20.0), Vector(30.0))
    )
    val output = relation.leg.apply(input).toOption.get

    assertEquals(relation.leg.descriptor.representation, OperatorRepresentation.Sparse)
    assertEquals(output.toRows, Vector(Vector(30.0), Vector(10.0)))

  test("a reindexing with a foreign population cannot bind to the axis"):
    val foreignSpace = IndexSpace.of(4).toOption.get
    val foreign = Injection.from(indices(0, 1), foreignSpace).toOption.get

    assert(
      ReindexingLeg
        .injection(axis, foreign)
        .left
        .exists:
          case AxisRefError.ReindexingCodomainMismatch(3, 4) => true
          case _                                             => false
    )

  test("identity permutation is numerically neutral"):
    val identity = Permutation.identity(axis.size).toOption.get
    val relation = ReindexingLeg.permutation(axis, identity).toOption.get
    val input = GaleTestMatrix.fromRows(
      Vector(Vector(4.0), Vector(5.0), Vector(6.0))
    )

    assertEquals(relation.leg.apply(input).toOption.get.toRows, input.toRows)
    assertEquals(relation.child.keys, axis.keys)
