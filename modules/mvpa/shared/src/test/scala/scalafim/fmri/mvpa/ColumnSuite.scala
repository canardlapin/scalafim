package scalafim.fmri.mvpa

import resample4s.core.Draw
import resample4s.core.IndexSpace
import resample4s.core.Injection
import resample4s.core.Permutation
import resample4s.core.Selection

class ColumnSuite extends munit.FunSuite:

  private final case class RunKey(value: String)
  private final case class ItemKey(value: String)
  private final case class SubjectKey(value: String)

  private def sampleAxis(id: String = "samples"): AxisRef[SampleId] =
    AxisRef
      .create(
        AxisId.unsafe(id),
        AxisPurpose.Samples,
        Vector("sample-11", "sample-29", "sample-47").map(SampleId.unsafe),
        CoordinateBasis.unsafe("trial-table"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("fixture", "v1")
      )
      .toOption
      .get

  private def indices(values: Int*): IArray[Int] =
    IArray.unsafeFromArray(values.toArray)

  test("targets and metadata are ordinary values owned by one exact row axis"):
    val rows = sampleAxis()
    val categorical = Column(rows, Vector("face", "scene", "face")).toOption.get
    val numeric = Column(rows, Vector(1.5, 2.5, 3.5)).toOption.get
    val runs = Column(rows, Vector(RunKey("run-1"), RunKey("run-1"), RunKey("run-2"))).toOption.get
    val items = Column(rows, Vector(ItemKey("item-a"), ItemKey("item-b"), ItemKey("item-c"))).toOption.get
    val subjects = Column(rows, Vector.fill(3)(SubjectKey("subject-01"))).toOption.get
    val covariate = numeric.map(value => value - 1.0)

    assert(categorical.rows eq rows.evidence)
    assert(numeric.rows eq rows.evidence)
    assert(runs.rows eq rows.evidence)
    assert(items.rows eq rows.evidence)
    assert(subjects.rows eq rows.evidence)
    assert(covariate.rows eq rows.evidence)
    assertEquals(categorical.toVector, Vector("face", "scene", "face"))
    assertEquals(covariate.toVector, Vector(0.5, 1.5, 2.5))

  test("construction rejects length, duplicate-key, and runtime owner mismatches"):
    val rows = sampleAxis()

    assert(Column(rows, Vector(1, 2)).left.exists:
      case ColumnError.LengthMismatch(3, 2) => true
      case _                                => false)
    assert(
      AxisRef
        .create(
          AxisId.unsafe("duplicate-samples"),
          AxisPurpose.Samples,
          Vector(SampleId.unsafe("same"), SampleId.unsafe("same")),
          CoordinateBasis.unsafe("trial-table"),
          None,
          AxisScale.nominal,
          CoordinateProvenance.unsafe("fixture", "v1")
        )
        .isLeft
    )

    val foreignRows = sampleAxis("foreign-samples")
    assert(
      Column
        .decode(rows, foreignRows.identity.toRecord, indices(1, 2, 3))
        .left
        .exists:
          case ColumnError.Axis(AxisRefError.RuntimeIdentityMismatch(_, _)) => true
          case _                                                            => false
    )

  test("selection, injection, permutation, and draw reindex values and ownership together"):
    val rows = sampleAxis()
    val column = Column(rows, Vector("a", "b", "c")).toOption.get
    val space = IndexSpace.of(rows.size).toOption.get
    val selected = ReindexingLeg
      .selection(rows, Selection.from(indices(0, 2), space).toOption.get)
      .toOption
      .get
    val injected = ReindexingLeg
      .injection(rows, Injection.from(indices(2, 0), space).toOption.get)
      .toOption
      .get
    val permuted = ReindexingLeg
      .permutation(rows, Permutation.from(indices(2, 0, 1)).toOption.get)
      .toOption
      .get
    val drawn = ReindexingLeg
      .draw(rows, Draw.from(indices(2, 0, 2, 1), space).toOption.get)
      .toOption
      .get

    val selectedColumn = column.reindex(selected).toOption.get
    val injectedColumn = column.reindex(injected).toOption.get
    val permutedColumn = column.reindex(permuted).toOption.get
    val drawnColumn = column.reindex(drawn).toOption.get

    assertEquals(selectedColumn.toVector, Vector("a", "c"))
    assertEquals(injectedColumn.toVector, Vector("c", "a"))
    assertEquals(permutedColumn.toVector, Vector("c", "a", "b"))
    assertEquals(drawnColumn.toVector, Vector("c", "a", "c", "b"))
    assert(selectedColumn.rows eq selected.child.evidence)
    assert(injectedColumn.rows eq injected.child.evidence)
    assert(permutedColumn.rows eq permuted.child.evidence)
    assert(drawnColumn.rows eq drawn.child.evidence)
    assertEquals(drawnColumn.rowIdentity, drawn.child.identity)

  test("wrong-axis reindexing is rejected by the nominal type"):
    val errors = compileErrors("""
      import multivar.core.SemanticSpace
      import resample4s.core.Permutation
      import scalafim.fmri.mvpa.*
      import scala.reflect.ClassTag

      def wrong[
          Left <: SemanticSpace,
          Right <: SemanticSpace,
          A: ClassTag
      ](
          column: Column[Left, A],
          relation: ReindexingLeg[Right, SampleId, SampleId, Permutation]
      ) = column.reindex(relation)
    """)

    assert(errors.nonEmpty)
