package scalafim.multivar

import multivar.core.IndexAxis
import scalafim.locus.{FiniteSpace, Selection, SpaceKey}

class LocusSelectionAdapterSuite extends munit.FunSuite:

  private final class Feature

  private val features =
    FiniteSpace
      .make[Feature](SpaceKey.unsafe("multivar-features"), 5)
      .toOption
      .get

  test("locus selection order becomes explicit multivar feature order"):
    val selection =
      Selection
        .fromOrdinals(features, Vector(4, 1, 3))
        .toOption
        .get
    val indices =
      LocusSelectionAdapter
        .indexSet(selection)
        .toOption
        .get
    val roi =
      LocusSelectionAdapter
        .roiPlan("visual", selection, Some("Visual"))
        .toOption
        .get

    assertEquals(indices.axis, IndexAxis.Feature)
    assertEquals(indices.indices, Vector(4, 1, 3))
    assertEquals(roi.columns.indices, Vector(4, 1, 3))
    assertEquals(roi.label, Some("Visual"))

  test("axis choice remains an explicit multivar policy"):
    val selection =
      Selection
        .fromOrdinals(features, Vector(2, 0))
        .toOption
        .get
    val rows =
      LocusSelectionAdapter
        .indexSet(selection, IndexAxis.Row)
        .toOption
        .get

    assertEquals(rows.axis, IndexAxis.Row)
    assertEquals(rows.indices, Vector(2, 0))

  test("empty locus selections remain invalid algorithm feature sets"):
    val empty = Selection.empty(features)

    assert(LocusSelectionAdapter.indexSet(empty).isLeft)
    assert(LocusSelectionAdapter.roiPlan("empty", empty).isLeft)
