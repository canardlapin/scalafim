package scalafim.fmri.design

import scalafim.fmri.hrf.design.SamplingFrame

class EventOnsetSelectionSuite extends munit.FunSuite:
  test("explicit onset policy counts missing and run-boundary exclusions"):
    val frame = SamplingFrame(blockLens = Seq(4, 3), tr = Seq(2.0, 1.0))
    val runs = Vector(1, 1, 1, 1, 2, 2).map(RunIndex.unsafeOneBased)
    val onsets = Vector(Double.NaN, -1.0, 0.0, 8.0, 2.9, 3.0)
    val selected = EventOnsetSelection.select(onsets, runs, frame, EventOnsetPolicy.DropOutsideRun).toOption.get
    assertEquals(selected.retainedRows, Vector(2, 4))
    assertEquals(selected.excludedCount, 4)
    assertEquals(selected.excluded.map(_.sourceRow), Vector(0, 1, 3, 5))
    assert(EventOnsetSelection.select(onsets, runs, frame, EventOnsetPolicy.RejectInvalid).isLeft)
    assert(EventOnsetSelection.select(Vector(0), Vector(RunIndex.unsafeOneBased(3)), frame, EventOnsetPolicy.DropOutsideRun).isLeft)
