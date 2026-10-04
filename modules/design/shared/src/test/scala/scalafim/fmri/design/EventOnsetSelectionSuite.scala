package scalafim.fmri.design

import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.design.SamplingFrame

class EventOnsetSelectionSuite extends munit.FunSuite:
  private def at(value: Double): Option[Seconds] = Some(Seconds(value))
  private val frame = SamplingFrame(blockLens = Seq(4, 3), tr = Seq(2.0, 1.0))

  test("explicit onset policy counts missing and run-boundary exclusions"):
    val runs = Vector(1, 1, 1, 1, 2, 2).map(RunIndex.unsafeOneBased)
    val onsets = Vector(None, at(-1.0), at(0.0), at(8.0), at(2.9), at(3.0))
    val selected = EventOnsetSelection.select(onsets, runs, frame, EventOnsetPolicy.DropOutsideRun).toOption.get
    assertEquals(selected.retainedRows, Vector(2, 4))
    assertEquals(selected.excludedCount, 4)
    assertEquals(selected.excluded.map(_.sourceRow), Vector(0, 1, 3, 5))
    assertEquals(selected.excluded.map(_.reason), Vector(
      ExcludedEventOnsetReason.Missing, ExcludedEventOnsetReason.BeforeRun,
      ExcludedEventOnsetReason.AtOrAfterRunEnd, ExcludedEventOnsetReason.AtOrAfterRunEnd
    ))
    assertEquals(selected.excluded.map(_.onset), Vector(None, at(-1.0), at(8.0), at(3.0)))
    EventOnsetSelection.select(onsets, runs, frame, EventOnsetPolicy.RejectInvalid) match
      case Left(EventOnsetSelectionError.Rejected(events)) => assertEquals(events, selected.excluded)
      case other => fail(s"expected rejection, found $other")
    assertEquals(
      EventOnsetSelection.select(Vector(at(0.0)), Vector(RunIndex.unsafeOneBased(3)), frame, EventOnsetPolicy.DropOutsideRun).left.toOption,
      Some(EventOnsetSelectionError.UnknownRun(0, RunIndex.unsafeOneBased(3), 2))
    )

  test("run boundaries use each run's own TR: onset 0 is retained and the end is exclusive"):
    val runs = Vector(2, 2, 2, 1).map(RunIndex.unsafeOneBased)
    // Run 2 lasts 3 scans x 1 s; run 1 lasts 4 scans x 2 s.
    val onsets = Vector(at(0.0), at(2.999), at(3.0), at(7.999))
    val selected = EventOnsetSelection.select(onsets, runs, frame, EventOnsetPolicy.DropOutsideRun).toOption.get
    assertEquals(selected.retainedRows, Vector(0, 1, 3))
    assertEquals(selected.excluded, Vector(ExcludedEventOnset(2, RunIndex.unsafeOneBased(2), at(3.0), ExcludedEventOnsetReason.AtOrAfterRunEnd)))

  test("RejectInvalid accepts all-valid input unchanged"):
    val runs = Vector(1, 2).map(RunIndex.unsafeOneBased)
    val selected = EventOnsetSelection.select(Vector(at(0.0), at(0.0)), runs, frame, EventOnsetPolicy.RejectInvalid)
    assertEquals(selected, Right(EventOnsetSelection(Vector(0, 1), Vector.empty)))
    assertEquals(selected.toOption.get.auditEntries(), Vector.empty)

  test("dropped rows become design-audit exclusions only through the explicit helper"):
    val runs = Vector(1, 1).map(RunIndex.unsafeOneBased)
    val selected = EventOnsetSelection.select(Vector(None, at(-0.5)), runs, frame, EventOnsetPolicy.DropOutsideRun).toOption.get
    assertEquals(selected.auditEntries(Some(TermId.unsafe("task"))), Vector(
      EventExclusion(0, "onset-missing", Some(TermId.unsafe("task"))),
      EventExclusion(1, "onset-before-run", Some(TermId.unsafe("task")))
    ))
