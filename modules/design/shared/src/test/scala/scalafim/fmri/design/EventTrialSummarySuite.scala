package scalafim.fmri.design

import scalafim.fmri.design.contrast.LevelId
import scalafim.fmri.design.event.*
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.design.fixtures.ModelStudioEventTimingFixture

class EventTrialSummarySuite extends munit.FunSuite:
  private val frame = SamplingFrame(blockLens = Seq(24), tr = Seq(1.0))
  private val parents = Vector("one", "two", "three").map(TrialId.unsafe)
  private val sourceRows = Vector(3, 8, 12)
  private val events = Vector(Event.factor(Vector("face", "face", "scene"), "stimulus"))
  private val sample = EventPhase.fromParts(
    PhaseId.unsafe("sample"), Vector(1.0, 5.0, 10.0).map(Seconds(_)), Vector.fill(3)(0.5.s),
    Vector.fill(3)(0), parents, sourceRows
  ).toOption.get
  private val probe = EventPhase.fromParts(
    PhaseId.unsafe("probe"), Vector(4.0, 8.0, 12.0).map(Seconds(_)), Vector.fill(3)(0.0.s),
    Vector.fill(3)(0), parents, sourceRows
  ).toOption.get
  private val phasedModel = EventModel.build(
    MultiphaseEventTerm.validated(events, Vector(sample, probe), Some("memory")).toOption.get.phaseTerms
      .map(_.convolve(Hrfs.SPMG1, frame)),
    frame
  )
  private val parentOnsets = ParentTrialOnsets.validated(Vector(
    ParentTrialOnset(ParentTrialKey(RunIndex.unsafeOneBased(1), parents(0)), 0.0.s),
    ParentTrialOnset(ParentTrialKey(RunIndex.unsafeOneBased(1), parents(1)), 3.0.s),
    ParentTrialOnset(ParentTrialKey(RunIndex.unsafeOneBased(1), parents(2)), 7.0.s)
  )).toOption.get

  private def cell(level: String): CellKey =
    CellKey.unsafe(Vector(CellAssignment(FactorId.unsafe("stimulus"), LevelId.unsafe(level))))

  private def summary(values: Vector[EventTrialSummary], phase: String, level: String): EventTrialSummary =
    values.find(value => value.phase.contains(PhaseId.unsafe(phase)) && value.cell == cell(level)).getOrElse(fail(s"missing $phase/$level summary"))

  test("phase summaries retain term, categorical cell, run, duration and explicit parent offsets") {
    val values = EventTrialSummary.forModel(phasedModel, Some(parentOnsets)).toOption.get
    assertEquals(values.size, 4)
    val sampleFace = summary(values, "sample", "face")
    assertEquals(sampleFace.term.value, "memory")
    assertEquals(sampleFace.run, RunIndex.unsafeOneBased(1))
    assertEquals(sampleFace.events, 2)
    assertEquals(sampleFace.duration, EventTimingSummary(0.5.s, 0.5.s, 0.5.s))
    assertEquals(sampleFace.onsetOffset, Some(EventTimingSummary(1.0.s, 1.5.s, 2.0.s)))
    val probeFace = summary(values, "probe", "face")
    assertEquals(probeFace.events, 2)
    assertEquals(probeFace.duration, EventTimingSummary(0.0.s, 0.0.s, 0.0.s))
    assertEquals(probeFace.onsetOffset, Some(EventTimingSummary(4.0.s, 4.5.s, 5.0.s)))
    assertEquals(summary(values, "sample", "scene").onsetOffset, Some(EventTimingSummary(3.0.s, 3.0.s, 3.0.s)))
  }

  test("phased terms refuse absent, duplicate, and missing parent timing") {
    EventTrialSummary.forModel(phasedModel) match
      case Left(EventTrialSummaryError.ParentTimingRequired(term)) => assertEquals(term.value, "memory")
      case other => fail(s"expected missing timing error, found $other")
    ParentTrialOnsets.validated(Vector(
      ParentTrialOnset(ParentTrialKey(RunIndex.unsafeOneBased(1), parents.head), 0.0.s),
      ParentTrialOnset(ParentTrialKey(RunIndex.unsafeOneBased(1), parents.head), 1.0.s)
    )) match
      case Left(EventTrialSummaryError.DuplicateParentTrial(_)) => ()
      case other => fail(s"expected duplicate parent error, found $other")
    val incomplete = ParentTrialOnsets.validated(Vector(
      ParentTrialOnset(ParentTrialKey(RunIndex.unsafeOneBased(1), parents.head), 0.0.s)
    )).toOption.get
    EventTrialSummary.forModel(phasedModel, Some(incomplete)) match
      case Left(EventTrialSummaryError.MissingParentTrial(_, term, _)) => assertEquals(term.value, "memory")
      case other => fail(s"expected missing parent error, found $other")
  }

  test("ordinary event terms remain useful without invented trial timing") {
    val ordinary = EventTerm.validated(
      Vector(Event.factor(Vector("go", "go"), "outcome")), Vector(2.0.s, 6.0.s), Vector(1.0.s, 3.0.s),
      Vector(0, 0), Some("go")
    ).toOption.get
    val model = EventModel.build(Vector(ordinary.convolve(Hrfs.SPMG1, frame)), frame)
    val value = EventTrialSummary.forModel(model).toOption.get.head
    assertEquals(value.term.value, "go")
    assertEquals(value.events, 2)
    assertEquals(value.duration, EventTimingSummary(1.0.s, 2.0.s, 3.0.s))
    assertEquals(value.onsetOffset, None)
  }

  test("unphased provenance is validated but does not demand parent timing") {
    val ordinary = EventTerm(
      events = Vector(Event.factor(Vector("go"), "outcome")),
      onsets = Vector(2.0.s),
      durations = Vector(0.0.s),
      blockIds = Vector(0),
      termTag = Some("observed"),
      eventProvenance = Vector(EventRowProvenance(parents.head, None, 0, 0, 2.0.s, 0.0.s))
    )
    val model = EventModel.build(Vector(ordinary.convolve(Hrfs.SPMG1, frame)), frame)
    val summary = EventTrialSummary.forModel(model).toOption.get.head
    assertEquals(summary.events, 1)
    assertEquals(summary.onsetOffset, None)
  }

  test("inconsistent phase provenance is rejected before aggregation") {
    val phase = PhaseId.unsafe("probe")
    val bad = EventTerm(
      events = Vector(Event.factor(Vector("face"), "stimulus")),
      onsets = Vector(1.0.s),
      durations = Vector(0.0.s),
      blockIds = Vector(0),
      termTag = Some("bad"),
      phaseId = Some(phase),
      eventProvenance = Vector(EventRowProvenance(parents.head, Some(phase), 0, 0, 2.0.s, 0.0.s))
    )
    val model = EventModel.build(Vector(bad.convolve(Hrfs.SPMG1, frame)), frame)
    val timing = ParentTrialOnsets.validated(Vector(
      ParentTrialOnset(ParentTrialKey(RunIndex.unsafeOneBased(1), parents.head), 0.0.s)
    )).toOption.get
    EventTrialSummary.forModel(model, Some(timing)) match
      case Left(EventTrialSummaryError.InconsistentProvenance(term, _, _)) => assertEquals(term.value, "bad")
      case other => fail(s"expected provenance error, found $other")
  }

  test("m7b's five synthetic studies retain their generated event ribbons") {
    assertEquals(ModelStudioEventTimingFixture.sourceEventHashes.keySet, Set("block", "gamble", "dms", "stop", "dense"))
    val summaries = ModelStudioEventTimingFixture.studies.map { study =>
      study.name -> EventTrialSummary.forModel(study.model, study.parents).toOption.get
    }.toMap
    def events(study: String, term: String, run: Int, assignments: Map[String, String]): Int =
      summaries(study).find { value =>
        value.term.value == term && value.run.oneBased == run &&
          value.cell.assignments.map(a => a.factor.value -> a.level.value).toMap == assignments
      }.map(_.events).getOrElse(fail(s"missing $study/$term run $run $assignments"))
    assertEquals(summaries("block").size, 6)
    assertEquals(events("block", "blocks", 1, Map("condition" -> "faces")), 4)
    assertEquals(summaries("gamble").size, 6)
    assertEquals(events("gamble", "gamble", 1, Map.empty), 56)
    assertEquals(events("gamble", "response", 2, Map("response" -> "accept")), 30)
    assertEquals(summaries("dms").size, 17)
    assertEquals(events("dms", "sample", 1, Map("load" -> "low", "stim" -> "face")), 6)
    assertEquals(events("dms", "probe", 1, Map("load" -> "high")), 9)
    assertEquals(events("dms", "errors", 1, Map.empty), 3)
    val delay = summaries("dms").find(v => v.term.value == "delay" && v.run.oneBased == 1 && v.cell.assignments.head.level.value == "low").get
    assertEquals(delay.duration, EventTimingSummary(4.0.s, 6.0.s, 8.0.s))
    assertEquals(delay.onsetOffset, Some(EventTimingSummary(2.0.s, 2.0.s, 2.0.s)))
    assertEquals(summaries("stop").size, 6)
    assertEquals(events("stop", "trials", 1, Map("outcome" -> "go")), 86)
    assertEquals(events("stop", "trials", 2, Map("outcome" -> "stop_success")), 12)
    assertEquals(summaries("dense").size, 24)
    assertEquals(events("dense", "stim", 2, Map("category" -> "words", "task" -> "ignore")), 11)
  }

  test("terms sharing a tag are summarized under the model's unique term keys") {
    def go(onsets: Vector[Double]): ConvolvedTerm =
      EventTerm.validated(
        Vector(Event.factor(Vector.fill(onsets.length)("go"), "outcome")), onsets.map(Seconds(_)),
        Vector.fill(onsets.length)(1.0.s), Vector.fill(onsets.length)(0), Some("cond")
      ).toOption.get.convolve(Hrfs.SPMG1, frame)
    val model = EventModel.build(Vector(go(Vector(2.0, 6.0)), go(Vector(10.0))), frame)
    val values = EventTrialSummary.forModel(model).toOption.get
    assertEquals(values.map(_.term.value), model.termKeys)
    assertEquals(values.map(_.events), Vector(2, 1))

    // A hand-assembled model whose distinct keys carry the same tag is not pooled either.
    val sameTag = model.copy(terms = model.terms.zip(Vector("first", "second")).map { case ((_, term), key) =>
      term match
        case convolved: ConvolvedTerm => key -> convolved.copy(term = convolved.term.copy(termTag = Some("cond")))
        case other => key -> other
    })
    val pooled = EventTrialSummary.forModel(sameTag).toOption.get
    assertEquals(pooled.map(value => value.term.value -> value.events), Vector("first" -> 2, "second" -> 1))
    assertEquals(pooled.map(_.duration), Vector(EventTimingSummary(1.0.s, 1.0.s, 1.0.s), EventTimingSummary(1.0.s, 1.0.s, 1.0.s)))
  }

  private val twoRuns = SamplingFrame(blockLens = Seq(20, 20), tr = Seq(1.0, 1.0))
  private def twoRunModel(probeOnsets: Vector[Double]): EventModel =
    // Parent ids are global, so the builder qualifies per-run ids with their run.
    val ids = Vector("run1:one", "run1:two", "run2:one", "run2:two").map(TrialId.unsafe)
    val probe = EventPhase.fromParts(
      PhaseId.unsafe("probe"), probeOnsets.map(Seconds(_)), Vector.fill(4)(0.0.s),
      Vector(0, 0, 1, 1), ids, Vector(0, 1, 2, 3)
    ).toOption.get
    EventModel.build(
      MultiphaseEventTerm.validated(Vector(Event.factor(Vector.fill(4)("face"), "stimulus")), Vector(probe), Some("memory"))
        .toOption.get.phaseTerms.map(_.convolve(Hrfs.SPMG1, twoRuns)),
      twoRuns
    )
  private def runOnsets(values: (Int, String, Double)*): ParentTrialOnsets =
    ParentTrialOnsets.validated(values.toVector.map((run, trial, onset) =>
      ParentTrialOnset(ParentTrialKey(RunIndex.unsafeOneBased(run), TrialId.unsafe(s"run$run:$trial")), Seconds(onset))
    )).toOption.get

  test("parent timing is keyed by run and measured on each run's own clock") {
    val model = twoRunModel(Vector(3.0, 9.0, 4.0, 12.0))
    val timing = runOnsets((1, "one", 1.0), (1, "two", 6.0), (2, "one", 0.0), (2, "two", 10.0))
    val values = EventTrialSummary.forModel(model, Some(timing)).toOption.get
    assertEquals(values.map(_.run.oneBased), Vector(1, 2))
    assertEquals(values.map(_.events), Vector(2, 2))
    assertEquals(values(0).onsetOffset, Some(EventTimingSummary(2.0.s, 2.5.s, 3.0.s)))
    assertEquals(values(1).onsetOffset, Some(EventTimingSummary(2.0.s, 3.0.s, 4.0.s)))
  }

  test("a phase earlier than its parent onset is rejected, including global-clock parent timing") {
    val model = twoRunModel(Vector(3.0, 9.0, 4.0, 12.0))
    EventTrialSummary.forModel(model, Some(runOnsets((1, "one", 1.0), (1, "two", 9.5), (2, "one", 0.0), (2, "two", 10.0)))) match
      case Left(EventTrialSummaryError.PhaseBeforeParent(term, 1, key, offset)) =>
        assertEquals(term.value, "memory")
        assertEquals(key, ParentTrialKey(RunIndex.unsafeOneBased(1), TrialId.unsafe("run1:two")))
        assertEqualsDouble(offset.value, -0.5, 0.0)
      case other => fail(s"expected a negative parent offset error, found $other")
    // Run 2 parents given on the concatenated clock (run 2 starts at 20 s).
    val global = runOnsets((1, "one", 1.0), (1, "two", 6.0), (2, "one", 20.0), (2, "two", 30.0))
    EventTrialSummary.forModel(model, Some(global)) match
      case Left(EventTrialSummaryError.PhaseBeforeParent(_, 2, key, _)) => assertEquals(key.run.oneBased, 2)
      case other => fail(s"expected a global-clock parent error, found $other")
  }
