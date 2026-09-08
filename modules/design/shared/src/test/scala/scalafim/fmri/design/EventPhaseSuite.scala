package scalafim.fmri.design

import scalafim.fmri.design.event.*
import scalafim.fmri.hrf.{Hrfs, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame

class EventPhaseSuite extends munit.FunSuite:

  private val parents = Vector(
    TrialId.unsafe("trial-1"),
    TrialId.unsafe("trial-2"),
    TrialId.unsafe("trial-3")
  )
  private val sourceRows = Vector(4, 7, 9)
  private val events = Vector(
    Event.factor(Vector("face", "house", "face"), "stimulus"),
    Event.factor(Vector("low", "high", "low"), "load")
  )

  private val sample = EventPhase.fromParts(
    id = PhaseId.unsafe("sample"),
    onsets = Vector(1.25, 5.5, 10.25).map(Seconds(_)),
    durations = Vector(0.5, 0.5, 0.5).map(Seconds(_)),
    blockIds = Vector(0, 0, 0),
    parentTrialIds = parents,
    sourceRows = sourceRows
  ).toOption.get

  private val probe = EventPhase.fromParts(
    id = PhaseId.unsafe("probe"),
    onsets = Vector(4.0, 8.0, 12.0).map(Seconds(_)),
    durations = Vector(0.0, 0.0, 0.0).map(Seconds(_)),
    blockIds = Vector(0, 0, 0),
    parentTrialIds = parents,
    sourceRows = sourceRows
  ).toOption.get

  test("phase lowering retains parent rows and structural phase identities") {
    val multi = MultiphaseEventTerm.validated(events, Vector(sample, probe), Some("dms"))
      .toOption
      .getOrElse(fail("valid phase schedules should lower"))
    val terms = multi.phaseTerms
    assertEquals(terms.map(_.phaseId.map(_.value)), Vector(Some("sample"), Some("probe")))
    assertEquals(terms.map(_.eventProvenance.map(_.parent.value)), Vector(parents.map(_.value), parents.map(_.value)))
    assertEquals(terms.map(_.eventProvenance.map(_.sourceRow)), Vector(sourceRows, sourceRows))

    val sampling = SamplingFrame(blockLens = Seq(24), tr = Seq(1.0))
    val model = EventModel.build(
      terms.map(_.convolve(Hrfs.SPMG1, sampling)),
      sampling
    )
    val phases = model.designSchema.columns.flatMap {
      case column =>
        column.origin match
          case StructuralColumnOrigin.Event(_, phase, _, _, _, _, _) => phase.map(_.value)
          case _ => None
    }.distinct
    assertEquals(phases, Vector("sample", "probe"))
    assertEquals(model.designSchema.audit.eventProvenance.length, 6)
    assertEquals(
      model.designSchema.audit.eventProvenance.map(_.parent.value).distinct,
      parents.map(_.value)
    )
    assert(model.designSchema.fingerprint.value.nonEmpty)
  }

  test("multiphase lowering rejects silently misaligned source rows") {
    val misaligned = EventPhase.fromParts(
      id = PhaseId.unsafe("delay"),
      onsets = Vector(2.0, 6.0, 11.0).map(Seconds(_)),
      durations = Vector(2.0, 2.0, 2.0).map(Seconds(_)),
      blockIds = Vector(0, 0, 0),
      parentTrialIds = parents,
      sourceRows = Vector(4, 9, 7)
    ).toOption.get
    val result = MultiphaseEventTerm.validated(events, Vector(sample, misaligned), Some("dms"))
    assert(result.left.toOption.exists(_.message.contains("source-row ordering")))
  }

  test("phase schedules reject duplicate source rows") {
    val result = EventPhase.fromParts(
      id = PhaseId.unsafe("bad"),
      onsets = Vector(1.0, 2.0).map(Seconds(_)),
      durations = Vector(0.0, 0.0).map(Seconds(_)),
      blockIds = Vector(0, 0),
      parentTrialIds = parents.take(2),
      sourceRows = Vector(1, 1)
    )
    assert(result.left.toOption.exists(_.message.contains("source rows must be unique")))
  }

  test("phase schedules reject duplicate parent trial identities") {
    val result = EventPhase.fromParts(
      id = PhaseId.unsafe("bad"),
      onsets = Vector(1.0, 2.0).map(Seconds(_)),
      durations = Vector(0.0, 0.0).map(Seconds(_)),
      blockIds = Vector(0, 0),
      parentTrialIds = Vector.fill(2)(TrialId.unsafe("trial-1")),
      sourceRows = Vector(0, 1)
    )
    assert(result.left.toOption.exists(_.message.contains("parent trial ids must be unique")))
  }


  private def repeatedParentsPhase(id: String, blocks: Vector[Int]): EventPhase =
    EventPhase.fromParts(
      id = PhaseId.unsafe(id),
      onsets = Vector(1.0, 1.0).map(Seconds(_)),
      durations = Vector(0.0, 0.0).map(Seconds(_)),
      blockIds = blocks,
      parentTrialIds = Vector.fill(2)(TrialId.unsafe("trial-1")),
      sourceRows = Vector(0, 1)
    ).fold(error => fail(error.message), identity)

  test("phase parents may repeat across runs without losing source identity") {
    val phase = repeatedParentsPhase("sample", Vector(0, 1))
    assertEquals(phase.provenance.map(p => (p.blockId, p.parent.value, p.sourceRow)),
      Vector((0, "trial-1", 0), (1, "trial-1", 1)))
    val factors = Vector(Event.factor(Vector("A", "B"), "condition"))
    val other = repeatedParentsPhase("probe", Vector(0, 1))
    val multi = MultiphaseEventTerm.validated(factors, Vector(phase, other))
      .fold(error => fail(error.message), identity)
    assertEquals(multi.phaseTerms.map(_.eventProvenance.map(_.blockId)), Vector(Vector(0, 1), Vector(0, 1)))
  }

  test("repeated parent labels cannot hide mismatched runs across phases") {
    val phase = repeatedParentsPhase("sample", Vector(0, 1))
    val other = repeatedParentsPhase("probe", Vector(0, 2))
    val factors = Vector(Event.factor(Vector("A", "B"), "condition"))
    val result = MultiphaseEventTerm.validated(factors, Vector(phase, other))
    assert(result.left.toOption.exists(_.message.contains("same run ordering")))
  }
