package scalafim.fmri.workflow

import bids4s.{BidsEvents, BidsTable, ConfoundSelectionConfig}
import scalafim.dataset.{DatasetValue, RunId}
import scalafim.fmri.model.ModelBuildSpec

class FirstLevelTablesSuite extends munit.FunSuite:
  private def run(id: String): RunInput = RunInput.unsafe(
    RunId(id), RepetitionTime.unsafe(1.0), 3,
    WorkflowArtifactRef.unsafe[BoldImageResource](s"file:///fixture/$id-bold.nii"),
    WorkflowArtifactRef.unsafe[EventsTableResource](s"file:///fixture/$id-events.tsv"))
  private val first = run("02")
  private val second = run("01")
  private def events(extra: String = ""): BidsTable =
    BidsEvents.readTable("onset\tduration\ttrial_id" + extra + "\n0\t0\ttrial-1" +
      (if extra.isEmpty then "" else "\twrong") + "\n").toOption.get
  private val confounds = BidsTable.parse("motion_x\n1\n2\n4\n").toOption.get
  private val config = ConfoundSelectionConfig(variables = Vector("motion_x"), clean = Vector.empty)
  private val tables = Map(first.id -> FirstLevelRunTables(events(), Some(confounds)),
    second.id -> FirstLevelRunTables(events(), Some(confounds)))

  test("binding preserves selected run order, leading zeros, repeated trials and nuisance names") {
    val bound = FirstLevelTables.bind(Vector(first, second), tables, "run", Some(config))
      .fold(e => fail(e.message), identity)
    assertEquals(bound.runIds, Vector(first.id, second.id))
    assertEquals(bound.events.rows.map(_("run")), Vector("02", "01"))
    assertEquals(bound.events.rows.map(_("trial_id")), Vector("trial-1", "trial-1"))
    assert(bound.events.typedRows.forall(_.values.values.exists(_ == DatasetValue.Text("trial-1"))))
    assertEquals(bound.nuisance.flatMap(_.names), Some(Vector(Vector("motion_x"), Vector("motion_x"))))
    assertEquals(bound.nuisance.get.matrices.map(_(2, 0)), Vector(4.0, 4.0))
    assertEquals(bound.confoundSelections.map(_._1), bound.runIds)
    assertEquals(bound.modelSpec(ModelBuildSpec("onset ~ hrf(trial_id)")).toOption.get.blockColumn, Some("run"))
    assert(bound.modelSpec(ModelBuildSpec("onset ~ hrf(trial_id)", blockColumn = Some("other"))).isLeft)
  }

  test("binding refuses missing or duplicate runs and conflicting run labels") {
    assert(FirstLevelTables.bind(Vector(first, first), tables, "run").isLeft)
    assert(FirstLevelTables.bind(Vector(first, second), tables - second.id, "run").isLeft)
    val conflict = tables.updated(first.id, FirstLevelRunTables(events("\trun")))
    assert(FirstLevelTables.bind(Vector(first, second), conflict, "run").left.toOption.get.message.contains("conflicts"))
  }

  test("binding refuses missing confounds and scan count mismatch") {
    val absent = tables.updated(first.id, FirstLevelRunTables(events()))
    assert(FirstLevelTables.bind(Vector(first, second), absent, "run", Some(config)).isLeft)
    val short = BidsTable.parse("motion_x\n1\n2\n").toOption.get
    val mismatch = tables.updated(first.id, FirstLevelRunTables(events(), Some(short)))
    assert(FirstLevelTables.bind(Vector(first, second), mismatch, "run", Some(config))
      .left.toOption.get.message.contains("differ from scans"))
  }

  test("binding refuses unresolved event missingness and nonfinite confounds") {
    val missing = BidsEvents.readTable("onset\tduration\ttrial_id\n0\t0\tn/a\n").toOption.get
    assert(FirstLevelTables.bind(Vector(first), Map(first.id -> FirstLevelRunTables(missing)), "run").isLeft)
    val missingConfound = BidsTable.parse("motion_x\n1\nn/a\n4\n").toOption.get
    assert(FirstLevelTables.bind(Vector(first), Map(first.id -> FirstLevelRunTables(events(), Some(missingConfound))),
      "run", Some(config)).isLeft)
  }

  test("explicit event selection ignores unused missing cells and preserves original companions") {
    val source = BidsTable.parse("onset\tduration\ttrial_type\tamplitude\n0\t0\ta\tn/a\n1\t0\tb\t2\n").toOption.get
    val inputs = Map(first.id -> FirstLevelRunTables(source))
    val bound = FirstLevelTables.bind(Vector(first), inputs, "run",
      eventColumns = Some(Vector("trial_type"))).fold(e => fail(e.message), identity)
    assertEquals(bound.events.nrows, 2)
    assertEquals(bound.events.rows.map(_("trial_type")), Vector("a", "b"))
    assertEquals(bound.sourceTables, inputs)
    assertEquals(bound.sourceTables(first.id).events.rows.head.last, None)
    assert(!bound.events.rows.head.contains("amplitude"))
    assert(FirstLevelTables.bind(Vector(first), inputs, "run",
      eventColumns = Some(Vector("trial_type", "amplitude"))).left.toOption.get.message.contains("amplitude"))
    assert(FirstLevelTables.bind(Vector(first), inputs, "run",
      eventColumns = Some(Vector("absent"))).isLeft)
  }

  test("event selection cannot hide invalid timing or conflicting source run identity") {
    Vector(
      "onset\tduration\ttrial_type\n0\tn/a\ta\n",
      "onset\tduration\ttrial_type\nn/a\t0\ta\n",
      "onset\tduration\ttrial_type\trun\n0\t0\ta\tother\n"
    ).foreach { text =>
      val source = BidsTable.parse(text).toOption.get
      assert(FirstLevelTables.bind(Vector(first), Map(first.id -> FirstLevelRunTables(source)), "run",
        eventColumns = Some(Vector("trial_type"))).isLeft)
    }
  }
