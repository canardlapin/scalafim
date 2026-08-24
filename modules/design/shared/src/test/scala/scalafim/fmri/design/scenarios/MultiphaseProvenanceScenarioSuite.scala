package scalafim.fmri.design.scenarios

import scalafim.fmri.design.*
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.design.SamplingFrame

/** Public phase-lowering workflow over one ordinary runtime trial table. */
class MultiphaseProvenanceScenarioSuite extends munit.FunSuite:
  private val ScenarioId = "design.multiphase-provenance.v1"

  test("multiphase provenance scenario returns a clean public result") {
    val result = runScenario()
    if !result.ciPass then fail(result.render)
  }

  private def runScenario(): ScenarioResult =
    val built =
      FactorLevelRegistry.of("condition" -> Seq("A", "B")).flatMap { levels =>
        EventModelBuilder.buildEither(
          formula =
            """onset ~
              |  hrf(condition, onsets = sample_onset, durations = sample_duration, basis = spmg1, phase = sample, parent = trial_id, id = sample) +
              |  hrf(condition, center_within(rt, condition), onsets = probe_onset, durations = probe_duration, basis = spmg2, phase = probe, parent = trial_id, id = probe)""".stripMargin,
          data = trials,
          samplingFrame = samplingFrame,
          blockIds = Vector(0, 0, 0, 1, 1, 1),
          factorLevels = levels,
          missingValuePolicy = MissingValuePolicy.ZeroContribution,
          strict = true
        )
      }
    built match
      case Left(error) =>
        ScenarioHarness.result(
          ScenarioId,
          Vector(ScenarioHarness.fact("public multiphase formula builds", passed = false, detail = error.message))
        )
      case Right(model) => evaluate(model)

  private def evaluate(model: EventModel): ScenarioResult =
    val schema = model.designSchema
    val eventOrigins = schema.columns.flatMap {
      case StructuralColumn(_, _, origin: StructuralColumnOrigin.Event, _, _, _) => Some(origin)
      case _ => None
    }
    val provenance = schema.audit.eventProvenance
    val missingSource = schema.audit.missingValues.flatMap(_.source)
    val phaseWidths = eventOrigins.groupMapReduce(_.phase.map(_.value).getOrElse("unphased"))(_ => 1)(_ + _)
    val overlapping = (0 until schema.matrix.rows).exists { row =>
      val sampleActive = (0 until 2).exists(column => math.abs(schema.matrix(row, column)) > 1e-8)
      val probeActive = (2 until schema.matrix.cols).exists(column => math.abs(schema.matrix(row, column)) > 1e-8)
      sampleActive && probeActive
    }

    ScenarioHarness.result(
      ScenarioId,
      Vector(
        ScenarioHarness.fact("one runtime table lowers both phases", model.termKeys == Vector("sample", "probe"), s"terms=${model.termKeys.mkString(",")}"),
        ScenarioHarness.fact("phase-specific basis widths remain structural", phaseWidths == Map("sample" -> 2, "probe" -> 4), s"actual=$phaseWidths"),
        ScenarioHarness.fact("every source trial survives in every phase", provenance.length == 12 && provenance.map(_.parent).distinct.length == 6, s"provenance=${provenance.length}"),
        ScenarioHarness.fact("source rows and runs remain attached", provenance.map(_.sourceRow).distinct.sorted == Vector(0, 1, 2, 3, 4, 5) && provenance.map(_.blockId).distinct.sorted == Vector(0, 1), s"rows=${provenance.map(_.sourceRow).distinct.sorted};runs=${provenance.map(_.blockId).distinct.sorted}"),
        ScenarioHarness.fact("phase epochs retain their own durations", provenance.filter(_.phase.contains(PhaseId.unsafe("sample"))).forall(_.duration.value == 0.5) && provenance.filter(_.phase.contains(PhaseId.unsafe("probe"))).forall(_.duration.value == 0.25), "phase duration provenance differs"),
        ScenarioHarness.fact("missing modulation points to its parent probe trial", missingSource.exists(value => value.parent == TrialId.unsafe("trial-5") && value.phase.contains(PhaseId.unsafe("probe")) && value.sourceRow == 4 && value.blockId == 1), s"missing=$missingSource"),
        ScenarioHarness.fact("within-condition centering is a library receipt", schema.audit.centeringReceipts.exists(_.policy == CenteringPolicy.WithinFactor(FactorId.unsafe("condition"))), s"centering=${schema.audit.centeringReceipts.map(_.policy)}"),
        ScenarioHarness.fact("overlapping phase responses superpose", overlapping, "no scan retained simultaneous sample and probe support"),
        ScenarioHarness.fact("fingerprint owns phase and policy evidence", schema.fingerprint.canonicalEncoding.contains("phase=sample") && schema.fingerprint.canonicalEncoding.contains("phase=probe") && schema.fingerprint.canonicalEncoding.contains("missing="), "canonical encoding omitted phase or policy provenance"),
        ScenarioHarness.finite("compiled matrix finite", schema.matrix.data)
      )
    )

  private val samplingFrame = SamplingFrame(
    blockLens = Seq(30, 30),
    tr = Seq(1.0, 1.0),
    startTime = Seq(0.0, 0.0)
  )

  private val trials = DataTable.fromColumns(
    "onset" -> Column.Doubles(Vector.fill(6)(0.0)),
    "sample_onset" -> Column.Doubles(Vector(1.0, 10.0, 19.0, 1.0, 10.0, 19.0)),
    "sample_duration" -> Column.Doubles(Vector.fill(6)(0.5)),
    "probe_onset" -> Column.Doubles(Vector(2.0, 11.0, 20.0, 2.0, 11.0, 20.0)),
    "probe_duration" -> Column.Doubles(Vector.fill(6)(0.25)),
    "condition" -> Column.Strings(Vector("A", "B", "A", "B", "A", "B")),
    "rt" -> Column.Doubles(Vector(0.5, 0.8, 0.6, 0.9, Double.NaN, 1.0)),
    "trial_id" -> Column.Strings(Vector("trial-1", "trial-2", "trial-3", "trial-4", "trial-5", "trial-6"))
  )
