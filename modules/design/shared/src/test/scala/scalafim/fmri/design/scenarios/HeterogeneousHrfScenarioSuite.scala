package scalafim.fmri.design.scenarios

import scalafim.fmri.design.*
import scalafim.fmri.design.contrast.LevelId
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.fixtures.HeterogeneousHrfRFixture
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

/** Executable public example for phase/cell mixed-width HRF assignment. */
class HeterogeneousHrfScenarioSuite extends munit.FunSuite:
  private val ScenarioId = "design.heterogeneous-hrf.v1"
  private val ConditionFactor = FactorId("condition").fold(error => fail(error.message), identity)
  private val OracleProfile = ScenarioComparisonTolerance.bounded(
    maxAbsoluteL2Error = 1e-10,
    maxRelativeL2Error = 1e-10,
    minimumSignedCorrelation = 0.9999999999,
    maxNormRatioDeviation = 1e-10
  )

  test("phase and cell HRFs agree with one independent continuous-time oracle") {
    val result = runScenario()
    if !result.ciPass then fail(result.render)
  }

  private def runScenario(): ScenarioResult =
    val phaseHrfs = for
      probe <- HrfByCell.oneFactor(
        "condition",
        "A" -> probeA,
        "B" -> probeB
      )
      assignments <- HrfByPhase.named(
        "sample" -> HrfAssignment.Shared(sampleHrf),
        "probe" -> HrfAssignment.ByCell(probe)
      )
    yield assignments
    val data = DataTable.fromColumns(
      "sample_onset" -> Column.Doubles(HeterogeneousHrfRFixture.sampleOnsets),
      "probe_onset" -> Column.Doubles(HeterogeneousHrfRFixture.probeOnsets),
      "condition" -> Column.Strings(HeterogeneousHrfRFixture.conditions),
      "trial" -> Column.Strings(Vector.tabulate(HeterogeneousHrfRFixture.conditions.length)(i => s"trial-${i + 1}"))
    )
    val model = for
      assignments <- phaseHrfs
      factors <- FactorLevelRegistry.of("condition" -> Seq("A", "B"))
      request <- EventModelBuilder.EventDesignRequest.fromText(
        formula =
          "sample_onset ~ " +
            "hrf(condition, onsets = sample_onset, phase = sample, parent = trial, id = sample) + " +
            "hrf(condition, onsets = probe_onset, phase = probe, parent = trial, id = probe)",
        data = data,
        samplingFrame = SamplingFrame(blockLens = Seq(28), tr = Seq(1.0), startTime = Seq(0.0)),
        options = EventModelBuilder.BuildOptions(
          precision = Seconds(1.0),
          factorLevels = factors,
          hrfByPhase = Some(assignments)
        )
      )
      compiled <- EventModelBuilder.buildEither(request)
    yield compiled

    model match
      case Left(error) =>
        ScenarioHarness.result(
          ScenarioId,
          Vector(ScenarioHarness.fact("public model compiles", passed = false, detail = error.message))
        )
      case Right(compiled) =>
        val provenance = compiled.designSchema.audit.eventProvenance
        val aligned = alignColumns(compiled)
        val expected = HeterogeneousHrfRFixture.columnKeys.zipWithIndex.map { case (key, column) =>
          key -> HeterogeneousHrfRFixture.matrix.map(_(column))
        }.toMap
        val alignedValues = HeterogeneousHrfRFixture.columnKeys.flatMap(key => aligned.getOrElse(key, Vector.empty))
        val expectedValues = HeterogeneousHrfRFixture.columnKeys.flatMap(key => expected.getOrElse(key, Vector.empty))
        val matrixAgreement =
          aligned.keySet == expected.keySet && expected.forall { case (key, values) =>
            aligned.get(key).exists(_.zip(values).forall { case (actual, reference) =>
              math.abs(actual - reference) <= 1e-12
            })
          }
        val origins = compiled.designSchema.columns.flatMap(_.origin match
          case event: StructuralColumnOrigin.Event => Some(event)
          case _                                   => None
        )
        val phaseWidths = origins.flatMap(origin => origin.phase.map(_.value -> origin))
          .groupBy(_._1)
          .view
          .mapValues(_.length)
          .toMap

        val lags = (0 to 400).toVector.map(index => Lag(index.toDouble / 100.0))
        val temporalValues = lags.map(lag => probeB(lag).data(1))
        val temporalPeakLag = lags(temporalValues.zipWithIndex.maxBy(_._1)._2).value
        val sampleIntegral = lags.sliding(2).map { pair =>
          val left = sampleHrf(pair(0)).data(0)
          val right = sampleHrf(pair(1)).data(0)
          0.005 * (left + right)
        }.sum

        ScenarioHarness.result(
          ScenarioId,
          Vector(
            ScenarioHarness.fact(
              "one public formula retains sample and probe parent provenance",
              provenance.map(_.phase.map(_.value)).toSet == Set(Some("sample"), Some("probe")) &&
                provenance.map(_.parent.value).toSet == Set("trial-1", "trial-2", "trial-3", "trial-4"),
              s"provenance=$provenance"
            ),
            ScenarioHarness.fact(
              "phase assignment derives mixed structural widths",
              phaseWidths == Map("sample" -> 2, "probe" -> 3),
              s"widths=$phaseWidths"
            ),
            ScenarioHarness.fact(
              "every event column retains phase cell basis role and element identity",
              origins.forall(origin => origin.phase.nonEmpty && origin.basis.exists(ref => ref.role.nonEmpty && ref.elementId.nonEmpty)),
              s"origins=$origins"
            ),
            ScenarioHarness.fact(
              "independent base-R continuous-time oracle agrees",
              matrixAgreement,
              s"actualKeys=${aligned.keys.toVector.sorted} expectedKeys=${expected.keys.toVector.sorted}"
            ),
            ScenarioHarness.fact(
              "unscaled coordinates remain explicit",
              compiled.designSchema.columns.forall(_.hrfScale == HrfColumnScale.identity),
              s"scales=${compiled.designSchema.columns.map(_.hrfScale)}"
            ),
            ScenarioHarness.fact(
              "oracle conventions are explicit",
              HeterogeneousHrfRFixture.acceptedDifferences.length == 2,
              HeterogeneousHrfRFixture.acceptedDifferences.mkString("; ")
            ),
            ScenarioHarness.finite("mixed-width matrix finite", compiled.designMatrix.data)
          ) ++
            ScenarioHarness.comparisonMetrics(
              "independent mixed-width design",
              alignedValues,
              expectedValues,
              OracleProfile
            ) ++
            Vector(
              ScenarioHarness.scalar(
                "probe temporal peak lag",
                temporalPeakLag,
                1.0,
                ScenarioTolerance.absolute(0.01)
              ),
              ScenarioHarness.scalar(
                "sample kernel integral on compact support",
                sampleIntegral,
                1.0 - math.exp(-4.0),
                ScenarioTolerance.absolute(2e-5)
              )
            )
        )

  private def alignColumns(model: scalafim.fmri.design.event.EventModel): Map[String, Vector[Double]] =
    model.designSchema.columns.zipWithIndex.flatMap { case (column, index) =>
      column.origin match
        case StructuralColumnOrigin.Event(_, Some(phase), cell, _, Some(basis), _, _) =>
          val suffix = phase.value match
            case "sample" => Some("")
            case "probe" => basis.role.collect {
              case BasisRole.Canonical          => "_canonical"
              case BasisRole.TemporalDerivative => "_temporal"
            }
            case _ => None
          for
            value <- cell.get(ConditionFactor).map(_.value)
            suffixValue <- suffix
          yield
            val key = s"${phase.value}_${value}$suffixValue"
            key -> Vector.tabulate(model.designMatrix.rows)(row => model.designMatrix(row, index))
        case _ => None
    }.toMap

  private def sampleHrf: Hrf =
    identified(
      "sample-oracle",
      Vector(("sample-canonical", BasisRole.Canonical, "sample canonical")),
      lag => Array(math.exp(-lag.value))
    )

  private def probeA: Hrf =
    identified(
      "probe-a-oracle",
      Vector(("probe-a-canonical", BasisRole.Canonical, "probe A canonical")),
      lag => Array(math.exp(-0.5 * lag.value))
    )

  private def probeB: Hrf =
    identified(
      "probe-b-oracle",
      Vector(
        ("probe-b-canonical", BasisRole.Canonical, "probe B canonical"),
        ("probe-b-temporal", BasisRole.TemporalDerivative, "probe B temporal derivative")
      ),
      lag =>
        val decay = math.exp(-lag.value)
        Array(decay, lag.value * decay)
    )

  private def identified(
      name: String,
      coordinates: Vector[(String, BasisRole, String)],
      evaluate: Lag => Array[Double]
  ): Hrf =
    val elements = coordinates.zipWithIndex.map { case ((id, role, label), index) =>
      val parsed = BasisElementId.from(id).fold(error => fail(error.message), identity)
      BasisElement(parsed, index + 1, role, label)
    }
    Hrf.multiWithBasisElements(
      name = name,
      elements = elements,
      span = Seconds(4.0),
      support = Support.Compact(Seconds(4.0))
    )(evaluate).fold(error => fail(error.message), identity)
