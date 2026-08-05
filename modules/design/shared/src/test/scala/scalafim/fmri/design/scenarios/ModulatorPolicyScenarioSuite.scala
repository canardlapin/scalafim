package scalafim.fmri.design.scenarios

import scalafim.fmri.design.*
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.event.{ContinuousEvent, ConvolvedTerm}
import scalafim.fmri.design.fixtures.ModulatorPolicyRFixture
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.design.SamplingFrame

/** Executable public example for explicit additive-modulator policy. */
class ModulatorPolicyScenarioSuite extends munit.FunSuite:
  private val ScenarioId = "design.modulator-policies.v1"
  private val OracleProfile = ScenarioComparisonTolerance.bounded(1e-11, 1e-11, 0.999999999, 1e-11)

  test("ordered within-cell modulator policy returns one clean scenario result") {
    val result = runScenario()
    if !result.ciPass then fail(result.render)
  }

  private def runScenario(): ScenarioResult =
    val policy =
      ModulatorOrthogonalization
        .ordered("slopes", "x", "y")
        .flatMap(_.withinCells("group"))
    val data = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0, 13.0)),
      "group" -> Column.Strings(ModulatorPolicyRFixture.cells),
      "x" -> Column.Doubles(ModulatorPolicyRFixture.cellX),
      "y" -> Column.Doubles(ModulatorPolicyRFixture.cellY)
    )
    val model = for
      declared <- policy
      request <- EventModelBuilder.EventDesignRequest.fromText(
        formula = "onset ~ hrf(group, modulators(x, y), id = slopes)",
        data = data,
        samplingFrame = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0)),
        options = EventModelBuilder.BuildOptions(
          orthogonalization = ModulatorOrthogonalizationPlan.one(declared)
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
        val modulatorEvent = compiled.terms.iterator.collectFirst {
          case (_, term: ConvolvedTerm) =>
            term.term.events.collectFirst {
              case event: ContinuousEvent if event.modulatorIds.map(_.value) == Vector("x", "y") => event
            }
        }.flatten
        val residual = modulatorEvent.toVector.flatMap { event =>
          Vector.tabulate(event.value.rows)(row => event.value(row, 1))
        }
        val eventColumns = compiled.designSchema.columns.collect {
          case StructuralColumn(_, _, origin: StructuralColumnOrigin.Event, _, _, _) => origin
        }
        val receipt = compiled.designSchema.audit.orthogonalizationReceipts match
          case Vector(value) => Some(value)
          case _             => None
        val groups = receipt.toVector.flatMap(_.steps).flatMap(_.groups)

        ScenarioHarness.result(
          ScenarioId,
          Vector(
            ScenarioHarness.fact(
              "public formula realizes sibling cell slopes",
              eventColumns.length == 4 && eventColumns.flatMap(_.modulator.map(_.value)).toSet == Set("x", "y"),
              s"origins=$eventColumns"
            ),
            ScenarioHarness.fact(
              "base-R within-cell residuals agree",
              residual.length == ModulatorPolicyRFixture.cellResidual.length &&
                residual.zip(ModulatorPolicyRFixture.cellResidual).forall { case (actual, expected) =>
                  math.abs(actual - expected) <= 1e-12
                },
              s"actual=$residual expected=${ModulatorPolicyRFixture.cellResidual}"
            ),
            ScenarioHarness.fact(
              "receipt preserves declared order and cell partitions",
              receipt.exists(_.policy.order.map(_.value) == Vector("x", "y")) &&
                groups.map(_.key) == Vector("cell:group=A", "cell:group=B") &&
                groups.map(_.sourceRows) == Vector(Vector(0, 1), Vector(2, 3)),
              s"receipt=$receipt"
            ),
            ScenarioHarness.fact(
              "R policy differences are explicit",
              ModulatorPolicyRFixture.acceptedDifferences.length == 2,
              ModulatorPolicyRFixture.acceptedDifferences.mkString("; ")
            ),
            ScenarioHarness.fact(
              "policy is part of design identity",
              compiled.designSchema.fingerprint.canonicalEncoding.contains("orthogonalization="),
              compiled.designSchema.fingerprint.value
            ),
            ScenarioHarness.finite("compiled matrix finite", compiled.designMatrix.data)
          ) ++ ScenarioHarness.comparisonMetrics(
            "base-R within-cell residuals",
            residual,
            ModulatorPolicyRFixture.cellResidual,
            OracleProfile
          )
        )
