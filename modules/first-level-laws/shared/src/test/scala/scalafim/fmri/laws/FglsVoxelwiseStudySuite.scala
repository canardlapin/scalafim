package scalafim.fmri.laws

import scalafim.scenarios.{ScenarioObservation, ScenarioResult}
import ujson.{Num, Obj, Str}

class FglsVoxelwiseStudySuite extends munit.FunSuite:
  override val munitTimeout: scala.concurrent.duration.FiniteDuration =
    scala.concurrent.duration.FiniteDuration(180, scala.concurrent.duration.MINUTES)
  private val study = FglsVoxelwiseStudy

  test("portable F upper tail matches R pf"):
    Vector(
      (3.1, 2.0, 216.0, 0.047059491160643944),
      (4.2, 1.0, 37.3, 0.047505334807284035),
      (0.5, 2.0, 6.27, 0.62881567975344232),
      (2.0, 1.0, 8.39, 0.19331730474932937),
      (12.0, 2.0, 80.5, 2.7469673298433128e-05)
    ).foreach { (f, d1, d2, expected) =>
      assertEqualsDouble(FglsDistribution.fUpper(f, d1, d2), expected, 1e-12 * math.max(1.0, expected) + 1e-15)
    }

  test("every FGLS study engine measures one short censored replicate"):
    val cell = study.cells.head
    val errors = study.noise(cell, study.seed(FglsStudyProfile.Pilot, cell.group, 0))
    val data = study.response(cell, errors)
    study.engines(cell).foreach { engine =>
      val measured =
        if engine.existing then study.measureExisting(cell, errors, engine, 0)
        else study.measureCandidate(cell, data, engine, 0)
      val trial = measured.fold(error => fail(s"${engine.label}: $error"), identity)
      assert(trial.nullVariance > 0.0 && trial.whiteness.isFinite, engine.label)
      if engine == FglsEngine.KnownContinuous then
        assertEqualsDouble(trial.tDf, (cell.base.rows - 16 - cell.base.censorRows.length).toDouble, 0.0)
        assertEqualsDouble(trial.phiSquaredError, 0.0, 0.0)
    }

  private val profile = FglsStudyProfile.current

  if LawEnvironment.get("SCALAFIM_FGLS_STUDY_PROFILE").isDefined then
    study.cells.foreach { cell =>
      test(s"FGLS voxelwise ${profile.label}: ${cell.id}"):
        val engines = study.engines(cell)
        val trials = Vector.newBuilder[FglsTrial]
        val failures = Vector.newBuilder[String]
        var replicate = 0
        while replicate < profile.replicates do
          val errors = study.noise(cell, study.seed(profile, cell.group, replicate))
          val data = study.response(cell, errors)
          engines.foreach { engine =>
            val measured =
              if engine.existing then study.measureExisting(cell, errors, engine, replicate)
              else study.measureCandidate(cell, data, engine, replicate)
            measured match
              case Left(error) =>
                failures += s"$replicate/${engine.label}: $error"
                GlsStudyTrace.emit(
                  "FGLS_STUDY_FAILURE " + ujson.write(
                    Obj(
                      "cell" -> Str(cell.id),
                      "profile" -> Str(profile.label),
                      "engine" -> Str(engine.label),
                      "replicate" -> Num(replicate),
                      "error" -> Str(error)
                    )
                  )
                )
              case Right(trial) =>
                trials += trial
                GlsStudyTrace.emit("FGLS_STUDY_TRIAL " + ujson.write(trial.json(cell, profile)))
          }
          replicate += 1
        val values = trials.result()
        val errors = failures.result()
        val facts = Vector.newBuilder[ScenarioObservation]
        facts += ScenarioObservation.Fact("no-refusals", errors.isEmpty, errors.take(20).mkString("; "))
        engines.foreach { engine =>
          val rows = values.filter(_.engine == engine)
          facts += ScenarioObservation.Fact(
            s"${engine.label}-complete",
            rows.length == profile.replicates,
            s"${rows.length}/${profile.replicates}"
          )
          facts += ScenarioObservation.Fact(
            s"${engine.label}-finite",
            rows.forall(row =>
              Vector(row.nullEstimate, row.nullVariance, row.signalEstimate, row.signalVariance, row.whiteness)
                .forall(_.isFinite)
            ),
            "retained numeric outputs"
          )
        }
        val engineering = ScenarioResult(s"fgls-voxelwise.${profile.label}.${cell.id}.engineering", facts.result())
        GlsStudyTrace.emit(engineering.render)
        assert(engineering.ciPass, engineering.render)
    }
