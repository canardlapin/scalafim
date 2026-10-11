package scalafim.fmri.laws

import scalafim.scenarios.{ScenarioObservation, ScenarioResult}
import ujson.{Num, Obj, Str}

class GlsFactorStudySuite extends munit.FunSuite:
  override val munitTimeout: scala.concurrent.duration.FiniteDuration =
    scala.concurrent.duration.FiniteDuration(20, scala.concurrent.duration.MINUTES)
  private val profile = GlsFactorProfile.current
  private val study = CorrectedGlsQualification

  if LawEnvironment.get("SCALAFIM_GLS_FACTOR_PROFILE").isDefined then
    GlsFactorStudy.cells(profile).foreach { cell =>
      test(s"GLS factors ${profile.label}: ${cell.id}"):
        val trials = Vector.newBuilder[GlsStudyTrial]
        val failures = Vector.newBuilder[String]
        val facts = Vector.newBuilder[ScenarioObservation]
        var replicate = 0
        while replicate < profile.replicates do
          val group = if cell.duration == GlsStudyDuration.Extended then 1 else 0
          val errors = study.noise(cell, GlsFactorStudy.seed(profile, group, replicate))
          val model = study.model(cell, errors, s"factor-${cell.id}-${profile.label}-$replicate")
          GlsFactorStudy.engines.foreach { engine =>
            var captured: Option[Either[GlsStudyError, GlsInferenceParts]] = None
            val measured = study
              .measure(cell, model, engine, replicate, fitted => captured = Some(GlsFactorStudy.parts(fitted)))
              .flatMap { trial =>
                captured
                  .toRight(GlsStudyError.Pipeline(GlsStudyStage.Contrast, "no inference parts captured"))
                  .flatMap(identity)
                  .map(parts => trial -> parts)
              }
            measured match
              case Left(error) =>
                failures += s"$replicate/${engine.label}: ${error.message}"
                GlsStudyTrace.emit(
                  "GLS_STUDY_FAILURE " + ujson.write(
                    Obj(
                      "cell" -> Str(cell.id),
                      "profile" -> Str(profile.label),
                      "engine" -> Str(engine.label),
                      "replicate" -> Num(replicate),
                      "error" -> Str(error.message)
                    )
                  )
                )
              case Right((trial, parts)) =>
                trials += trial
                val difference = math.abs(trial.nullVariance - parts.scale * parts.geometry)
                facts += ScenarioObservation.Fact(
                  s"$replicate/${engine.label}-public-variance-decomposition",
                  difference <= 1e-10 * (1.0 + math.abs(trial.nullVariance)) && parts.df == cell.df,
                  s"gap=$difference reportedDf=${parts.df} expectedDf=${cell.df}"
                )
                GlsStudyTrace.emit("GLS_STUDY_TRIAL " + ujson.write(GlsFactorStudy.record(cell, profile, trial, parts)))
          }
          replicate += 1
        val values = trials.result()
        val errors = failures.result()
        facts += ScenarioObservation.Fact("no-refusals", errors.isEmpty, errors.mkString("; "))
        GlsFactorStudy.engines.foreach { engine =>
          val rows = values.filter(_.engine == engine)
          facts += ScenarioObservation.Fact(
            s"${engine.label}-complete",
            rows.length == profile.replicates,
            s"${rows.length}/${profile.replicates}"
          )
          facts += ScenarioObservation.Fact(
            s"${engine.label}-finite",
            rows.forall(row =>
              Vector(
                row.nullEstimate,
                row.nullVariance,
                row.signalEstimate,
                row.signalVariance,
                row.phiSquaredError,
                row.whiteness
              ).forall(_.isFinite)
            ),
            "retained numeric outputs"
          )
        }
        val engineering = ScenarioResult(s"gls-factors.${profile.label}.${cell.id}.engineering", facts.result())
        GlsStudyTrace.emit(engineering.render)
        assert(engineering.ciPass, engineering.render)
        if profile == GlsFactorProfile.Confirmation then
          val scientific = study.summary(cell, GlsStudyProfile.Confirmation, values, errors, GlsFactorStudy.engines)
          GlsStudyTrace.emit(scientific.render)
          assert(scientific.ciPass, scientific.render)
    }
