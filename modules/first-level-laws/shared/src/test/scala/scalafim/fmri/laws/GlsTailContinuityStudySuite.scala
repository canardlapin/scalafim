package scalafim.fmri.laws

import scalafim.fmri.model.{ArBiasCorrection, ArCensorTreatment, ArInitialization}
import scalafim.scenarios.{ScenarioObservation, ScenarioResult}
import ujson.{Num, Obj, Str}

class GlsTailContinuityStudySuite extends munit.FunSuite:
  override val munitTimeout: scala.concurrent.duration.FiniteDuration =
    scala.concurrent.duration.FiniteDuration(30, scala.concurrent.duration.MINUTES)
  private val study = CorrectedGlsQualification
  private val requested = LawEnvironment.get("SCALAFIM_GLS_TAIL_CONTINUITY_PROFILE")
  private val engines = Vector(GlsStudyEngine.KnownPhi, GlsStudyEngine.Corrected)
  private val bases = Vector(
    GlsStudyCell("ar2-high-complete-voxelwise", Vector(0.45, -0.1), 12, false, StudyPooling.Voxelwise),
    GlsStudyCell("ar2-high-censored-voxelwise", Vector(0.45, -0.1), 12, true, StudyPooling.Voxelwise),
    GlsStudyCell(
      "ar2-high-censored-voxelwise-long",
      Vector(0.45, -0.1),
      12,
      true,
      StudyPooling.Voxelwise,
      GlsStudyDuration.Extended
    ),
    GlsStudyCell("ar1-high-censored-voxelwise", Vector(0.5), 12, true, StudyPooling.Voxelwise)
  )
  requested.foreach: label =>
    val profile = label match
      case "pilot"        => GlsStudyProfile.Pilot
      case "screen"       => GlsStudyProfile.Screen
      case "confirmation" => GlsStudyProfile.Confirmation
      case other          => throw new IllegalArgumentException(s"unknown tail/continuity profile $other")
    require(study.rootSeed == 1339142671, "tail/continuity campaign requires its declared independent root")
    bases.foreach: base =>
      Vector("adaptive-restart", "tail-restart", "tail-continuous").foreach: policy =>
        val correction = if policy == "adaptive-restart" then ArBiasCorrection.Ols
        else ArBiasCorrection.olsTailAnchored(25).toOption.get
        val treatment =
          if policy == "tail-continuous" then ArCensorTreatment.EstimateOnly else ArCensorTreatment.RestartWhitening
        val cell = base.copy(
          id = s"${base.id}-$policy",
          initialization = Some(ArInitialization.Stationary),
          correction = Some(correction),
          censorTreatment = treatment
        )
        test(s"GLS tail/continuity $label ${cell.id}"):
          val trials = Vector.newBuilder[GlsStudyTrial]
          val failures = Vector.newBuilder[String]
          var replicate = 0
          while replicate < profile.replicates do
            val group = if cell.duration == GlsStudyDuration.Extended then 1 else 0
            val errors = study.noise(cell, study.seed(profile.domain + 40, group, replicate))
            val model = study.model(cell, errors, s"tail-continuity-${cell.id}-$label-$replicate")
            engines.foreach: engine =>
              study.measure(cell, model, engine, replicate) match
                case Left(error) =>
                  failures += s"$replicate/${engine.label}: ${error.message}"
                  GlsStudyTrace.emit(
                    "GLS_STUDY_FAILURE " + ujson.write(
                      Obj(
                        "cell" -> Str(cell.id),
                        "profile" -> Str(label),
                        "engine" -> Str(engine.label),
                        "replicate" -> Num(replicate),
                        "error" -> Str(error.message)
                      )
                    )
                  )
                case Right(trial) =>
                  trials += trial
                  GlsStudyTrace.emit("GLS_STUDY_TRIAL " + ujson.write(trial.json(cell, profile, study.rootSeed)))
            replicate += 1
          val values = trials.result()
          val errors = failures.result()
          val engineering = ScenarioResult(
            s"gls-tail-continuity.$label.${cell.id}.engineering",
            Vector(
              ScenarioObservation.Fact("no-refusals", errors.isEmpty, errors.mkString("; ")),
              ScenarioObservation.Fact(
                "complete",
                values.length == engines.length * profile.replicates,
                s"${values.length} fits"
              )
            )
          )
          assert(engineering.ciPass, engineering.render)
          val scientific = study.summary(cell, profile, values, errors, engines)
          GlsStudyTrace.emit(scientific.render)
          if profile == GlsStudyProfile.Confirmation && policy == "tail-continuous" then
            assert(scientific.ciPass, scientific.render)
