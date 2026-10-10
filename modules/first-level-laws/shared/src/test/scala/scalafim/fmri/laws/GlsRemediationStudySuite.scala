package scalafim.fmri.laws

import scalafim.fmri.model.ArInitialization
import scalafim.scenarios.{ScenarioObservation, ScenarioResult}
import ujson.{Num, Obj, Str}

/** A separate declared campaign; historical negative receipts remain unchanged. */
class GlsRemediationStudySuite extends munit.FunSuite:
  override val munitTimeout: scala.concurrent.duration.FiniteDuration =
    scala.concurrent.duration.FiniteDuration(30, scala.concurrent.duration.MINUTES)
  private val study = CorrectedGlsQualification
  private val requested = LawEnvironment.get("SCALAFIM_GLS_REMEDIATION_PROFILE")
  private val engines = Vector(GlsStudyEngine.KnownPhi, GlsStudyEngine.Corrected)
  private val baseCells = Vector(
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
      case other          => throw new IllegalArgumentException(s"unknown GLS remediation profile $other")
    require(study.rootSeed == 1283197733, "remediation campaign requires its declared independent root seed")
    baseCells.foreach: base =>
      Vector(ArInitialization.ExactAr1, ArInitialization.Stationary).foreach: initialization =>
        val policy = if initialization == ArInitialization.Stationary then "stationary" else "legacy"
        val cell = base.copy(id = s"${base.id}-$policy", initialization = Some(initialization))
        test(s"GLS remediation $label ${cell.id}"):
          val trials = Vector.newBuilder[GlsStudyTrial]
          val failures = Vector.newBuilder[String]
          var replicate = 0
          while replicate < profile.replicates do
            val group = if cell.duration == GlsStudyDuration.Extended then 1 else 0
            val seed = study.seed(profile.domain + 30, group, replicate)
            val errors = study.noise(cell, seed)
            val model = study.model(cell, errors, s"remediation-${cell.id}-$label-$replicate")
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
            s"gls-remediation.$label.${cell.id}.engineering",
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
          // Screen measures all failures; only the frozen candidate confirmation confers admission.
          if profile == GlsStudyProfile.Confirmation && initialization == ArInitialization.Stationary then
            assert(scientific.ciPass, scientific.render)
