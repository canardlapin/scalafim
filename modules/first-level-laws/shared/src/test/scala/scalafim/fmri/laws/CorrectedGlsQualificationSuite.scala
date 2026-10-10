package scalafim.fmri.laws

import ujson.{Num, Obj, Str}

class CorrectedGlsQualificationSuite extends munit.FunSuite:
  override val munitTimeout: scala.concurrent.duration.FiniteDuration =
    scala.concurrent.duration.FiniteDuration(20, scala.concurrent.duration.MINUTES)
  private val study = CorrectedGlsQualification
  private val profile = GlsStudyProfile.current

  // Qualification is an explicit campaign. The complete six-cell gate remains
  // negative in the frozen 2026-10-08 confirmation receipt; simulator QA runs
  // in the ordinary suite independently of statistical admission.
  private val campaignRequested = LawEnvironment.get("SCALAFIM_GLS_STUDY_PROFILE").isDefined ||
    LawRunProfile.current == LawRunProfile.Calibration

  if campaignRequested then
    study.cells.zipWithIndex.foreach { (cell, index) =>
      test(s"corrected GLS ${profile.label}: ${cell.id}"):
        val trials = Vector.newBuilder[GlsStudyTrial]
        val failures = Vector.newBuilder[String]
        var replicate = 0
        while replicate < profile.replicates do
          val errors = study.noise(cell, study.seed(profile.domain, index, replicate))
          val model = study.model(cell, errors, s"${cell.id}-${profile.label}-$replicate")
          GlsStudyEngine.values.foreach { engine =>
            study.measure(cell, model, engine, replicate) match
              case Left(error) =>
                val message = s"$replicate/${engine.label}: ${error.message}"
                failures += message
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
              case Right(value) =>
                trials += value
                GlsStudyTrace.emit("GLS_STUDY_TRIAL " + ujson.write(value.json(cell, profile, study.rootSeed)))
          }
          replicate += 1
        val result = study.summary(cell, profile, trials.result(), failures.result())
        GlsStudyTrace.emit(result.render)
        assert(result.ciPass, result.render)
    }
