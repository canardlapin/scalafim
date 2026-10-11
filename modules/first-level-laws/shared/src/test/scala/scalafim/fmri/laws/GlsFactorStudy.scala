package scalafim.fmri.laws

import scalafim.fmri.fit.*
import ujson.{Arr, Num, Obj, Str}

private[laws] enum GlsFactorProfile(val label: String, val replicates: Int, val domain: Int):
  case Pilot extends GlsFactorProfile("pilot", 8, 1)
  case Diagnostic extends GlsFactorProfile("diagnostic", 512, 2)
  case Confirmation extends GlsFactorProfile("confirmation", 2048, 3)

private[laws] object GlsFactorProfile:
  val current: GlsFactorProfile = LawEnvironment.get("SCALAFIM_GLS_FACTOR_PROFILE") match
    case None | Some("pilot") => Pilot
    case Some("diagnostic")   => Diagnostic
    case Some("confirmation") => Confirmation
    case Some(other)          => throw new IllegalArgumentException(s"unknown GLS factor profile: $other")

private[laws] final case class GlsInferenceParts(scale: Double, geometry: Double, df: Int):
  require(scale > 0.0 && scale.isFinite && geometry > 0.0 && geometry.isFinite && df > 0)

private[laws] object GlsFactorStudy:
  val engines: Vector[GlsStudyEngine] = Vector(GlsStudyEngine.KnownPhi, GlsStudyEngine.Corrected)
  val diagnosticCells: Vector[GlsStudyCell] =
    for
      order <- Vector(1, 2)
      nuisance <- Vector(0, 12)
      censored <- Vector(false, true)
      pooling <- StudyPooling.values.toVector
    yield
      val load = if nuisance == 0 then "low" else "high"
      val mask = if censored then "censored" else "complete"
      val pool = pooling.toString.toLowerCase
      GlsStudyCell(
        s"ar$order-$load-$mask-$pool",
        if order == 1 then Vector(0.5) else Vector(0.45, -0.10),
        nuisance,
        censored,
        pooling
      )

  val confirmationCells: Vector[GlsStudyCell] = Vector(
    GlsStudyCell("ar2-high-censored-global", Vector(0.45, -0.10), 12, true, StudyPooling.Global),
    GlsStudyCell("ar2-high-censored-run", Vector(0.45, -0.10), 12, true, StudyPooling.Run),
    GlsStudyCell(
      "ar2-high-censored-voxelwise-long",
      Vector(0.45, -0.10),
      12,
      true,
      StudyPooling.Voxelwise,
      GlsStudyDuration.Extended
    )
  )

  def cells(profile: GlsFactorProfile): Vector[GlsStudyCell] = profile match
    case GlsFactorProfile.Confirmation => confirmationCells
    case _                             => diagnosticCells

  def seed(profile: GlsFactorProfile, group: Int, replicate: Int): Int =
    ((GlsFactorBounds.DefaultSeed.toLong + profile.domain.toLong * 10000019L +
      (group + 1L) * 1000003L + replicate.toLong * 7919L) % 2147483646L + 1L).toInt

  def parts(fit: DenseFmriFitResult): Either[GlsStudyError, GlsInferenceParts] =
    fit.coefficientCovariance
      .matrixForVoxelPosition(0)
      .left
      .map(error => GlsStudyError.Pipeline(GlsStudyStage.Contrast, error.message))
      .map(matrix =>
        GlsInferenceParts(
          fit.inference.varianceScale(0),
          matrix(0, 0),
          fit.inference.residualDegreesOfFreedom.value
        )
      )

  def record(cell: GlsStudyCell, profile: GlsFactorProfile, trial: GlsStudyTrial, parts: GlsInferenceParts): Obj =
    val json = trial.json(cell, GlsStudyProfile.Screen, GlsFactorBounds.DefaultSeed)
    json("profile") = Str(profile.label)
    json("order") = Num(cell.phi.length)
    json("nuisance") = Num(cell.nuisance)
    json("censored") = ujson.Bool(cell.censored)
    json("pooling") = Str(cell.pooling.toString.toLowerCase)
    json("runLengths") = Arr.from(cell.runLengths)
    json("innovationGroup") = Num(if cell.duration == GlsStudyDuration.Extended then 1 else 0)
    json("varianceScale") = Num(parts.scale)
    json("covarianceGeometry") = Num(parts.geometry)
    json("residualDf") = Num(parts.df)
    json
