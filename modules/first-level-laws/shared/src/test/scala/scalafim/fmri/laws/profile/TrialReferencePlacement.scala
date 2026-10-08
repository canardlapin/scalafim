package scalafim.fmri.laws.profile

import scalafim.fmri.fit.profile.*

/** Development candidates are evaluated before selecting and freezing a bank.
  * Confirmation seeds never enter the selection criterion.
  */
object TrialReferencePlacement:
  val candidates: Vector[(String, Vector[Int])] = Vector(
    "4x2x1" -> Vector(4, 2, 1), "4x1x2" -> Vector(4, 1, 2), "8x1x1" -> Vector(8, 1, 1))

  def points(name: String): TrialReferencePoints =
    val counts = candidates.find(_._1 == name).get._2
    TrialReferencePoints(TrialDomainProposal.chart, Vector.tabulate(counts.product): index =>
      var remaining = index
      val unit = counts.map: count =>
        val value = (remaining % count + 0.5) / count
        remaining /= count
        value
      TrialDomainProposal.coordinates(unit))

  def confirmation(trials: Int, freshCount: Int, sensitivityCount: Int): Vector[TrialDomainGate.Sample] =
    val rng = new scala.util.Random(if trials == 30 then 2026100813L else 2026100814L)
    val fresh = Vector.fill(freshCount)(Vector.fill(3)(rng.nextDouble()))
    def sample(cohort: String, unit: Vector[Double], index: Int, noise: Double) =
      TrialDomainGate.Sample(s"$cohort-$index-$noise", cohort, unit, noise, 2026200815L + 10000L * trials + index)
    fresh.zipWithIndex.map((u, i) => sample("fresh", u, i, 0.1)) ++
      TrialDomainGate.boundaryUnits.zipWithIndex.map((u, i) => sample("boundary", u, freshCount + i, 0.1)) ++
      Vector(0.0, 1.0).flatMap(noise => fresh.take(sensitivityCount).zipWithIndex.map((u, i) => sample("sensitivity", u, i, noise)))

  def run(name: String, trials: Int, fresh: Int, sensitivity: Int, confirm: Boolean,
      completed: String => Unit = _ => ()): TrialDomainGate.Result =
    TrialDomainGate.run(trials, fresh, sensitivity, completed = completed,
      referencePoints = Some(points(name)), referenceStorage = TrialReferenceStorage.ReconstructSecondBands,
      cohort = if confirm then Some(confirmation(trials, fresh, sensitivity)) else None)

  def stressBank(name: String): TrialReferenceBank =
    val f = TrialDomainProposal.fixture(1200)
    val prep = TrialBandedPreparation.prepare(f.expanded, Some(f.whitening), Some(gale.linalg.DMat.tabulate(f.rows, f.nuisance)((r, c) => f.baseline.designMatrix(r, c))), 1.0)
      .fold(e => throw new IllegalArgumentException(e.message), identity)
    // No default grid bank is constructed or retained.
    prep.referenceBank(points(name), TrialReferenceStorage.ReconstructSecondBands)
      .fold(e => throw new IllegalArgumentException(e.message), identity)
