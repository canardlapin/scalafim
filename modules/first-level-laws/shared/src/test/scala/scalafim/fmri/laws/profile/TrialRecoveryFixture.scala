package scalafim.fmri.laws.profile

import gale.linalg.DMat
import scalafim.fmri.ar.WhiteningTransform
import scalafim.fmri.hrf.family.ShapePoint

/** Matched B0 signal/noise ladder. Truth is used only to generate and audit data. */
object TrialRecoveryFixture:
  val truth: Vector[Double] = Vector(math.log(0.5), 0.0, 0.3)
  val noiseRatios: Vector[Double] = Vector(0.0, 0.1, 0.5, 1.0, 2.0)

  final case class Ladder(
      fixture: DecodedTrialCheckpoint.Fixture,
      clean: Array[Double],
      unitNoise: Array[Double],
      signalRms: Vector[Double]
  ):
    def response(noiseRatio: Double): Array[Double] =
      require(noiseRatio >= 0.0 && noiseRatio.isFinite)
      Array.tabulate(clean.length)(i => clean(i) + noiseRatio * unitNoise(i))

    def whiten(raw: Array[Double]): DMat =
      WhiteningTransform
        .matrix(
          fixture.whitening,
          DMat.tabulate(fixture.rows, fixture.inputBlockVoxels)((t, v) => raw(t * fixture.inputBlockVoxels + v))
        )
        .fold(e => throw new IllegalArgumentException(e.toString), identity)

  def apply(f: DecodedTrialCheckpoint.Fixture): Ladder =
    require(f.config.geometry == DecodedTrialCheckpoint.Geometry.B0Dense)
    val coefficients = new Array[Double](f.expanded.rank)
    f.plan.basis.coefficientsInto(ShapePoint.unsafe(truth), new Array[Double](f.plan.basis.fineCount), coefficients)
    val conditions = new Array[Double](f.rows * 3)
    var t = 0
    while t < f.rows do
      var trial = 0
      while trial < f.trials do
        var value = 0.0
        var p = 0
        while p < coefficients.length do
          value += f.expanded.term.data(t, p * f.trials + trial) * coefficients(p)
          p += 1
        conditions(t * 3 + trial % 3) += value
        trial += 1
      t += 1
    val clean = new Array[Double](f.rawBlock.length)
    val rms = new Array[Double](f.inputBlockVoxels)
    val rng = new scala.util.Random(20260911L)
    var v = 0
    while v < f.inputBlockVoxels do
      val means = Array.tabulate(3)(_ => (if rng.nextBoolean() then 1.0 else -1.0) * (0.5 + 1.5 * rng.nextDouble()))
      var squared = 0.0
      t = 0
      while t < f.rows do
        var value = 0.0
        var c = 0
        while c < 3 do
          value += conditions(t * 3 + c) * means(c)
          c += 1
        clean(t * f.inputBlockVoxels + v) = value
        squared += value * value
        t += 1
      rms(v) = math.sqrt(squared / f.rows)
      t = 0
      while t < f.rows do
        clean(t * f.inputBlockVoxels + v) += 4.0 * rms(v)
        // Preserve the frozen generator's RNG sequence before the next voxel.
        val _ = rng.nextGaussian()
        t += 1
      v += 1
    val noise = Array.tabulate(clean.length)(i => (f.rawBlock(i) - clean(i)) / 2.0)
    Ladder(f, clean, noise, rms.toVector)
