package scalafim.fmri.group

import gale.linalg.Matrix
import scalafim.dataset.SubjectId

/** Pointwise null calibration: independent normal effects, known or independently
  * chi-square-estimated sampling variances. Rows, variance noise and outcomes
  * are fixed before running any estimator so comparisons use identical data.
  */
object GroupRepairCalibration:
  private def value[A](x: Either[GroupError,A]): A = x.fold(e => throw new IllegalStateException(e.message),identity)
  def main(args: Array[String]): Unit =
    val samples = if args.nonEmpty then args(0).toInt else 20000
    val modes = Vector(
      GroupWeighting.Unweighted,
      GroupWeighting.RandomEffects(TauEstimator.DerSimonianLaird, MetaInference.Normal),
      GroupWeighting.RandomEffects(TauEstimator.DerSimonianLaird, MetaInference.ModifiedKnappHartung),
      GroupWeighting.RandomEffects(TauEstimator.PauleMandel, MetaInference.ModifiedKnappHartung))
    for n <- Vector(8,20,80); p <- Vector(1,3); varianceDf <- Vector(0,8,40); tau <- Vector(0.0,0.2,1.0) do
      val seed = 2026090800L + n * 10000 + p * 1000 + varianceDf * 10 + (tau * 10).toLong
      val random = new java.util.Random(seed)
      val y = Matrix.newBuilder(n,samples)
      val variances = Matrix.newBuilder(n,samples)
      val x = Matrix.tabulate(n,p) { (i,j) =>
        if j == 0 then 1.0
        else if j == 1 then (if i < math.max(2,n/4) then 1.0 else 0.0)
        else math.cos((i+0.5)*2*math.Pi/n)
      }
      var i = 0
      while i < n do
        // Unequal precision, including imbalance correlated with group membership.
        val v = 0.04 + 0.96*i.toDouble/(n-1)
        var s = 0
        while s < samples do
          y(i,s) = math.sqrt(v+tau)*random.nextGaussian()
          var multiplier = 1.0
          if varianceDf > 0 then
            var chiSquare = 0.0
            var k = 0
            while k < varianceDf do
              val z = random.nextGaussian()
              chiSquare += z*z
              k += 1
            multiplier = chiSquare / varianceDf
          variances(i,s) = v*multiplier
          s += 1
        i += 1
      val design = value(GroupDesign.fromMatrix(x,Vector.tabulate(p)(j=>s"x$j")))
      val data = value(GroupData.withVariances(Vector.tabulate(n)(i=>SubjectId(s"s$i")),GroupSpace.SampleAxis(samples),"effect",y.result(),variances.result()))
      for mode <- modes do
        val fit = value(GroupEngine.fit(value(GroupModel.build(data,design,mode)))).fit("effect").get
        val tests = fit.term(if p == 1 then "x0" else "x1").get
        var rejected = 0
        var bad = 0
        var s = 0
        while s < samples do
          val pValue = tests.pValues(s)
          if !pValue.isFinite then bad += 1
          else if pValue < 0.05 then rejected += 1
          s += 1
        println(s"CALIBRATION,${mode.label},$n,$p,$varianceDf,$tau,$samples,$rejected,$bad,$seed")
