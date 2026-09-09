package scalafim.fmri.group

import gale.linalg.Matrix
import scalafim.dataset.SubjectId

/** Held-out product-path calibration. Each Monte Carlo batch has an independent
  * action seed; retain batch counts so uncertainty can be clustered by plan.
  */
object GroupSymmetryCalibration:
  private def get[A](x: Either[GroupError,A]): A = x.fold(e=>throw new IllegalStateException(e.message),identity)
  private val assumption = GroupSymmetryAssumption.IndependentSymmetricErrorsConditionalOnSelectionAndPrecisions
  def main(args: Array[String]): Unit =
    val exactStudies = if args.nonEmpty then args(0).toInt else 10000
    val batches = if args.length > 1 then args(1).toInt else 100
    val batchStudies = if args.length > 2 then args(2).toInt else 20
    val draws = if args.length > 3 then args(3).toInt else 999
    val confirm = args.contains("--confirm")
    val outcomeRoot = if confirm then 202609110000L else 202609090000L
    val planRoot = if confirm then 2026091200L else 2026091000L
    val started = System.nanoTime()
    println("method,n,df,tau,errors,precision,batch,studies,question,rejections,failures,seed,planSeed")
    for n <- (if confirm then Vector(80) else Vector(8,20,80)) do
      val subjects = Vector.tabulate(n)(i=>SubjectId(s"s$i"))
      val design = GroupDesign.intercept(n)
      val count = if n==8 then 1 else batches
      val size = if n==8 then exactStudies else batchStudies
      val plans = Vector.tabulate(count) { batch =>
        val seed = planRoot+n*10000+batch
        val sampling = if n==8 then GroupSignSampling.Exact else GroupSignSampling.MonteCarlo(draws,seed)
        get(GroupSignFlipPlan.compile(subjects,sampling))
      }
      for df <- (if confirm then Vector(8) else Vector(0,8,40)); tau <- (if confirm then Vector(1.0) else Vector(0.0,.2,1.0)); heavy <- (if confirm then Vector(true) else Vector(false,true)); dominant <- (if confirm then Vector(true) else Vector(false,true)); batch <- 0 until count do
        val seed = outcomeRoot+n*1000000L+df*10000+(tau*10).toInt*1000+(if heavy then 500 else 0)+(if dominant then 200 else 0)+batch
        val random = new java.util.Random(seed)
        def chiSquare(df: Int): Double =
          var sum=0.0; var i=0
          while i<df do
            val z=random.nextGaussian();sum+=z*z;i+=1
          sum
        val y = Matrix.newBuilder(n,size*3)
        val variances = Matrix.newBuilder(n,size*3)
        var i=0
        while i<n do
          val v = if dominant then (if i==0 then .01 else 1.0) else .04+.96*i/(n-1)
          var s=0
          while s<size do
            val z=random.nextGaussian()
            val error=math.sqrt(v+tau)*(if heavy then z/math.sqrt(chiSquare(3)) else z)
            val variance=v*(if df==0 then 1.0 else chiSquare(df)/df)
            y(i,s)=error
            y(i,size+s)=error+.7
            y(i,2*size+s)=error+.35
            variances(i,s)=variance;variances(i,size+s)=variance;variances(i,2*size+s)=variance
            s+=1
          i+=1
        val data=get(GroupData.withVariances(subjects,GroupSpace.SampleAxis(size*3),"effect",y.result(),variances.result()))
        // All questions share the realized errors and variance estimates.
        val pm=get(GroupEngine.fit(get(GroupModel.mixedEffects(data,design,TauEstimator.PauleMandel,MetaInference.ModifiedKnappHartung)))).fit("effect").get
        val pmTest=pm.term("(Intercept)").get
        def printCounts(method:String,question:String,rejected:Int,bad:Int):Unit =
          println(s"$method,$n,$df,$tau,${if heavy then "t3" else "normal"},${if dominant then "dominant" else "spread"},$batch,$size,$question,$rejected,$bad,$seed,${planRoot+n*10000+batch}")
        for (name,q) <- Vector("null"->0,"power"->2) do
          var rejected=0;var bad=0;var s=0
          while s<size do
            val p=pmTest.pValues(q*size+s)
            if !p.isFinite then bad+=1 else if p<=.05 then rejected+=1
            s+=1
          printCounts("PM-mKH",name,rejected,bad)
        for weighting <- GroupSymmetryWeighting.values do
          val result=get(GroupSignFlip.test(data,design,"effect",plans(batch),assumption,weighting))
          val shifted=get(GroupSignFlip.test(data,design,"effect",plans(batch),assumption,weighting,nullCenter=.7))
          for (name,q) <- Vector("null"->0,"coverage-miss"->1,"power"->2) do
            val source=if q==1 then shifted else result
            var rejected=0;var bad=0;var s=0
            while s<size do
              val p=source.pValues(q*size+s)
              if !p.isFinite then bad+=1 else if p<=.05 then rejected+=1
              s+=1
            printCounts(weighting.toString,name,rejected,bad)
    System.err.println(s"elapsedSeconds=${(System.nanoTime()-started)/1e9}")
