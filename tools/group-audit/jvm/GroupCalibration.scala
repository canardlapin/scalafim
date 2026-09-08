package scalafim.fmri.group
import gale.linalg.Matrix
import scalafim.dataset.SubjectId
object GroupCalibration:
  private def value[A](x:Either[GroupError,A]):A=x.fold(e=>throw new IllegalStateException(e.message),identity)
  def main(args:Array[String]):Unit=
    val samples=20000
    for n<-Vector(8,20,80);tau<-Vector(0.0,0.2,1.0) do
      val random=new java.util.Random(20260908L+n+(tau*100).toLong)
      val y=Matrix.newBuilder(n,samples);val variances=Matrix.newBuilder(n,samples)
      var i=0
      while i<n do
        val v=0.04+0.96*i.toDouble/(n-1)
        var s=0
        while s<samples do
          y(i,s)=math.sqrt(v+tau)*random.nextGaussian()
          variances(i,s)=v;s+=1
        i+=1
      val data=value(GroupData.withVariances(Vector.tabulate(n)(i=>SubjectId(s"s$i")),GroupSpace.SampleAxis(samples),"effect",y.result(),variances.result()))
      for mode<-Vector(GroupWeighting.Unweighted,GroupWeighting.RandomEffects()) do
        val fit=value(GroupEngine.fit(value(GroupModel.build(data,GroupDesign.intercept(n),mode)))).fit("effect").get
        val tests=fit.term("(Intercept)").get
        var rejected=0;var covered=0;var bad=0;var s=0
        while s<samples do
          val p=tests.pValues(s)
          if !p.isFinite then bad+=1 else if p<0.05 then rejected+=1
          s+=1
        println(s"CALIBRATION,${mode.label},$n,$tau,$samples,$rejected,$bad")
