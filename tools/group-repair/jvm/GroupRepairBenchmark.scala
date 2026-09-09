package scalafim.fmri.group

import gale.linalg.{DMat,Matrix}
import scalafim.dataset.SubjectId
import java.lang.management.ManagementFactory

object GroupRepairBenchmark:
  private var consumed = 0.0
  private def value[A](x: Either[GroupError,A]): A = x.fold(e => throw new IllegalStateException(e.message),identity)
  private def matrix(n:Int,m:Int)(f:(Int,Int)=>Double):DMat =
    val b=Matrix.newBuilder(n,m)
    var i=0
    while i<n do
      var j=0
      while j<m do
        b(i,j)=f(i,j);j+=1
      i+=1
    b.result()
  def main(args:Array[String]):Unit =
    val n=args(0).toInt;val p=args(1).toInt;val m=args(2).toInt
    val mode=args(3)
    val weighting=mode match
      case "OLS"=>GroupWeighting.Unweighted
      case "FE"=>GroupWeighting.InverseVariance
      case "DL"=>GroupWeighting.RandomEffects()
    val init=System.nanoTime()
    val x=matrix(n,p)((i,j)=>if j==0 then 1.0 else math.cos((i+0.5)*j*math.Pi/n))
    val random = new java.util.Random(20260908L)
    val y=matrix(n,m)((i,s)=>0.5+0.4*x(i,1)+random.nextGaussian())
    val vs=matrix(n,m)((i,s)=>0.04+0.96*random.nextDouble())
    val design=value(GroupDesign.fromMatrix(x,Vector.tabulate(p)(j=>s"x$j")))
    val data=value(GroupData.withVariances(Vector.tabulate(n)(i=>SubjectId(s"s$i")),GroupSpace.SampleAxis(m),"effect",y,vs))
    val model=value(GroupModel.build(data,design,weighting))
    val setup=(System.nanoTime()-init)/1e9
    val bean=ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
    bean.setThreadAllocatedMemoryEnabled(true)
    val tid=Thread.currentThread().threadId()
    // Warm the same public path and shape; discard and consume each result.
    var warm=0
    while warm<10 do
      val f=value(GroupEngine.fit(model)).fit("effect").get
      consumed+=f.coefficients(1,m-1);warm+=1
    var r=0
    while r<5 do
      System.gc()
      val alloc0=bean.getThreadAllocatedBytes(tid)
      val gc0=ManagementFactory.getGarbageCollectorMXBeans.toArray.map(_.asInstanceOf[java.lang.management.GarbageCollectorMXBean].getCollectionTime).sum
      val osBean=ManagementFactory.getOperatingSystemMXBean.asInstanceOf[com.sun.management.OperatingSystemMXBean]
      val cpuStart=bean.getCurrentThreadCpuTime()
      val processStart=osBean.getProcessCpuTime()
      val start=System.nanoTime()
      val result=value(GroupEngine.fit(model))
      val fit=result.fit("effect").get
      val fitTime=(System.nanoTime()-start)/1e9
      val threadCpu=(bean.getCurrentThreadCpuTime()-cpuStart)/1e9
      val processCpu=(osBean.getProcessCpuTime()-processStart)/1e9
      val fitAlloc=bean.getThreadAllocatedBytes(tid)-alloc0
      var coefficientSum = 0.0
      var seSum = 0.0
      var row = 0
      while row < p do
        var sample = 0
        while sample < m do
          val beta = fit.coefficients(row,sample)
          val se = fit.standardErrors(row,sample)
          if !beta.isFinite || !se.isFinite then throw new IllegalStateException("incomplete benchmark map")
          coefficientSum += beta
          seSum += se
          sample += 1
        row += 1
      val ciStart=System.nanoTime()
      val c=value(GroupContrast.term("x1").evaluate(fit))
      val contrastTime=(System.nanoTime()-ciStart)/1e9
      val fdrStart=System.nanoTime()
      val q=c.adjustedP()
      val fdrTime=(System.nanoTime()-fdrStart)/1e9
      consumed+=q(0)+c.estimates(m-1)
      if !fit.coefficients(1,m-1).isFinite || !q(0).isFinite then throw new IllegalStateException("non-finite benchmark result")
      val gc=ManagementFactory.getGarbageCollectorMXBeans.toArray.map(_.asInstanceOf[java.lang.management.GarbageCollectorMXBean].getCollectionTime).sum-gc0
      println(s"BENCH,$mode,$n,$p,$m,$r,$setup,$fitTime,$fitAlloc,$contrastTime,$fdrTime,$gc,$consumed,$threadCpu,$processCpu,$coefficientSum,$seSum")
      r+=1
