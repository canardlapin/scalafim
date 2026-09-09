package scalafim.fmri.group

import gale.linalg.Matrix
import scalafim.dataset.SubjectId
import java.lang.management.ManagementFactory

/** Completed pointwise map tests; plan generation and reusable-plan execution
  * are timed separately. Run forks sequentially, after compilation/calibration.
  */
object GroupSymmetryBenchmark:
  private def get[A](x:Either[GroupError,A]):A=x.fold(e=>throw new IllegalStateException(e.message),identity)
  def main(args:Array[String]):Unit =
    val n=args(0).toInt;val samples=args(1).toInt;val draws=args(2).toInt
    val weighting=if args(3)=="equal" then GroupSymmetryWeighting.EqualSubjects else GroupSymmetryWeighting.FixedInverseVariance
    val bean=ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
    bean.setThreadAllocatedMemoryEnabled(true)
    val tid=Thread.currentThread().threadId()
    val subjects=Vector.tabulate(n)(i=>SubjectId(s"s$i"))
    val rng=new java.util.Random(718223+n)
    val y=Matrix.tabulate(n,samples)((i,j)=>rng.nextGaussian()*math.sqrt(.1+i.toDouble/n))
    val v=Matrix.tabulate(n,samples)((i,j)=>.1+i.toDouble/n+(j%13)*.01)
    val data=get(GroupData.withVariances(subjects,GroupSpace.SampleAxis(samples),"effect",y,v))
    val design=GroupDesign.intercept(n)
    val start=System.nanoTime();val alloc=bean.getThreadAllocatedBytes(tid);val cpu=bean.getCurrentThreadCpuTime()
    val plan=get(GroupSignFlipPlan.compile(subjects,GroupSignSampling.MonteCarlo(draws,82231L)))
    println(s"PLAN,$n,$samples,$draws,$weighting,${(System.nanoTime()-start)/1e9},${(bean.getCurrentThreadCpuTime()-cpu)/1e9},${bean.getThreadAllocatedBytes(tid)-alloc},${plan.signBytes}")
    var iteration=0
    while iteration<8 do
      val start=System.nanoTime();val alloc=bean.getThreadAllocatedBytes(tid);val cpu=bean.getCurrentThreadCpuTime()
      val result=get(GroupSignFlip.test(data,design,"effect",plan,
        GroupSymmetryAssumption.IndependentSymmetricErrorsConditionalOnSelectionAndPrecisions,weighting))
      val elapsed=(System.nanoTime()-start)/1e9
      val cpuSeconds=(bean.getCurrentThreadCpuTime()-cpu)/1e9
      val allocated=bean.getThreadAllocatedBytes(tid)-alloc
      require(result.failures.isEmpty)
      var checksum=0.0;var i=0
      while i<samples do
        require(result.pValues(i).isFinite && result.pValues(i)>0 && result.pValues(i)<=1)
        require(result.estimate(i).isFinite && result.score(i).isFinite)
        checksum+=result.pValues(i);i+=1
      if iteration>=3 then println(s"FIT,$n,$samples,$draws,$weighting,$iteration,$elapsed,$cpuSeconds,$allocated,$checksum")
      iteration+=1
