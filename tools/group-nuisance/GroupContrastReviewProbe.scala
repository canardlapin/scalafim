package scalafim.fmri.group

import gale.linalg.Matrix
import scalafim.dataset.SubjectId

/** Native review fixture plus cold/warm review timing, never a fitting benchmark. */
object GroupContrastReviewProbe:
  def main(args: Array[String]): Unit =
    val n=if args.nonEmpty then args(0).toInt else 20
    val subjects=Vector.tabulate(n)(i=>SubjectId(s"subject-${i+1}"))
    val x=Matrix.tabulate(n,3) { (i,j) => j match
      case 0 => 1.0
      case 1 => if i<n/4 then 1.0 else 0.0
      case _ => math.cos((i+.5)*2*math.Pi/n)
    }
    val design=GroupDesign.fromMatrix(x,Vector("intercept","group","covariate")).toOption.get
    val contrast=GroupContrast.term("group")
    def run()=GroupContrastDiagnostics.review(subjects,design,contrast).fold(e=>throw new IllegalArgumentException(e.message),identity)
    val start=System.nanoTime();val result=run();val coldMs=(System.nanoTime()-start)/1e6
    val df=result.cr2WorkingInformation match
      case GroupCr2WorkingInformation.Available(v)=>v
      case GroupCr2WorkingInformation.Unavailable(why)=>throw new IllegalStateException(why)
    val rows=result.rows.zipWithIndex.map { (r,i) =>
      s"""{"subject":"subject-${i+1}","design":[${x(i,0)},${x(i,1)},${x(i,2)}],"leverage":${r.leverage},"influence":${r.standardizedInfluence},"share":${r.workingVarianceShare}}"""
    }.mkString(",")
    println(s"""{"method":"${result.methodId}","subjects":$n,"residualDf":${result.residualDf},"workingDf":$df,"effectiveContributors":${result.workingEffectiveContributors},"rows":[$rows]}""")
    var sink=0.0
    for _ <- 0 until 20 do sink+=run().maxLeverage
    val bean=java.lang.management.ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
    val tid=Thread.currentThread().threadId()
    val count=50
    val allocated=bean.getThreadAllocatedBytes(tid);val cpu=bean.getCurrentThreadCpuTime();val wall=System.nanoTime()
    for _ <- 0 until count do sink+=run().maxLeverage
    val wallMs=(System.nanoTime()-wall)/1e6/count
    val cpuMs=(bean.getCurrentThreadCpuTime()-cpu)/1e6/count
    val bytes=(bean.getThreadAllocatedBytes(tid)-allocated)/count
    System.err.println(s"""{"subjects":$n,"terms":3,"coldMs":$coldMs,"warmWallMs":$wallMs,"warmCpuMs":$cpuMs,"warmBytes":$bytes,"sink":$sink}""")
