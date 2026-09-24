package scalafim.fmri.group

import com.sun.management.ThreadMXBean
import gale.linalg.{DMat, Matrix}
import scalafim.dataset.SubjectId

import java.lang.management.ManagementFactory

/** Bounded allocation and CPU probe for the ordinary public group path.
  *
  * Arguments are subjects, terms, samples, mode, warmups, and measured runs.
  * Mode is one of OLS, FE, DL, or PM-MKH.
  */
object GroupBaselineBenchmark:
  private var consumed = 0.0

  private def value[A](result: Either[GroupError, A]): A =
    result.fold(error => throw new IllegalStateException(error.message), identity)

  private def matrix(rows: Int, cols: Int)(f: (Int, Int) => Double): DMat =
    val out = Matrix.newBuilder(rows, cols)
    var row = 0
    while row < rows do
      var col = 0
      while col < cols do
        out(row, col) = f(row, col)
        col += 1
      row += 1
    out.result()

  def main(args: Array[String]): Unit =
    val subjects = args(0).toInt
    val terms = args(1).toInt
    val samples = args(2).toInt
    val mode = args(3)
    val warmups = args(4).toInt
    val runs = args(5).toInt
    val weighting = mode match
      case "OLS" => GroupWeighting.Unweighted
      case "FE" => GroupWeighting.InverseVariance
      case "DL" => GroupWeighting.RandomEffects(TauEstimator.DerSimonianLaird, MetaInference.Normal)
      case "PM-MKH" => GroupWeighting.RandomEffects(TauEstimator.PauleMandel, MetaInference.ModifiedKnappHartung)
      case other => throw new IllegalArgumentException(s"unknown mode $other")

    val designMatrix = matrix(subjects, terms): (row, col) =>
      if col == 0 then 1.0
      else math.cos((row + 0.5) * col * math.Pi / subjects)
    val random = new java.util.Random(20260914L)
    val effects = matrix(subjects, samples): (row, sample) =>
      0.5 + 0.4 * designMatrix(row, 1) + random.nextGaussian() + sample * 1e-9
    val variances = matrix(subjects, samples): (_, _) =>
      0.04 + 0.96 * random.nextDouble()
    val design = value(GroupDesign.fromMatrix(designMatrix, Vector.tabulate(terms)(index => s"x$index")))
    val data = value(
      GroupData.withVariances(
        Vector.tabulate(subjects)(index => SubjectId(s"s$index")),
        GroupSpace.SampleAxis(samples),
        "effect",
        effects,
        variances
      )
    )
    val model = value(GroupModel.build(data, design, weighting))

    var warmup = 0
    while warmup < warmups do
      val fit = value(GroupEngine.fit(model)).fit("effect").get
      consumed += fit.coefficients(1, samples - 1)
      warmup += 1

    val bean = ManagementFactory.getThreadMXBean.asInstanceOf[ThreadMXBean]
    bean.setThreadAllocatedMemoryEnabled(true)
    val thread = Thread.currentThread().threadId()
    var run = 0
    while run < runs do
      val allocationStart = bean.getThreadAllocatedBytes(thread)
      val cpuStart = bean.getCurrentThreadCpuTime()
      val wallStart = System.nanoTime()
      val fit = value(GroupEngine.fit(model)).fit("effect").get
      val wallSeconds = (System.nanoTime() - wallStart) / 1e9
      val cpuSeconds = (bean.getCurrentThreadCpuTime() - cpuStart) / 1e9
      val allocatedBytes = bean.getThreadAllocatedBytes(thread) - allocationStart

      var coefficientSum = 0.0
      var standardErrorSum = 0.0
      var term = 0
      while term < terms do
        var sample = 0
        while sample < samples do
          val coefficient = fit.coefficients(term, sample)
          val standardError = fit.standardErrors(term, sample)
          if !coefficient.isFinite || !standardError.isFinite then
            throw new IllegalStateException(s"incomplete output at term $term sample $sample")
          coefficientSum += coefficient
          standardErrorSum += standardError
          sample += 1
        term += 1
      consumed += coefficientSum + standardErrorSum
      println(
        s"GROUP_BASELINE,$mode,$subjects,$terms,$samples,$run,$wallSeconds,$cpuSeconds,$allocatedBytes,$coefficientSum,$standardErrorSum,$consumed"
      )
      run += 1
