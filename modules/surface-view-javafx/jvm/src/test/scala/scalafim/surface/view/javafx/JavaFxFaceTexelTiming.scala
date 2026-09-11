package scalafim.surface.view.javafx

import java.nio.file.Path
import scalafim.surface.view.*

/** Splits a face-texel colour update on the real bilateral cortex into its CPU
  * parts, without JavaFX: rule lowering, single-threaded and parallel colour
  * evaluation. Texel writes and uploads are timed by the native bench.
  *
  * args: [scene=beta] [repetitions=20]
  */
object JavaFxFaceTexelTiming:
  def main(args: Array[String]): Unit =
    val scene = args.lift(0).getOrElse("beta")
    val repetitions = args.lift(1).fold(20)(_.toInt)
    val inputsDir = Path.of(System.getProperty("probe.inputs", "/Users/bbuchsbaum/code/scala/plsneuro-fixtures/spike-lut-1/inputs"))
    val inputs = JavaFxScalarLutCortexProbe.load(inputsDir, scene)
    val over = JavaFxScalarLutCortexProbe.overlayMapping(inputs.range, inputs.cutoff)
    val plan = JavaFxFaceTexelCortexProbe.facePlan(inputs, inputs.values, over, SurfaceLighting.Unlit, SurfaceFaceReduction.Mean)
    def median(values: Seq[Double]): Double = values.sorted.apply(values.length / 2)
    def time(body: => Unit): Double =
      val s = System.nanoTime(); body; (System.nanoTime() - s) / 1e6
    val lower = Vector.fill(repetitions + 4)(time(plan.meshes.foreach(mesh => SurfaceFaceTexels.lower(plan, mesh).toOption.get))).drop(4)
    val rules = plan.meshes.map(mesh => SurfaceFaceTexels.lower(plan, mesh).toOption.get)
    val serial = Vector.fill(repetitions + 4)(time(rules.foreach(rule => rule.write(new Array[Int](rule.faceCount), 0, rule.faceCount)))).drop(4)
    val parallel = Vector.fill(repetitions + 4)(time(JavaFxFaceTexelWork.colors(rules))).drop(4)
    val a = JavaFxFaceTexelWork.colors(rules)
    val b = plan.meshes.map(mesh => SurfaceFaceTexels.colors(plan, mesh).toOption.get)
    require(a.zip(b).forall((x, y) => java.util.Arrays.equals(x, y)), "parallel colours differ from serial colours")
    println(f"""face_timing={"scene":"$scene","faces":${rules.map(_.faceCount).sum},"processors":${Runtime.getRuntime.availableProcessors},""" +
      f""""lowerMedianMs":${median(lower)}%.3f,"serialColoursMedianMs":${median(serial)}%.3f,"parallelColoursMedianMs":${median(parallel)}%.3f,"parallelEqualsSerial":true}""")
