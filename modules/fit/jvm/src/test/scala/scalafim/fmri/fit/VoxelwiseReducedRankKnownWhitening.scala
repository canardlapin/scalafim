package scalafim.fmri.fit

import gale.linalg.DMat
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan}
import scalafim.fmri.model.*
import java.io.PrintWriter
import java.nio.ByteBuffer
import java.security.MessageDigest
import scala.util.control.NonFatal

/** Diagnostic only: bypass AR estimation in both point fit and bootstrap.
  * This does not change the public estimator or certify the residual generator.
  */
object VoxelwiseReducedRankKnownWhitening:
  private val contrasts = Vector(
    VoxelwiseBootstrapContrast.unsafe("difference", Vector(0 -> 1.0, 1 -> -1.0)),
    VoxelwiseBootstrapContrast.unsafe("average", Vector(0 -> 0.5, 1 -> 0.5))
  )
  private def hash(matrix: DMat): String =
    val digest = MessageDigest.getInstance("SHA-256")
    val buffer = ByteBuffer.allocate(8)
    for r <- 0 until matrix.rows; c <- 0 until matrix.cols do
      buffer.clear()
      buffer.putDouble(matrix(r, c))
      digest.update(buffer.array())
    digest.digest().map(b => f"${b & 0xff}%02x").mkString
  private def array(x: Iterable[Double]): String = x.mkString("[", ",", "]")
  private def quote(s: String): String = "\"" + s.flatMap {
    case '"' => "\\\""
    case '\\' => "\\\\"
    case c if c < ' ' => f"\\u${c.toInt}%04x"
    case c => c.toString
  } + "\""

  def main(args: Array[String]): Unit =
    require(args.length == 1 && !new java.io.File(args(0)).exists(), "requires a new output JSONL path")
    val writer = new PrintWriter(args(0), "UTF-8")
    try
      for d <- 0 until 400 do
        val (x, y, truth, runs) = VoxelwiseReducedRankQualification.baselineInput(d)
        val responseHash = hash(y)
        val design = DesignMatrix.unsafe(x)
        val response = ResponseBlock.unsafe(y)
        val partition = ReducedRankDesignPartition.fromColumns(3, Vector(0, 1), Vector(2)).toOption.get
        val ids = Vector(0, 1, 2)
        val rows = (0 until 80).toVector
        val plans = Vector(0.1, 0.5, -0.2).map(r => WhiteningPlan.global(ArmaCoefficients.ar(r), Vector(TimeSegment(0, 80, 0))))
        val estimatedConfig = ReducedRankGlsConfig.unsafe(
          components = ReducedRankComponentSpec.unsafeFixed(1),
          autocorrelation = AutocorrelationConfig.unsafe(order = 1, voxelwise = true),
          inference = ReducedRankInferencePolicy.EstimatesOnly
        )
        // Reproduce the archived public estimator on these responses as an
        // additional pairing check. No resampling is required for this check.
        val original = ReducedRankGlsPrepared.prepare(design, response, runs, estimatedConfig, ids, partition)
          .flatMap(_.fitBlock(FitBlockInput(design, response, ids, rows, runs)))
          .toOption.get.asInstanceOf[VoxelwiseReducedRankFitBlockResult].estimate
        val originalValues = for r <- 0 until 2; v <- 0 until 3 yield original.coefficients(r, v)
        for rank <- Vector(1, 2) do
          val started = System.nanoTime()
          val base = s"\"scenario\":\"baseline_known_rank$rank\",\"dataset\":$d,\"mode\":\"FrozenWhitening\",\"budget\":\"main\",\"replicates\":399,\"seed\":${2017 + 101 * d},\"response_sha256\":\"$responseHash\",\"whitening_source\":\"known_generating_coefficients_in_point_fit_and_replicates\",\"archived_baseline_estimate_check\":${array(originalValues)}"
          try
            val bootstrap = VoxelwiseReducedRankBootstrapConfig.unsafe(
              ReducedRankBootstrapConfig.unsafe(399, 1, 2017 + 101 * d), contrasts = contrasts
            )
            val config = ReducedRankGlsConfig.unsafe(
              components = ReducedRankComponentSpec.unsafeFixed(rank),
              inference = ReducedRankInferencePolicy.VoxelwiseBootstrap(bootstrap)
            )
            val result = for
              gs <- VoxelwiseReducedRankGls.geometry(x, y, plans, partition)
              fit <- VoxelwiseReducedRankGls.solve(gs, partition, rank, config.solver)
              uncertainty <- VoxelwiseReducedRankBootstrap.run(design, y, runs, ids, plans, gs, partition, rank, fit, config, bootstrap)
            yield (fit, uncertainty.bootstrap.get)
            val seconds = (System.nanoTime() - started).toDouble / 1e9
            result match
              case Left(error) =>
                val completed = error match
                  case FitError.ReducedRankBootstrapFailed(index, _) => index - 1
                  case _ => 0
                writer.println(s"{$base,\"seconds\":$seconds,\"status\":\"failed\",\"completed_replicates\":$completed,\"error\":${quote(error.message)}}")
              case Right((fit, u)) =>
                require(u.diagnostics.refittedWhiteningReplicates == 0)
                val cov = ids.map(v => u.covariance.matrixForVoxelPosition(v).toOption.get)
                val estimates = (for r <- 0 until 2; v <- ids yield fit.coefficients(r, v)) ++ u.contrasts.flatMap(_.estimate.toSeq)
                val variance = (for r <- 0 until 2; v <- ids yield cov(v)(r, r)) ++ u.contrasts.flatMap(_.standardErrors.toSeq.map(x => x * x))
                val lower = (for r <- 0 until 2; v <- ids yield u.lower(r, v)) ++ u.contrasts.flatMap(_.lower.toSeq)
                val upper = (for r <- 0 until 2; v <- ids yield u.upper(r, v)) ++ u.contrasts.flatMap(_.upper.toSeq)
                val targets = (for r <- 0 until 2; v <- ids yield truth(r, v)) ++ contrasts.flatMap(c => ids.map(v => c.weights.map((r, w) => w * truth(r, v)).sum))
                writer.println(s"{$base,\"seconds\":$seconds,\"status\":\"ok\",\"completed_replicates\":399,\"estimate\":${array(estimates)},\"variance\":${array(variance)},\"lower\":${array(lower)},\"upper\":${array(upper)},\"truth\":${array(targets)}}")
          catch
            case NonFatal(error) =>
              val seconds = (System.nanoTime() - started).toDouble / 1e9
              writer.println(s"{$base,\"seconds\":$seconds,\"status\":\"failed\",\"completed_replicates\":null,\"error\":${quote(error.toString)}}")
          writer.flush()
          if writer.checkError() then throw new java.io.IOException("diagnostic evidence write failed")
        if (d + 1) % 50 == 0 then println(s"RRG_KNOWN_WHITENING_PROGRESS ${d + 1}/400")
    finally writer.close()
