package scalafim.phrfcmp.exec

import scalafim.phrfcmp.ingest.{Digests, FitInputs}
import scalafim.phrfcmp.prep.InputFingerprint

/** Bind every fitter-visible input, including trial stimulus identities and the GLMsingle noise pool, which
  * are intentionally outside the common native preparation fingerprint. No generator truth enters this hash.
  */
private[phrfcmp] object ScoringInputFingerprint:
  def of(in: FitInputs): String =
    val prefix = ("phrf-scoring-input-v1:" + InputFingerprint.of(in)).getBytes("UTF-8")
    val size = prefix.length.toLong + 4L + in.evStim.length.toLong * 4L + 1L +
      in.yPool.fold(0L)(p => 12L + p.data.length.toLong * 8L)
    require(size <= Int.MaxValue, "scoring input fingerprint too large")
    val out = new Array[Byte](size.toInt)
    Array.copy(prefix, 0, out, 0, prefix.length)
    var offset = prefix.length
    def byte(v: Int): Unit =
      out(offset) = v.toByte
      offset += 1
    def int(v: Int): Unit =
      byte(v >>> 24); byte(v >>> 16); byte(v >>> 8); byte(v)
    int(in.evStim.length)
    in.evStim.foreach(int)
    in.yPool match
      case None => byte(0)
      case Some(p) =>
        byte(1); int(p.rows); int(p.cols); int(p.data.length)
        p.data.foreach { d =>
          val bits = java.lang.Double.doubleToLongBits(d)
          int((bits >>> 32).toInt); int(bits.toInt)
        }
    Digests.sha256Hex(out)
