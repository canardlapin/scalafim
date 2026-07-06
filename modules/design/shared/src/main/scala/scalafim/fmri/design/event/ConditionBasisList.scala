package scalafim.fmri.design.event

import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

import scala.collection.immutable.VectorMap

object ConditionBasisList:

  def list(
      term: EventTerm,
      hrf: Hrf,
      samplingFrame: SamplingFrame,
      precision: Seconds = 0.3.s,
      dropEmpty: Boolean = true,
      summate: Boolean = true
  ): VectorMap[String, Mat] =
    val dm = term.designMatrix(dropEmpty = dropEmpty)
    val condTags = dm.conditionTags
    val nConds = condTags.length
    val nb = hrf.nbasis

    val conv = term.convolve(hrf, samplingFrame, precision = precision, dropEmpty = dropEmpty, summate = summate)
    val totalRows = conv.data.rows
    val totalCols = conv.data.cols

    if nConds == 0 || nb == 0 || totalRows == 0 then VectorMap.empty
    else
      require(totalCols == nConds * nb, "internal: convolved matrix col mismatch with condTags*nbasis")
      val out = Vector.newBuilder[(String, Mat)]
      var cond = 0
      while cond < nConds do
        val data = new Array[Double](totalRows * nb)
        var r = 0
        while r < totalRows do
          var b = 0
          while b < nb do
            val inCol = b * nConds + cond
            data(r * nb + b) = conv.data.data(r * totalCols + inCol)
            b += 1
          r += 1
        out += (condTags(cond) -> Mat.unsafe(totalRows, nb, data))
        cond += 1
      VectorMap.from(out.result())

  def matrix(
      term: EventTerm,
      hrf: Hrf,
      samplingFrame: SamplingFrame,
      precision: Seconds = 0.3.s,
      dropEmpty: Boolean = true,
      summate: Boolean = true
  ): Mat =
    term.convolve(hrf, samplingFrame, precision = precision, dropEmpty = dropEmpty, summate = summate).data
