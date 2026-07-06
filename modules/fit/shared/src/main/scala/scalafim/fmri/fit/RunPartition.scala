package scalafim.fmri.fit

import scalafim.fmri.hrf.design.SamplingFrame

final case class RunPartition(
    runIndex: Int,
    rowIndices: Vector[Int],
    timepoints: Vector[Int]
):
  require(runIndex >= 0, "run index must be non-negative")
  require(rowIndices.nonEmpty, "run partition must contain rows")
  require(rowIndices.length == timepoints.length, "run partition rows and timepoints must align")

object RunPartition:
  def fromSamplingFrame(
      samplingFrame: SamplingFrame,
      selectedTimepoints: IndexedSeq[Int]
  ): Vector[RunPartition] =
    require(selectedTimepoints.nonEmpty, "selected timepoints must be non-empty")

    val starts = new Array[Int](samplingFrame.blockLens.length)
    var offset = 0
    var run = 0
    while run < samplingFrame.blockLens.length do
      starts(run) = offset
      offset += samplingFrame.blockLens(run)
      run += 1

    val out = Vector.newBuilder[RunPartition]
    run = 0
    while run < samplingFrame.blockLens.length do
      val start = starts(run)
      val end = start + samplingFrame.blockLens(run)
      val rows = Vector.newBuilder[Int]
      val timepoints = Vector.newBuilder[Int]
      var selectedRow = 0
      while selectedRow < selectedTimepoints.length do
        val timepoint = selectedTimepoints(selectedRow)
        if timepoint >= start && timepoint < end then
          rows += selectedRow
          timepoints += timepoint
        selectedRow += 1

      val rowVector = rows.result()
      if rowVector.nonEmpty then
        out += RunPartition(runIndex = run, rowIndices = rowVector, timepoints = timepoints.result())
      run += 1

    out.result()
