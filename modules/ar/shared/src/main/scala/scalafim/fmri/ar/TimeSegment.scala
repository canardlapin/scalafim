package scalafim.fmri.ar

final case class TimeSegment(
    start: Int,
    endExclusive: Int,
    runIndex: Int
):
  require(start >= 0, "segment start must be non-negative")
  require(endExclusive > start, "segment end must be greater than start")
  require(runIndex >= 0, "run index must be non-negative")

  def length: Int = endExclusive - start
  def contains(row: Int): Boolean = row >= start && row < endExclusive

object TimeSegments:

  def continuous(length: Int): Vector[TimeSegment] =
    require(length > 0, "length must be positive")
    Vector(TimeSegment(0, length, runIndex = 0))

  def fromRunLengths(lengths: Seq[Int]): Vector[TimeSegment] =
    require(lengths.nonEmpty, "run lengths must be non-empty")
    require(lengths.forall(_ > 0), "run lengths must be positive")
    val out = Vector.newBuilder[TimeSegment]
    var start = 0
    var run = 0
    lengths.foreach { length =>
      val end = start + length
      out += TimeSegment(start, end, run)
      start = end
      run += 1
    }
    out.result()

  def fromRunLabels(labels: IndexedSeq[Int]): Vector[TimeSegment] =
    require(labels.nonEmpty, "run labels must be non-empty")
    val out = Vector.newBuilder[TimeSegment]
    var start = 0
    var run = 0
    var row = 1
    while row < labels.length do
      if labels(row) != labels(row - 1) then
        out += TimeSegment(start, row, run)
        start = row
        run += 1
      row += 1
    out += TimeSegment(start, labels.length, run)
    out.result()

  def withCensorResets(segments: Vector[TimeSegment], censoredTimepoints: Set[Int]): Vector[TimeSegment] =
    require(segments.nonEmpty, "segments must be non-empty")
    if censoredTimepoints.isEmpty then segments
    else
      val out = Vector.newBuilder[TimeSegment]
      segments.foreach { segment =>
        val resetStarts =
          censoredTimepoints.iterator
            .filter(c => c >= segment.start && c < segment.endExclusive - 1)
            .map(_ + 1)
            .toVector
            .sorted

        var start = segment.start
        resetStarts.foreach { reset =>
          out += TimeSegment(start, reset, segment.runIndex)
          start = reset
        }
        out += TimeSegment(start, segment.endExclusive, segment.runIndex)
      }
      out.result()

  def validateCoverage(segments: Vector[TimeSegment], rows: Int): Either[ArError, Unit] =
    if rows <= 0 then Left(ArError.SegmentCoverage("row count must be positive"))
    else if segments.isEmpty then Left(ArError.SegmentCoverage("segments must be non-empty"))
    else
      var expectedStart = 0
      var i = 0
      while i < segments.length do
        val segment = segments(i)
        if segment.start != expectedStart then
          return Left(ArError.SegmentCoverage(s"expected segment $i to start at $expectedStart, got ${segment.start}"))
        if segment.endExclusive > rows then
          return Left(ArError.SegmentOutOfBounds(segment, rows))
        expectedStart = segment.endExclusive
        i += 1

      if expectedStart == rows then Right(())
      else Left(ArError.SegmentCoverage(s"segments end at $expectedStart but matrix has $rows rows"))
