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

final case class SegmentLayout private (segments: Vector[TimeSegment]):
  require(segments.nonEmpty, "segment layout must be non-empty")

  def nTimepoints: Int = segments.last.endExclusive
  def runCount: Int = segments.map(_.runIndex).max + 1
  def runs: Vector[Int] = segments.map(_.runIndex).distinct.sorted

  def coverRows(rows: Int): Either[ArError, CoveredSegments] =
    CoveredSegments.fromLayout(this, rows)

object SegmentLayout:

  def fromSegments(segments: Vector[TimeSegment]): Either[ArError, SegmentLayout] =
    if segments.isEmpty then Left(ArError.EmptySegments)
    else
      var expectedStart = 0
      var previousRunIndex = -1
      var index = 0
      while index < segments.length do
        val segment = segments(index)
        if segment.start != expectedStart then
          return Left(ArError.SegmentGap(index, expectedStart, segment.start))
        if segment.runIndex != previousRunIndex && segment.runIndex != previousRunIndex + 1 then
          return Left(ArError.NonContiguousRunIndex(index, previousRunIndex, segment.runIndex))
        expectedStart = segment.endExclusive
        previousRunIndex = segment.runIndex
        index += 1
      Right(new SegmentLayout(segments))

  def unsafe(segments: Vector[TimeSegment]): SegmentLayout =
    fromSegments(segments).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class CoveredSegments private (layout: SegmentLayout, rows: Int):
  require(rows > 0, "covered rows must be positive")
  require(layout.nTimepoints == rows, "segment layout must cover exactly the requested rows")

  def segments: Vector[TimeSegment] = layout.segments
  def nTimepoints: Int = rows
  def runCount: Int = layout.runCount
  def runs: Vector[Int] = layout.runs

  def validateRows(candidateRows: Int): Either[ArError, Unit] =
    if candidateRows <= 0 then Left(ArError.NonPositiveRows(candidateRows))
    else if candidateRows == rows then Right(())
    else Left(ArError.SegmentCoverageMismatch(rows, candidateRows))

object CoveredSegments:

  def fromSegments(segments: Vector[TimeSegment], rows: Int): Either[ArError, CoveredSegments] =
    SegmentLayout.fromSegments(segments).flatMap(fromLayout(_, rows))

  def fromLayout(layout: SegmentLayout, rows: Int): Either[ArError, CoveredSegments] =
    if rows <= 0 then Left(ArError.NonPositiveRows(rows))
    else if layout.nTimepoints > rows then
      val segment = layout.segments.find(_.endExclusive > rows).getOrElse(layout.segments.last)
      Left(ArError.SegmentOutOfBounds(segment, rows))
    else if layout.nTimepoints != rows then Left(ArError.SegmentCoverageMismatch(layout.nTimepoints, rows))
    else Right(new CoveredSegments(layout, rows))

  def unsafe(segments: Vector[TimeSegment], rows: Int): CoveredSegments =
    fromSegments(segments, rows).fold(error => throw new IllegalArgumentException(error.message), identity)

/** Separates the rows retained for whitening from the rows allowed to estimate
  * the noise model. Whitening must preserve the complete matrix row axis;
  * censored rows may remain on that axis while being absent from
  * `estimationSegments`.
  */
final case class NoiseEstimationLayout private (
    coveredSegments: CoveredSegments,
    estimationSegments: Vector[TimeSegment],
    excludedRows: Vector[Int]
):
  def whiteningSegments: Vector[TimeSegment] = coveredSegments.segments
  def rows: Int = coveredSegments.nTimepoints
  def retainedRows: Int = estimationSegments.map(_.length).sum
  def runCount: Int = coveredSegments.runCount

  def segmentsForRun(runIndex: Int): Vector[TimeSegment] =
    estimationSegments.filter(_.runIndex == runIndex)

object NoiseEstimationLayout:

  def allRows(
      whiteningSegments: Vector[TimeSegment],
      rows: Int
  ): Either[ArError, NoiseEstimationLayout] =
    excludingRows(whiteningSegments, rows, Set.empty)

  def excludingRows(
      whiteningSegments: Vector[TimeSegment],
      rows: Int,
      excludedRows: Set[Int]
  ): Either[ArError, NoiseEstimationLayout] =
    CoveredSegments.fromSegments(whiteningSegments, rows).flatMap { covered =>
      excludedRows.find(row => row < 0 || row >= rows) match
        case Some(row) => Left(ArError.ExcludedRowOutOfBounds(row, rows))
        case None =>
          val estimation = Vector.newBuilder[TimeSegment]
          covered.segments.foreach { segment =>
            var retainedStart = -1
            var row = segment.start
            while row < segment.endExclusive do
              if excludedRows.contains(row) then
                if retainedStart >= 0 then
                  estimation += TimeSegment(retainedStart, row, segment.runIndex)
                  retainedStart = -1
              else if retainedStart < 0 then
                retainedStart = row
              row += 1
            if retainedStart >= 0 then
              estimation += TimeSegment(retainedStart, segment.endExclusive, segment.runIndex)
          }
          Right(new NoiseEstimationLayout(covered, estimation.result(), excludedRows.toVector.sorted))
    }

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

  def fromRunLabels[A](labels: IndexedSeq[A]): Either[ArError, Vector[TimeSegment]] =
    if labels.isEmpty then Left(ArError.EmptySegments)
    else
      val out = Vector.newBuilder[TimeSegment]
      val firstRows = scala.collection.mutable.HashMap.empty[A, Int]
      firstRows += labels.head -> 0
      var start = 0
      var run = 0
      var row = 1
      while row < labels.length do
        if labels(row) != labels(row - 1) then
          firstRows.get(labels(row)) match
            case Some(firstRow) =>
              return Left(ArError.NonContiguousRunLabel(firstRow, row))
            case None =>
              out += TimeSegment(start, row, run)
              firstRows += labels(row) -> row
              start = row
              run += 1
        row += 1
      out += TimeSegment(start, labels.length, run)
      Right(out.result())

  def unsafeFromRunLabels[A](labels: IndexedSeq[A]): Vector[TimeSegment] =
    fromRunLabels(labels).fold(error => throw new IllegalArgumentException(error.message), identity)

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
    CoveredSegments.fromSegments(segments, rows).map(_ => ())
