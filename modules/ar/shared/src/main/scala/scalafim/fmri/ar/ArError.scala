package scalafim.fmri.ar

enum ArError:
  case RowMismatch(leftRows: Int, rightRows: Int)
  case SegmentOutOfBounds(segment: TimeSegment, rows: Int)
  case SegmentCoverage(detail: String)
  case CoefficientMismatch(detail: String)
  case InvalidExactFirstAr1(phi: Double)

  def message: String =
    this match
      case RowMismatch(leftRows, rightRows) =>
        s"row mismatch: $leftRows vs $rightRows"
      case SegmentOutOfBounds(segment, rows) =>
        s"segment ${segment.start}:${segment.endExclusive} is out of bounds for $rows rows"
      case SegmentCoverage(detail) =>
        s"invalid segment coverage: $detail"
      case CoefficientMismatch(detail) =>
        s"invalid AR coefficients: $detail"
      case InvalidExactFirstAr1(phi) =>
        s"exact AR(1) first-row scaling requires abs(phi) < 1, got $phi"
