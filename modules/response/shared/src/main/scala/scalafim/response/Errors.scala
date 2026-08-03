package scalafim.response

sealed trait ResponseFailure:
  def message: String

enum IdentityError extends ResponseFailure:
  case Empty(label: String)
  case Invalid(label: String, value: String)

  def message: String =
    this match
      case Empty(label) =>
        s"$label must not be empty"
      case Invalid(label, value) =>
        s"$label contains unsupported characters: '$value'"

enum IndexError extends ResponseFailure:
  case Negative(value: Int)
  case Empty(domain: String)
  case InvalidDomainSize(size: Int)
  case OutOfBounds(value: Int, size: Int)
  case Duplicate(value: Int)

  def message: String =
    this match
      case Negative(value) =>
        s"axis index must be non-negative; got $value"
      case Empty(domain) =>
        s"ordered selection for domain '$domain' must not be empty"
      case InvalidDomainSize(size) =>
        s"axis domain size must be positive; got $size"
      case OutOfBounds(value, size) =>
        s"axis index $value is outside [0, $size)"
      case Duplicate(value) =>
        s"axis selection contains duplicate index $value"

enum SchemaError extends ResponseFailure:
  case InvalidCount(label: String, count: Int)
  case InvalidCoordinate(label: String, value: Double)
  case NonIncreasingCoordinate(previous: Double, current: Double)
  case CardinalityMismatch(label: String, expected: Int, actual: Int)

  def message: String =
    this match
      case InvalidCount(label, count) =>
        s"$label count must be positive; got $count"
      case InvalidCoordinate(label, value) =>
        s"$label coordinate must be finite and valid; got $value"
      case NonIncreasingCoordinate(previous, current) =>
        s"explicit time coordinates must increase strictly; found $previous then $current"
      case CardinalityMismatch(label, expected, actual) =>
        s"$label cardinality mismatch: expected $expected but got $actual"

enum SelectionError extends ResponseFailure:
  case SchemaMismatch(expected: ResponseSchemaId, actual: ResponseSchemaId)
  case TimeDomainMismatch(expected: DomainId[TimeAxis], actual: DomainId[TimeAxis])
  case SampleDomainMismatch(expected: DomainId[SampleAxis], actual: DomainId[SampleAxis])
  case AxisCardinalityMismatch(axes: SelectionAxes, expected: Int, actual: Int)

  def message: String =
    this match
      case SchemaMismatch(expected, actual) =>
        s"selection schema '${actual.value}' does not match '${expected.value}'"
      case TimeDomainMismatch(expected, actual) =>
        s"time domain '${actual.value}' does not match '${expected.value}'"
      case SampleDomainMismatch(expected, actual) =>
        s"sample domain '${actual.value}' does not match '${expected.value}'"
      case AxisCardinalityMismatch(axes, expected, actual) =>
        s"${axes.label} domain cardinality mismatch: expected $expected but got $actual"

enum ResponseShapeError extends ResponseFailure:
  case ValueCountOverflow(rows: Int, columns: Int)
  case ValueCountMismatch(expected: Int, actual: Int)
  case NonFiniteValue(index: Int, value: Double)

  def message: String =
    this match
      case ValueCountOverflow(rows, columns) =>
        s"response block shape $rows by $columns exceeds the supported primitive array size"
      case ValueCountMismatch(expected, actual) =>
        s"response block requires $expected values but received $actual"
      case NonFiniteValue(index, value) =>
        s"response value $index must be finite; got $value"

enum ConsistencyError extends ResponseFailure:
  case NegativeUlps(value: Int)
  case InvalidTolerance(label: String, value: Double)

  def message: String =
    this match
      case NegativeUlps(value) =>
        s"maximum ULP distance must be non-negative; got $value"
      case InvalidTolerance(label, value) =>
        s"$label tolerance must be finite and non-negative; got $value"

enum ProvenanceError extends ResponseFailure:
  case Empty
  case DuplicateNode(id: ProvenanceId)
  case MissingParent(node: ProvenanceId, parent: ProvenanceId)
  case InvalidRoot(id: ProvenanceId)
  case DuplicateRoot(id: ProvenanceId)

  def message: String =
    this match
      case Empty =>
        "provenance must contain at least one node and root"
      case DuplicateNode(id) =>
        s"provenance contains duplicate node '${id.value}'"
      case MissingParent(node, parent) =>
        s"provenance node '${node.value}' references unavailable parent '${parent.value}'"
      case InvalidRoot(id) =>
        s"provenance root '${id.value}' is not a node"
      case DuplicateRoot(id) =>
        s"provenance contains duplicate root '${id.value}'"

enum ReceiptConformanceError extends ResponseFailure:
  case EmptyCapabilities
  case DuplicateCapability(axes: SelectionAxes)
  case MissingEvidence(axes: SelectionAxes)
  case DuplicateEvidence(axes: SelectionAxes)
  case LocalityMismatch(
      axes: SelectionAxes,
      declared: PhysicalLocality,
      observed: PhysicalLocality
  )
  case InvalidEvidence(axes: SelectionAxes, detail: String)
  case UnknownPayload(id: PayloadId)
  case UnknownObject(id: ObjectId)
  case UnknownChunk(payload: PayloadId, chunk: ChunkId)
  case TouchOutsideCover(axes: SelectionAxes, detail: String)
  case InvalidByteCount(label: String, value: Long)

  def message: String =
    this match
      case EmptyCapabilities =>
        "read capabilities must contain at least one axis-locality claim"
      case DuplicateCapability(axes) =>
        s"read capabilities contain duplicate '${axes.label}' claims"
      case MissingEvidence(axes) =>
        s"read receipt has no '${axes.label}' physical evidence"
      case DuplicateEvidence(axes) =>
        s"read receipt has duplicate '${axes.label}' physical evidence"
      case LocalityMismatch(axes, declared, observed) =>
        s"${axes.label} locality declared $declared but observed $observed"
      case InvalidEvidence(axes, detail) =>
        s"invalid ${axes.label} physical evidence: $detail"
      case UnknownPayload(id) =>
        s"receipt references unknown payload '${id.value}'"
      case UnknownObject(id) =>
        s"receipt references unknown object '${id.value}'"
      case UnknownChunk(payload, chunk) =>
        s"receipt references unknown chunk '${chunk.value}' in payload '${payload.value}'"
      case TouchOutsideCover(axes, detail) =>
        s"${axes.label} receipt touched data outside its declared cover: $detail"
      case InvalidByteCount(label, value) =>
        s"$label byte count must be non-negative; got $value"

enum ReadPlanningError extends ResponseFailure:
  case InvalidSelection(error: SelectionError)
  case DuplicateSelectionUnsupported(axis: SelectionAxes, value: Int)
  case SourceLimitExceeded(detail: String)
  case Unsupported(detail: String)

  def message: String =
    this match
      case InvalidSelection(error) =>
        error.message
      case DuplicateSelectionUnsupported(axis, value) =>
        s"source does not support duplicate ${axis.label} index $value"
      case SourceLimitExceeded(detail) =>
        s"read planning limit exceeded: $detail"
      case Unsupported(detail) =>
        s"read plan is unsupported: $detail"

enum ReadResultMismatch extends ResponseFailure:
  case ReceiptSchema(expected: ResponseSchemaId, actual: ResponseSchemaId)
  case TimepointOrder(expected: Vector[Int], actual: Vector[Int])
  case SampleOrder(expected: Vector[Int], actual: Vector[Int])
  case BlockShape(
      expectedRows: Int,
      expectedColumns: Int,
      actualRows: Int,
      actualColumns: Int
  )

  def message: String =
    this match
      case ReceiptSchema(expected, actual) =>
        s"receipt schema '${actual.value}' does not match block schema '${expected.value}'"
      case TimepointOrder(expected, actual) =>
        s"receipt timepoint order $actual does not match block order $expected"
      case SampleOrder(expected, actual) =>
        s"receipt sample order $actual does not match block order $expected"
      case BlockShape(expectedRows, expectedColumns, actualRows, actualColumns) =>
        s"source returned ${actualRows}x$actualColumns, expected ${expectedRows}x$expectedColumns"

enum ReadError extends ResponseFailure:
  case Planning(error: ReadPlanningError)
  case SourceFailure(source: SourceId, detail: String)
  case InvalidBlock(error: ResponseShapeError)
  case InvalidProvenance(error: ProvenanceError)
  case InvalidReceipt(error: ReceiptConformanceError)
  case ResultMismatch(error: ReadResultMismatch)

  def message: String =
    this match
      case Planning(error) =>
        error.message
      case SourceFailure(source, detail) =>
        s"response source '${source.value}' failed: $detail"
      case InvalidBlock(error) =>
        error.message
      case InvalidProvenance(error) =>
        error.message
      case InvalidReceipt(error) =>
        error.message
      case ResultMismatch(error) =>
        error.message
