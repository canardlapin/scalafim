package scalafim.response

enum SelectionAxes:
  case Time
  case Samples
  case TimeAndSamples

  def label: String =
    this match
      case Time =>
        "time"
      case Samples =>
        "sample"
      case TimeAndSamples =>
        "time-and-sample"

final class ResolvedResponseSelection private (
    val schema: ResponseSchemaId,
    val timepoints: OrderedIndices[TimeAxis],
    val samples: OrderedIndices[SampleAxis]
):
  def rows: Int =
    timepoints.size

  def columns: Int =
    samples.size

  def selectedAxes: SelectionAxes =
    val selectedTime = timepoints.size != timepoints.domainSize
    val selectedSamples = samples.size != samples.domainSize
    if selectedTime && !selectedSamples then SelectionAxes.Time
    else if !selectedTime && selectedSamples then SelectionAxes.Samples
    else SelectionAxes.TimeAndSamples

  override def equals(other: Any): Boolean =
    other match
      case that: ResolvedResponseSelection =>
        schema == that.schema &&
          timepoints == that.timepoints &&
          samples == that.samples
      case _ =>
        false

  override def hashCode(): Int =
    31 * (31 * schema.value.hashCode + timepoints.hashCode) + samples.hashCode

  override def toString: String =
    s"ResolvedResponseSelection(${schema.value},$timepoints,$samples)"

object ResolvedResponseSelection:
  def make(
      schema: ResponseSchema,
      timepoints: OrderedIndices[TimeAxis],
      samples: OrderedIndices[SampleAxis]
  ): Either[SelectionError, ResolvedResponseSelection] =
    if timepoints.domain != schema.time.id then
      Left(SelectionError.TimeDomainMismatch(schema.time.id, timepoints.domain))
    else if samples.domain != schema.samples.id then
      Left(SelectionError.SampleDomainMismatch(schema.samples.id, samples.domain))
    else if timepoints.domainSize != schema.time.count then
      Left(
        SelectionError.AxisCardinalityMismatch(
          SelectionAxes.Time,
          schema.time.count,
          timepoints.domainSize
        )
      )
    else if samples.domainSize != schema.samples.count then
      Left(
        SelectionError.AxisCardinalityMismatch(
          SelectionAxes.Samples,
          schema.samples.count,
          samples.domainSize
        )
      )
    else
      Right(new ResolvedResponseSelection(schema.id, timepoints, samples))

  def all(
      schema: ResponseSchema
  ): Either[IndexError | SelectionError, ResolvedResponseSelection] =
    for
      timepoints <- OrderedIndices.all(schema.time.id, schema.time.count)
      samples <- OrderedIndices.all(schema.samples.id, schema.samples.count)
      selection <- make(schema, timepoints, samples)
    yield selection

  def validateFor(
      schema: ResponseSchema,
      selection: ResolvedResponseSelection
  ): Either[SelectionError, Unit] =
    if selection.schema != schema.id then
      Left(SelectionError.SchemaMismatch(schema.id, selection.schema))
    else if selection.timepoints.domain != schema.time.id then
      Left(SelectionError.TimeDomainMismatch(schema.time.id, selection.timepoints.domain))
    else if selection.samples.domain != schema.samples.id then
      Left(SelectionError.SampleDomainMismatch(schema.samples.id, selection.samples.domain))
    else if selection.timepoints.domainSize != schema.time.count then
      Left(
        SelectionError.AxisCardinalityMismatch(
          SelectionAxes.Time,
          schema.time.count,
          selection.timepoints.domainSize
        )
      )
    else if selection.samples.domainSize != schema.samples.count then
      Left(
        SelectionError.AxisCardinalityMismatch(
          SelectionAxes.Samples,
          schema.samples.count,
          selection.samples.domainSize
        )
      )
    else
      Right(())
