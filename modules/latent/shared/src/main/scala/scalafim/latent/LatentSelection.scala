package scalafim.latent

enum LatentAxis(val label: String):
  case Timepoint extends LatentAxis("timepoint")
  case Sample extends LatentAxis("sample")

opaque type TimepointIndex = Int

object TimepointIndex:
  def apply(value: Int): Either[LatentError, TimepointIndex] =
    if value < 0 then Left(LatentError.NegativeIndex(LatentAxis.Timepoint.label, value))
    else Right(value)

  def unsafe(value: Int): TimepointIndex =
    apply(value).fold(error => throw IllegalArgumentException(error.message), identity)

  extension (index: TimepointIndex)
    def value: Int = index

opaque type SampleIndex = Int

object SampleIndex:
  def apply(value: Int): Either[LatentError, SampleIndex] =
    if value < 0 then Left(LatentError.NegativeIndex(LatentAxis.Sample.label, value))
    else Right(value)

  def unsafe(value: Int): SampleIndex =
    apply(value).fold(error => throw IllegalArgumentException(error.message), identity)

  extension (index: SampleIndex)
    def value: Int = index

final case class TypedLatentSelection(
    timepoints: Option[IndexedSeq[TimepointIndex]] = None,
    samples: Option[IndexedSeq[SampleIndex]] = None
):
  def toLatentSelection: LatentSelection =
    LatentSelection(
      timepoints = timepoints.map(_.map(_.value)),
      samples = samples.map(_.map(_.value))
    )

object TypedLatentSelection:
  val All: TypedLatentSelection =
    TypedLatentSelection()

  def checked(
      timepoints: Option[IndexedSeq[Int]] = None,
      samples: Option[IndexedSeq[Int]] = None
  ): Either[LatentError, TypedLatentSelection] =
    for
      t <- traverse(timepoints.getOrElse(Vector.empty))(TimepointIndex.apply)
      s <- traverse(samples.getOrElse(Vector.empty))(SampleIndex.apply)
    yield TypedLatentSelection(
      timepoints = timepoints.map(_ => t),
      samples = samples.map(_ => s)
    )

final case class ResolvedLatentSelection(
    timepoints: IndexedSeq[Int],
    samples: IndexedSeq[Int]
):
  require(timepoints.nonEmpty, "timepoint selection must be non-empty")
  require(samples.nonEmpty, "sample selection must be non-empty")

final case class LatentSelection(
    timepoints: Option[IndexedSeq[Int]] = None,
    samples: Option[IndexedSeq[Int]] = None
):
  def resolve(timepointCount: Int, sampleCount: Int): Either[LatentError, ResolvedLatentSelection] =
    for
      t <- LatentSelection.resolveAxis("timepoint", timepointCount, timepoints)
      s <- LatentSelection.resolveAxis("sample", sampleCount, samples)
    yield ResolvedLatentSelection(t, s)

object LatentSelection:
  val All: LatentSelection =
    LatentSelection()

  def fromTyped(selection: TypedLatentSelection): LatentSelection =
    selection.toLatentSelection

  private def resolveAxis(
      axis: String,
      count: Int,
      requested: Option[IndexedSeq[Int]]
  ): Either[LatentError, IndexedSeq[Int]] =
    if count <= 0 then Left(LatentError.NonPositiveDimension(axis, count))
    else
      requested match
        case None =>
          Right(0 until count)
        case Some(indices) =>
          if indices.isEmpty then Left(LatentError.EmptySelection(axis))
          else
            val seen = Array.fill(count)(false)
            var i = 0
            var error = Option.empty[LatentError]
            while i < indices.length && error.isEmpty do
              val index = indices(i)
              if index < 0 || index >= count then
                error = Some(LatentError.IndexOutOfBounds(axis, index, count))
              else if seen(index) then
                error = Some(LatentError.DuplicateSelection(axis, index))
              else
                seen(index) = true
              i += 1
            error match
              case Some(err) => Left(err)
              case None      => Right(indices)

private def traverse[A, B](values: Iterable[A])(f: A => Either[LatentError, B]): Either[LatentError, IndexedSeq[B]] =
  val out = Vector.newBuilder[B]
  val it = values.iterator
  var error = Option.empty[LatentError]
  while it.hasNext && error.isEmpty do
    f(it.next()) match
      case Right(value) => out += value
      case Left(err)    => error = Some(err)
  error match
    case Some(err) => Left(err)
    case None      => Right(out.result())
