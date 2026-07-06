package scalafim.latent

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
