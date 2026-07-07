package scalafim.fmri.mvpa

final case class SampleAxis private (samples: Int):
  require(samples > 0, "sample axis must contain at least one sample")

  def indices: Vector[SampleIndex] =
    (0 until samples).map(SampleIndex.unsafe).toVector

object SampleAxis:
  def apply(samples: Int): Either[MvpaError, SampleAxis] =
    if samples <= 0 then Left(MvpaError.InvalidSampleAxis("sample axis must contain at least one sample"))
    else Right(new SampleAxis(samples))

  def unsafe(samples: Int): SampleAxis =
    apply(samples).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class ResponseContext private (
    axis: SampleAxis,
    response: Response
):
  def samples: Int =
    axis.samples

object ResponseContext:
  def apply(response: Response, axis: SampleAxis): Either[MvpaError, ResponseContext] =
    response.validate(axis.samples).map(valid => new ResponseContext(axis, valid))

  def unsafe(response: Response, axis: SampleAxis): ResponseContext =
    apply(response, axis).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class FoldedResponseContext private (
    responseContext: ResponseContext,
    folds: FoldPlan
):
  require(responseContext.samples == folds.samples, "fold sample count must match response sample axis")

  def axis: SampleAxis =
    responseContext.axis

  def response: Response =
    responseContext.response

  def samples: Int =
    responseContext.samples

object FoldedResponseContext:
  def apply(responseContext: ResponseContext, folds: FoldPlan): Either[MvpaError, FoldedResponseContext] =
    if folds.samples != responseContext.samples then
      Left(MvpaError.ResponseLengthMismatch(responseContext.samples, folds.samples))
    else
      Right(new FoldedResponseContext(responseContext, folds))

  def unsafe(responseContext: ResponseContext, folds: FoldPlan): FoldedResponseContext =
    apply(responseContext, folds).fold(error => throw new IllegalArgumentException(error.message), identity)
