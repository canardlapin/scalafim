package scalafim.latent

import gale.linalg.{DMat, DVec}

final class ExplicitLatentResponse private (
    val basis: DMat,
    val loadings: DMat,
    val offset: Option[DVec],
    val sourceDomain: DomainId,
    val targetDomain: DomainId,
    val latentLabel: LatentLabel,
    val typedMetadata: LatentMetadata
) extends LatentResponse:

  override val shape: LatentShape =
    LatentShape(
      timepoints = basis.rows,
      samples = loadings.rows,
      coefficients = basis.cols
    )

  override def coefTime: DMat =
    basis

  override def decodeSemantics: LatentDecodeSemantics =
    LatentDecodeSemantics.linear(offset = offset.nonEmpty)

  override def decodeCoefficients(coefficients: DMat): Either[LatentError, DMat] =
    if coefficients.rows != shape.coefficients then
      Left(LatentError.DimensionMismatch("coefficient rows", shape.coefficients, coefficients.rows))
    else
      val out = new Array[Double](shape.samples * coefficients.cols)
      var sample = 0
      while sample < shape.samples do
        var component = 0
        while component < shape.coefficients do
          val loading = loadings(sample, component)
          val outOffset = sample * coefficients.cols
          var col = 0
          while col < coefficients.cols do
            out(outOffset + col) += loading * coefficients(component, col)
            col += 1
          component += 1
        sample += 1
      Right(LatentNumerics.matrixFromRowMajor(shape.samples, coefficients.cols, out))

  override def reconstruct(selection: LatentSelection = LatentSelection.All): Either[LatentError, DMat] =
    selection.resolve(shape.timepoints, shape.samples).map { resolved =>
      val out = new Array[Double](resolved.timepoints.length * resolved.samples.length)
      var outTime = 0
      while outTime < resolved.timepoints.length do
        val time = resolved.timepoints(outTime)
        var outSample = 0
        while outSample < resolved.samples.length do
          val sample = resolved.samples(outSample)
          var sum = offset.fold(0.0)(_(sample))
          var component = 0
          while component < shape.coefficients do
            sum += basis(time, component) * loadings(sample, component)
            component += 1
          out(outTime * resolved.samples.length + outSample) = sum
          outSample += 1
        outTime += 1
      LatentNumerics.matrixFromRowMajor(resolved.timepoints.length, resolved.samples.length, out)
    }

object ExplicitLatentResponse:
  def apply(
      basis: DMat,
      loadings: DMat,
      offset: Option[DVec] = None,
      sourceDomain: DomainId = DomainId.unsafe("latent.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("latent.samples"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, ExplicitLatentResponse] =
    for
      _ <- validate(basis, loadings, offset)
      annotation <- LatentAnnotation(label, metadata)
    yield unsafe(basis, loadings, offset, sourceDomain, targetDomain, annotation.label, annotation.metadata)

  private[scalafim] def unsafe(
      basis: DMat,
      loadings: DMat,
      offset: Option[DVec],
      sourceDomain: DomainId,
      targetDomain: DomainId,
      label: LatentLabel,
      metadata: LatentMetadata
  ): ExplicitLatentResponse =
    new ExplicitLatentResponse(basis, loadings, offset, sourceDomain, targetDomain, label, metadata)

  private def validate(
      basis: DMat,
      loadings: DMat,
      offset: Option[DVec]
  ): Either[LatentError, Unit] =
    if basis.rows <= 0 then Left(LatentError.NonPositiveDimension("basis rows", basis.rows))
    else if basis.cols <= 0 then Left(LatentError.NonPositiveDimension("basis columns", basis.cols))
    else if loadings.rows <= 0 then Left(LatentError.NonPositiveDimension("loadings rows", loadings.rows))
    else if loadings.cols != basis.cols then Left(LatentError.DimensionMismatch("loading columns", basis.cols, loadings.cols))
    else
      offset match
        case Some(value) if value.length != loadings.rows =>
          Left(LatentError.DimensionMismatch("offset length", loadings.rows, value.length))
        case _ =>
          val offsetIssue =
            offset match
              case Some(value) => firstNonFinite("offset", value)
              case None        => None
          firstNonFinite("basis", basis)
            .orElse(firstNonFinite("loadings", loadings))
            .orElse(offsetIssue) match
            case Some(error) => Left(error)
            case None        => Right(())

  private def firstNonFinite(label: String, matrix: DMat): Option[LatentError] =
    val data = matrix.copyData
    var i = 0
    var error = Option.empty[LatentError]
    while i < data.length && error.isEmpty do
      val value = data(i)
      if !value.isFinite then error = Some(LatentError.NonFiniteValue(label, i, value))
      i += 1
    error

  private def firstNonFinite(label: String, vector: DVec): Option[LatentError] =
    var i = 0
    var error = Option.empty[LatentError]
    while i < vector.length && error.isEmpty do
      val value = vector(i)
      if !value.isFinite then error = Some(LatentError.NonFiniteValue(label, i, value))
      i += 1
    error
