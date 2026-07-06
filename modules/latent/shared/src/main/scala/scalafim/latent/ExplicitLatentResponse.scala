package scalafim.latent

import scalafim.linalg.{DoubleMatrix, DoubleVector}

final class ExplicitLatentResponse private (
    val basis: DoubleMatrix,
    val loadings: DoubleMatrix,
    val offset: Option[DoubleVector],
    val sourceDomain: DomainId,
    val targetDomain: DomainId,
    val label: String,
    val metadata: Map[String, String]
) extends LatentResponse:

  override val shape: LatentShape =
    LatentShape(
      timepoints = basis.rows,
      samples = loadings.rows,
      coefficients = basis.cols
    )

  override def coefTime: DoubleMatrix =
    basis

  override def decodeCoefficients(coefficients: DoubleMatrix): Either[LatentError, DoubleMatrix] =
    if coefficients.rows != shape.coefficients then
      Left(LatentError.DimensionMismatch("coefficient rows", shape.coefficients, coefficients.rows))
    else
      val out = new Array[Double](shape.samples * coefficients.cols)
      var sample = 0
      while sample < shape.samples do
        var component = 0
        while component < shape.coefficients do
          val loading = loadings.dataArray(sample * loadings.cols + component)
          val coeffOffset = component * coefficients.cols
          val outOffset = sample * coefficients.cols
          var col = 0
          while col < coefficients.cols do
            out(outOffset + col) += loading * coefficients.dataArray(coeffOffset + col)
            col += 1
          component += 1
        sample += 1
      Right(DoubleMatrix.unsafe(shape.samples, coefficients.cols, out))

  override def reconstruct(selection: LatentSelection = LatentSelection.All): Either[LatentError, DoubleMatrix] =
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
            sum += basis.dataArray(time * basis.cols + component) *
              loadings.dataArray(sample * loadings.cols + component)
            component += 1
          out(outTime * resolved.samples.length + outSample) = sum
          outSample += 1
        outTime += 1
      DoubleMatrix.unsafe(resolved.timepoints.length, resolved.samples.length, out)
    }

object ExplicitLatentResponse:
  def apply(
      basis: DoubleMatrix,
      loadings: DoubleMatrix,
      offset: Option[DoubleVector] = None,
      sourceDomain: DomainId = DomainId.unsafe("latent.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("latent.samples"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, ExplicitLatentResponse] =
    validate(basis, loadings, offset).map { _ =>
      unsafe(basis, loadings, offset, sourceDomain, targetDomain, label, metadata)
    }

  private[scalafim] def unsafe(
      basis: DoubleMatrix,
      loadings: DoubleMatrix,
      offset: Option[DoubleVector],
      sourceDomain: DomainId,
      targetDomain: DomainId,
      label: String,
      metadata: Map[String, String]
  ): ExplicitLatentResponse =
    new ExplicitLatentResponse(basis, loadings, offset, sourceDomain, targetDomain, label, metadata)

  private def validate(
      basis: DoubleMatrix,
      loadings: DoubleMatrix,
      offset: Option[DoubleVector]
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

  private def firstNonFinite(label: String, matrix: DoubleMatrix): Option[LatentError] =
    var i = 0
    var error = Option.empty[LatentError]
    while i < matrix.dataArray.length && error.isEmpty do
      val value = matrix.dataArray(i)
      if !value.isFinite then error = Some(LatentError.NonFiniteValue(label, i, value))
      i += 1
    error

  private def firstNonFinite(label: String, vector: DoubleVector): Option[LatentError] =
    var i = 0
    var error = Option.empty[LatentError]
    while i < vector.length && error.isEmpty do
      val value = vector(i)
      if !value.isFinite then error = Some(LatentError.NonFiniteValue(label, i, value))
      i += 1
    error
