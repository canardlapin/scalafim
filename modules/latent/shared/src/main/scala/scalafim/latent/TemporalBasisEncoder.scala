package scalafim.latent

import scalafim.linalg.{DoubleMatrix, DoubleVector}
import scalafim.linalg.GramProjection

object TemporalBasisEncoder:
  def encodeProvided(
      data: DoubleMatrix,
      basis: DoubleMatrix,
      center: Boolean = false,
      ridge: Double = 0.0,
      sourceDomain: DomainId = DomainId.unsafe("latent.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("latent.samples"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, ExplicitLatentResponse] =
    encode(
      data = data,
      basis = basis,
      center = center,
      ridge = ridge,
      sourceDomain = sourceDomain,
      targetDomain = targetDomain,
      label = label,
      metadata = metadata
    )

  def encodeDct(
      data: DoubleMatrix,
      components: Int,
      norm: DctNorm = DctNorm.Ortho,
      center: Boolean = false,
      ridge: Double = 0.0,
      sourceDomain: DomainId = DomainId.unsafe("latent.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("latent.samples"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, ExplicitLatentResponse] =
    DctBasis.build(data.rows, components, norm).flatMap { basis =>
      encode(
        data = data,
        basis = basis,
        center = center,
        ridge = ridge,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        label = if label.nonEmpty then label else s"DCT(${data.rows},$components,${norm.metadataValue})",
        metadata = dctMetadata(data.rows, components, norm, metadata)
      )
    }

  def encodeFullDct(
      data: DoubleMatrix,
      norm: DctNorm = DctNorm.Ortho,
      center: Boolean = false,
      ridge: Double = 0.0,
      sourceDomain: DomainId = DomainId.unsafe("latent.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("latent.samples"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, ExplicitLatentResponse] =
    encodeDct(
      data = data,
      components = data.rows,
      norm = norm,
      center = center,
      ridge = ridge,
      sourceDomain = sourceDomain,
      targetDomain = targetDomain,
      label = label,
      metadata = metadata
    )

  def encode(
      data: DoubleMatrix,
      basis: DoubleMatrix,
      center: Boolean = false,
      ridge: Double = 0.0,
      sourceDomain: DomainId = DomainId.unsafe("latent.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("latent.samples"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, ExplicitLatentResponse] =
    if data.rows != basis.rows then
      Left(LatentError.DimensionMismatch("data rows", basis.rows, data.rows))
    else if ridge < 0.0 || !ridge.isFinite then
      Left(LatentError.InvalidParameter("ridge", ridge))
    else
      firstNonFinite("data", data).orElse(firstNonFinite("basis", basis)) match
        case Some(error) =>
          Left(error)
        case None =>
          val centered =
            if center then centerColumns(data)
            else CenteredData(data, None)
          projectOntoBasis(centered.data, basis, ridge).flatMap { loadings =>
            ExplicitLatentResponse(
              basis = basis,
              loadings = loadings,
              offset = centered.offset,
              sourceDomain = sourceDomain,
              targetDomain = targetDomain,
              label = label,
              metadata = metadata
            )
          }

  def projectOntoBasis(
      data: DoubleMatrix,
      basis: DoubleMatrix,
      ridge: Double = 0.0
  ): Either[LatentError, DoubleMatrix] =
    if data.rows != basis.rows then
      Left(LatentError.DimensionMismatch("data rows", basis.rows, data.rows))
    else if ridge < 0.0 || !ridge.isFinite then
      Left(LatentError.InvalidParameter("ridge", ridge))
    else
      firstNonFinite("data", data).orElse(firstNonFinite("basis", basis)) match
        case Some(error) =>
          Left(error)
        case None =>
          GramProjection
            .coefficients(data, basis, ridge = ridge)
            .left
            .map(err => LatentError.ProjectionFailed(err.message))
            .map(_.transpose)

  private final case class CenteredData(data: DoubleMatrix, offset: Option[DoubleVector])

  private def dctMetadata(
      timepoints: Int,
      components: Int,
      norm: DctNorm,
      metadata: Map[String, String]
  ): Map[String, String] =
    metadata ++ Map(
      "family" -> "time_dct",
      "basis" -> "dct",
      "timepoints" -> timepoints.toString,
      "components" -> components.toString,
      "norm" -> norm.metadataValue
    )

  private def centerColumns(data: DoubleMatrix): CenteredData =
    val means = new Array[Double](data.cols)
    var col = 0
    while col < data.cols do
      var sum = 0.0
      var row = 0
      while row < data.rows do
        sum += data.dataArray(row * data.cols + col)
        row += 1
      means(col) = sum / data.rows.toDouble
      col += 1

    val centered = data.copyData
    var row = 0
    while row < data.rows do
      col = 0
      while col < data.cols do
        centered(row * data.cols + col) -= means(col)
        col += 1
      row += 1

    CenteredData(DoubleMatrix.unsafe(data.rows, data.cols, centered), Some(DoubleVector.unsafe(means)))

  private def firstNonFinite(label: String, matrix: DoubleMatrix): Option[LatentError] =
    var i = 0
    var error = Option.empty[LatentError]
    while i < matrix.dataArray.length && error.isEmpty do
      val value = matrix.dataArray(i)
      if !value.isFinite then error = Some(LatentError.NonFiniteValue(label, i, value))
      i += 1
    error
