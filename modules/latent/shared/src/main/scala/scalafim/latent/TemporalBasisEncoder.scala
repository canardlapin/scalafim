package scalafim.latent

import gale.linalg.{DMat, DVec}

object TemporalBasisEncoder:
  def encodeProvided(
      data: DMat,
      basis: DMat,
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
      data: DMat,
      components: Int,
      norm: DctNorm = DctNorm.Ortho,
      center: Boolean = false,
      ridge: Double = 0.0,
      sourceDomain: DomainId = DomainId.unsafe("latent.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("latent.samples"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, ExplicitLatentResponse] =
    for
      penalty <- RidgePenalty(ridge)
      spec <- DctSpec(data.rows, components, norm)
      response <- encodeDctSpec(
        data = data,
        spec = spec,
        center = center,
        ridge = penalty,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        label = label,
        metadata = metadata
      )
    yield response

  def encodeDctSpec(
      data: DMat,
      spec: DctSpec,
      center: Boolean = false,
      ridge: RidgePenalty = RidgePenalty.Zero,
      sourceDomain: DomainId = DomainId.unsafe("latent.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("latent.samples"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, ExplicitLatentResponse] =
    if data.rows != spec.timepoints then
      Left(LatentError.DimensionMismatch("DCT spec timepoints", spec.timepoints, data.rows))
    else
      DctBasis.build(spec).flatMap { basis =>
        encode(
          data = data,
          basis = basis,
          center = center,
          ridge = ridge.value,
          sourceDomain = sourceDomain,
          targetDomain = targetDomain,
          label = if label.nonEmpty then label else s"DCT(${spec.timepoints},${spec.components},${spec.norm.metadataValue})",
          metadata = dctMetadata(spec.timepoints, spec.components, spec.norm, center, ridge, metadata)
        )
      }

  def encodeHaar(
      data: DMat,
      components: Int,
      center: Boolean = false,
      ridge: Double = 0.0,
      sourceDomain: DomainId = DomainId.unsafe("latent.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("latent.samples"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, ExplicitLatentResponse] =
    for
      penalty <- RidgePenalty(ridge)
      spec <- HaarSpec(data.rows, components)
      response <- encodeHaarSpec(
        data = data,
        spec = spec,
        center = center,
        ridge = penalty,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        label = label,
        metadata = metadata
      )
    yield response

  def encodeHaarSpec(
      data: DMat,
      spec: HaarSpec,
      center: Boolean = false,
      ridge: RidgePenalty = RidgePenalty.Zero,
      sourceDomain: DomainId = DomainId.unsafe("latent.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("latent.samples"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, ExplicitLatentResponse] =
    if data.rows != spec.timepoints then
      Left(LatentError.DimensionMismatch("Haar spec timepoints", spec.timepoints, data.rows))
    else
      HaarBasis.build(spec).flatMap { basis =>
        encode(
          data = data,
          basis = basis,
          center = center,
          ridge = ridge.value,
          sourceDomain = sourceDomain,
          targetDomain = targetDomain,
          label = if label.nonEmpty then label else s"Haar(${spec.timepoints},${spec.components})",
          metadata = haarMetadata(spec.timepoints, spec.components, spec.levels, center, ridge, metadata)
        )
      }

  def encodeFullHaar(
      data: DMat,
      center: Boolean = false,
      ridge: Double = 0.0,
      sourceDomain: DomainId = DomainId.unsafe("latent.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("latent.samples"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, ExplicitLatentResponse] =
    encodeHaar(
      data = data,
      components = data.rows,
      center = center,
      ridge = ridge,
      sourceDomain = sourceDomain,
      targetDomain = targetDomain,
      label = label,
      metadata = metadata
    )

  def encodeFullDct(
      data: DMat,
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
      data: DMat,
      basis: DMat,
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
      data: DMat,
      basis: DMat,
      ridge: Double = 0.0
  ): Either[LatentError, DMat] =
    if data.rows != basis.rows then
      Left(LatentError.DimensionMismatch("data rows", basis.rows, data.rows))
    else if ridge < 0.0 || !ridge.isFinite then
      Left(LatentError.InvalidParameter("ridge", ridge))
    else
      firstNonFinite("data", data).orElse(firstNonFinite("basis", basis)) match
        case Some(error) =>
          Left(error)
        case None =>
          LatentNumerics
            .coefficients(data, basis, ridge = ridge)
            .left
            .map(err => LatentError.ProjectionFailed(err.message))
            .map(_.transpose)

  private final case class CenteredData(data: DMat, offset: Option[DVec])

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

  private def dctMetadata(
      timepoints: Int,
      components: Int,
      norm: DctNorm,
      center: Boolean,
      ridge: RidgePenalty,
      metadata: Map[String, String]
  ): Map[String, String] =
    dctMetadata(timepoints, components, norm, metadata) ++ Map(
      "center" -> center.toString,
      "ridge" -> ridge.metadataValue
    )

  private def haarMetadata(
      timepoints: Int,
      components: Int,
      levels: Int,
      center: Boolean,
      ridge: RidgePenalty,
      metadata: Map[String, String]
  ): Map[String, String] =
    metadata ++ Map(
      "family" -> "time_haar",
      "basis" -> "haar",
      "timepoints" -> timepoints.toString,
      "components" -> components.toString,
      "levels" -> levels.toString,
      "center" -> center.toString,
      "ridge" -> ridge.metadataValue
    )

  private def centerColumns(data: DMat): CenteredData =
    val means = new Array[Double](data.cols)
    var col = 0
    while col < data.cols do
      var sum = 0.0
      var row = 0
      while row < data.rows do
        sum += data(row, col)
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

    CenteredData(LatentNumerics.matrixFromRowMajor(data.rows, data.cols, centered), Some(LatentNumerics.vectorFromArray(means)))

  private def firstNonFinite(label: String, matrix: DMat): Option[LatentError] =
    val data = matrix.copyData
    var i = 0
    var error = Option.empty[LatentError]
    while i < data.length && error.isEmpty do
      val value = data(i)
      if !value.isFinite then error = Some(LatentError.NonFiniteValue(label, i, value))
      i += 1
    error
