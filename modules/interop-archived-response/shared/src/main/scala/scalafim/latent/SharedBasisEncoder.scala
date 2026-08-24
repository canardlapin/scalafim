package scalafim.latent

import scalafim.archive.lna.{SharedBasisArtifact, SharedBasisId, SharedBasisLocator}
import gale.linalg.{DMat, DVec}

final case class SharedBasisEncoding(
    response: ExplicitLatentResponse,
    basis: SharedBasisArtifact,
    basisId: SharedBasisId,
    locator: Option[SharedBasisLocator]
):
  def coefficients: DMat = response.coefTime
  def offset: Option[DVec] = response.offset

object SharedBasisEncoder:
  def encode(
      data: DMat,
      basis: SharedBasisArtifact,
      basisId: SharedBasisId,
      locator: Option[SharedBasisLocator] = None,
      center: Boolean = true,
      ridge: Double = 0.0,
      sourceDomain: DomainId = DomainId.unsafe("shared_basis.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("voxels"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, SharedBasisEncoding] =
    if data.rows <= 0 then Left(LatentError.NonPositiveDimension("data rows", data.rows))
    else if data.cols <= 0 then Left(LatentError.NonPositiveDimension("data columns", data.cols))
    else if ridge < 0.0 || !ridge.isFinite then Left(LatentError.InvalidParameter("ridge", ridge))
    else if basis.nVoxels != data.cols then
      Left(LatentError.DimensionMismatch("shared basis voxels", data.cols, basis.nVoxels))
    else
      firstNonFinite("data", data) match
        case Some(error) =>
          Left(error)
        case None =>
          SharedBasisArtifact
            .validateFinite(basis)
            .left
            .map(err => LatentError.ProjectionFailed(err.message))
            .flatMap { validBasis =>
              val loadings = validBasis.loadings
              val offset = Option.when(center)(columnMeans(data))
              for
                coefficients <- project(data, loadings, offset, ridge)
                response <- ExplicitLatentResponse(
                  basis = coefficients,
                  loadings = loadings,
                  offset = offset,
                  sourceDomain = sourceDomain,
                  targetDomain = targetDomain,
                  label = if label.nonEmpty then label else s"shared-basis:${basisId.value}",
                  metadata = sharedBasisMetadata(validBasis, basisId, center, ridge, metadata)
                )
              yield SharedBasisEncoding(response, validBasis, basisId, locator)
            }

  def project(
      data: DMat,
      loadings: DMat,
      offset: Option[DVec] = None,
      ridge: Double = 0.0
  ): Either[LatentError, DMat] =
    if data.cols != loadings.rows then
      Left(LatentError.DimensionMismatch("loading rows", data.cols, loadings.rows))
    else if ridge < 0.0 || !ridge.isFinite then Left(LatentError.InvalidParameter("ridge", ridge))
    else
      offset match
        case Some(value) if value.length != data.cols =>
          Left(LatentError.DimensionMismatch("offset length", data.cols, value.length))
        case _ =>
          firstNonFinite("data", data)
            .orElse(firstNonFinite("loadings", loadings))
            .orElse(offset.flatMap(value => firstNonFinite("offset", value))) match
            case Some(error) =>
              Left(error)
            case None =>
              val gram = LatentNumerics.crossProduct(loadings)
              val rhs = loadingTransposeTimesData(data, loadings, offset)
              LatentNumerics
                .solveGram(gram, rhs, ridge = ridge)
                .left
                .map(err => LatentError.ProjectionFailed(err.message))
                .map(_.transpose)

  private def loadingTransposeTimesData(
      data: DMat,
      loadings: DMat,
      offset: Option[DVec]
  ): DMat =
    val out = new Array[Double](loadings.cols * data.rows)
    var time = 0
    while time < data.rows do
      var voxel = 0
      while voxel < data.cols do
        val value =
          data(time, voxel) -
            offset.fold(0.0)(_(voxel))
        var atom = 0
        while atom < loadings.cols do
          out(atom * data.rows + time) += loadings(voxel, atom) * value
          atom += 1
        voxel += 1
      time += 1
    LatentNumerics.matrixFromRowMajor(loadings.cols, data.rows, out)

  private def columnMeans(data: DMat): DVec =
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
    LatentNumerics.vectorFromArray(means)

  private def sharedBasisMetadata(
      basis: SharedBasisArtifact,
      basisId: SharedBasisId,
      center: Boolean,
      ridge: Double,
      metadata: Map[String, String]
  ): Map[String, String] =
    metadata ++ Map(
      "family" -> "shared_basis",
      "basis.id" -> basisId.value,
      "basis.checksum" -> basis.checksum.value,
      "basis.kind" -> basis.kind,
      "basis.n_atoms" -> basis.nAtoms.toString,
      "basis.n_voxels" -> basis.nVoxels.toString,
      "center" -> center.toString,
      "ridge" -> ridge.toString
    )

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
