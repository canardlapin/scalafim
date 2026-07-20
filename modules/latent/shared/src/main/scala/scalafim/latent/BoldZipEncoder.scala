package scalafim.latent

import gale.linalg.{DMat, DVec}

enum BoldZipEncoderMode:
  case IdentityDetail

final case class BoldZipEncoderSpec private (
    mode: BoldZipEncoderMode,
    center: Boolean,
    sourceDomain: DomainId,
    targetDomain: DomainId,
    annotation: LatentAnnotation
)

object BoldZipEncoderSpec:
  def identityDetail(
      center: Boolean = false,
      sourceDomain: DomainId = DomainId.unsafe("boldzip.identity.carriers"),
      targetDomain: DomainId = DomainId.unsafe("boldzip.samples"),
      label: String = "boldzip_identity_detail",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, BoldZipEncoderSpec] =
    LatentAnnotation(label, metadata).map { annotation =>
      BoldZipEncoderSpec(
        mode = BoldZipEncoderMode.IdentityDetail,
        center = center,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        annotation = annotation
      )
    }

final case class BoldZipEncodingQuality private (
    maxAbsError: Double,
    relativeFrobeniusError: Double
):
  require(maxAbsError >= 0.0 && maxAbsError.isFinite, "max absolute error must be finite and non-negative")
  require(relativeFrobeniusError >= 0.0 && relativeFrobeniusError.isFinite, "relative Frobenius error must be finite and non-negative")

object BoldZipEncodingQuality:
  def fromMatrices(
      expected: DMat,
      actual: DMat
  ): Either[LatentError, BoldZipEncodingQuality] =
    if expected.rows != actual.rows || expected.cols != actual.cols then
      Left(
        LatentError.MatrixShapeMismatch(
          "BOLDZip reconstruction",
          expected.rows,
          expected.cols,
          actual.rows,
          actual.cols
        )
      )
    else
      val expectedData = expected.copyData
      val actualData = actual.copyData
      var i = 0
      var maxAbs = 0.0
      var sumSquaredError = 0.0
      var sumSquaredExpected = 0.0
      while i < expectedData.length do
        val expectedValue = expectedData(i)
        val actualValue = actualData(i)
        val diff = actualValue - expectedValue
        maxAbs = math.max(maxAbs, math.abs(diff))
        sumSquaredError += diff * diff
        sumSquaredExpected += expectedValue * expectedValue
        i += 1

      val errorNorm = math.sqrt(sumSquaredError)
      val relative =
        if sumSquaredExpected == 0.0 then errorNorm
        else errorNorm / math.sqrt(sumSquaredExpected)
      Right(BoldZipEncodingQuality(maxAbs, relative))

final case class BoldZipEncoding(
    payload: BoldZipPayload,
    quality: BoldZipEncodingQuality
)

object BoldZipEncoder:
  def encode(
      data: DMat,
      spec: BoldZipEncoderSpec
  ): Either[LatentError, BoldZipEncoding] =
    spec.mode match
      case BoldZipEncoderMode.IdentityDetail =>
        encodeIdentityDetail(data, spec)

  def identityDetail(
      data: DMat,
      center: Boolean = false,
      sourceDomain: DomainId = DomainId.unsafe("boldzip.identity.carriers"),
      targetDomain: DomainId = DomainId.unsafe("boldzip.samples"),
      label: String = "boldzip_identity_detail",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, BoldZipEncoding] =
    for
      spec <- BoldZipEncoderSpec.identityDetail(
        center = center,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        label = label,
        metadata = metadata
      )
      encoding <- encode(data, spec)
    yield encoding

  private def encodeIdentityDetail(
      data: DMat,
      spec: BoldZipEncoderSpec
  ): Either[LatentError, BoldZipEncoding] =
    for
      _ <- validateData(data)
      centered <- centerData(data, spec.center)
      spatial <- BoldZipSpatialBasis(sampleCount = data.cols)
      texture <- identityTexture(data.cols)
      payload <- BoldZipPayload(
        temporalBasis = DMat.eye(data.rows),
        carrierTheta = centered.matrix.transpose,
        carrierLoadings = DMat.zeros(0, data.cols),
        spatialBasis = spatial,
        texture = texture,
        events = Vector.empty,
        offset = centered.offset,
        sourceDomain = spec.sourceDomain,
        targetDomain = spec.targetDomain,
        label = spec.annotation.labelValue,
        metadata = spec.annotation.metadataValues ++ Map(
          "encoder" -> "identity_detail",
          "encoder.center" -> spec.center.toString,
          "encoder.carriers" -> data.cols.toString,
          "encoder.texture_entries" -> data.cols.toString
        )
      )
      reconstructed <- payload.reconstruct()
      quality <- BoldZipEncodingQuality.fromMatrices(data, reconstructed)
    yield BoldZipEncoding(payload, quality)

  private final case class CenteredData(
      matrix: DMat,
      offset: Option[DVec]
  )

  private def centerData(data: DMat, center: Boolean): Either[LatentError, CenteredData] =
    if !center then Right(CenteredData(data, None))
    else
      val means = new Array[Double](data.cols)
      var col = 0
      while col < data.cols do
        var row = 0
        var sum = 0.0
        while row < data.rows do
          sum += data(row, col)
          row += 1
        means(col) = sum / data.rows.toDouble
        col += 1

      val centered = new Array[Double](data.rows * data.cols)
      val source = data.copyData
      var i = 0
      while i < source.length do
        centered(i) = source(i) - means(i % data.cols)
        i += 1
      Right(CenteredData(LatentNumerics.matrixFromRowMajor(data.rows, data.cols, centered), Some(LatentNumerics.vectorFromArray(means))))

  private def identityTexture(samples: Int): Either[LatentError, Vector[BoldZipTextureEntry]] =
    val out = Vector.newBuilder[BoldZipTextureEntry]
    out.sizeHint(samples)
    var sample = 0
    var error = Option.empty[LatentError]
    while sample < samples && error.isEmpty do
      BoldZipTextureEntry.checked(atom = sample, carrier = sample, amplitude = 1.0) match
        case Right(entry) => out += entry
        case Left(err)    => error = Some(err)
      sample += 1
    error match
      case Some(err) => Left(err)
      case None      => Right(out.result())

  private def validateData(data: DMat): Either[LatentError, Unit] =
    if data.rows <= 0 then Left(LatentError.NonPositiveDimension("BOLDZip encoder timepoints", data.rows))
    else if data.cols <= 0 then Left(LatentError.NonPositiveDimension("BOLDZip encoder samples", data.cols))
    else
      firstNonFinite(data) match
        case Some(error) => Left(error)
        case None        => Right(())

  private def firstNonFinite(data: DMat): Option[LatentError] =
    val values = data.copyData
    var i = 0
    var error = Option.empty[LatentError]
    while i < values.length && error.isEmpty do
      val value = values(i)
      if !value.isFinite then error = Some(LatentError.NonFiniteValue("BOLDZip encoder data", i, value))
      i += 1
    error
