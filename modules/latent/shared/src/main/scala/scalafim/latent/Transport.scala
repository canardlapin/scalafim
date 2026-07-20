package scalafim.latent

import scalafim.linalg.{CsrMatrix, DoubleMatrix, DoubleVector, GramProjection, LinearMap}

enum CoefficientCoordinates:
  case Analysis, Raw

enum CoefficientBlock:
  case Analysis(override val values: DoubleMatrix)
  case Raw(override val values: DoubleMatrix)

  def values: DoubleMatrix =
    this match
      case Analysis(values) => values
      case Raw(values)      => values

  def coordinates: CoefficientCoordinates =
    this match
      case Analysis(_) => CoefficientCoordinates.Analysis
      case Raw(_)      => CoefficientCoordinates.Raw

object CoefficientBlock:
  def apply(values: DoubleMatrix, coordinates: CoefficientCoordinates): CoefficientBlock =
    coordinates match
      case CoefficientCoordinates.Analysis => Analysis(values)
      case CoefficientCoordinates.Raw      => Raw(values)

enum CoefficientCovariance:
  case Analysis(override val values: DoubleMatrix)
  case Raw(override val values: DoubleMatrix)

  def values: DoubleMatrix =
    this match
      case Analysis(values) => values
      case Raw(values)      => values

  def coordinates: CoefficientCoordinates =
    this match
      case Analysis(_) => CoefficientCoordinates.Analysis
      case Raw(_)      => CoefficientCoordinates.Raw

object CoefficientCovariance:
  def apply(values: DoubleMatrix, coordinates: CoefficientCoordinates): CoefficientCovariance =
    coordinates match
      case CoefficientCoordinates.Analysis => Analysis(values)
      case CoefficientCoordinates.Raw      => Raw(values)

enum TransportSpace:
  case Native, Template

enum TransportAdjointConvention:
  case EuclideanDiscrete

enum TransportDecoders:
  case NativeOnly(native: LinearMap)
  case TemplateCapable(native: LinearMap, template: LinearMap)

  def nativeDecoder: LinearMap =
    this match
      case NativeOnly(native)              => native
      case TemplateCapable(native, _)      => native

  def templateDecoder: Option[LinearMap] =
    this match
      case NativeOnly(_)                   => None
      case TemplateCapable(_, template)    => Some(template)

  def templateCapable: Boolean =
    templateDecoder.nonEmpty

  def decoder(space: TransportSpace): Either[LatentError, LinearMap] =
    space match
      case TransportSpace.Native =>
        Right(nativeDecoder)
      case TransportSpace.Template =>
        templateDecoder.toRight(LatentError.MissingComponent("template decoder"))

  def metadataValue: String =
    this match
      case NativeOnly(_)                => "native_only"
      case TemplateCapable(_, _)        => "template_capable"

object TransportDecoders:
  def apply(
      nativeDecoder: LinearMap,
      templateDecoder: Option[LinearMap]
  ): TransportDecoders =
    templateDecoder match
      case Some(template) => TransportDecoders.TemplateCapable(nativeDecoder, template)
      case None           => TransportDecoders.NativeOnly(nativeDecoder)

final class CoefficientTransform private (
    val toAnalysis: LinearMap,
    val toRaw: LinearMap
):
  def dimension: Int =
    toAnalysis.rows

  def analysis(rawCoefficients: DoubleMatrix): Either[LatentError, DoubleMatrix] =
    toAnalysis.forward(rawCoefficients).left.map(linearMapError)

  def raw(analysisCoefficients: DoubleMatrix): Either[LatentError, DoubleMatrix] =
    toRaw.forward(analysisCoefficients).left.map(linearMapError)

  def rawMetric: Either[LatentError, DoubleMatrix] =
    toAnalysis.forward(DoubleMatrix.eye(dimension)).left.map(linearMapError).map { matrix =>
      DoubleMatrix.crossProduct(matrix)
    }

object CoefficientTransform:
  def identity(size: Int): Either[LatentError, CoefficientTransform] =
    CsrMatrix.identity(size).left.map(linearMapError).map { id =>
      new CoefficientTransform(id, id)
    }

  def apply(
      toAnalysis: LinearMap,
      toRaw: LinearMap
  ): Either[LatentError, CoefficientTransform] =
    if toAnalysis.rows != toAnalysis.cols then
      Left(LatentError.MatrixShapeMismatch("toAnalysis", toAnalysis.cols, toAnalysis.cols, toAnalysis.rows, toAnalysis.cols))
    else if toRaw.rows != toRaw.cols then
      Left(LatentError.MatrixShapeMismatch("toRaw", toRaw.cols, toRaw.cols, toRaw.rows, toRaw.cols))
    else if toAnalysis.rows != toRaw.rows then
      Left(LatentError.DimensionMismatch("transform dimension", toAnalysis.rows, toRaw.rows))
    else Right(new CoefficientTransform(toAnalysis, toRaw))

final class TransportLatentResponse private (
    val coefficientsAnalysis: DoubleMatrix,
    val decoders: TransportDecoders,
    val transform: CoefficientTransform,
    val offset: Option[DoubleVector],
    val sourceDomain: DomainId,
    val targetDomain: DomainId,
    val latentLabel: LatentLabel,
    val typedMetadata: LatentMetadata,
    val adjointConvention: TransportAdjointConvention
) extends LatentResponse:

  override val shape: LatentShape =
    LatentShape(
      timepoints = coefficientsAnalysis.rows,
      samples = nativeDecoder.rows,
      coefficients = coefficientsAnalysis.cols
    )

  def nativeDecoder: LinearMap =
    decoders.nativeDecoder

  def templateDecoder: Option[LinearMap] =
    decoders.templateDecoder

  def coefficientBlock: CoefficientBlock =
    CoefficientBlock.Analysis(coefficientsAnalysis)

  override def coefTime: DoubleMatrix =
    coefficientsAnalysis

  override def decodeSemantics: LatentDecodeSemantics =
    LatentDecodeSemantics.linear(offset = offset.nonEmpty)

  def decoder(
      space: TransportSpace = TransportSpace.Native,
      coordinates: CoefficientCoordinates = CoefficientCoordinates.Analysis
  ): Either[LatentError, LinearMap] =
    decoders.decoder(space).flatMap { map =>
      coordinates match
        case CoefficientCoordinates.Analysis =>
          Right(map)
        case CoefficientCoordinates.Raw =>
          LinearMap.compose(transform.toAnalysis, map).left.map(linearMapError)
    }

  override def decodeCoefficients(coefficients: DoubleMatrix): Either[LatentError, DoubleMatrix] =
    decodeCoefficients(coefficients, TransportSpace.Native, CoefficientCoordinates.Analysis)

  def decodeCoefficientBlock(
      coefficients: CoefficientBlock,
      space: TransportSpace = TransportSpace.Native
  ): Either[LatentError, DoubleMatrix] =
    decodeCoefficients(coefficients.values, space, coefficients.coordinates)

  def decodeCoefficients(
      coefficients: DoubleMatrix,
      space: TransportSpace,
      coordinates: CoefficientCoordinates
  ): Either[LatentError, DoubleMatrix] =
    decoder(space, coordinates).flatMap { map =>
      if coefficients.rows != map.cols then Left(LatentError.DimensionMismatch("coefficient rows", map.cols, coefficients.rows))
      else map.forward(coefficients).left.map(linearMapError)
    }

  def covarianceDiagonal(
      covariance: CoefficientCovariance,
      space: TransportSpace
  ): Either[LatentError, DoubleVector] =
    covarianceDiagonal(covariance.values, space, covariance.coordinates)

  def covarianceDiagonal(
      covariance: DoubleMatrix,
      space: TransportSpace = TransportSpace.Native,
      coordinates: CoefficientCoordinates = CoefficientCoordinates.Analysis
  ): Either[LatentError, DoubleVector] =
    decoder(space, coordinates).flatMap { map =>
      if covariance.rows != map.cols || covariance.cols != map.cols then
        Left(LatentError.MatrixShapeMismatch("covariance", map.cols, map.cols, covariance.rows, covariance.cols))
      else
        for
          basis <- map.forward(DoubleMatrix.eye(map.cols)).left.map(linearMapError)
          weighted <- map.forward(covariance).left.map(linearMapError)
        yield
          val out = new Array[Double](map.rows)
          var row = 0
          while row < map.rows do
            var sum = 0.0
            var col = 0
            while col < map.cols do
              sum += basis.dataArray(row * basis.cols + col) * weighted.dataArray(row * weighted.cols + col)
              col += 1
            out(row) = sum
            row += 1
          DoubleVector.unsafe(out)
    }

  override def reconstruct(selection: LatentSelection = LatentSelection.All): Either[LatentError, DoubleMatrix] =
    selection.resolve(shape.timepoints, shape.samples).flatMap { resolved =>
      for
        restricted <- LinearMap
          .restrict(nativeDecoder, targetRows = Some(resolved.samples))
          .left
          .map(linearMapError)
        coeff <- selectedCoefficientColumns(resolved.timepoints)
        decoded <- restricted.forward(coeff).left.map(linearMapError)
      yield transposeDecoded(decoded, resolved.samples)
    }

  private def selectedCoefficientColumns(timepoints: IndexedSeq[Int]): Either[LatentError, DoubleMatrix] =
    val out = new Array[Double](shape.coefficients * timepoints.length)
    var outCol = 0
    while outCol < timepoints.length do
      val time = timepoints(outCol)
      var component = 0
      while component < shape.coefficients do
        out(component * timepoints.length + outCol) =
          coefficientsAnalysis.dataArray(time * coefficientsAnalysis.cols + component)
        component += 1
      outCol += 1
    Right(DoubleMatrix.unsafe(shape.coefficients, timepoints.length, out))

  private def transposeDecoded(decoded: DoubleMatrix, samples: IndexedSeq[Int]): DoubleMatrix =
    val out = new Array[Double](decoded.cols * decoded.rows)
    var time = 0
    while time < decoded.cols do
      var sampleIndex = 0
      while sampleIndex < decoded.rows do
        val sample = samples(sampleIndex)
        val value = decoded.dataArray(sampleIndex * decoded.cols + time) + offset.fold(0.0)(_(sample))
        out(time * decoded.rows + sampleIndex) = value
        sampleIndex += 1
      time += 1
    DoubleMatrix.unsafe(decoded.cols, decoded.rows, out)

object TransportLatentResponse:
  def apply(
      coefficientsAnalysis: DoubleMatrix,
      decoders: TransportDecoders,
      transform: CoefficientTransform,
      offset: Option[DoubleVector],
      sourceDomain: DomainId,
      targetDomain: DomainId,
      label: String,
      metadata: Map[String, String],
      adjointConvention: TransportAdjointConvention
  ): Either[LatentError, TransportLatentResponse] =
    for
      _ <- validate(coefficientsAnalysis, decoders, transform, offset)
      annotation <- LatentAnnotation(
        label,
        metadata ++ Map(
          "family" -> "transport",
          "coordinates" -> "analysis",
          "transport_decoders" -> decoders.metadataValue,
          "has_template_decoder" -> decoders.templateCapable.toString,
          "adjoint_convention" -> "euclidean_discrete"
        )
      )
    yield
      new TransportLatentResponse(
        coefficientsAnalysis = coefficientsAnalysis,
        decoders = decoders,
        transform = transform,
        offset = offset,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        latentLabel = annotation.label,
        typedMetadata = annotation.metadata,
        adjointConvention = adjointConvention
      )

  def apply(
      coefficientsAnalysis: DoubleMatrix,
      nativeDecoder: LinearMap,
      transform: CoefficientTransform,
      templateDecoder: Option[LinearMap] = None,
      offset: Option[DoubleVector] = None,
      sourceDomain: DomainId = DomainId.unsafe("transport.coefficients.analysis"),
      targetDomain: DomainId = DomainId.unsafe("transport.native"),
      label: String = "",
      metadata: Map[String, String] = Map.empty,
      adjointConvention: TransportAdjointConvention = TransportAdjointConvention.EuclideanDiscrete
  ): Either[LatentError, TransportLatentResponse] =
    apply(
      coefficientsAnalysis = coefficientsAnalysis,
      decoders = TransportDecoders(nativeDecoder, templateDecoder),
      transform = transform,
      offset = offset,
      sourceDomain = sourceDomain,
      targetDomain = targetDomain,
      label = label,
      metadata = metadata,
      adjointConvention = adjointConvention
    )

  def fromDecoders(
      coefficientsAnalysis: DoubleMatrix,
      decoders: TransportDecoders,
      transform: CoefficientTransform,
      offset: Option[DoubleVector] = None,
      sourceDomain: DomainId = DomainId.unsafe("transport.coefficients.analysis"),
      targetDomain: DomainId = DomainId.unsafe("transport.native"),
      label: String = "",
      metadata: Map[String, String] = Map.empty,
      adjointConvention: TransportAdjointConvention = TransportAdjointConvention.EuclideanDiscrete
  ): Either[LatentError, TransportLatentResponse] =
    apply(
      coefficientsAnalysis = coefficientsAnalysis,
      decoders = decoders,
      transform = transform,
      offset = offset,
      sourceDomain = sourceDomain,
      targetDomain = targetDomain,
      label = label,
      metadata = metadata,
      adjointConvention = adjointConvention
    )

  def withIdentityTransform(
      coefficientsAnalysis: DoubleMatrix,
      nativeDecoder: LinearMap,
      templateDecoder: Option[LinearMap] = None,
      offset: Option[DoubleVector] = None,
      sourceDomain: DomainId = DomainId.unsafe("transport.coefficients.analysis"),
      targetDomain: DomainId = DomainId.unsafe("transport.native"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, TransportLatentResponse] =
    CoefficientTransform.identity(nativeDecoder.cols).flatMap { transform =>
      fromDecoders(
        coefficientsAnalysis = coefficientsAnalysis,
        decoders = TransportDecoders(nativeDecoder, templateDecoder),
        transform = transform,
        offset = offset,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        label = label,
        metadata = metadata
      )
    }

  private def validate(
      coefficientsAnalysis: DoubleMatrix,
      decoders: TransportDecoders,
      transform: CoefficientTransform,
      offset: Option[DoubleVector]
  ): Either[LatentError, Unit] =
    val nativeDecoder = decoders.nativeDecoder
    if coefficientsAnalysis.rows <= 0 then Left(LatentError.NonPositiveDimension("coefficient rows", coefficientsAnalysis.rows))
    else if coefficientsAnalysis.cols <= 0 then Left(LatentError.NonPositiveDimension("coefficient columns", coefficientsAnalysis.cols))
    else if nativeDecoder.cols != coefficientsAnalysis.cols then
      Left(LatentError.DimensionMismatch("native decoder source", coefficientsAnalysis.cols, nativeDecoder.cols))
    else if transform.dimension != coefficientsAnalysis.cols then
      Left(LatentError.DimensionMismatch("transform dimension", coefficientsAnalysis.cols, transform.dimension))
    else
      decoders.templateDecoder match
        case Some(decoder) if decoder.cols != coefficientsAnalysis.cols =>
          Left(LatentError.DimensionMismatch("template decoder source", coefficientsAnalysis.cols, decoder.cols))
        case _ =>
          offset match
            case Some(value) if value.length != nativeDecoder.rows =>
              Left(LatentError.DimensionMismatch("offset length", nativeDecoder.rows, value.length))
            case _ =>
              firstNonFinite("coefficients", coefficientsAnalysis)
                .orElse(offset.flatMap(value => firstNonFinite("offset", value))) match
                case Some(error) => Left(error)
                case None        => Right(())

object TransportProjection:
  def coefficientsWithPenalty(
      targetData: DoubleMatrix,
      decoder: LinearMap,
      ridge: RidgePenalty,
      roughness: Option[DoubleMatrix] = None
  ): Either[LatentError, DoubleMatrix] =
    coefficientsValidated(targetData, decoder, ridge, roughness)

  def coefficients(
      targetData: DoubleMatrix,
      decoder: LinearMap,
      ridge: Double = 0.0,
      roughness: Option[DoubleMatrix] = None
  ): Either[LatentError, DoubleMatrix] =
    RidgePenalty(ridge).flatMap { penalty =>
      coefficientsValidated(targetData, decoder, penalty, roughness)
    }

  private def coefficientsValidated(
      targetData: DoubleMatrix,
      decoder: LinearMap,
      ridge: RidgePenalty,
      roughness: Option[DoubleMatrix]
  ): Either[LatentError, DoubleMatrix] =
    if targetData.rows != decoder.rows then Left(LatentError.DimensionMismatch("target rows", decoder.rows, targetData.rows))
    else
      for
        basis <- decoder.forward(DoubleMatrix.eye(decoder.cols)).left.map(linearMapError)
        gram0 = DoubleMatrix.crossProduct(basis).addToDiagonal(ridge.value)
        gram <- addRoughness(gram0, roughness, decoder.cols)
        rhs = DoubleMatrix.transposeMultiply(basis, targetData)
        coeff <- GramProjection.solveGram(gram, rhs).left.map(err => LatentError.ProjectionFailed(err.message))
      yield coeff

  private def addRoughness(
      gram: DoubleMatrix,
      roughness: Option[DoubleMatrix],
      size: Int
  ): Either[LatentError, DoubleMatrix] =
    roughness match
      case None =>
        Right(gram)
      case Some(value) if value.rows != size || value.cols != size =>
        Left(LatentError.MatrixShapeMismatch("roughness", size, size, value.rows, value.cols))
      case Some(value) =>
        val out = gram.copyData
        var i = 0
        var error = Option.empty[LatentError]
        while i < out.length && error.isEmpty do
          val rough = value.dataArray(i)
          if !rough.isFinite then error = Some(LatentError.NonFiniteValue("roughness", i, rough))
          else out(i) += rough
          i += 1
        error match
          case Some(value) => Left(value)
          case None        => Right(DoubleMatrix.unsafe(size, size, out))

private def linearMapError(error: scalafim.linalg.LinearMapError): LatentError =
  LatentError.ProjectionFailed(error.message)

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
