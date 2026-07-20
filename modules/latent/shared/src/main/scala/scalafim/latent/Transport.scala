package scalafim.latent

import gale.linalg.{DMat, DVec, DoubleLinearOperator, LinAlgError}

enum CoefficientCoordinates:
  case Analysis, Raw

enum CoefficientBlock:
  case Analysis(override val values: DMat)
  case Raw(override val values: DMat)

  def values: DMat =
    this match
      case Analysis(values) => values
      case Raw(values)      => values

  def coordinates: CoefficientCoordinates =
    this match
      case Analysis(_) => CoefficientCoordinates.Analysis
      case Raw(_)      => CoefficientCoordinates.Raw

object CoefficientBlock:
  def apply(values: DMat, coordinates: CoefficientCoordinates): CoefficientBlock =
    coordinates match
      case CoefficientCoordinates.Analysis => Analysis(values)
      case CoefficientCoordinates.Raw      => Raw(values)

enum CoefficientCovariance:
  case Analysis(override val values: DMat)
  case Raw(override val values: DMat)

  def values: DMat =
    this match
      case Analysis(values) => values
      case Raw(values)      => values

  def coordinates: CoefficientCoordinates =
    this match
      case Analysis(_) => CoefficientCoordinates.Analysis
      case Raw(_)      => CoefficientCoordinates.Raw

object CoefficientCovariance:
  def apply(values: DMat, coordinates: CoefficientCoordinates): CoefficientCovariance =
    coordinates match
      case CoefficientCoordinates.Analysis => Analysis(values)
      case CoefficientCoordinates.Raw      => Raw(values)

enum TransportSpace:
  case Native, Template

enum TransportAdjointConvention:
  case EuclideanDiscrete

enum TransportDecoders:
  case NativeOnly(native: DoubleLinearOperator)
  case TemplateCapable(native: DoubleLinearOperator, template: DoubleLinearOperator)

  def nativeDecoder: DoubleLinearOperator =
    this match
      case NativeOnly(native)              => native
      case TemplateCapable(native, _)      => native

  def templateDecoder: Option[DoubleLinearOperator] =
    this match
      case NativeOnly(_)                   => None
      case TemplateCapable(_, template)    => Some(template)

  def templateCapable: Boolean =
    templateDecoder.nonEmpty

  def decoder(space: TransportSpace): Either[LatentError, DoubleLinearOperator] =
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
      nativeDecoder: DoubleLinearOperator,
      templateDecoder: Option[DoubleLinearOperator]
  ): TransportDecoders =
    templateDecoder match
      case Some(template) => TransportDecoders.TemplateCapable(nativeDecoder, template)
      case None           => TransportDecoders.NativeOnly(nativeDecoder)

final class CoefficientTransform private (
    val toAnalysis: DoubleLinearOperator,
    val toRaw: DoubleLinearOperator
):
  def dimension: Int =
    toAnalysis.rows

  def analysis(rawCoefficients: DMat): Either[LatentError, DMat] =
    toAnalysis.applyTo(rawCoefficients).left.map(linearMapError)

  def raw(analysisCoefficients: DMat): Either[LatentError, DMat] =
    toRaw.applyTo(analysisCoefficients).left.map(linearMapError)

  def rawMetric: Either[LatentError, DMat] =
    toAnalysis.applyTo(DMat.eye(dimension)).left.map(linearMapError).map { matrix =>
      LatentNumerics.crossProduct(matrix)
    }

object CoefficientTransform:
  def identity(size: Int): Either[LatentError, CoefficientTransform] =
    LatentOperators.identity(size).left.map(linearMapError).map { id =>
      new CoefficientTransform(id, id)
    }

  def apply(
      toAnalysis: DoubleLinearOperator,
      toRaw: DoubleLinearOperator
  ): Either[LatentError, CoefficientTransform] =
    if toAnalysis.rows != toAnalysis.cols then
      Left(LatentError.MatrixShapeMismatch("toAnalysis", toAnalysis.cols, toAnalysis.cols, toAnalysis.rows, toAnalysis.cols))
    else if toRaw.rows != toRaw.cols then
      Left(LatentError.MatrixShapeMismatch("toRaw", toRaw.cols, toRaw.cols, toRaw.rows, toRaw.cols))
    else if toAnalysis.rows != toRaw.rows then
      Left(LatentError.DimensionMismatch("transform dimension", toAnalysis.rows, toRaw.rows))
    else Right(new CoefficientTransform(toAnalysis, toRaw))

final class TransportLatentResponse private (
    val coefficientsAnalysis: DMat,
    val decoders: TransportDecoders,
    val transform: CoefficientTransform,
    val offset: Option[DVec],
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

  def nativeDecoder: DoubleLinearOperator =
    decoders.nativeDecoder

  def templateDecoder: Option[DoubleLinearOperator] =
    decoders.templateDecoder

  def coefficientBlock: CoefficientBlock =
    CoefficientBlock.Analysis(coefficientsAnalysis)

  override def coefTime: DMat =
    coefficientsAnalysis

  override def decodeSemantics: LatentDecodeSemantics =
    LatentDecodeSemantics.linear(offset = offset.nonEmpty)

  def decoder(
      space: TransportSpace = TransportSpace.Native,
      coordinates: CoefficientCoordinates = CoefficientCoordinates.Analysis
  ): Either[LatentError, DoubleLinearOperator] =
    decoders.decoder(space).flatMap { map =>
      coordinates match
        case CoefficientCoordinates.Analysis =>
          Right(map)
        case CoefficientCoordinates.Raw =>
          LatentOperators.compose(transform.toAnalysis, map).left.map(linearMapError)
    }

  override def decodeCoefficients(coefficients: DMat): Either[LatentError, DMat] =
    decodeCoefficients(coefficients, TransportSpace.Native, CoefficientCoordinates.Analysis)

  def decodeCoefficientBlock(
      coefficients: CoefficientBlock,
      space: TransportSpace = TransportSpace.Native
  ): Either[LatentError, DMat] =
    decodeCoefficients(coefficients.values, space, coefficients.coordinates)

  def decodeCoefficients(
      coefficients: DMat,
      space: TransportSpace,
      coordinates: CoefficientCoordinates
  ): Either[LatentError, DMat] =
    decoder(space, coordinates).flatMap { map =>
      if coefficients.rows != map.cols then Left(LatentError.DimensionMismatch("coefficient rows", map.cols, coefficients.rows))
      else map.applyTo(coefficients).left.map(linearMapError)
    }

  def covarianceDiagonal(
      covariance: CoefficientCovariance,
      space: TransportSpace
  ): Either[LatentError, DVec] =
    covarianceDiagonal(covariance.values, space, covariance.coordinates)

  def covarianceDiagonal(
      covariance: DMat,
      space: TransportSpace = TransportSpace.Native,
      coordinates: CoefficientCoordinates = CoefficientCoordinates.Analysis
  ): Either[LatentError, DVec] =
    decoder(space, coordinates).flatMap { map =>
      if covariance.rows != map.cols || covariance.cols != map.cols then
        Left(LatentError.MatrixShapeMismatch("covariance", map.cols, map.cols, covariance.rows, covariance.cols))
      else
        for
          basis <- map.applyTo(DMat.eye(map.cols)).left.map(linearMapError)
          weighted <- map.applyTo(covariance).left.map(linearMapError)
        yield
          val out = new Array[Double](map.rows)
          var row = 0
          while row < map.rows do
            var sum = 0.0
            var col = 0
            while col < map.cols do
              sum += basis(row, col) * weighted(row, col)
              col += 1
            out(row) = sum
            row += 1
          LatentNumerics.vectorFromArray(out)
    }

  override def reconstruct(selection: LatentSelection = LatentSelection.All): Either[LatentError, DMat] =
    selection.resolve(shape.timepoints, shape.samples).flatMap { resolved =>
      for
        restricted <- LatentOperators
          .restrict(nativeDecoder, targetRows = Some(resolved.samples))
          .left
          .map(linearMapError)
        coeff <- selectedCoefficientColumns(resolved.timepoints)
        decoded <- restricted.applyTo(coeff).left.map(linearMapError)
      yield transposeDecoded(decoded, resolved.samples)
    }

  private def selectedCoefficientColumns(timepoints: IndexedSeq[Int]): Either[LatentError, DMat] =
    val out = new Array[Double](shape.coefficients * timepoints.length)
    var outCol = 0
    while outCol < timepoints.length do
      val time = timepoints(outCol)
      var component = 0
      while component < shape.coefficients do
        out(component * timepoints.length + outCol) =
          coefficientsAnalysis(time, component)
        component += 1
      outCol += 1
    Right(LatentNumerics.matrixFromRowMajor(shape.coefficients, timepoints.length, out))

  private def transposeDecoded(decoded: DMat, samples: IndexedSeq[Int]): DMat =
    val out = new Array[Double](decoded.cols * decoded.rows)
    var time = 0
    while time < decoded.cols do
      var sampleIndex = 0
      while sampleIndex < decoded.rows do
        val sample = samples(sampleIndex)
        val value = decoded(sampleIndex, time) + offset.fold(0.0)(_(sample))
        out(time * decoded.rows + sampleIndex) = value
        sampleIndex += 1
      time += 1
    LatentNumerics.matrixFromRowMajor(decoded.cols, decoded.rows, out)

object TransportLatentResponse:
  def apply(
      coefficientsAnalysis: DMat,
      decoders: TransportDecoders,
      transform: CoefficientTransform,
      offset: Option[DVec],
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
      coefficientsAnalysis: DMat,
      nativeDecoder: DoubleLinearOperator,
      transform: CoefficientTransform,
      templateDecoder: Option[DoubleLinearOperator] = None,
      offset: Option[DVec] = None,
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
      coefficientsAnalysis: DMat,
      decoders: TransportDecoders,
      transform: CoefficientTransform,
      offset: Option[DVec] = None,
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
      coefficientsAnalysis: DMat,
      nativeDecoder: DoubleLinearOperator,
      templateDecoder: Option[DoubleLinearOperator] = None,
      offset: Option[DVec] = None,
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
      coefficientsAnalysis: DMat,
      decoders: TransportDecoders,
      transform: CoefficientTransform,
      offset: Option[DVec]
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
      targetData: DMat,
      decoder: DoubleLinearOperator,
      ridge: RidgePenalty,
      roughness: Option[DMat] = None
  ): Either[LatentError, DMat] =
    coefficientsValidated(targetData, decoder, ridge, roughness)

  def coefficients(
      targetData: DMat,
      decoder: DoubleLinearOperator,
      ridge: Double = 0.0,
      roughness: Option[DMat] = None
  ): Either[LatentError, DMat] =
    RidgePenalty(ridge).flatMap { penalty =>
      coefficientsValidated(targetData, decoder, penalty, roughness)
    }

  private def coefficientsValidated(
      targetData: DMat,
      decoder: DoubleLinearOperator,
      ridge: RidgePenalty,
      roughness: Option[DMat]
  ): Either[LatentError, DMat] =
    if targetData.rows != decoder.rows then Left(LatentError.DimensionMismatch("target rows", decoder.rows, targetData.rows))
    else
      for
        basis <- decoder.applyTo(DMat.eye(decoder.cols)).left.map(linearMapError)
        gram0 = LatentNumerics.crossProduct(basis).addToDiagonal(ridge.value)
        gram <- addRoughness(gram0, roughness, decoder.cols)
        rhs = LatentNumerics.transposeMultiply(basis, targetData)
        coeff <- LatentNumerics.solveGram(gram, rhs).left.map(err => LatentError.ProjectionFailed(err.message))
      yield coeff

  private def addRoughness(
      gram: DMat,
      roughness: Option[DMat],
      size: Int
  ): Either[LatentError, DMat] =
    roughness match
      case None =>
        Right(gram)
      case Some(value) if value.rows != size || value.cols != size =>
        Left(LatentError.MatrixShapeMismatch("roughness", size, size, value.rows, value.cols))
      case Some(value) =>
        val out = gram.copyData
        val roughnessData = value.copyData
        var i = 0
        var error = Option.empty[LatentError]
        while i < out.length && error.isEmpty do
          val rough = roughnessData(i)
          if !rough.isFinite then error = Some(LatentError.NonFiniteValue("roughness", i, rough))
          else out(i) += rough
          i += 1
        error match
          case Some(value) => Left(value)
          case None        => Right(LatentNumerics.matrixFromRowMajor(size, size, out))

private def linearMapError(error: LinAlgError): LatentError =
  LatentError.ProjectionFailed(error.message)

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
