package scalafim.latent

import scalafim.linalg.{DoubleMatrix, DoubleVector}

final case class BoldZipSpatialBasis(
    sampleCount: Int,
    phiCoarse: Option[DoubleMatrix] = None,
    phiDetail: Option[DoubleMatrix] = None,
    label: String = ""
):
  require(sampleCount > 0, "sampleCount must be positive")
  phiCoarse.foreach(phi => require(phi.rows == sampleCount, "phiCoarse rows must match sampleCount"))
  phiDetail.foreach(phi => require(phi.rows == sampleCount, "phiDetail rows must match sampleCount"))

  def detailAtoms: Int =
    phiDetail.fold(sampleCount)(_.cols)

final case class BoldZipTextureEntry(
    atom: Int,
    carrier: Int,
    amplitude: Double,
    lag: Int = 0
)

final case class BoldZipResidualEvent(
    atom: Int,
    time: Int,
    amplitude: Double,
    duration: Int = 1
)

final class BoldZipPayload private (
    val temporalBasis: DoubleMatrix,
    val carrierTheta: DoubleMatrix,
    val carrierLoadings: DoubleMatrix,
    val spatialBasis: BoldZipSpatialBasis,
    val texture: Vector[BoldZipTextureEntry],
    val events: Vector[BoldZipResidualEvent],
    val offset: Option[DoubleVector],
    val sourceDomain: DomainId,
    val targetDomain: DomainId,
    val label: String,
    val metadata: Map[String, String]
) extends LatentResponse:

  override val shape: LatentShape =
    LatentShape(
      timepoints = temporalBasis.rows,
      samples = spatialBasis.sampleCount,
      coefficients = carrierTheta.rows
    )

  override def coefTime: DoubleMatrix =
    val out = new Array[Double](shape.timepoints * shape.coefficients)
    var time = 0
    while time < shape.timepoints do
      var carrier = 0
      while carrier < shape.coefficients do
        out(time * shape.coefficients + carrier) = carrierValue(carrier, time)
        carrier += 1
      time += 1
    DoubleMatrix.unsafe(shape.timepoints, shape.coefficients, out)

  override def decodeCoefficients(coefficients: DoubleMatrix): Either[LatentError, DoubleMatrix] =
    if coefficients.rows != shape.coefficients then
      Left(LatentError.DimensionMismatch("coefficient rows", shape.coefficients, coefficients.rows))
    else
      Right(decodeCarrierColumns(coefficients, includeEvents = false, includeOffset = false))

  override def reconstruct(selection: LatentSelection = LatentSelection.All): Either[LatentError, DoubleMatrix] =
    selection.resolve(shape.timepoints, shape.samples).map { resolved =>
      val out = new Array[Double](resolved.timepoints.length * resolved.samples.length)
      var outTime = 0
      while outTime < resolved.timepoints.length do
        val time = resolved.timepoints(outTime)
        var outSample = 0
        while outSample < resolved.samples.length do
          val sample = resolved.samples(outSample)
          out(outTime * resolved.samples.length + outSample) =
            sampleValue(sample, time, includeEvents = true, includeOffset = true)
          outSample += 1
        outTime += 1
      DoubleMatrix.unsafe(resolved.timepoints.length, resolved.samples.length, out)
    }

  private def decodeCarrierColumns(
      carriersByColumn: DoubleMatrix,
      includeEvents: Boolean,
      includeOffset: Boolean
  ): DoubleMatrix =
    val out = new Array[Double](shape.samples * carriersByColumn.cols)
    var sample = 0
    while sample < shape.samples do
      var col = 0
      while col < carriersByColumn.cols do
        out(sample * carriersByColumn.cols + col) =
          sampleValueFromCarrierColumn(
            sample = sample,
            column = col,
            carriersByColumn = carriersByColumn,
            includeEvents = includeEvents,
            includeOffset = includeOffset
          )
        col += 1
      sample += 1
    DoubleMatrix.unsafe(shape.samples, carriersByColumn.cols, out)

  private def sampleValue(sample: Int, time: Int, includeEvents: Boolean, includeOffset: Boolean): Double =
    var value = 0.0
    spatialBasis.phiCoarse match
      case Some(phi) =>
        var atom = 0
        while atom < phi.cols do
          value += phi.dataArray(sample * phi.cols + atom) * coarseAtomValue(atom, time)
          atom += 1
      case None =>
        ()

    spatialBasis.phiDetail match
      case Some(phi) =>
        var atom = 0
        while atom < phi.cols do
          value += phi.dataArray(sample * phi.cols + atom) * detailAtomValue(atom, time, includeEvents)
          atom += 1
      case None =>
        value += detailAtomValue(sample, time, includeEvents)

    if includeOffset then value += offset.fold(0.0)(_(sample))
    value

  private def sampleValueFromCarrierColumn(
      sample: Int,
      column: Int,
      carriersByColumn: DoubleMatrix,
      includeEvents: Boolean,
      includeOffset: Boolean
  ): Double =
    var value = 0.0
    spatialBasis.phiCoarse match
      case Some(phi) =>
        var atom = 0
        while atom < phi.cols do
          value += phi.dataArray(sample * phi.cols + atom) * coarseAtomValueFromCarrierColumn(atom, column, carriersByColumn)
          atom += 1
      case None =>
        ()

    spatialBasis.phiDetail match
      case Some(phi) =>
        var atom = 0
        while atom < phi.cols do
          value += phi.dataArray(sample * phi.cols + atom) *
            detailAtomValueFromCarrierColumn(atom, column, carriersByColumn, includeEvents)
          atom += 1
      case None =>
        value += detailAtomValueFromCarrierColumn(sample, column, carriersByColumn, includeEvents)

    if includeOffset then value += offset.fold(0.0)(_(sample))
    value

  private def coarseAtomValue(atom: Int, time: Int): Double =
    var sum = 0.0
    var carrier = 0
    while carrier < shape.coefficients do
      sum += carrierLoadings.dataArray(atom * carrierLoadings.cols + carrier) * carrierValue(carrier, time)
      carrier += 1
    sum

  private def coarseAtomValueFromCarrierColumn(atom: Int, column: Int, carriersByColumn: DoubleMatrix): Double =
    var sum = 0.0
    var carrier = 0
    while carrier < shape.coefficients do
      sum += carrierLoadings.dataArray(atom * carrierLoadings.cols + carrier) *
        carriersByColumn.dataArray(carrier * carriersByColumn.cols + column)
      carrier += 1
    sum

  private def detailAtomValue(atom: Int, time: Int, includeEvents: Boolean): Double =
    var sum = 0.0
    var i = 0
    while i < texture.length do
      val entry = texture(i)
      if entry.atom == atom then
        sum += entry.amplitude * laggedCarrierValue(entry.carrier, time, entry.lag)
      i += 1
    if includeEvents then sum += eventValue(atom, time)
    sum

  private def detailAtomValueFromCarrierColumn(
      atom: Int,
      column: Int,
      carriersByColumn: DoubleMatrix,
      includeEvents: Boolean
  ): Double =
    var sum = 0.0
    var i = 0
    while i < texture.length do
      val entry = texture(i)
      if entry.atom == atom then
        val sourceColumn = column - entry.lag
        if sourceColumn >= 0 && sourceColumn < carriersByColumn.cols then
          sum += entry.amplitude * carriersByColumn.dataArray(entry.carrier * carriersByColumn.cols + sourceColumn)
      i += 1
    if includeEvents then sum += eventValue(atom, column)
    sum

  private def carrierValue(carrier: Int, time: Int): Double =
    var sum = 0.0
    var component = 0
    while component < temporalBasis.cols do
      sum += carrierTheta.dataArray(carrier * carrierTheta.cols + component) *
        temporalBasis.dataArray(time * temporalBasis.cols + component)
      component += 1
    sum

  private def laggedCarrierValue(carrier: Int, time: Int, lag: Int): Double =
    val sourceTime = time - lag
    if sourceTime < 0 || sourceTime >= shape.timepoints then 0.0
    else carrierValue(carrier, sourceTime)

  private def eventValue(atom: Int, time: Int): Double =
    var sum = 0.0
    var i = 0
    while i < events.length do
      val event = events(i)
      if event.atom == atom && time >= event.time && time < event.time + event.duration then
        sum += event.amplitude
      i += 1
    sum

object BoldZipPayload:
  def apply(
      temporalBasis: DoubleMatrix,
      carrierTheta: DoubleMatrix,
      carrierLoadings: DoubleMatrix,
      spatialBasis: BoldZipSpatialBasis,
      texture: Vector[BoldZipTextureEntry] = Vector.empty,
      events: Vector[BoldZipResidualEvent] = Vector.empty,
      offset: Option[DoubleVector] = None,
      sourceDomain: DomainId = DomainId.unsafe("boldzip.carriers"),
      targetDomain: DomainId = DomainId.unsafe("boldzip.samples"),
      label: String = "boldzip_sr",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, BoldZipPayload] =
    validate(temporalBasis, carrierTheta, carrierLoadings, spatialBasis, texture, events, offset).map { _ =>
      new BoldZipPayload(
        temporalBasis = temporalBasis,
        carrierTheta = carrierTheta,
        carrierLoadings = carrierLoadings,
        spatialBasis = spatialBasis,
        texture = texture,
        events = events,
        offset = offset,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        label = label,
        metadata = metadata ++ Map(
          "family" -> "boldzip_sr",
          "codec_orientation" -> "samples_x_time",
          "orientation" -> "time_x_samples"
        )
      )
    }

  private def validate(
      temporalBasis: DoubleMatrix,
      carrierTheta: DoubleMatrix,
      carrierLoadings: DoubleMatrix,
      spatialBasis: BoldZipSpatialBasis,
      texture: Vector[BoldZipTextureEntry],
      events: Vector[BoldZipResidualEvent],
      offset: Option[DoubleVector]
  ): Either[LatentError, Unit] =
    if temporalBasis.rows <= 0 then Left(LatentError.NonPositiveDimension("temporal basis rows", temporalBasis.rows))
    else if temporalBasis.cols <= 0 then Left(LatentError.NonPositiveDimension("temporal basis columns", temporalBasis.cols))
    else if carrierTheta.rows <= 0 then Left(LatentError.NonPositiveDimension("carrier rows", carrierTheta.rows))
    else if carrierTheta.cols != temporalBasis.cols then
      Left(LatentError.DimensionMismatch("carrier theta columns", temporalBasis.cols, carrierTheta.cols))
    else if carrierLoadings.cols != carrierTheta.rows then
      Left(LatentError.DimensionMismatch("carrier loading columns", carrierTheta.rows, carrierLoadings.cols))
    else
      spatialBasis.phiCoarse match
        case Some(phi) if carrierLoadings.rows != phi.cols =>
          Left(LatentError.DimensionMismatch("carrier loading rows", phi.cols, carrierLoadings.rows))
        case None if carrierLoadings.rows != 0 =>
          Left(LatentError.DimensionMismatch("carrier loading rows", 0, carrierLoadings.rows))
        case _ =>
          validateEntries(temporalBasis.rows, carrierTheta.rows, spatialBasis.detailAtoms, texture, events)
            .flatMap(_ => validateOffset(spatialBasis.sampleCount, offset))
            .flatMap(_ => validateFinite(temporalBasis, carrierTheta, carrierLoadings, spatialBasis, texture, events, offset))

  private def validateEntries(
      timepoints: Int,
      carriers: Int,
      detailAtoms: Int,
      texture: Vector[BoldZipTextureEntry],
      events: Vector[BoldZipResidualEvent]
  ): Either[LatentError, Unit] =
    var textureIndex = 0
    var error = Option.empty[LatentError]
    while textureIndex < texture.length && error.isEmpty do
      val entry = texture(textureIndex)
      if entry.atom < 0 || entry.atom >= detailAtoms then
        error = Some(LatentError.IndexOutOfBounds("texture atom", entry.atom, detailAtoms))
      else if entry.carrier < 0 || entry.carrier >= carriers then
        error = Some(LatentError.IndexOutOfBounds("texture carrier", entry.carrier, carriers))
      else if math.abs(entry.lag) >= timepoints then
        error = Some(LatentError.IndexOutOfBounds("texture lag", entry.lag, timepoints))
      else if !entry.amplitude.isFinite then
        error = Some(LatentError.NonFiniteValue("texture amplitude", textureIndex, entry.amplitude))
      textureIndex += 1

    var eventIndex = 0
    while eventIndex < events.length && error.isEmpty do
      val event = events(eventIndex)
      if event.atom < 0 || event.atom >= detailAtoms then
        error = Some(LatentError.IndexOutOfBounds("event atom", event.atom, detailAtoms))
      else if event.time < 0 || event.time >= timepoints then
        error = Some(LatentError.IndexOutOfBounds("event time", event.time, timepoints))
      else if event.duration <= 0 then
        error = Some(LatentError.NonPositiveDimension("event duration", event.duration))
      else if event.time + event.duration > timepoints then
        error = Some(LatentError.IndexOutOfBounds("event duration", event.time + event.duration - 1, timepoints))
      else if !event.amplitude.isFinite then
        error = Some(LatentError.NonFiniteValue("event amplitude", eventIndex, event.amplitude))
      eventIndex += 1

    error match
      case Some(value) => Left(value)
      case None        => Right(())

  private def validateOffset(sampleCount: Int, offset: Option[DoubleVector]): Either[LatentError, Unit] =
    offset match
      case Some(value) if value.length != sampleCount =>
        Left(LatentError.DimensionMismatch("offset length", sampleCount, value.length))
      case _ =>
        Right(())

  private def validateFinite(
      temporalBasis: DoubleMatrix,
      carrierTheta: DoubleMatrix,
      carrierLoadings: DoubleMatrix,
      spatialBasis: BoldZipSpatialBasis,
      texture: Vector[BoldZipTextureEntry],
      events: Vector[BoldZipResidualEvent],
      offset: Option[DoubleVector]
  ): Either[LatentError, Unit] =
    firstNonFinite("temporal basis", temporalBasis)
      .orElse(firstNonFinite("carrier theta", carrierTheta))
      .orElse(firstNonFinite("carrier loadings", carrierLoadings))
      .orElse(spatialBasis.phiCoarse.flatMap(firstNonFinite("phiCoarse", _)))
      .orElse(spatialBasis.phiDetail.flatMap(firstNonFinite("phiDetail", _)))
      .orElse(offset.flatMap(firstNonFinite("offset", _))) match
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
