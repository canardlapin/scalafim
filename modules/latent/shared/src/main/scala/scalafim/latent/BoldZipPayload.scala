package scalafim.latent

import gale.linalg.{DMat, DVec}

opaque type BoldZipAtomIndex = Int

object BoldZipAtomIndex:
  def apply(value: Int): Either[LatentError, BoldZipAtomIndex] =
    nonNegativeIndex("BOLDZip atom", value)

  def unsafe(value: Int): BoldZipAtomIndex =
    require(value >= 0, "BOLDZip atom index must be non-negative")
    value

  extension (index: BoldZipAtomIndex)
    inline def value: Int = index

opaque type BoldZipCarrierIndex = Int

object BoldZipCarrierIndex:
  def apply(value: Int): Either[LatentError, BoldZipCarrierIndex] =
    nonNegativeIndex("BOLDZip carrier", value)

  def unsafe(value: Int): BoldZipCarrierIndex =
    require(value >= 0, "BOLDZip carrier index must be non-negative")
    value

  extension (index: BoldZipCarrierIndex)
    inline def value: Int = index

opaque type BoldZipFrameIndex = Int

object BoldZipFrameIndex:
  def apply(value: Int): Either[LatentError, BoldZipFrameIndex] =
    nonNegativeIndex("BOLDZip frame", value)

  def unsafe(value: Int): BoldZipFrameIndex =
    require(value >= 0, "BOLDZip frame index must be non-negative")
    value

  extension (index: BoldZipFrameIndex)
    inline def value: Int = index

opaque type BoldZipLag = Int

object BoldZipLag:
  val Zero: BoldZipLag = 0

  def apply(value: Int): Either[LatentError, BoldZipLag] =
    if value == Int.MinValue then Left(LatentError.InvalidParameter("BOLDZip lag", value.toDouble))
    else Right(value)

  def unsafe(value: Int): BoldZipLag =
    apply(value).fold(error => throw IllegalArgumentException(error.message), identity)

  extension (lag: BoldZipLag)
    inline def value: Int = lag

opaque type BoldZipAmplitude = Double

object BoldZipAmplitude:
  def apply(value: Double): Either[LatentError, BoldZipAmplitude] =
    if !value.isFinite then Left(LatentError.InvalidParameter("BOLDZip amplitude", value))
    else Right(value)

  def unsafe(value: Double): BoldZipAmplitude =
    apply(value).fold(error => throw IllegalArgumentException(error.message), identity)

  extension (amplitude: BoldZipAmplitude)
    inline def value: Double = amplitude

opaque type BoldZipDuration = Int

object BoldZipDuration:
  val One: BoldZipDuration = 1

  def apply(value: Int): Either[LatentError, BoldZipDuration] =
    if value <= 0 then Left(LatentError.NonPositiveDimension("BOLDZip event duration", value))
    else Right(value)

  def unsafe(value: Int): BoldZipDuration =
    require(value > 0, "BOLDZip event duration must be positive")
    value

  extension (duration: BoldZipDuration)
    inline def value: Int = duration

enum BoldZipCoarseBasis:
  case Absent
  case MatrixBasis(values: DMat)

  def matrix: Option[DMat] =
    this match
      case Absent              => None
      case MatrixBasis(values) => Some(values)

  def atomCount: Int =
    this match
      case Absent              => 0
      case MatrixBasis(values) => values.cols

  def metadataValue: String =
    this match
      case Absent         => "absent"
      case MatrixBasis(_) => "matrix"

object BoldZipCoarseBasis:
  def fromOption(values: Option[DMat]): BoldZipCoarseBasis =
    values match
      case Some(matrix) => BoldZipCoarseBasis.MatrixBasis(matrix)
      case None         => BoldZipCoarseBasis.Absent

enum BoldZipDetailBasis:
  case IdentitySamples
  case MatrixBasis(values: DMat)

  def matrix: Option[DMat] =
    this match
      case IdentitySamples     => None
      case MatrixBasis(values) => Some(values)

  def atomCount(sampleCount: Int): Int =
    this match
      case IdentitySamples     => sampleCount
      case MatrixBasis(values) => values.cols

  def metadataValue: String =
    this match
      case IdentitySamples => "identity_samples"
      case MatrixBasis(_)  => "matrix"

object BoldZipDetailBasis:
  def fromOption(values: Option[DMat]): BoldZipDetailBasis =
    values match
      case Some(matrix) => BoldZipDetailBasis.MatrixBasis(matrix)
      case None         => BoldZipDetailBasis.IdentitySamples

final class BoldZipSpatialBasis private (
    val sampleCount: Int,
    val coarse: BoldZipCoarseBasis,
    val detail: BoldZipDetailBasis,
    val label: String
):
  def phiCoarse: Option[DMat] =
    coarse.matrix

  def phiDetail: Option[DMat] =
    detail.matrix

  def coarseAtoms: Int =
    coarse.atomCount

  def detailAtoms: Int =
    detail.atomCount(sampleCount)

object BoldZipSpatialBasis:
  def apply(
      sampleCount: Int,
      coarse: BoldZipCoarseBasis = BoldZipCoarseBasis.Absent,
      detail: BoldZipDetailBasis = BoldZipDetailBasis.IdentitySamples,
      label: String = ""
  ): Either[LatentError, BoldZipSpatialBasis] =
    if sampleCount <= 0 then Left(LatentError.NonPositiveDimension("BOLDZip sample count", sampleCount))
    else
      validateMatrix("BOLDZip coarse basis", sampleCount, coarse.matrix)
        .flatMap(_ => validateMatrix("BOLDZip detail basis", sampleCount, detail.matrix))
        .map(_ => new BoldZipSpatialBasis(sampleCount, coarse, detail, label))

  def unsafe(
      sampleCount: Int,
      coarse: BoldZipCoarseBasis = BoldZipCoarseBasis.Absent,
      detail: BoldZipDetailBasis = BoldZipDetailBasis.IdentitySamples,
      label: String = ""
  ): BoldZipSpatialBasis =
    apply(sampleCount, coarse, detail, label).fold(error => throw new IllegalArgumentException(error.message), identity)

  def fromOptional(
      sampleCount: Int,
      phiCoarse: Option[DMat],
      phiDetail: Option[DMat],
      label: String = ""
  ): Either[LatentError, BoldZipSpatialBasis] =
    apply(
      sampleCount = sampleCount,
      coarse = BoldZipCoarseBasis.fromOption(phiCoarse),
      detail = BoldZipDetailBasis.fromOption(phiDetail),
      label = label
    )

  private def validateMatrix(
      label: String,
      sampleCount: Int,
      matrix: Option[DMat]
  ): Either[LatentError, Unit] =
    matrix match
      case Some(values) if values.rows != sampleCount =>
        Left(LatentError.DimensionMismatch(s"$label rows", sampleCount, values.rows))
      case Some(values) if values.cols <= 0 =>
        Left(LatentError.NonPositiveDimension(s"$label columns", values.cols))
      case _ =>
        Right(())

final case class BoldZipTextureEntry(
    atom: BoldZipAtomIndex,
    carrier: BoldZipCarrierIndex,
    amplitude: BoldZipAmplitude,
    lag: BoldZipLag = BoldZipLag.Zero
):
  def amplitudeValue: Double =
    amplitude.value

object BoldZipTextureEntry:
  def checked(
      atom: Int,
      carrier: Int,
      amplitude: Double,
      lag: Int = 0
  ): Either[LatentError, BoldZipTextureEntry] =
    for
      typedAtom <- BoldZipAtomIndex(atom)
      typedCarrier <- BoldZipCarrierIndex(carrier)
      typedAmplitude <- BoldZipAmplitude(amplitude)
      typedLag <- BoldZipLag(lag)
    yield BoldZipTextureEntry(typedAtom, typedCarrier, typedAmplitude, typedLag)

  def unsafe(
      atom: Int,
      carrier: Int,
      amplitude: Double,
      lag: Int = 0
  ): BoldZipTextureEntry =
    BoldZipTextureEntry(
      atom = BoldZipAtomIndex.unsafe(atom),
      carrier = BoldZipCarrierIndex.unsafe(carrier),
      amplitude = BoldZipAmplitude.unsafe(amplitude),
      lag = BoldZipLag.unsafe(lag)
    )

final case class BoldZipResidualEvent(
    atom: BoldZipAtomIndex,
    frame: BoldZipFrameIndex,
    amplitude: BoldZipAmplitude,
    duration: BoldZipDuration = BoldZipDuration.One
):
  def time: Int =
    frame.value

  def amplitudeValue: Double =
    amplitude.value

object BoldZipResidualEvent:
  def checked(
      atom: Int,
      frame: Int,
      amplitude: Double,
      duration: Int = 1
  ): Either[LatentError, BoldZipResidualEvent] =
    for
      typedAtom <- BoldZipAtomIndex(atom)
      typedFrame <- BoldZipFrameIndex(frame)
      typedAmplitude <- BoldZipAmplitude(amplitude)
      typedDuration <- BoldZipDuration(duration)
    yield BoldZipResidualEvent(typedAtom, typedFrame, typedAmplitude, typedDuration)

  def unsafe(
      atom: Int,
      frame: Int,
      amplitude: Double,
      duration: Int = 1
  ): BoldZipResidualEvent =
    BoldZipResidualEvent(
      atom = BoldZipAtomIndex.unsafe(atom),
      frame = BoldZipFrameIndex.unsafe(frame),
      amplitude = BoldZipAmplitude.unsafe(amplitude),
      duration = BoldZipDuration.unsafe(duration)
    )

final class BoldZipPayload private (
    val temporalBasis: DMat,
    val carrierTheta: DMat,
    val carrierLoadings: DMat,
    val spatialBasis: BoldZipSpatialBasis,
    val texture: Vector[BoldZipTextureEntry],
    val events: Vector[BoldZipResidualEvent],
    val offset: Option[DVec],
    val sourceDomain: DomainId,
    val targetDomain: DomainId,
    val latentLabel: LatentLabel,
    val typedMetadata: LatentMetadata
) extends LatentResponse:

  override val shape: LatentShape =
    LatentShape(
      timepoints = temporalBasis.rows,
      samples = spatialBasis.sampleCount,
      coefficients = carrierTheta.rows
    )

  override def coefTime: DMat =
    val out = new Array[Double](shape.timepoints * shape.coefficients)
    var time = 0
    while time < shape.timepoints do
      var carrier = 0
      while carrier < shape.coefficients do
        out(time * shape.coefficients + carrier) = carrierValue(carrier, time)
        carrier += 1
      time += 1
    LatentNumerics.matrixFromRowMajor(shape.timepoints, shape.coefficients, out)

  override def decodeSemantics: LatentDecodeSemantics =
    LatentDecodeSemantics.boldZip(offset = offset.nonEmpty, residualEvents = events.nonEmpty)

  override def decodeCoefficients(coefficients: DMat): Either[LatentError, DMat] =
    if coefficients.rows != shape.coefficients then
      Left(LatentError.DimensionMismatch("coefficient rows", shape.coefficients, coefficients.rows))
    else
      Right(decodeCarrierColumns(coefficients, includeEvents = false, includeOffset = false))

  override def reconstruct(selection: LatentSelection = LatentSelection.All): Either[LatentError, DMat] =
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
      LatentNumerics.matrixFromRowMajor(resolved.timepoints.length, resolved.samples.length, out)
    }

  private def decodeCarrierColumns(
      carriersByColumn: DMat,
      includeEvents: Boolean,
      includeOffset: Boolean
  ): DMat =
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
    LatentNumerics.matrixFromRowMajor(shape.samples, carriersByColumn.cols, out)

  private def sampleValue(sample: Int, time: Int, includeEvents: Boolean, includeOffset: Boolean): Double =
    var value = 0.0
    spatialBasis.coarse match
      case BoldZipCoarseBasis.MatrixBasis(phi) =>
        var atom = 0
        while atom < phi.cols do
          value += phi(sample, atom) * coarseAtomValue(atom, time)
          atom += 1
      case BoldZipCoarseBasis.Absent =>
        ()

    spatialBasis.detail match
      case BoldZipDetailBasis.MatrixBasis(phi) =>
        var atom = 0
        while atom < phi.cols do
          value += phi(sample, atom) * detailAtomValue(atom, time, includeEvents)
          atom += 1
      case BoldZipDetailBasis.IdentitySamples =>
        value += detailAtomValue(sample, time, includeEvents)

    if includeOffset then value += offset.fold(0.0)(_(sample))
    value

  private def sampleValueFromCarrierColumn(
      sample: Int,
      column: Int,
      carriersByColumn: DMat,
      includeEvents: Boolean,
      includeOffset: Boolean
  ): Double =
    var value = 0.0
    spatialBasis.coarse match
      case BoldZipCoarseBasis.MatrixBasis(phi) =>
        var atom = 0
        while atom < phi.cols do
          value += phi(sample, atom) * coarseAtomValueFromCarrierColumn(atom, column, carriersByColumn)
          atom += 1
      case BoldZipCoarseBasis.Absent =>
        ()

    spatialBasis.detail match
      case BoldZipDetailBasis.MatrixBasis(phi) =>
        var atom = 0
        while atom < phi.cols do
          value += phi(sample, atom) *
            detailAtomValueFromCarrierColumn(atom, column, carriersByColumn, includeEvents)
          atom += 1
      case BoldZipDetailBasis.IdentitySamples =>
        value += detailAtomValueFromCarrierColumn(sample, column, carriersByColumn, includeEvents)

    if includeOffset then value += offset.fold(0.0)(_(sample))
    value

  private def coarseAtomValue(atom: Int, time: Int): Double =
    var sum = 0.0
    var carrier = 0
    while carrier < shape.coefficients do
      sum += carrierLoadings(atom, carrier) * carrierValue(carrier, time)
      carrier += 1
    sum

  private def coarseAtomValueFromCarrierColumn(atom: Int, column: Int, carriersByColumn: DMat): Double =
    var sum = 0.0
    var carrier = 0
    while carrier < shape.coefficients do
      sum += carrierLoadings(atom, carrier) * carriersByColumn(carrier, column)
      carrier += 1
    sum

  private def detailAtomValue(atom: Int, time: Int, includeEvents: Boolean): Double =
    var sum = 0.0
    var i = 0
    while i < texture.length do
      val entry = texture(i)
      if entry.atom.value == atom then
        sum += entry.amplitude.value * laggedCarrierValue(entry.carrier.value, time, entry.lag.value)
      i += 1
    if includeEvents then sum += eventValue(atom, time)
    sum

  private def detailAtomValueFromCarrierColumn(
      atom: Int,
      column: Int,
      carriersByColumn: DMat,
      includeEvents: Boolean
  ): Double =
    var sum = 0.0
    var i = 0
    while i < texture.length do
      val entry = texture(i)
      if entry.atom.value == atom then
        val sourceColumn = column - entry.lag.value
        if sourceColumn >= 0 && sourceColumn < carriersByColumn.cols then
          sum += entry.amplitude.value * carriersByColumn(entry.carrier.value, sourceColumn)
      i += 1
    if includeEvents then sum += eventValue(atom, column)
    sum

  private def carrierValue(carrier: Int, time: Int): Double =
    var sum = 0.0
    var component = 0
    while component < temporalBasis.cols do
      sum += carrierTheta(carrier, component) * temporalBasis(time, component)
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
      val start = event.frame.value
      val stop = start + event.duration.value
      if event.atom.value == atom && time >= start && time < stop then
        sum += event.amplitude.value
      i += 1
    sum

object BoldZipPayload:
  def apply(
      temporalBasis: DMat,
      carrierTheta: DMat,
      carrierLoadings: DMat,
      spatialBasis: BoldZipSpatialBasis,
      texture: Vector[BoldZipTextureEntry] = Vector.empty,
      events: Vector[BoldZipResidualEvent] = Vector.empty,
      offset: Option[DVec] = None,
      sourceDomain: DomainId = DomainId.unsafe("boldzip.carriers"),
      targetDomain: DomainId = DomainId.unsafe("boldzip.samples"),
      label: String = "boldzip_sr",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, BoldZipPayload] =
    for
      _ <- validate(temporalBasis, carrierTheta, carrierLoadings, spatialBasis, texture, events, offset)
      annotation <- LatentAnnotation(
        label,
        metadata ++ Map(
          "family" -> "boldzip_sr",
          "codec_orientation" -> "samples_x_time",
          "orientation" -> "time_x_samples",
          "coarse_basis" -> spatialBasis.coarse.metadataValue,
          "detail_basis" -> spatialBasis.detail.metadataValue
        )
      )
    yield
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
        latentLabel = annotation.label,
        typedMetadata = annotation.metadata
      )

  private def validate(
      temporalBasis: DMat,
      carrierTheta: DMat,
      carrierLoadings: DMat,
      spatialBasis: BoldZipSpatialBasis,
      texture: Vector[BoldZipTextureEntry],
      events: Vector[BoldZipResidualEvent],
      offset: Option[DVec]
  ): Either[LatentError, Unit] =
    if temporalBasis.rows <= 0 then Left(LatentError.NonPositiveDimension("temporal basis rows", temporalBasis.rows))
    else if temporalBasis.cols <= 0 then Left(LatentError.NonPositiveDimension("temporal basis columns", temporalBasis.cols))
    else if carrierTheta.rows <= 0 then Left(LatentError.NonPositiveDimension("carrier rows", carrierTheta.rows))
    else if carrierTheta.cols != temporalBasis.cols then
      Left(LatentError.DimensionMismatch("carrier theta columns", temporalBasis.cols, carrierTheta.cols))
    else if carrierLoadings.cols != carrierTheta.rows then
      Left(LatentError.DimensionMismatch("carrier loading columns", carrierTheta.rows, carrierLoadings.cols))
    else if carrierLoadings.rows != spatialBasis.coarseAtoms then
      Left(LatentError.DimensionMismatch("carrier loading rows", spatialBasis.coarseAtoms, carrierLoadings.rows))
    else
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
      if entry.atom.value >= detailAtoms then
        error = Some(LatentError.IndexOutOfBounds("texture atom", entry.atom.value, detailAtoms))
      else if entry.carrier.value >= carriers then
        error = Some(LatentError.IndexOutOfBounds("texture carrier", entry.carrier.value, carriers))
      else if math.abs(entry.lag.value) >= timepoints then
        error = Some(LatentError.IndexOutOfBounds("texture lag", entry.lag.value, timepoints))
      else if !entry.amplitude.value.isFinite then
        error = Some(LatentError.NonFiniteValue("texture amplitude", textureIndex, entry.amplitude.value))
      textureIndex += 1

    var eventIndex = 0
    while eventIndex < events.length && error.isEmpty do
      val event = events(eventIndex)
      val frame = event.frame.value
      val duration = event.duration.value
      if event.atom.value >= detailAtoms then
        error = Some(LatentError.IndexOutOfBounds("event atom", event.atom.value, detailAtoms))
      else if frame >= timepoints then
        error = Some(LatentError.IndexOutOfBounds("event frame", frame, timepoints))
      else if frame + duration > timepoints then
        error = Some(LatentError.IndexOutOfBounds("event duration", frame + duration - 1, timepoints))
      else if !event.amplitude.value.isFinite then
        error = Some(LatentError.NonFiniteValue("event amplitude", eventIndex, event.amplitude.value))
      eventIndex += 1

    error match
      case Some(value) => Left(value)
      case None        => Right(())

  private def validateOffset(sampleCount: Int, offset: Option[DVec]): Either[LatentError, Unit] =
    offset match
      case Some(value) if value.length != sampleCount =>
        Left(LatentError.DimensionMismatch("offset length", sampleCount, value.length))
      case _ =>
        Right(())

  private def validateFinite(
      temporalBasis: DMat,
      carrierTheta: DMat,
      carrierLoadings: DMat,
      spatialBasis: BoldZipSpatialBasis,
      texture: Vector[BoldZipTextureEntry],
      events: Vector[BoldZipResidualEvent],
      offset: Option[DVec]
  ): Either[LatentError, Unit] =
    firstNonFinite("temporal basis", temporalBasis)
      .orElse(firstNonFinite("carrier theta", carrierTheta))
      .orElse(firstNonFinite("carrier loadings", carrierLoadings))
      .orElse(spatialBasis.coarse.matrix.flatMap(firstNonFinite("coarse basis", _)))
      .orElse(spatialBasis.detail.matrix.flatMap(firstNonFinite("detail basis", _)))
      .orElse(offset.flatMap(firstNonFinite("offset", _))) match
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

private def nonNegativeIndex(label: String, value: Int): Either[LatentError, Int] =
  if value < 0 then Left(LatentError.NegativeIndex(label, value))
  else Right(value)
