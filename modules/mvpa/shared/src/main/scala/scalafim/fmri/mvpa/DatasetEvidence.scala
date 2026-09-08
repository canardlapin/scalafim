package scalafim.fmri.mvpa

import cats.Monad
import cats.data.EitherT
import gale.linalg.Matrix
import multivar.core.ValueId
import scalafim.dataset.*

enum DatasetEvidenceError:
  case DatasetRead(dataset: DatasetId, error: DatasetError)
  case SeriesShapeMismatch(dataset: DatasetId, expected: DatasetShape, actual: DatasetShape)
  case TimepointOutsideDataset(dataset: DatasetId, timepoint: TimepointIndex)
  case VoxelOutsideDataset(dataset: DatasetId, voxel: VoxelIndex)
  case Identity(error: AxisIdentityError)
  case Axis(error: AxisRefError)
  case Evidence(error: EvidenceTableError)
  case Observations(error: ObservationsError)

  def message: String =
    this match
      case DatasetRead(dataset, error) =>
        s"could not read dataset '${dataset.value}': ${error.message}"
      case SeriesShapeMismatch(dataset, expected, actual) =>
        s"series shape $actual does not match dataset '${dataset.value}' shape $expected"
      case TimepointOutsideDataset(dataset, timepoint) =>
        s"timepoint ${timepoint.value} is not in dataset '${dataset.value}'"
      case VoxelOutsideDataset(dataset, voxel) =>
        s"voxel ${voxel.value} is not readable in dataset '${dataset.value}'"
      case Identity(error)     => error.message
      case Axis(error)         => error.message
      case Evidence(error)     => error.message
      case Observations(error) => error.message

/** A dataset read lowered directly into the identified MVPA evidence core.
  *
  * `timepoints` and `voxels` retain the typed coordinates selected by the dataset boundary. Targets and metadata are
  * ordinary [[Column]] values built against `samples`; no second experiment-table algebra is introduced here.
  */
final class DatasetEvidence private (
    val datasetId: DatasetId,
    val samples: AxisRef[SampleId],
    val features: AxisRef[FeatureId],
    val timepoints: Vector[TimepointIndex],
    val voxels: Vector[VoxelIndex]
)(
    val observations: Observations[samples.Id, features.Id, FeatureId]
):
  require(timepoints.length == samples.size, "timepoint coordinates must match the sample axis")
  require(voxels.length == features.size, "voxel coordinates must match the feature axis")

object DatasetEvidence:
  private val Protocol = "scalafim-mvpa-dataset-evidence/v1"

  /** Lower an already resolved series. The dataset descriptor supplies the authoritative acquisition and spatial
    * coordinate systems.
    */
  def fromSeries(
      dataset: FmriDataset,
      series: FmriSeries
  ): Either[DatasetEvidenceError, DatasetEvidence] =
    for
      _ <- validateSeries(dataset, series)
      samples <- sampleAxis(dataset, series)
      features <- featureAxis(dataset, series)
      patterns <- EvidenceTable
        .dense(
          samples,
          features,
          copyPatterns(series),
          valueId(dataset, series, samples, features)
        )
        .left
        .map(DatasetEvidenceError.Evidence.apply)
      observations <- Observations(patterns).left
        .map(DatasetEvidenceError.Observations.apply)
    yield new DatasetEvidence(
      dataset.id,
      samples,
      features,
      series.timepointIndices,
      series.voxelIndexValues
    )(observations)

  def fromReader(
      reader: DatasetSeriesReader,
      selection: DataSelection = DataSelection.All
  ): Either[DatasetEvidenceError, DatasetEvidence] =
    reader
      .seriesEither(selection)
      .left
      .map(DatasetEvidenceError.DatasetRead(reader.dataset.id, _))
      .flatMap(fromSeries(reader.dataset, _))

  def fromDataset(
      dataset: FmriDataset,
      selection: DataSelection = DataSelection.All
  ): Either[DatasetEvidenceError, DatasetEvidence] =
    SynchronousFmriDataset
      .readerFor(dataset)
      .left
      .map(DatasetEvidenceError.DatasetRead(dataset.id, _))
      .flatMap(fromReader(_, selection))

  def fromOpened[F[_]: Monad](
      opened: OpenedDataset[F],
      query: DatasetRunQuery = DatasetRunQuery.All,
      selection: DataSelection = DataSelection.All
  ): EitherT[F, DatasetEvidenceError, DatasetEvidence] =
    opened
      .read(query, selection, AssemblyPolicy.Segmented)
      .leftMap(DatasetEvidenceError.DatasetRead(opened.dataset.id, _))
      .subflatMap: result =>
        result
          .toFmriSeries(opened.dataset)
          .left
          .map(DatasetEvidenceError.DatasetRead(opened.dataset.id, _))
      .subflatMap(fromSeries(opened.dataset, _))

  private def validateSeries(
      dataset: FmriDataset,
      series: FmriSeries
  ): Either[DatasetEvidenceError, Unit] =
    if series.shape != dataset.shape then
      Left(
        DatasetEvidenceError.SeriesShapeMismatch(
          dataset.id,
          dataset.shape,
          series.shape
        )
      )
    else
      series.timepointIndices.find(index => dataset.timeAxis.blockFor(index).isLeft) match
        case Some(index) =>
          Left(DatasetEvidenceError.TimepointOutsideDataset(dataset.id, index))
        case None =>
          series.voxelIndexValues.find(index => !dataset.voxelDomain.contains(index)) match
            case Some(index) =>
              Left(DatasetEvidenceError.VoxelOutsideDataset(dataset.id, index))
            case None => Right(())

  private def sampleAxis(
      dataset: FmriDataset,
      series: FmriSeries
  ): Either[DatasetEvidenceError, AxisRef[SampleId]] =
    val descriptor = descriptorDigest(dataset)
    for
      basis <- CoordinateBasis(
        "dataset-timepoints",
        Vector(
          "dataset-id" -> dataset.id.value,
          "sampling-frame" -> samplingFrameEncoding(dataset),
          "time-coordinate" -> "global-timepoint-index"
        )
      ).left.map(DatasetEvidenceError.Identity.apply)
      provenance <- CoordinateProvenance(
        "scalafim.dataset",
        descriptor,
        Vector("resolved-fmri-series-timepoints")
      ).left.map(DatasetEvidenceError.Identity.apply)
      axis <- AxisRef
        .create(
          AxisId.unsafe(s"dataset-samples-${descriptor.take(24)}"),
          AxisPurpose.Samples,
          series.timepointIndices.map(index => SampleId.unsafe(s"timepoint:${index.value}")),
          basis,
          None,
          AxisScale.nominal,
          provenance
        )
        .left
        .map(DatasetEvidenceError.Axis.apply)
    yield axis

  private def featureAxis(
      dataset: FmriDataset,
      series: FmriSeries
  ): Either[DatasetEvidenceError, AxisRef[FeatureId]] =
    val descriptor = descriptorDigest(dataset)
    for
      basis <- CoordinateBasis(
        "dataset-voxels",
        Vector(
          "affine" -> affineEncoding(dataset),
          "dataset-id" -> dataset.id.value,
          "dimensions" -> dataset.shape.spatialDims.mkString("x"),
          "voxel-coordinate" -> "linear-grid-index"
        )
      ).left.map(DatasetEvidenceError.Identity.apply)
      provenance <- CoordinateProvenance(
        "scalafim.dataset",
        descriptor,
        Vector("resolved-fmri-series-voxels")
      ).left.map(DatasetEvidenceError.Identity.apply)
      axis <- AxisRef
        .create(
          AxisId.unsafe(s"dataset-features-${descriptor.take(24)}"),
          AxisPurpose.NeuralFeatures,
          series.voxelIndexValues.map(index => FeatureId.unsafe(s"voxel:${index.value}")),
          basis,
          None,
          AxisScale.nominal,
          provenance
        )
        .left
        .map(DatasetEvidenceError.Axis.apply)
    yield axis

  private def copyPatterns(series: FmriSeries): gale.linalg.DMat =
    val builder = Matrix.newBuilder(series.nTimepoints, series.nVoxels)
    var row = 0
    while row < series.nTimepoints do
      var column = 0
      while column < series.nVoxels do
        builder(row, column) = series.data(row, column)
        column += 1
      row += 1
    builder.result()

  private def valueId(
      dataset: FmriDataset,
      series: FmriSeries,
      samples: AxisRef[SampleId],
      features: AxisRef[FeatureId]
  ): ValueId =
    val writer = CanonicalWriter()
    writer.string(Protocol)
    writer.string(dataset.id.value)
    writer.string(samples.identity.fingerprint.value)
    writer.string(features.identity.fingerprint.value)
    writer.int(series.nTimepoints)
    writer.int(series.nVoxels)
    var row = 0
    while row < series.nTimepoints do
      var column = 0
      while column < series.nVoxels do
        writer.double(series.data(row, column))
        column += 1
      row += 1
    ValueId.unsafe(s"dataset-evidence-${AxisDigest.sha256Hex(writer.result())}")

  private def descriptorDigest(dataset: FmriDataset): String =
    val writer = CanonicalWriter()
    writer.string(Protocol)
    writer.string(dataset.id.value)
    writer.int(dataset.shape.timepoints)
    dataset.shape.spatialDims.foreach(writer.int)
    writer.string(affineEncoding(dataset))
    writer.string(samplingFrameEncoding(dataset))
    AxisDigest.sha256Hex(writer.result())

  private def affineEncoding(dataset: FmriDataset): String =
    val transform = dataset.shape.space.trans
    val values = Vector.newBuilder[String]
    values.sizeHint(transform.rows * transform.cols)
    var row = 0
    while row < transform.rows do
      var column = 0
      while column < transform.cols do
        values += java.lang.Double.toHexString(transform(row, column))
        column += 1
      row += 1
    values.result().mkString(",")

  private def samplingFrameEncoding(dataset: FmriDataset): String =
    val frame = dataset.samplingFrame
    val lengths = frame.blockLens.mkString(",")
    val repetitionTimes = frame.tr.map(value => java.lang.Double.toHexString(value.value)).mkString(",")
    val starts = frame.startTime.map(value => java.lang.Double.toHexString(value.value)).mkString(",")
    val precision = java.lang.Double.toHexString(frame.precision.value)
    s"blocks=$lengths;tr=$repetitionTimes;start=$starts;precision=$precision"
