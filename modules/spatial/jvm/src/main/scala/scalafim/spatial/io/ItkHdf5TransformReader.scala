package scalafim.spatial.io

import gale.backend.Backend.given
import gale.linalg.{DMat as GaleDMat}
import image4s.geometry.{Affine, D3, Frame}
import io.jhdf.HdfFile
import io.jhdf.api.{Dataset, Group}
import ravel.NDArray as RavelArray
import ravel.Rank
import reframe4s.field.CoordinateBoundaryPolicy
import scalafim.image.{GridSpec, Resample, SpatialPullbacks}
import scalafim.spatial.{CoordinateMap, SpatialError}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

final case class ItkHdf5Metadata(
  itkVersion: Option[String],
  hdfVersion: Option[String],
  osName: Option[String],
  osVersion: Option[String]
)

final case class ItkHdf5ComponentProvenance(
  index: Int,
  transformType: String,
  parameterCount: Int,
  fixedParameterCount: Int,
  parameterDataset: Option[String],
  fixedParameterDataset: Option[String]
):
  def usesLegacyDatasetAlias: Boolean =
    parameterDataset.contains("TranformParameters") ||
      fixedParameterDataset.contains("TranformFixedParameters")

final case class ItkHdf5ContainerProvenance(
  metadata: ItkHdf5Metadata,
  components: Vector[ItkHdf5ComponentProvenance]
):
  def usesLegacyDatasetAliases: Boolean =
    components.exists(_.usesLegacyDatasetAlias)

private[io] sealed trait ItkHdf5NumericValues:
  def length: Int
  def apply(index: Int): Double

  final def copyRange(from: Int, until: Int): Array[Double] =
    val out = Array.ofDim[Double](until - from)
    var index = from
    while index < until do
      out(index - from) = apply(index)
      index += 1
    out

private[io] object ItkHdf5NumericValues:
  final case class Doubles(private val values: Array[Double]) extends ItkHdf5NumericValues:
    def length: Int = values.length
    def apply(index: Int): Double = values(index)

  final case class Floats(private val values: Array[Float]) extends ItkHdf5NumericValues:
    def length: Int = values.length
    def apply(index: Int): Double = values(index).toDouble

private[io] final case class ItkHdf5NumericDataset(name: String, values: ItkHdf5NumericValues)

private[io] final case class ItkHdf5Component(
  index: Int,
  transformType: String,
  parameters: Option[ItkHdf5NumericDataset],
  fixedParameters: Option[ItkHdf5NumericDataset]
):
  def provenance: ItkHdf5ComponentProvenance =
    ItkHdf5ComponentProvenance(
      index,
      transformType,
      parameters.fold(0)(_.values.length),
      fixedParameters.fold(0)(_.values.length),
      parameters.map(_.name),
      fixedParameters.map(_.name)
    )

private[io] final case class ItkHdf5TransformFile(
  metadata: ItkHdf5Metadata,
  components: Vector[ItkHdf5Component]
):
  def provenance: ItkHdf5ContainerProvenance =
    ItkHdf5ContainerProvenance(metadata, components.map(_.provenance))

private[io] object ItkHdf5TransformReader:
  private val ParameterNames = Vector("TransformParameters", "TranformParameters")
  private val FixedParameterNames = Vector("TransformFixedParameters", "TranformFixedParameters")

  def read(path: Path): Either[SpatialIoError, ItkHdf5TransformFile] =
    if !Files.isRegularFile(path) then Left(SpatialIoError.IoFailure(path, "file does not exist"))
    else
      try
        val hdf = new HdfFile(path)
        try
          for
            transformGroup <- requiredGroup(hdf, "/TransformGroup", path)
            components <- readComponents(transformGroup, path)
            metadata <- readMetadata(hdf, path)
          yield ItkHdf5TransformFile(metadata, components)
        finally hdf.close()
      catch
        case NonFatal(error) =>
          Left(SpatialIoError.MalformedTransformAsset(path, detail(error)))

  private def readMetadata(hdf: HdfFile, path: Path): Either[SpatialIoError, ItkHdf5Metadata] =
    for
      itk <- optionalStringDataset(hdf, "/ITKVersion", path)
      hdfVersion <- optionalStringDataset(hdf, "/HDFVersion", path)
      osName <- optionalStringDataset(hdf, "/OSName", path)
      osVersion <- optionalStringDataset(hdf, "/OSVersion", path)
    yield ItkHdf5Metadata(itk, hdfVersion, osName, osVersion)

  private def readComponents(group: Group, path: Path): Either[SpatialIoError, Vector[ItkHdf5Component]] =
    val indexed = Vector.newBuilder[(Int, Group)]
    var error = Option.empty[SpatialIoError]
    group.getChildren.asScala.toVector.foreach { case (name, node) =>
      if error.isEmpty then
        name.toIntOption match
          case None =>
            error = Some(SpatialIoError.MalformedTransformAsset(path, s"TransformGroup child '$name' is not numerically indexed"))
          case Some(index) if index < 0 =>
            error = Some(SpatialIoError.MalformedTransformAsset(path, s"TransformGroup index must be non-negative, got $index"))
          case Some(index) =>
            node match
              case child: Group => indexed += index -> child
              case _ =>
                error = Some(SpatialIoError.MalformedTransformAsset(path, s"TransformGroup/$name is not a group"))
    }

    error match
      case Some(err) => Left(err)
      case None =>
        val ordered = indexed.result().sortBy(_._1)
        val indices = ordered.map(_._1)
        if ordered.isEmpty then
          Left(SpatialIoError.MalformedTransformAsset(path, "TransformGroup contains no transform components"))
        else if indices.distinct.length != indices.length then
          Left(SpatialIoError.MalformedTransformAsset(path, "TransformGroup contains duplicate numeric indices"))
        else if indices != Vector.tabulate(indices.length)(identity) then
          Left(
            SpatialIoError.MalformedTransformAsset(
              path,
              s"TransformGroup indices must be contiguous from zero, got ${indices.mkString(",")}"
            )
          )
        else collectComponents(ordered, path)

  private def collectComponents(
    ordered: Vector[(Int, Group)],
    path: Path
  ): Either[SpatialIoError, Vector[ItkHdf5Component]] =
    val out = Vector.newBuilder[ItkHdf5Component]
    var position = 0
    var error = Option.empty[SpatialIoError]
    while position < ordered.length && error.isEmpty do
      val (index, group) = ordered(position)
      readComponent(index, group, path) match
        case Left(err) => error = Some(err)
        case Right(component) => out += component
      position += 1
    error match
      case Some(err) => Left(err)
      case None => Right(out.result())

  private def readComponent(
    index: Int,
    group: Group,
    path: Path
  ): Either[SpatialIoError, ItkHdf5Component] =
    for
      transformType <- requiredStringDataset(group, "TransformType", path, index)
      parameters <- optionalNumericDataset(group, ParameterNames, path, index)
      fixed <- optionalNumericDataset(group, FixedParameterNames, path, index)
    yield ItkHdf5Component(index, transformType, parameters, fixed)

  private def requiredGroup(hdf: HdfFile, hdfPath: String, path: Path): Either[SpatialIoError, Group] =
    hdf.getByPath(hdfPath) match
      case group: Group => Right(group)
      case null => Left(SpatialIoError.MalformedTransformAsset(path, s"missing HDF5 group $hdfPath"))
      case other => Left(SpatialIoError.MalformedTransformAsset(path, s"HDF5 path $hdfPath is ${other.getClass.getName}, not a group"))

  private def requiredStringDataset(
    group: Group,
    name: String,
    path: Path,
    index: Int
  ): Either[SpatialIoError, String] =
    group.getChild(name) match
      case dataset: Dataset => stringValue(dataset, path, s"/TransformGroup/$index/$name")
      case null => Left(SpatialIoError.MalformedTransformAsset(path, s"missing /TransformGroup/$index/$name"))
      case other => Left(SpatialIoError.MalformedTransformAsset(path, s"/TransformGroup/$index/$name is ${other.getClass.getName}, not a dataset"))

  private def optionalStringDataset(
    hdf: HdfFile,
    hdfPath: String,
    path: Path
  ): Either[SpatialIoError, Option[String]] =
    try
      hdf.getByPath(hdfPath) match
        case null => Right(None)
        case dataset: Dataset => stringValue(dataset, path, hdfPath).map(Some.apply)
        case other => Left(SpatialIoError.MalformedTransformAsset(path, s"HDF5 path $hdfPath is ${other.getClass.getName}, not a dataset"))
    catch
      case NonFatal(_) => Right(None)

  private def stringValue(dataset: Dataset, path: Path, hdfPath: String): Either[SpatialIoError, String] =
    dataset.getData match
      case value: String if value.nonEmpty => Right(value)
      case values: Array[String] if values.length == 1 && values(0).nonEmpty => Right(values(0))
      case value: String => Left(SpatialIoError.MalformedTransformAsset(path, s"HDF5 string dataset $hdfPath is empty"))
      case values: Array[String] => Left(SpatialIoError.MalformedTransformAsset(path, s"HDF5 string dataset $hdfPath has ${values.length} values"))
      case other => Left(SpatialIoError.MalformedTransformAsset(path, s"HDF5 dataset $hdfPath is not a string: ${other.getClass.getName}"))

  private def optionalNumericDataset(
    group: Group,
    names: Vector[String],
    path: Path,
    index: Int
  ): Either[SpatialIoError, Option[ItkHdf5NumericDataset]] =
    names.find(name => group.getChild(name) != null) match
      case None => Right(None)
      case Some(name) =>
        group.getChild(name) match
          case dataset: Dataset => numericValues(dataset, path, s"/TransformGroup/$index/$name").map(values => Some(ItkHdf5NumericDataset(name, values)))
          case other => Left(SpatialIoError.MalformedTransformAsset(path, s"/TransformGroup/$index/$name is ${other.getClass.getName}, not a dataset"))

  private def numericValues(dataset: Dataset, path: Path, hdfPath: String): Either[SpatialIoError, ItkHdf5NumericValues] =
    dataset.getDataFlat match
      case values: Array[Double] => Right(ItkHdf5NumericValues.Doubles(values))
      case values: Array[Float] => Right(ItkHdf5NumericValues.Floats(values))
      case values: Array[Long] => Right(ItkHdf5NumericValues.Doubles(values.map(_.toDouble)))
      case values: Array[Int] => Right(ItkHdf5NumericValues.Doubles(values.map(_.toDouble)))
      case values: Array[Short] => Right(ItkHdf5NumericValues.Doubles(values.map(_.toDouble)))
      case values: Array[Byte] => Right(ItkHdf5NumericValues.Doubles(values.map(_.toDouble)))
      case other => Left(SpatialIoError.MalformedTransformAsset(path, s"HDF5 dataset $hdfPath is not numeric: ${other.getClass.getName}"))

  private def detail(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)

private[io] final case class DecodedItkHdf5Transform(
  coordinateMap: CoordinateMap,
  provenance: ItkHdf5ContainerProvenance
)

private[io] object AntsHdf5TransformAdapter:
  private val CompositePattern = "CompositeTransform_(?:float|double)_3_3".r
  private val AffinePattern = "(?:AffineTransform|MatrixOffsetTransformBase)_(?:float|double)_3_3".r
  private val DisplacementPattern = "DisplacementFieldTransform_(?:float|double)_3_3".r

  private val LpsToRas =
    GaleDMat.dense(
      4,
      4,
      Vector(
        -1.0, 0.0, 0.0, 0.0,
        0.0, -1.0, 0.0, 0.0,
        0.0, 0.0, 1.0, 0.0,
        0.0, 0.0, 0.0, 1.0
      )
    )

  def read(
    path: Path,
    sourceGrid: GridSpec,
    targetGrid: GridSpec,
    interpolation: Resample.Method
  ): Either[SpatialIoError, DecodedItkHdf5Transform] =
    ItkHdf5TransformReader.read(path).flatMap(
      decode(path, _, sourceGrid, targetGrid, interpolation)
    )

  private def decode(
    path: Path,
    file: ItkHdf5TransformFile,
    sourceGrid: GridSpec,
    targetGrid: GridSpec,
    interpolation: Resample.Method
  ): Either[SpatialIoError, DecodedItkHdf5Transform] =
    val executableCount = file.components.count(component => CompositePattern.findFirstIn(component.transformType).isEmpty)
    val intermediateFrames = Vector.newBuilder[Frame[D3]]
    var boundaryIndex = 1
    var boundaryError = Option.empty[SpatialIoError]
    while boundaryIndex < executableCount && boundaryError.isEmpty do
      Frame.named[D3](s"itk-hdf5-${path.getFileName}-stage-$boundaryIndex") match
        case Left(cause) => boundaryError = Some(SpatialIoError.Geometry(path, cause))
        case Right(frame) => intermediateFrames += frame
      boundaryIndex += 1
    val boundaries = Vector(targetGrid.providerFrame) ++ intermediateFrames.result() ++ Vector(sourceGrid.providerFrame)
    val maps = Vector.newBuilder[CoordinateMap]
    var position = 0
    var executablePosition = 0
    var markerSeen = false
    var error = boundaryError
    while position < file.components.length && error.isEmpty do
      val component = file.components(position)
      component.transformType match
        case CompositePattern() =>
          if component.index != 0 || markerSeen then
            error = Some(
              SpatialIoError.MalformedTransformAsset(path, "the optional CompositeTransform marker must occur exactly once at index zero")
            )
          else markerSeen = true
        case AffinePattern() =>
          val inputFrame = boundaries(executableCount - 1 - executablePosition)
          val outputFrame = boundaries(executableCount - executablePosition)
          decodeAffine(path, component, outputFrame, inputFrame) match
            case Left(err) => error = Some(err)
            case Right(map) => maps += map
          executablePosition += 1
        case DisplacementPattern() =>
          val inputFrame = boundaries(executableCount - 1 - executablePosition)
          val outputFrame = boundaries(executableCount - executablePosition)
          decodeDisplacement(path, component, outputFrame, inputFrame, interpolation) match
            case Left(err) => error = Some(err)
            case Right(map) => maps += map
          executablePosition += 1
        case unsupported =>
          error = Some(SpatialIoError.UnsupportedItkTransformType(path, component.index, unsupported))
      position += 1

    error match
      case Some(err) => Left(err)
      case None =>
        CoordinateMap
          .compose(maps.result().reverse)
          .left
          .map(error => SpatialIoError.MalformedTransformAsset(path, error.message))
          .map(map => DecodedItkHdf5Transform(map, file.provenance))

  private def decodeAffine(
    path: Path,
    component: ItkHdf5Component,
    outputFrame: Frame[D3],
    inputFrame: Frame[D3]
  ): Either[SpatialIoError, CoordinateMap] =
    for
      parameters <- requiredValues(path, component, component.parameters, "TransformParameters", 12)
      fixed <- requiredValues(path, component, component.fixedParameters, "TransformFixedParameters", 3)
      _ <- finiteValues(path, component.index, "affine parameters", parameters)
      _ <- finiteValues(path, component.index, "affine fixed parameters", fixed)
      native = affineMatrix(parameters, fixed)
      ras = LpsToRas * native * LpsToRas
      affine <- Affine
        .fromRowMajor[D3](ras.valuesRowMajor)
        .left
        .map(cause => SpatialIoError.Geometry(path, cause))
      map <- CoordinateMap
        .affineBetween(outputFrame, inputFrame, affine)
        .left
        .map(error => SpatialIoError.MalformedTransformAsset(path, error.message))
    yield CoordinateMap.Geometric(map)

  private def decodeDisplacement(
    path: Path,
    component: ItkHdf5Component,
    outputFrame: Frame[D3],
    inputFrame: Frame[D3],
    interpolation: Resample.Method
  ): Either[SpatialIoError, CoordinateMap] =
    for
      fixed <- requiredValues(path, component, component.fixedParameters, "TransformFixedParameters", 18)
      _ <- finiteValues(path, component.index, "displacement fixed parameters", fixed)
      dims <- displacementDims(path, component.index, fixed)
      count <- voxelCount(path, component.index, dims)
      parameters <- requiredValues(path, component, component.parameters, "TransformParameters", count * 3)
      _ <- finiteValues(path, component.index, "displacement parameters", parameters)
      grid <- displacementGrid(path, component.index, dims, fixed)
      field = displacementField(parameters, grid)
      pullback <- SpatialPullbacks
        .displacementBetween(
          outputFrame,
          inputFrame,
          grid,
          field,
          interpolation,
          CoordinateBoundaryPolicy.PreserveSource
        )
        .left
        .map(error => SpatialIoError.MalformedTransformAsset(path, error.message))
      map <- CoordinateMap
        .dense(pullback)
        .left
        .map(error => SpatialIoError.MalformedTransformAsset(path, error.message))
    yield map

  private def requiredValues(
    path: Path,
    component: ItkHdf5Component,
    dataset: Option[ItkHdf5NumericDataset],
    canonicalName: String,
    expected: Int
  ): Either[SpatialIoError, ItkHdf5NumericValues] =
    dataset match
      case None =>
        Left(SpatialIoError.MalformedTransformAsset(path, s"component ${component.index} is missing $canonicalName"))
      case Some(value) if value.values.length != expected =>
        Left(
          SpatialIoError.MalformedTransformAsset(
            path,
            s"component ${component.index} $canonicalName expected $expected values, got ${value.values.length}"
          )
        )
      case Some(value) => Right(value.values)

  private def finiteValues(
    path: Path,
    index: Int,
    label: String,
    values: ItkHdf5NumericValues
  ): Either[SpatialIoError, Unit] =
    var position = 0
    while position < values.length && values(position).isFinite do position += 1
    if position == values.length then Right(())
    else Left(SpatialIoError.MalformedTransformAsset(path, s"component $index $label contains a non-finite value at $position"))

  private def affineMatrix(parameters: ItkHdf5NumericValues, fixed: ItkHdf5NumericValues): GaleDMat =
    val builder = GaleDMat.newBuilder(4, 4)
    var row = 0
    while row < 3 do
      var col = 0
      var centered = 0.0
      while col < 3 do
        val value = parameters(row * 3 + col)
        builder(row, col) = value
        centered += value * fixed(col)
        col += 1
      builder(row, 3) = parameters(9 + row) + fixed(row) - centered
      row += 1
    builder(3, 0) = 0.0
    builder(3, 1) = 0.0
    builder(3, 2) = 0.0
    builder(3, 3) = 1.0
    builder.result()

  private def displacementDims(
    path: Path,
    index: Int,
    fixed: ItkHdf5NumericValues
  ): Either[SpatialIoError, Vector[Int]] =
    val out = Vector.newBuilder[Int]
    var axis = 0
    var error = Option.empty[SpatialIoError]
    while axis < 3 && error.isEmpty do
      val value = fixed(axis)
      val rounded = math.rint(value)
      if value <= 0.0 || value > Int.MaxValue.toDouble || math.abs(value - rounded) > 1e-9 then
        error = Some(SpatialIoError.MalformedTransformAsset(path, s"component $index displacement size[$axis] is not a positive integer: $value"))
      else out += rounded.toInt
      axis += 1
    error match
      case Some(err) => Left(err)
      case None => Right(out.result())

  private def voxelCount(path: Path, index: Int, dims: Vector[Int]): Either[SpatialIoError, Int] =
    val count = dims.foldLeft(1L)(_ * _.toLong)
    if count > Int.MaxValue.toLong / 3L then
      Left(SpatialIoError.MalformedTransformAsset(path, s"component $index displacement field is too large: ${dims.mkString("x")}"))
    else Right(count.toInt)

  private def displacementGrid(
    path: Path,
    index: Int,
    dims: Vector[Int],
    fixed: ItkHdf5NumericValues
  ): Either[SpatialIoError, GridSpec] =
    val origin = fixed.copyRange(3, 6)
    val spacing = fixed.copyRange(6, 9)
    if spacing.exists(value => !value.isFinite || value <= 0.0) then
      Left(SpatialIoError.MalformedTransformAsset(path, s"component $index displacement spacing must be finite and positive"))
    else
      val direction = fixed.copyRange(9, 18)
      val determinant = determinant3(direction)
      if !determinant.isFinite || math.abs(determinant) <= 1e-10 then
        Left(SpatialIoError.MalformedTransformAsset(path, s"component $index displacement direction is singular"))
      else
        val native = GaleDMat.newBuilder(4, 4)
        var row = 0
        while row < 3 do
          var col = 0
          while col < 3 do
            native(row, col) = direction(row * 3 + col) * spacing(col)
            col += 1
          native(row, 3) = origin(row)
          row += 1
        native(3, 0) = 0.0
        native(3, 1) = 0.0
        native(3, 2) = 0.0
        native(3, 3) = 1.0
        val ras = LpsToRas * native.result()
        Affine
          .fromRowMajor[D3](ras.valuesRowMajor)
          .left
          .map(cause => SpatialIoError.Geometry(path, cause))
          .map(affine => GridSpec(dims, affine))

  private def displacementField(
    parameters: ItkHdf5NumericValues,
    grid: GridSpec
  ): RavelArray[Double, Rank[4]] =
    val nx = grid.shape.x
    val ny = grid.shape.y
    RavelArray.tabulate[Double](nx, ny, grid.shape.z, 3) {
      (x, y, z, component) =>
        val voxel = x + nx * (y + ny * z)
        val value = parameters(voxel * 3 + component)
        if component < 2 then -value else value
    }

  private def determinant3(values: Array[Double]): Double =
    values(0) * (values(4) * values(8) - values(5) * values(7)) -
      values(1) * (values(3) * values(8) - values(5) * values(6)) +
      values(2) * (values(3) * values(7) - values(4) * values(6))
