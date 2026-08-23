package scalafim.spatial.io

import gale.backend.Backend.given
import gale.linalg.{DMat as GaleDMat, DVec}
import ravel.NDArray as RavelArray
import ravel.Rank
import ravel.Shape
import scalafim.image.{DMat as ImageDMat, DenseFieldMorphism, GridSpec, NeuroSpace, Resample, SpatialDomainId}
import scalafim.image.io.Nifti
import scalafim.spatial.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scala.util.Using
import scala.util.control.NonFatal

enum TransformDirection:
  case ForwardSourceToTarget, PullbackTargetToSource

enum TransformCoordinateConvention:
  case RasMillimeters, LpsMillimeters, FslScaledVoxel

enum DenseTransformEncoding:
  case Displacement, AbsoluteCoordinates

final case class TransformLoadOptions(
  direction: TransformDirection,
  convention: TransformCoordinateConvention,
  denseEncoding: DenseTransformEncoding = DenseTransformEncoding.Displacement,
  interpolation: Resample.Method = Resample.Method.Linear
)

object TransformLoadOptions:
  def defaults(format: TransformFileFormat): TransformLoadOptions =
    format match
      case TransformFileFormat.AntsAffine =>
        TransformLoadOptions(
          TransformDirection.ForwardSourceToTarget,
          TransformCoordinateConvention.LpsMillimeters
        )
      case TransformFileFormat.ANTsH5 =>
        TransformLoadOptions(
          TransformDirection.PullbackTargetToSource,
          TransformCoordinateConvention.LpsMillimeters
        )
      case TransformFileFormat.AntsDisplacement =>
        TransformLoadOptions(
          TransformDirection.PullbackTargetToSource,
          TransformCoordinateConvention.LpsMillimeters
        )
      case TransformFileFormat.FslFlirt =>
        TransformLoadOptions(
          TransformDirection.ForwardSourceToTarget,
          TransformCoordinateConvention.FslScaledVoxel
        )
      case TransformFileFormat.FslFnirt =>
        TransformLoadOptions(
          TransformDirection.PullbackTargetToSource,
          TransformCoordinateConvention.FslScaledVoxel,
          DenseTransformEncoding.AbsoluteCoordinates
        )
      case TransformFileFormat.AfniAffine =>
        TransformLoadOptions(
          TransformDirection.ForwardSourceToTarget,
          TransformCoordinateConvention.LpsMillimeters
        )
      case TransformFileFormat.AfniWarp =>
        TransformLoadOptions(
          TransformDirection.PullbackTargetToSource,
          TransformCoordinateConvention.LpsMillimeters
        )
      case TransformFileFormat.FreeSurferLta | TransformFileFormat.X5 =>
        TransformLoadOptions(
          TransformDirection.ForwardSourceToTarget,
          TransformCoordinateConvention.RasMillimeters
        )

final case class TransformAssetSpec(
  path: Path,
  format: TransformFileFormat,
  options: TransformLoadOptions
)

object TransformAssetSpec:
  def apply(path: Path, format: TransformFileFormat): TransformAssetSpec =
    new TransformAssetSpec(path, format, TransformLoadOptions.defaults(format))

final case class TransformAssetFingerprint(
  size: Long,
  modifiedMillis: Long,
  sha256: String
)

final case class TransformAssetProvenance(
  primaryPath: Path,
  format: TransformFileFormat,
  tool: TransformTool,
  options: TransformLoadOptions,
  normalization: String,
  fingerprint: TransformAssetFingerprint,
  inverseAsset: Option[(TransformAssetSpec, TransformAssetFingerprint)],
  container: Option[ItkHdf5ContainerProvenance] = None
)

final case class LoadedTransform(
  descriptor: TransformDescriptor,
  morphism: Morphism,
  provenance: TransformAssetProvenance
)

object TransformAssetLoader:
  private final case class LoadedMap(
    coordinateMap: CoordinateMap,
    normalization: String,
    fingerprint: TransformAssetFingerprint
  )

  private val NumberPattern =
    "[-+]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][-+]?[0-9]+)?".r

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

  def load(
    descriptor: TransformDescriptor,
    source: Domain,
    target: Domain,
    options: Option[TransformLoadOptions] = None,
    inverseAsset: Option[TransformAssetSpec] = None
  ): Either[SpatialIoError, LoadedTransform] =
    for
      _ <- validateDomains(descriptor, source, target)
      format <- resolveFormat(descriptor)
      _ <- validateFormat(descriptor, format)
      chosenOptions = options.getOrElse(TransformLoadOptions.defaults(format))
      loaded <-
        if format == TransformFileFormat.ANTsH5 then
          loadHdf5(descriptor, source, target, chosenOptions, inverseAsset)
        else loadStandard(descriptor, source, target, format, chosenOptions, inverseAsset)
    yield loaded

  private def loadStandard(
    descriptor: TransformDescriptor,
    source: Domain,
    target: Domain,
    format: TransformFileFormat,
    chosenOptions: TransformLoadOptions,
    inverseAsset: Option[TransformAssetSpec]
  ): Either[SpatialIoError, LoadedTransform] =
    for
      primary <- loadMap(descriptor.path, format, chosenOptions, descriptor, source, target)
      combined <- attachInverse(primary, descriptor, source, target, inverseAsset)
      morphism <- Morphism
        .build(
          descriptor.id,
          descriptor.source,
          descriptor.target,
          descriptor.kind.morphismKind,
          descriptor.routeTag,
          descriptor.cost,
          descriptor.inverseQuality.toInverse,
          combined._1
        )
        .left
        .map(error => SpatialIoError.InvalidTransformDescriptor(descriptor.id.value, error.message))
      provenance = TransformAssetProvenance(
        descriptor.path.toAbsolutePath.normalize(),
        format,
        descriptor.tool,
        chosenOptions,
        primary.normalization,
        primary.fingerprint,
        combined._2,
        None
      )
    yield LoadedTransform(descriptor, morphism, provenance)

  private def loadHdf5(
    descriptor: TransformDescriptor,
    source: Domain,
    target: Domain,
    options: TransformLoadOptions,
    inverseAsset: Option[TransformAssetSpec]
  ): Either[SpatialIoError, LoadedTransform] =
    for
      _ <- validateHdf5Options(descriptor.path, options)
      primaryRole =
        options.direction match
          case TransformDirection.PullbackTargetToSource => source.id -> target.id
          case TransformDirection.ForwardSourceToTarget => target.id -> source.id
      primary <- AntsHdf5TransformAdapter.read(
        descriptor.path,
        primaryRole._1,
        primaryRole._2,
        options.interpolation,
        descriptor.cost
      )
      primaryStamp <- fingerprint(descriptor.path)
      assembled <- assembleHdf5CoordinateMap(descriptor, source, target, options, primary, inverseAsset)
      _ <- requireDeclaredCompositeInverse(descriptor, assembled._1)
      morphism <- Morphism
        .build(
          descriptor.id,
          descriptor.source,
          descriptor.target,
          descriptor.kind.morphismKind,
          descriptor.routeTag,
          descriptor.cost,
          descriptor.inverseQuality.toInverse,
          assembled._1
        )
        .left
        .map(error => SpatialIoError.InvalidTransformDescriptor(descriptor.id.value, error.message))
      provenance = TransformAssetProvenance(
        descriptor.path.toAbsolutePath.normalize(),
        TransformFileFormat.ANTsH5,
        descriptor.tool,
        options,
        assembled._3,
        primaryStamp,
        assembled._2,
        Some(primary.provenance)
      )
    yield LoadedTransform(descriptor, morphism, provenance)

  private def assembleHdf5CoordinateMap(
    descriptor: TransformDescriptor,
    source: Domain,
    target: Domain,
    options: TransformLoadOptions,
    primary: DecodedItkHdf5Transform,
    inverseAsset: Option[TransformAssetSpec]
  ): Either[
    SpatialIoError,
    (CoordinateMap, Option[(TransformAssetSpec, TransformAssetFingerprint)], String)
  ] =
    options.direction match
      case TransformDirection.PullbackTargetToSource =>
        inverseAsset match
          case None =>
            Right(
              (
                primary.coordinateMap,
                None,
                "ITK-HDF5:LPS-mm-pullback->ordered-RAS-mm-composite;affine=gale.linalg.DMat"
              )
            )
          case Some(asset) =>
            for
              _ <- validateHdf5InverseAsset(descriptor, asset)
              _ <- validateHdf5Options(asset.path, asset.options)
              inverse <- AntsHdf5TransformAdapter.read(
                asset.path,
                target.id,
                source.id,
                asset.options.interpolation,
                descriptor.cost
              )
              map <- attachCompositeInverse(descriptor, primary.coordinateMap, inverse.coordinateMap)
              stamp <- fingerprint(asset.path)
            yield
              (
                map,
                Some(asset -> stamp),
                "ITK-HDF5:LPS-mm-pullback->ordered-RAS-mm-composite;explicit-inverse-asset;affine=gale.linalg.DMat"
              )
      case TransformDirection.ForwardSourceToTarget =>
        inverseAsset match
          case Some(asset) =>
            for
              _ <- validateHdf5InverseAsset(descriptor, asset)
              _ <- validateHdf5Options(asset.path, asset.options)
              inverse <- AntsHdf5TransformAdapter.read(
                asset.path,
                source.id,
                target.id,
                asset.options.interpolation,
                descriptor.cost
              )
              map <- attachCompositeInverse(descriptor, inverse.coordinateMap, primary.coordinateMap)
              stamp <- fingerprint(asset.path)
            yield
              (
                map,
                Some(asset -> stamp),
                "ITK-HDF5:forward-LPS-mm-composite+inverse-asset->ordered-RAS-mm-pullback;affine=gale.linalg.DMat"
              )
          case None =>
            primary.coordinateMap.inverted
              .left
              .map(_ =>
                SpatialIoError.TransformConventionMismatch(
                  descriptor.path,
                  "a forward HDF5 composite containing a nonlinear component requires an inverse HDF5 asset"
                )
              )
              .map(map =>
                (
                  map,
                  None,
                  "ITK-HDF5:forward-LPS-mm-affine-composite->exact-RAS-mm-pullback;affine=gale.linalg.DMat"
                )
              )

  private def validateHdf5Options(
    path: Path,
    options: TransformLoadOptions
  ): Either[SpatialIoError, Unit] =
    if options.convention != TransformCoordinateConvention.LpsMillimeters then
      Left(
        SpatialIoError.TransformConventionMismatch(
          path,
          s"ITK HDF5 transforms are encoded in LPS millimeters, not ${options.convention}"
        )
      )
    else if options.denseEncoding != DenseTransformEncoding.Displacement then
      Left(
        SpatialIoError.TransformConventionMismatch(
          path,
          s"ITK DisplacementFieldTransform parameters are displacements, not ${options.denseEncoding}"
        )
      )
    else Right(())

  private def validateHdf5InverseAsset(
    descriptor: TransformDescriptor,
    asset: TransformAssetSpec
  ): Either[SpatialIoError, Unit] =
    if asset.format == TransformFileFormat.ANTsH5 then Right(())
    else
      Left(
        SpatialIoError.InvalidTransformDescriptor(
          descriptor.id.value,
          s"an HDF5 composite requires an HDF5 inverse asset, got ${asset.format}"
        )
      )

  private def attachCompositeInverse(
    descriptor: TransformDescriptor,
    primary: CoordinateMap,
    inverse: CoordinateMap
  ): Either[SpatialIoError, CoordinateMap] =
    (primary, inverse) match
      case (CoordinateMap.Composite3D(forward), CoordinateMap.Composite3D(reverse)) =>
        CoordinateMap
          .composite3D(forward.components, Some(reverse.components))
          .left
          .map(error => SpatialIoError.InvalidTransformDescriptor(descriptor.id.value, error.message))
      case _ =>
        Left(
          SpatialIoError.InvalidTransformDescriptor(
            descriptor.id.value,
            "HDF5 decoding did not produce ordered composite coordinate maps"
          )
        )

  private def requireDeclaredCompositeInverse(
    descriptor: TransformDescriptor,
    coordinateMap: CoordinateMap
  ): Either[SpatialIoError, Unit] =
    coordinateMap match
      case CoordinateMap.Composite3D(map)
          if descriptor.inverseQuality.declared && map.containsDense && map.inverseComponents.isEmpty =>
        Left(
          SpatialIoError.InvalidTransformDescriptor(
            descriptor.id.value,
            s"${descriptor.inverseQuality} requires an executable inverse HDF5 asset for a nonlinear composite"
          )
        )
      case _ => Right(())

  private def validateDomains(
    descriptor: TransformDescriptor,
    source: Domain,
    target: Domain
  ): Either[SpatialIoError, Unit] =
    if descriptor.source != source.id || descriptor.target != target.id then
      Left(
        SpatialIoError.InvalidTransformDescriptor(
          descriptor.id.value,
          s"descriptor route ${descriptor.source.value}->${descriptor.target.value} does not match supplied domains ${source.id.value}->${target.id.value}"
        )
      )
    else if source.kind != DomainKind.Volume || target.kind != DomainKind.Volume then
      Left(
        SpatialIoError.InvalidTransformDescriptor(
          descriptor.id.value,
          "ANTs, FSL, and AFNI asset adapters currently require volume domains"
        )
      )
    else Right(())

  private def resolveFormat(descriptor: TransformDescriptor): Either[SpatialIoError, TransformFileFormat] =
    descriptor.format.orElse(TransformFileFormat.detect(descriptor.path)) match
      case Some(format) => Right(format)
      case None =>
        Left(
          SpatialIoError.InvalidTransformDescriptor(
            descriptor.id.value,
            s"cannot infer transform format from ${descriptor.path}; use TransformDescriptor.fromFile"
          )
        )

  private def validateFormat(
    descriptor: TransformDescriptor,
    format: TransformFileFormat
  ): Either[SpatialIoError, Unit] =
    if descriptor.tool != format.tool then
      Left(
        SpatialIoError.InvalidTransformDescriptor(
          descriptor.id.value,
          s"declared tool ${descriptor.tool} does not match $format (${format.tool})"
        )
      )
    else if descriptor.kind != format.defaultKind then
      Left(
        SpatialIoError.InvalidTransformDescriptor(
          descriptor.id.value,
          s"declared kind ${descriptor.kind} does not match $format (${format.defaultKind})"
        )
      )
    else Right(())

  private def attachInverse(
    primary: LoadedMap,
    descriptor: TransformDescriptor,
    source: Domain,
    target: Domain,
    inverseAsset: Option[TransformAssetSpec]
  ): Either[SpatialIoError, (CoordinateMap, Option[(TransformAssetSpec, TransformAssetFingerprint)])] =
    primary.coordinateMap match
      case CoordinateMap.Dense3D(primaryDense) =>
        inverseAsset match
          case None if descriptor.inverseQuality.declared =>
            Left(
              SpatialIoError.InvalidTransformDescriptor(
                descriptor.id.value,
                s"${descriptor.inverseQuality} requires an executable inverse asset for a dense transform"
              )
            )
          case None => Right((primary.coordinateMap, None))
          case Some(asset) =>
            val inverseDescriptor =
              TransformDescriptor.build(
                MorphismId.unsafe(s"${descriptor.id.value}:provided-inverse"),
                target.id,
                source.id,
                asset.format.tool,
                asset.format.defaultKind,
                asset.path,
                descriptor.routeTag,
                descriptor.cost,
                InverseQuality.Missing,
                CoordinateMap.Unspecified,
                Some(asset.format)
              )
            inverseDescriptor.flatMap { checked =>
              loadMap(asset.path, asset.format, asset.options, checked, target, source).flatMap {
                case LoadedMap(CoordinateMap.Dense3D(inverseDense), _, fingerprint) =>
                  CoordinateMap
                    .dense3D(primaryDense.pullback, Some(inverseDense.pullback), primaryDense.boundary)
                    .left
                    .map(error => SpatialIoError.InvalidTransformDescriptor(descriptor.id.value, error.message))
                    .map(map => (map, Some(asset -> fingerprint)))
                case _ =>
                  Left(
                    SpatialIoError.InvalidTransformDescriptor(
                      descriptor.id.value,
                      "a dense transform requires a dense inverse asset"
                    )
                  )
              }
            }
      case _ =>
        inverseAsset match
          case Some(asset) =>
            fingerprint(asset.path).map(value => (primary.coordinateMap, Some(asset -> value)))
          case None => Right((primary.coordinateMap, None))

  private def loadMap(
    path: Path,
    format: TransformFileFormat,
    options: TransformLoadOptions,
    descriptor: TransformDescriptor,
    source: Domain,
    target: Domain
  ): Either[SpatialIoError, LoadedMap] =
    format.defaultKind match
      case TransformKind.Affine3D => loadAffine(path, format, options, source, target)
      case TransformKind.Warp3D | TransformKind.DisplacementField3D | TransformKind.CoordinateField3D =>
        loadDense(path, format, options, descriptor, source, target)

  private def loadAffine(
    path: Path,
    format: TransformFileFormat,
    options: TransformLoadOptions,
    source: Domain,
    target: Domain
  ): Either[SpatialIoError, LoadedMap] =
    for
      text <- readText(path)
      native <- parseAffine(path, format, text)
      pullback <- normalizeAffine(path, native, options, volumeSpace(source), volumeSpace(target))
      _ <- validateAffine(path, pullback)
      coordinateMap <- CoordinateMap
        .affine3D(toImage(pullback))
        .left
        .map(error => SpatialIoError.MalformedTransformAsset(path, error.message))
      stamp <- fingerprint(path)
    yield
      LoadedMap(
        coordinateMap,
        s"${options.convention}:${options.direction}->RAS-mm-pullback;matrix=gale.linalg.DMat",
        stamp
      )

  private def loadDense(
    path: Path,
    format: TransformFileFormat,
    options: TransformLoadOptions,
    descriptor: TransformDescriptor,
    source: Domain,
    target: Domain
  ): Either[SpatialIoError, LoadedMap] =
    if format == TransformFileFormat.X5 || format == TransformFileFormat.FreeSurferLta then
      Left(SpatialIoError.UnsupportedTransformAsset(path, format, "no built-in executable adapter"))
    else if options.direction != TransformDirection.PullbackTargetToSource then
      Left(
        SpatialIoError.TransformConventionMismatch(
          path,
          "a forward dense field cannot be inverted exactly at ingestion; provide a target-grid pullback field"
        )
      )
    else
      try
        val sourceSpace = volumeSpace(source)
        val targetSpace = volumeSpace(target)
        val nativeResult =
          options.denseEncoding match
            case DenseTransformEncoding.Displacement =>
              Nifti
                .readDisplacementField(path)
                .map(field => field.values -> field.space)
            case DenseTransformEncoding.AbsoluteCoordinates =>
              Nifti
                .readSourceCoordinateField(path)
                .map(field => field.values -> field.space)
        nativeResult
          .left
          .map(error =>
            SpatialIoError.MalformedTransformAsset(path, error.message)
          )
          .flatMap: (native, nativeSpace) =>
            if !sameGrid(nativeSpace.spatialSpace, targetSpace) then
              Left(
                SpatialIoError.TransformGeometryMismatch(
                  path,
                  "dense transform grid must equal the target domain grid"
                )
              )
            else
              val grid = GridSpec.fromSpace(targetSpace)
              val field =
                normalizeDense(
                  native,
                  grid,
                  sourceSpace,
                  targetSpace,
                  options
                )
              DenseFieldMorphism
                .coordinates(
                  SpatialDomainId(source.id.value),
                  SpatialDomainId(target.id.value),
                  grid,
                  field,
                  options.interpolation,
                  descriptor.cost,
                  s"${format.tool.toString.toLowerCase}-${format.toString.toLowerCase}-pullback"
                )
                .left
                .map(error =>
                  SpatialIoError.MalformedTransformAsset(path, error.message)
                )
                .flatMap { dense =>
                  for
                    coordinateMap <- CoordinateMap
                      .dense3D(dense)
                      .left
                      .map(error =>
                        SpatialIoError.MalformedTransformAsset(
                          path,
                          error.message
                        )
                      )
                    stamp <- fingerprint(path)
                  yield
                    LoadedMap(
                      coordinateMap,
                      s"${options.convention}:${options.denseEncoding}:${options.direction}->absolute-RAS-mm-pullback",
                      stamp
                    )
                }
      catch
        case NonFatal(error) => Left(SpatialIoError.MalformedTransformAsset(path, detail(error)))

  private def normalizeDense(
    native: RavelArray[Double, Rank[4]],
    grid: GridSpec,
    source: NeuroSpace,
    target: NeuroSpace,
    options: TransformLoadOptions
  ): RavelArray[Double, Rank[4]] =
    val count = grid.nVoxels
    val sourceFslToWorld =
      options.convention match
        case TransformCoordinateConvention.FslScaledVoxel =>
          multiply(toGale(source.trans), inverseOrThrow(fslVoxelToScaled(source)))
        case _ => GaleDMat.eye(4)
    val targetVoxelToFsl =
      options.convention match
        case TransformCoordinateConvention.FslScaledVoxel => fslVoxelToScaled(target)
        case _ => GaleDMat.eye(4)

    val shape = Shape(grid.shape.x, grid.shape.y, grid.shape.z, 3)
    RavelArray.build[Double, Rank[4]](shape) { builder =>
      var voxel = 0
      while voxel < count do
        val coord =
          scalafim.image.Indexing.indexToGrid3D(grid.shape, voxel)
        val nativeValue =
          Vector(
            native(coord.x, coord.y, coord.z, 0),
            native(coord.x, coord.y, coord.z, 1),
            native(coord.x, coord.y, coord.z, 2)
          )
        val voxelCoord =
          Vector(
            coord.x.toDouble,
            coord.y.toDouble,
            coord.z.toDouble
          )
        val targetWorld = grid.voxelToWorld(voxelCoord)
        val sourceWorld =
          options.convention match
            case TransformCoordinateConvention.RasMillimeters =>
              options.denseEncoding match
                case DenseTransformEncoding.Displacement =>
                  zip3(targetWorld, nativeValue)(_ + _)
                case DenseTransformEncoding.AbsoluteCoordinates =>
                  nativeValue
            case TransformCoordinateConvention.LpsMillimeters =>
              val targetNative = rasToLps(targetWorld)
              val sourceNative =
                options.denseEncoding match
                  case DenseTransformEncoding.Displacement =>
                    zip3(targetNative, nativeValue)(_ + _)
                  case DenseTransformEncoding.AbsoluteCoordinates =>
                    nativeValue
              rasToLps(sourceNative)
            case TransformCoordinateConvention.FslScaledVoxel =>
              val targetNative =
                applyAffine(targetVoxelToFsl, voxelCoord)
              val sourceNative =
                options.denseEncoding match
                  case DenseTransformEncoding.Displacement =>
                    zip3(targetNative, nativeValue)(_ + _)
                  case DenseTransformEncoding.AbsoluteCoordinates =>
                    nativeValue
              applyAffine(sourceFslToWorld, sourceNative)
        val base =
          ((coord.x * grid.shape.y + coord.y) * grid.shape.z +
            coord.z) * 3
        var component = 0
        while component < 3 do
          builder.writeLinear(base + component, sourceWorld(component))
          component += 1
        voxel += 1
    }

  private def parseAffine(
    path: Path,
    format: TransformFileFormat,
    text: String
  ): Either[SpatialIoError, GaleDMat] =
    format match
      case TransformFileFormat.AntsAffine => parseAntsAffine(path, text)
      case TransformFileFormat.FslFlirt => parseRectangularAffine(path, text, expected = 16)
      case TransformFileFormat.AfniAffine => parseRectangularAffine(path, text, expected = 12)
      case other => Left(SpatialIoError.UnsupportedTransformAsset(path, other, "not an affine text format"))

  private def parseAntsAffine(path: Path, text: String): Either[SpatialIoError, GaleDMat] =
    val transformLines = text.linesIterator.filter(_.trim.startsWith("Transform:")).toVector
    if transformLines.length != 1 || !transformLines.head.toLowerCase.contains("affinetransform") then
      Left(SpatialIoError.MalformedTransformAsset(path, "expected exactly one ITK AffineTransform_double_3_3"))
    else
      val parameters = keyedNumbers(text, "Parameters:")
      val center = keyedNumbers(text, "FixedParameters:")
      if parameters.length != 12 || center.length < 3 then
        Left(
          SpatialIoError.MalformedTransformAsset(
            path,
            s"expected 12 affine parameters and 3 fixed parameters, got ${parameters.length} and ${center.length}"
          )
        )
      else
        val rows = Vector.tabulate(3) { row =>
          val translation =
            parameters(9 + row) + center(row) -
              Vector.tabulate(3)(col => parameters(row * 3 + col) * center(col)).sum
          Vector.tabulate(3)(col => parameters(row * 3 + col)) :+ translation
        } :+ Vector(0.0, 0.0, 0.0, 1.0)
        Right(fromRows(rows))

  private def parseRectangularAffine(
    path: Path,
    text: String,
    expected: Int
  ): Either[SpatialIoError, GaleDMat] =
    val clean = text.linesIterator.map(_.takeWhile(_ != '#')).mkString("\n")
    val numbers = NumberPattern.findAllIn(clean).map(_.toDouble).toVector
    if numbers.length != expected then
      Left(SpatialIoError.MalformedTransformAsset(path, s"expected $expected numeric values, got ${numbers.length}"))
    else if expected == 16 then Right(GaleDMat.dense(4, 4, numbers))
    else
      Right(
        GaleDMat.dense(
          4,
          4,
          numbers ++ Vector(0.0, 0.0, 0.0, 1.0)
        )
      )

  private def keyedNumbers(text: String, key: String): Vector[Double] =
    text.linesIterator
      .find(_.trim.startsWith(key))
      .toVector
      .flatMap(line => NumberPattern.findAllIn(line.drop(line.indexOf(key) + key.length)).map(_.toDouble))

  private def normalizeAffine(
    path: Path,
    native: GaleDMat,
    options: TransformLoadOptions,
    source: NeuroSpace,
    target: NeuroSpace
  ): Either[SpatialIoError, GaleDMat] =
    options.convention match
      case TransformCoordinateConvention.RasMillimeters =>
        val ras = native
        directionToPullback(path, ras, options.direction)
      case TransformCoordinateConvention.LpsMillimeters =>
        val ras = multiply(LpsToRas, multiply(native, LpsToRas))
        directionToPullback(path, ras, options.direction)
      case TransformCoordinateConvention.FslScaledVoxel =>
        val nativePullback = directionToPullback(path, native, options.direction)
        nativePullback.flatMap { pullback =>
          for
            sourceFslInverse <- inverse(path, fslVoxelToScaled(source))
            targetWorldInverse <- inverse(path, toGale(target.trans))
          yield
            multiply(
              toGale(source.trans),
              multiply(
                sourceFslInverse,
                multiply(pullback, multiply(fslVoxelToScaled(target), targetWorldInverse))
              )
            )
        }

  private def directionToPullback(
    path: Path,
    matrix: GaleDMat,
    direction: TransformDirection
  ): Either[SpatialIoError, GaleDMat] =
    direction match
      case TransformDirection.PullbackTargetToSource => Right(matrix)
      case TransformDirection.ForwardSourceToTarget => inverse(path, matrix)

  private def fslVoxelToScaled(space: NeuroSpace): GaleDMat =
    val affine = toGale(space.trans)
    val sx = columnNorm(affine, 0)
    val sy = columnNorm(affine, 1)
    val sz = columnNorm(affine, 2)
    val determinant = determinant3(affine)
    val flipX = determinant > 0.0
    GaleDMat.dense(
      4,
      4,
      Vector(
        if flipX then -sx else sx, 0.0, 0.0, if flipX then (space.spatialDims(0) - 1).toDouble * sx else 0.0,
        0.0, sy, 0.0, 0.0,
        0.0, 0.0, sz, 0.0,
        0.0, 0.0, 0.0, 1.0
      )
    )

  private def inverse(path: Path, matrix: GaleDMat): Either[SpatialIoError, GaleDMat] =
    matrix.lu
      .left
      .map(error => SpatialIoError.TransformConventionMismatch(path, error.toString))
      .flatMap { lu =>
        val builder = GaleDMat.newBuilder(matrix.rows, matrix.cols)
        var column = 0
        var error = Option.empty[SpatialIoError]
        while column < matrix.cols && error.isEmpty do
          val basis = DVec.tabulate(matrix.rows)(row => if row == column then 1.0 else 0.0)
          lu.solve(basis) match
            case Left(err) => error = Some(SpatialIoError.TransformConventionMismatch(path, err.toString))
            case Right(solution) =>
              var row = 0
              while row < matrix.rows do
                builder(row, column) = solution(row)
                row += 1
          column += 1
        error match
          case Some(err) => Left(err)
          case None => Right(builder.result())
      }

  private def inverseOrThrow(matrix: GaleDMat): GaleDMat =
    inverse(Path.of("<internal-fsl-convention>"), matrix).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def multiply(left: GaleDMat, right: GaleDMat): GaleDMat =
    left * right

  private def applyAffine(matrix: GaleDMat, point: Vector[Double]): Vector[Double] =
    Vector.tabulate(3) { row =>
      matrix(row, 3) + matrix(row, 0) * point(0) + matrix(row, 1) * point(1) + matrix(row, 2) * point(2)
    }

  private def validateAffine(path: Path, matrix: GaleDMat): Either[SpatialIoError, Unit] =
    val finite = matrix.valuesRowMajor.forall(_.isFinite)
    val homogeneous =
      math.abs(matrix(3, 0)) <= 1e-12 &&
        math.abs(matrix(3, 1)) <= 1e-12 &&
        math.abs(matrix(3, 2)) <= 1e-12 &&
        math.abs(matrix(3, 3) - 1.0) <= 1e-12
    if !finite then Left(SpatialIoError.MalformedTransformAsset(path, "affine contains non-finite values"))
    else if !homogeneous then Left(SpatialIoError.MalformedTransformAsset(path, "affine bottom row must be [0, 0, 0, 1]"))
    else inverse(path, matrix).map(_ => ())

  private def sameGrid(actual: NeuroSpace, expected: NeuroSpace): Boolean =
    actual.spatialDims == expected.spatialDims && matricesClose(actual.trans, expected.trans, 1e-5)

  private def matricesClose(left: ImageDMat, right: ImageDMat, tolerance: Double): Boolean =
    if left.rows != right.rows || left.cols != right.cols then false
    else
      var row = 0
      while row < left.rows do
        var col = 0
        while col < left.cols do
          if math.abs(left(row, col) - right(row, col)) > tolerance then return false
          col += 1
        row += 1
      true

  private def volumeSpace(domain: Domain): NeuroSpace =
    domain.geometry match
      case SamplingGeometry.Volume(space, _) => space
      case _ => throw new IllegalArgumentException(s"domain ${domain.id.value} is not volumetric")

  private def toGale(matrix: ImageDMat): GaleDMat =
    val builder = GaleDMat.newBuilder(matrix.rows, matrix.cols)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        builder(row, col) = matrix(row, col)
        col += 1
      row += 1
    builder.result()

  private def toImage(matrix: GaleDMat): ImageDMat =
    ImageDMat.fromRows(
      Vector.tabulate(matrix.rows)(row => Vector.tabulate(matrix.cols)(col => matrix(row, col)))
    )

  private def fromRows(rows: Vector[Vector[Double]]): GaleDMat =
    GaleDMat.dense(rows.length, rows.head.length, rows.flatten)

  private def columnNorm(matrix: GaleDMat, column: Int): Double =
    math.sqrt(
      matrix(0, column) * matrix(0, column) +
        matrix(1, column) * matrix(1, column) +
        matrix(2, column) * matrix(2, column)
    )

  private def determinant3(matrix: GaleDMat): Double =
    matrix(0, 0) * (matrix(1, 1) * matrix(2, 2) - matrix(1, 2) * matrix(2, 1)) -
      matrix(0, 1) * (matrix(1, 0) * matrix(2, 2) - matrix(1, 2) * matrix(2, 0)) +
      matrix(0, 2) * (matrix(1, 0) * matrix(2, 1) - matrix(1, 1) * matrix(2, 0))

  private def rasToLps(point: Vector[Double]): Vector[Double] =
    Vector(-point(0), -point(1), point(2))

  private def zip3(left: Vector[Double], right: Vector[Double])(f: (Double, Double) => Double): Vector[Double] =
    Vector(f(left(0), right(0)), f(left(1), right(1)), f(left(2), right(2)))

  private def readText(path: Path): Either[SpatialIoError, String] =
    try
      if !Files.isRegularFile(path) then Left(SpatialIoError.IoFailure(path, "file does not exist"))
      else Right(Files.readString(path, StandardCharsets.UTF_8))
    catch
      case NonFatal(error) => Left(SpatialIoError.IoFailure(path, detail(error)))

  private def fingerprint(path: Path): Either[SpatialIoError, TransformAssetFingerprint] =
    try
      if !Files.isRegularFile(path) then Left(SpatialIoError.IoFailure(path, "file does not exist"))
      else
        val digest = MessageDigest.getInstance("SHA-256")
        Using.resource(Files.newInputStream(path)) { in =>
          val buffer = Array.ofDim[Byte](8192)
          var count = in.read(buffer)
          while count >= 0 do
            if count > 0 then digest.update(buffer, 0, count)
            count = in.read(buffer)
        }
        val attributes = Files.readAttributes(path, classOf[java.nio.file.attribute.BasicFileAttributes])
        val hex = digest.digest().map(byte => f"${byte & 0xff}%02x").mkString
        Right(TransformAssetFingerprint(attributes.size(), attributes.lastModifiedTime().toMillis, hex))
    catch
      case NonFatal(error) => Left(SpatialIoError.IoFailure(path, detail(error)))

  private def detail(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)
