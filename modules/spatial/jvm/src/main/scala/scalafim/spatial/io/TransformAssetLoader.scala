package scalafim.spatial.io

import image4s.geometry.{Affine, D3, Frame}
import reframe4s.field.CoordinateBoundaryPolicy
import scalafim.image.world.{FreeSurferVolumeGeometry, FslVolumeGeometry}
import scalafim.spatial.*
import scalafim.transform.*
import scalafim.transform.afni.{Aff12Interpretation, AfniCardinal}
import scalafim.transform.field.{DenseContext, FnirtCoefficientContext, FnirtCoefficientInterpretation, FnirtContext, FnirtDefinition, FnirtFieldInterpretation, LpsDisplacementInterpretation}
import scalafim.transform.freesurfer.{LtaInterpretation, MniXfmInterpretation, RegisterDatInterpretation}
import scalafim.transform.fsl.FlirtInterpretation
import scalafim.transform.itk.{ItkHdf5Interpretation, ItkLinearInterpretation}
import scalafim.transform.x5.X5Interpretation

import java.nio.file.Path

/** Choices that change what a loaded map computes. None of them names a coordinate convention or a storage direction:
  * those are intrinsic to each format and decided by its `scalafim.transform` interpretation.
  *
  * @param boundary what a dense map does at target points outside its lattice. The default rejects them; ITK and ANTs
  *   themselves extend displacement fields by zero, which is `CoordinateBoundaryPolicy.PreserveSource`.
  * @param fnirtDefinition whether a FNIRT field is relative or absolute; `None` detects it and refuses when unsure.
  */
final case class TransformLoadOptions(
  boundary: CoordinateBoundaryPolicy = CoordinateBoundaryPolicy.Reject,
  fnirtDefinition: Option[FnirtDefinition] = None
)

/** A second file holding the opposite direction of a descriptor's file (e.g. ANTs `InverseWarp`, FSL `invwarp`).
  * Its own endpoints are the primary file's, swapped. `format = None` detects it from content.
  */
final case class TransformAssetSpec(path: Path, format: Option[TransformFormat] = None)

/** Where a loaded morphism came from: exact asset digests, the format, and the transform-level provenance. */
final case class TransformAssetProvenance(
  primaryPath: Path,
  primary: AssetRef,
  format: TransformFormat,
  tool: TransformTool,
  endpoints: TransformFileEndpoints,
  options: TransformLoadOptions,
  inverseAsset: Option[(TransformAssetSpec, AssetRef)],
  transform: TransformProvenance
)

/** A descriptor's file as a graph morphism, with the world transform it was built from. */
final case class LoadedTransform(
  descriptor: TransformDescriptor,
  morphism: Morphism,
  transform: WorldTransform[Frame[D3], Frame[D3]],
  provenance: TransformAssetProvenance
)

/** Spatial-graph adapter over the transform codecs: `TransformFiles` reads and decodes, each format's interpretation
  * yields a `WorldTransform` between the descriptor's volume frames, and its pullback becomes the morphism's provider
  * coordinate map (affines through `CoordinateMap.affineBetween`, dense fields through `CoordinateMap.dense`, other
  * provider maps under an identity derived from their assets' SHA-256 digests).
  */
object TransformAssetLoader:
  private type World = WorldTransform[Frame[D3], Frame[D3]]

  /** The geometry a format may need from a domain: its world frame and its voxel-to-world lattice. */
  private final case class Volume(frame: Frame[D3], dims: Vector[Int], voxelToWorld: Affine[D3])

  def load(
    descriptor: TransformDescriptor,
    source: Domain,
    target: Domain,
    options: TransformLoadOptions = TransformLoadOptions(),
    inverseAsset: Option[TransformAssetSpec] = None
  ): Either[SpatialIoError, LoadedTransform] =
    val path = descriptor.path
    for
      volumes <- volumes(descriptor, source, target)
      (sourceVolume, targetVolume) = volumes
      primary <- read(path, descriptor.format)
      _ <- validateTool(descriptor, primary.format)
      (fileSource, fileTarget) = descriptor.endpoints match
        case TransformFileEndpoints.AsFile   => (sourceVolume, targetVolume)
        case TransformFileEndpoints.Reversed => (targetVolume, sourceVolume)
      asFile <- interpret(path, primary, fileSource, fileTarget, options)
      inverse <- inverseAsset match
        case None => Right(None)
        case Some(spec) =>
          for
            loaded <- read(spec.path, spec.format)
            reverse <- interpret(spec.path, loaded, fileTarget, fileSource, options)
          yield Some((spec, loaded.asset, reverse))
      paired = attachInverse(asFile, inverse)
      routed <- descriptor.endpoints match
        case TransformFileEndpoints.AsFile   => Right(paired)
        case TransformFileEndpoints.Reversed => invert(path, paired)
      identity = s"${primary.format}:${digest(primary.asset)}|inverse=${inverse.fold("none")((_, asset, _) => digest(asset))}|endpoints=${descriptor.endpoints}|$options"
      coordinateMap <- coordinateMap(routed, identity).left.map(error => SpatialIoError.InvalidTransformDescriptor(descriptor.id.value, error.message))
      _ <- requireDeclaredInverse(descriptor, coordinateMap)
      morphism <- Morphism
        .build(
          descriptor.id,
          descriptor.source,
          descriptor.target,
          descriptor.kind.morphismKind,
          descriptor.routeTag,
          descriptor.cost,
          descriptor.inverseQuality.toInverse,
          coordinateMap
        )
        .left
        .map(error => SpatialIoError.InvalidTransformDescriptor(descriptor.id.value, error.message))
    yield LoadedTransform(
      descriptor,
      morphism,
      routed,
      TransformAssetProvenance(
        path.toAbsolutePath.normalize(),
        primary.asset,
        primary.format,
        descriptor.tool,
        descriptor.endpoints,
        options,
        inverse.map((spec, asset, _) => spec -> asset),
        routed.provenance
      )
    )

  private def read(path: Path, format: Option[TransformFormat]): Either[SpatialIoError, LoadedTransformFile] =
    TransformFiles.load(path, format).left.map(SpatialIoError.TransformRead(path, _))

  /** What the file means between the two volumes, as a transform from `from` (the file's moving space) to `to`. */
  private def interpret(
    path: Path,
    loaded: LoadedTransformFile,
    from: Volume,
    to: Volume,
    options: TransformLoadOptions
  ): Either[SpatialIoError, World] =
    val frames = Frames[Frame[D3], Frame[D3]](from.frame, to.frame)
    val dense = DenseContext(frames, options.boundary)
    val asset = loaded.asset
    def meaning[A](result: Either[TransformError, A]): Either[SpatialIoError, A] =
      result.left.map(SpatialIoError.TransformInterpretation(path, _))
    val interpreted: Either[SpatialIoError, World] =
      loaded.native match
        case NativeTransform.Itk(file, storage) =>
          meaning(ItkLinearInterpretation.interpretWith(file, frames, asset, storage).map(_.composed))
        case NativeTransform.ItkHdf5(file) =>
          meaning(ItkHdf5Interpretation.interpretWith(file, dense, asset).map(_.composed))
        case NativeTransform.AntsField(field) =>
          meaning(LpsDisplacementInterpretation.Ants.interpret(field, dense))
        case NativeTransform.AfniQwarp(field) =>
          meaning(LpsDisplacementInterpretation.AfniQwarp.interpret(field, dense))
        case NativeTransform.Flirt(matrix) =>
          meaning:
            for
              fromGeometry <- fslGeometry(from)
              toGeometry <- fslGeometry(to)
              linear <- FlirtInterpretation.interpretWith(matrix, FslGrids(from.frame, fromGeometry, to.frame, toGeometry), asset)
            yield linear
        case NativeTransform.FnirtField(field) =>
          meaning(fslGeometry(from).flatMap(geometry => FnirtFieldInterpretation.interpret(field, FnirtContext(frames, geometry, options.fnirtDefinition, options.boundary))))
        case NativeTransform.FnirtCoefficients(file) =>
          meaning:
            for
              fromGeometry <- fslGeometry(from)
              toGeometry <- fslGeometry(to)
              grids = FslGrids(from.frame, fromGeometry, to.frame, toGeometry)
              warp <- FnirtCoefficientInterpretation.interpretWith(file, FnirtCoefficientContext(grids, options.boundary), asset)
            yield warp
        case NativeTransform.Afni(series) =>
          // AFNI matrices act on cardinalised datasets; the domains' own affines say whether either side is oblique.
          val correction = CardinalCorrection.On(AfniCardinal.obliquity(from.voxelToWorld), AfniCardinal.obliquity(to.voxelToWorld))
          meaning(Aff12Interpretation.interpretWith(series, AfniContext(frames, correction), asset)).flatMap: linear =>
            linear.transforms match
              case Vector(single) => Right(single)
              case many =>
                Left(SpatialIoError.UnsupportedTransformAsset(path, loaded.format, s"${many.size} affines (a per-volume series) cannot be one route"))
        case NativeTransform.Lta(file) =>
          meaning(LtaInterpretation.interpretWith(file, frames, asset))
        case NativeTransform.Xfm(xfm) =>
          meaning(MniXfmInterpretation.interpret(xfm, frames))
        case NativeTransform.RegisterDatFile(dat) =>
          meaning(
            RegisterDatInterpretation.interpret(
              dat,
              TkRegGrids(from.frame, FreeSurferVolumeGeometry(from.dims, from.voxelToWorld), to.frame, FreeSurferVolumeGeometry(to.dims, to.voxelToWorld))
            )
          )
        case NativeTransform.X5(file) =>
          meaning(X5Interpretation.interpret(file, dense).map(_.composed))
    interpreted.map(stamp(_, asset))

  /** FSL's view of a domain: the domain's voxel-to-world affine as the selected (sform) affine, zooms its column norms. */
  private def fslGeometry(volume: Volume): Either[TransformError, FslVolumeGeometry] =
    val m = volume.voxelToWorld.rowMajor
    val zooms = Vector.tabulate(3)(c => math.sqrt(m(c) * m(c) + m(4 + c) * m(4 + c) + m(8 + c) * m(8 + c)))
    FslVolumeGeometry.fromHeader(volume.dims, zooms, 0, None, 1, Some(volume.voxelToWorld)).left.map(TransformError.Space(_))

  /** Record the file's digest on the read step: interpretations only know the asset they were handed. */
  private def stamp(transform: World, asset: AssetRef): World =
    def stamped(provenance: TransformProvenance): TransformProvenance =
      TransformProvenance(provenance.steps.map:
        case TransformProvenance.Step.Read(format, ref) if ref.sha256.isEmpty => TransformProvenance.Step.Read(format, asset)
        case step                                                             => step
      )
    transform match
      case WorldTransform.Linear(framed, provenance)          => WorldTransform.Linear(framed, stamped(provenance))
      case WorldTransform.Smooth(iso, provenance)             => WorldTransform.Smooth(iso, stamped(provenance))
      case WorldTransform.Mapped(pull, availability, provenance) => WorldTransform.Mapped(pull, availability, stamped(provenance))

  /** A dense primary gains its forward map from the inverse asset; an affine already has an exact one. */
  private def attachInverse(primary: World, inverse: Option[(TransformAssetSpec, AssetRef, World)]): World =
    (primary, inverse) match
      case (WorldTransform.Mapped(pull, _, provenance), Some((_, asset, reverse))) =>
        WorldTransform.Mapped(pull, PushAvailability.FromAsset(reverse.pull, asset), provenance)
      case _ => primary

  private def invert(path: Path, transform: World): Either[SpatialIoError, World] =
    transform match
      case WorldTransform.Linear(framed, provenance) => Right(WorldTransform.Linear(framed, provenance).inverse)
      case WorldTransform.Smooth(iso, provenance)    => Right(WorldTransform.Smooth(iso, provenance).inverse)
      case WorldTransform.Mapped(pull, availability, provenance) =>
        WorldTransform.Mapped(pull, availability, provenance).invert.left.map(SpatialIoError.TransformInterpretation(path, _))

  private def coordinateMap(transform: World, identity: String): Either[SpatialError, CoordinateMap] =
    CoordinateMap.fromWorldTransform(transform, identity)

  private def requireDeclaredInverse(descriptor: TransformDescriptor, coordinateMap: CoordinateMap): Either[SpatialIoError, Unit] =
    coordinateMap match
      case CoordinateMap.Geometric(binding) if descriptor.inverseQuality.declared && binding.inversePullback.isEmpty =>
        Left(
          SpatialIoError.InvalidTransformDescriptor(
            descriptor.id.value,
            s"${descriptor.inverseQuality} requires an executable inverse asset for a dense transform"
          )
        )
      case _ => Right(())

  private def validateTool(descriptor: TransformDescriptor, format: TransformFormat): Either[SpatialIoError, Unit] =
    if descriptor.tool == format.tool then Right(())
    else
      Left(
        SpatialIoError.InvalidTransformDescriptor(
          descriptor.id.value,
          s"declared tool ${descriptor.tool} does not match $format (${format.tool})"
        )
      )

  private def volumes(descriptor: TransformDescriptor, source: Domain, target: Domain): Either[SpatialIoError, (Volume, Volume)] =
    def volume(domain: Domain): Option[Volume] =
      domain.geometry match
        case SamplingGeometry.Volume(space, _) => Some(Volume(space.grid.frame, space.grid.shape, space.grid.indexToFrame))
        case _                                 => None
    if descriptor.source != source.id || descriptor.target != target.id then
      Left(
        SpatialIoError.InvalidTransformDescriptor(
          descriptor.id.value,
          s"descriptor route ${descriptor.source.value}->${descriptor.target.value} does not match supplied domains ${source.id.value}->${target.id.value}"
        )
      )
    else
      volume(source)
        .zip(volume(target))
        .toRight(SpatialIoError.InvalidTransformDescriptor(descriptor.id.value, "transform assets join volume domains only"))

  private def digest(asset: AssetRef): String =
    asset.sha256.getOrElse(asset.label)
