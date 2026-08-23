package scalafim.image

import image4s.geometry.{Dim, Dimension, Frame, GridRecord, LatticeIndex}
import image4s.locus.{
  GridDomain,
  GridDomainError,
  GridDomainLayout,
  GridDomainRecord
}
import locus4s.{
  Bijection,
  CertifiedMapError,
  DomainError,
  DomainRegistry,
  FiniteDomain,
  FiniteSpace,
  Index,
  Region,
  Selection,
  SpaceMismatch,
  TotalMap,
  TotalMapError
}
import ravel.{NDArray, Shape}
import scalafim.locus.IndexedField

/** Logical voxel linearization carried by a persisted ordinal artifact.
  *
  * This is independent of physical Ravel storage.
  */
enum VolumeOrdinalLayout(val id: String) derives CanEqual:
  case ScalaFimFirstAxisFastestV1
      extends VolumeOrdinalLayout("scalafim-first-axis-fastest/v1")
  case Image4sRowMajorLastAxisFastestV1
      extends VolumeOrdinalLayout("row-major-last-axis-fastest/v1")
  case Unrecognized(value: String) extends VolumeOrdinalLayout(value)

/** Neutral evidence tying an ordinal vector to one exact grid and layout. */
final case class VolumeOrdinalRecord(
    grid: GridRecord,
    layout: VolumeOrdinalLayout,
    ordinals: Vector[Int]
) derives CanEqual

enum VolumeOrdinalBridgeError:
  case CanonicalGrid(error: GridDomainError)
  case LegacyDomain(error: DomainError)
  case InvalidTotalMap(error: TotalMapError)
  case InvalidBijection(error: CertifiedMapError)
  case InvalidLegacyOrdinal(value: Int, size: Int)
  case InvalidCanonicalOrdinal(value: Int, size: Int)
  case InvalidCoordinate(coordinate: VoxelCoord, shape: SpatialDims)
  case InvalidRecord(detail: String)
  case GridRecordMismatch(expected: GridRecord, actual: GridRecord)
  case UnsupportedLayout(layout: VolumeOrdinalLayout)

  def message: String =
    this match
      case CanonicalGrid(error) => error.message
      case LegacyDomain(error) => error.message
      case InvalidTotalMap(error) => error.message
      case InvalidBijection(error) => error.message
      case InvalidLegacyOrdinal(value, size) =>
        s"legacy voxel ordinal $value is outside [0, $size)"
      case InvalidCanonicalOrdinal(value, size) =>
        s"canonical voxel ordinal $value is outside [0, $size)"
      case InvalidCoordinate(coordinate, shape) =>
        s"voxel coordinate $coordinate is outside ${shape.toVector.mkString("x")}"
      case InvalidRecord(detail) =>
        s"invalid voxel ordinal record: $detail"
      case GridRecordMismatch(expected, actual) =>
        s"voxel ordinal record belongs to grid ${actual.key.id.value}, " +
          s"expected ${expected.key.id.value}"
      case UnsupportedLayout(layout) =>
        s"unsupported voxel ordinal layout '${layout.id}'"

/** Checked coordinate-derived isomorphism between legacy ScalaFIM ordinals
  * and the canonical image4s-locus grid domain.
  */
final class VolumeOrdinalBridge[S, L] private[image] (
    val volumeSpace: VolumeSpace,
    val canonicalSpace: FiniteSpace[S],
    val legacySpace: FiniteDomain[L],
    val toCanonical: Bijection[L, S],
    val gridDomainRecord: GridDomainRecord
):
  val toLegacy: Bijection[S, L] =
    toCanonical.inverse

  val canonicalLayout: GridDomainLayout =
    gridDomainRecord.layout

  /** Compatibility name for the authoritative canonical finite space. */
  def finiteSpace: FiniteSpace[S] =
    canonicalSpace

  def region(
      voxelRegion: VoxelRegion
  ): Either[GridMismatch, Region[S]] =
    GridCompatibility.volume(volumeSpace, voxelRegion.space).map: _ =>
      val legacyRegion =
        Region
          .fromOrdinals(
            legacySpace,
            voxelRegion.linearIndices.iterator
          )
          .toOption
          .get
      toCanonical.toTotalMap.image(legacyRegion)

  def selection(
      voxelSelection: VoxelSelection
  ): Either[GridMismatch, Selection[S]] =
    GridCompatibility.volume(volumeSpace, voxelSelection.space).map: _ =>
      val canonicalValues =
        voxelSelection.linearIndices.iterator.map: legacyOrdinal =>
          canonicalPoint(legacyOrdinal).toOption.get.ordinal
      Selection
        .fromOrdinals(canonicalSpace, canonicalValues)
        .toOption
        .get

  def voxelRegion(
      region: Region[S]
  ): Either[SpaceMismatch, VoxelRegion] =
    checkSpace(region.space).map: _ =>
      val legacyRegion = toLegacy.toTotalMap.image(region)
      VoxelRegion
        .make(
          volumeSpace,
          NDArray.fromSeq(
            Shape(legacyRegion.cardinality),
            legacyRegion.ordinalsInDomainOrder
          )
        )
        .toOption
        .get

  def voxelSelection(
      selection: Selection[S]
  ): Either[SpaceMismatch, VoxelSelection] =
    checkSpace(selection.space).map: _ =>
      val legacyValues =
        selection.indices.map(toLegacy.apply).map(_.ordinal)
      VoxelSelection
        .make(
          volumeSpace,
          NDArray.fromSeq(Shape(selection.size), legacyValues)
        )
        .toOption
        .get

  def canonicalPoint(
      legacyOrdinal: Int
  ): Either[VolumeOrdinalBridgeError, Index[S]] =
    legacySpace.indexOption(legacyOrdinal) match
      case Some(legacy) => Right(toCanonical(legacy))
      case None =>
        Left(
          VolumeOrdinalBridgeError.InvalidLegacyOrdinal(
            legacyOrdinal,
            legacySpace.size
          )
        )

  def pointAtLegacyOrdinal(
      ordinal: Int
  ): Either[VolumeOrdinalBridgeError, Index[S]] =
    canonicalPoint(ordinal)

  def canonicalPointAt(
      coordinate: VoxelCoord
  ): Either[VolumeOrdinalBridgeError, Index[S]] =
    Indexing
      .gridToIndexChecked(volumeSpace.shape, coordinate)
      .left
      .map(_ =>
        VolumeOrdinalBridgeError.InvalidCoordinate(
          coordinate,
          volumeSpace.shape
        )
      )
      .flatMap(canonicalPoint)

  def pointAt(
      coordinate: VoxelCoord
  ): Either[VolumeOrdinalBridgeError, Index[S]] =
    canonicalPointAt(coordinate)

  def legacyOrdinal(point: Index[S]): Int =
    toLegacy(point).ordinal

  def legacyOrdinalOf(point: Index[S]): Int =
    legacyOrdinal(point)

  def coordinateOf(point: Index[S]): VoxelCoord =
    Indexing.indexToGrid3D(volumeSpace.shape, legacyOrdinal(point))

  def canonicalRecord(ordinals: Vector[Int]): VolumeOrdinalRecord =
    VolumeOrdinalRecord(
      gridDomainRecord.grid,
      VolumeOrdinalLayout.Image4sRowMajorLastAxisFastestV1,
      ordinals
    )

  def indexedField[A](
      volume: NeuroVol[A]
  ): Either[GridMismatch, IndexedField[S, A]] =
    GridCompatibility.volume(volumeSpace, volume.volumeSpace).map: _ =>
      IndexedField.tabulate(canonicalSpace): point =>
        volume.linear(legacyOrdinal(point))

  def supportWhere[A](
      field: IndexedField[S, A]
  )(
      predicate: A => Boolean
  ): Either[SpaceMismatch, Region[S]] =
    checkSpace(field.space).map: _ =>
      Region.tabulate(canonicalSpace)(point => predicate(field(point)))

  def recordRegion(region: Region[S]): VolumeOrdinalRecord =
    canonicalRecord(region.ordinalsInDomainOrder.toVector)

  def recordSelection(selection: Selection[S]): VolumeOrdinalRecord =
    canonicalRecord(selection.ordinals.toVector)

  def restoreRegion(
      record: VolumeOrdinalRecord
  ): Either[VolumeOrdinalBridgeError, Region[S]] =
    canonicalOrdinals(record).flatMap: ordinals =>
      Region
        .fromOrdinals(canonicalSpace, ordinals)
        .left
        .map(error =>
          VolumeOrdinalBridgeError.InvalidRecord(error.message)
        )

  def restoreSelection(
      record: VolumeOrdinalRecord
  ): Either[VolumeOrdinalBridgeError, Selection[S]] =
    canonicalOrdinals(record).flatMap: ordinals =>
      Selection
        .fromOrdinals(canonicalSpace, ordinals)
        .left
        .map(error =>
          VolumeOrdinalBridgeError.InvalidRecord(error.message)
        )

  /** Convert an explicitly versioned record to canonical ordinals.
    *
    * Unknown layouts are rejected. Legacy records are converted by the
    * certified coordinate-derived bijection; their integers are never
    * reinterpreted in place.
    */
  def canonicalOrdinals(
      record: VolumeOrdinalRecord
  ): Either[VolumeOrdinalBridgeError, Vector[Int]] =
    if record.grid != gridDomainRecord.grid then
      Left(
        VolumeOrdinalBridgeError.GridRecordMismatch(
          gridDomainRecord.grid,
          record.grid
        )
      )
    else
      record.layout match
        case VolumeOrdinalLayout.Image4sRowMajorLastAxisFastestV1 =>
          validateCanonical(record.ordinals)
        case VolumeOrdinalLayout.ScalaFimFirstAxisFastestV1 =>
          val out = Vector.newBuilder[Int]
          var index = 0
          var failure = Option.empty[VolumeOrdinalBridgeError]
          while index < record.ordinals.length && failure.isEmpty do
            canonicalPoint(record.ordinals(index)) match
              case Left(error) => failure = Some(error)
              case Right(point) => out += point.ordinal
            index += 1
          failure.toLeft(out.result())
        case layout: VolumeOrdinalLayout.Unrecognized =>
          Left(VolumeOrdinalBridgeError.UnsupportedLayout(layout))

  private def validateCanonical(
      ordinals: Vector[Int]
  ): Either[VolumeOrdinalBridgeError, Vector[Int]] =
    ordinals.find(value => value < 0 || value >= canonicalSpace.size) match
      case Some(value) =>
        Left(
          VolumeOrdinalBridgeError.InvalidCanonicalOrdinal(
            value,
            canonicalSpace.size
          )
        )
      case None => Right(ordinals)

  private def checkSpace[T](
      actual: FiniteDomain[T]
  ): Either[SpaceMismatch, Unit] =
    if canonicalSpace.sameRuntimeOwnerAs(actual) then Right(())
    else Left(SpaceMismatch.between(canonicalSpace, actual))

/** Fresh or registry-restored bridge with path-dependent domain owners. */
sealed trait VolumeOrdinalBridgeResolution:
  type S
  type L
  val registry: DomainRegistry
  val value: VolumeOrdinalBridge[S, L]

object VolumeOrdinalBridge:
  def register(
      volumeSpace: VolumeSpace,
      registry: DomainRegistry
  ): Either[VolumeOrdinalBridgeError, VolumeOrdinalBridgeResolution] =
    val sample = NeuroSpace.canonical(volumeSpace.toNeuroSpace)
    GridDomain
      .register(
        sample.grid,
        "ScalaFIM volume voxels",
        registry
      )
      .left
      .map(VolumeOrdinalBridgeError.CanonicalGrid.apply)
      .flatMap: canonicalResolution =>
        given Dimension[sample.D] = sample.dimension
        build(volumeSpace, canonicalResolution)

  private def build[
      F <: Frame[D],
      D <: Dim
  ](
      volumeSpace: VolumeSpace,
      canonicalResolution: image4s.locus.GridDomainResolution[F, D]
  )(using Dimension[D])
      : Either[VolumeOrdinalBridgeError, VolumeOrdinalBridgeResolution] =
    FiniteDomain
      .ephemeral(
        "ScalaFIM first-axis-fastest voxels",
        volumeSpace.nVoxels
      )
      .left
      .map(VolumeOrdinalBridgeError.LegacyDomain.apply)
      .flatMap: legacy =>
        val targets = Array.ofDim[Int](volumeSpace.nVoxels)
        var ordinal = 0
        var failure = Option.empty[VolumeOrdinalBridgeError]
        while ordinal < targets.length && failure.isEmpty do
          val coordinate =
            Indexing.indexToGrid3D(volumeSpace.shape, ordinal)
          LatticeIndex.fromVector[D](coordinate.toVector) match
            case Left(error) =>
              failure = Some(
                VolumeOrdinalBridgeError.CanonicalGrid(
                  GridDomainError.GeometryFailure(error)
                )
              )
            case Right(index) =>
              canonicalResolution.value.ordinalOf(index) match
                case Left(error) =>
                  failure = Some(
                    VolumeOrdinalBridgeError.CanonicalGrid(error)
                  )
                case Right(target) =>
                  targets(ordinal) = target
          ordinal += 1

        failure match
          case Some(error) => Left(error)
          case None =>
            TotalMap
              .fromTargetOrdinals(
                legacy.value,
                canonicalResolution.value.space,
                targets
              )
              .left
              .map(VolumeOrdinalBridgeError.InvalidTotalMap.apply)
              .flatMap: mapping =>
                Bijection
                  .fromTotalMap(mapping)
                  .left
                  .map(VolumeOrdinalBridgeError.InvalidBijection.apply)
                  .map: bijection =>
                    new VolumeOrdinalBridgeResolution:
                      type S = canonicalResolution.S
                      type L = legacy.S
                      val registry: DomainRegistry =
                        canonicalResolution.registry
                      val value: VolumeOrdinalBridge[S, L] =
                        new VolumeOrdinalBridge(
                          volumeSpace,
                          canonicalResolution.value.space,
                          legacy.value,
                          bijection,
                          canonicalResolution.value.record
                        )
