package scalafim.spatial

import scalafim.image.{Indexing, SpatialAxis, VoxelCoord}

enum DemandError:
  case EmptySelection(label: String)
  case DuplicateIndex(label: String, index: Int)
  case IndexOutOfBounds(label: String, index: Int, limit: Int)
  case DomainKindMismatch(selection: String, expected: DomainKind, actual: DomainKind)
  case DomainMismatch(expected: DomainId, actual: DomainId)
  case MaskLengthMismatch(expected: Int, actual: Int)
  case InvalidVoxelRegion(minInclusive: VoxelCoord, maxExclusive: VoxelCoord)
  case VoxelRegionOutOfBounds(maxExclusive: VoxelCoord, dimensions: Vector[Int])
  case InvalidTimeBlock(start: Int, length: Int, observations: Int)
  case StructuredSelectionRequiresFullDomain(selection: String)

  def message: String =
    this match
      case EmptySelection(label) =>
        s"$label selection must be non-empty"
      case DuplicateIndex(label, index) =>
        s"$label selection contains duplicate index $index"
      case IndexOutOfBounds(label, index, limit) =>
        s"$label index $index is outside [0, $limit)"
      case DomainKindMismatch(selection, expected, actual) =>
        s"$selection selection requires a $expected domain, got $actual"
      case DomainMismatch(expected, actual) =>
        s"demand domain ${actual.value} does not match view domain ${expected.value}"
      case MaskLengthMismatch(expected, actual) =>
        s"demand mask length $actual does not match target size $expected"
      case InvalidVoxelRegion(minInclusive, maxExclusive) =>
        s"voxel region [${minInclusive.toVector}, ${maxExclusive.toVector}) must be positive and non-empty"
      case VoxelRegionOutOfBounds(maxExclusive, dimensions) =>
        s"voxel region ending at ${maxExclusive.toVector} exceeds volume dimensions $dimensions"
      case InvalidTimeBlock(start, length, observations) =>
        s"time block start=$start length=$length is outside $observations observations"
      case StructuredSelectionRequiresFullDomain(selection) =>
        s"$selection selection requires an unselected target domain"

final case class VoxelRegion private (
  minInclusive: VoxelCoord,
  maxExclusive: VoxelCoord
)

object VoxelRegion:
  def build(
    minInclusive: VoxelCoord,
    maxExclusive: VoxelCoord
  ): Either[DemandError, VoxelRegion] =
    if minInclusive.x < 0 || minInclusive.y < 0 || minInclusive.z < 0 ||
       maxExclusive.x <= minInclusive.x ||
       maxExclusive.y <= minInclusive.y ||
       maxExclusive.z <= minInclusive.z then
      Left(DemandError.InvalidVoxelRegion(minInclusive, maxExclusive))
    else Right(new VoxelRegion(minInclusive, maxExclusive))

enum SpatialDemand:
  case Full
  case Rows(indices: Vector[Int])
  case Vertices(indices: Vector[Int])
  case Voxels(coords: Vector[VoxelCoord])
  case Roi(indices: Vector[Int])
  case Slice(axis: SpatialAxis, index: Int)
  case Mask(included: Vector[Boolean])
  case Region(region: VoxelRegion)

final case class TimeBlock private (start: Int, length: Int):
  def endExclusive: Int =
    start + length

object TimeBlock:
  def build(start: Int, length: Int): Either[DemandError, TimeBlock] =
    if start < 0 || length <= 0 then
      Left(DemandError.InvalidTimeBlock(start, length, 0))
    else Right(new TimeBlock(start, length))

enum ObservationDemand:
  case All
  case Block(block: TimeBlock)

enum ObservationSelection:
  case All
  case Indices(indices: Vector[Int])

  def size(fullSize: Int): Int =
    this match
      case ObservationSelection.All => fullSize
      case ObservationSelection.Indices(indices) => indices.length

final case class FieldDemand(
  spatial: SpatialDemand = SpatialDemand.Full,
  observations: ObservationDemand = ObservationDemand.All
)

object FieldDemand:
  val full: FieldDemand =
    FieldDemand()

  def rows(indices: Vector[Int]): FieldDemand =
    FieldDemand(spatial = SpatialDemand.Rows(indices))

  def vertices(indices: Vector[Int]): FieldDemand =
    FieldDemand(spatial = SpatialDemand.Vertices(indices))

  def voxels(coords: Vector[VoxelCoord]): FieldDemand =
    FieldDemand(spatial = SpatialDemand.Voxels(coords))

  def roi(indices: Vector[Int]): FieldDemand =
    FieldDemand(spatial = SpatialDemand.Roi(indices))

  def slice(axis: SpatialAxis, index: Int): FieldDemand =
    FieldDemand(spatial = SpatialDemand.Slice(axis, index))

  def mask(included: Vector[Boolean]): FieldDemand =
    FieldDemand(spatial = SpatialDemand.Mask(included))

  def region(region: VoxelRegion): FieldDemand =
    FieldDemand(spatial = SpatialDemand.Region(region))

  def time(block: TimeBlock): FieldDemand =
    FieldDemand(observations = ObservationDemand.Block(block))

opaque type DemandFingerprint = String

object DemandFingerprint:
  private[spatial] def unsafe(value: String): DemandFingerprint =
    value

  extension (fingerprint: DemandFingerprint)
    def value: String =
      fingerprint

final case class ResolvedDemand private (
  rowSelection: RowSelection,
  targetRows: Vector[Int],
  observationSelection: ObservationSelection,
  observationIndices: Vector[Int],
  targetSize: Int,
  rootObservationCount: Int,
  fingerprint: DemandFingerprint
):
  require(targetSize >= 0, "resolved demand target size must be non-negative")
  require(rootObservationCount >= 0, "resolved demand observation count must be non-negative")

  def sampleCount: Int =
    targetRows.length

  def observations: Int =
    observationIndices.length

  def isSpatiallyFull: Boolean =
    rowSelection == RowSelection.All

  def isTemporallyFull: Boolean =
    observationSelection == ObservationSelection.All

  def isFull: Boolean =
    isSpatiallyFull && isTemporallyFull

  def refine(
    requested: FieldDemand,
    domain: Domain
  ): Either[DemandError, ResolvedDemand] =
    if domain.nElements != targetSize then
      Left(DemandError.MaskLengthMismatch(targetSize, domain.nElements))
    else
      for
        nextRows <- refineRows(requested.spatial, domain)
        nextObservations <- refineObservations(requested.observations)
      yield ResolvedDemand.fromIndices(
        nextRows,
        nextObservations,
        targetSize,
        rootObservationCount
      )

  def refineRows(selection: RowSelection): Either[DemandError, ResolvedDemand] =
    val requested =
      selection match
        case RowSelection.All => SpatialDemand.Full
        case RowSelection.Rows(indices) => SpatialDemand.Rows(indices)
    refineRelativeRows(requested).map { rows =>
      ResolvedDemand.fromIndices(rows, observationIndices, targetSize, rootObservationCount)
    }

  private def refineRows(
    requested: SpatialDemand,
    domain: Domain
  ): Either[DemandError, Vector[Int]] =
    requested match
      case SpatialDemand.Full =>
        Right(targetRows)
      case SpatialDemand.Rows(indices) =>
        resolveIndices("row", indices, targetRows.length).map(_.map(targetRows))
      case other if !isSpatiallyFull =>
        Left(DemandError.StructuredSelectionRequiresFullDomain(selectionLabel(other)))
      case other =>
        resolveSpatial(other, domain)

  private def refineRelativeRows(
    requested: SpatialDemand
  ): Either[DemandError, Vector[Int]] =
    requested match
      case SpatialDemand.Full => Right(targetRows)
      case SpatialDemand.Rows(indices) =>
        resolveIndices("row", indices, targetRows.length).map(_.map(targetRows))
      case other =>
        Left(DemandError.StructuredSelectionRequiresFullDomain(selectionLabel(other)))

  private def refineObservations(
    requested: ObservationDemand
  ): Either[DemandError, Vector[Int]] =
    requested match
      case ObservationDemand.All =>
        Right(observationIndices)
      case ObservationDemand.Block(block) =>
        resolveTimeBlock(block, observationIndices.length).map(_.map(observationIndices))

object ResolvedDemand:
  def resolve(
    requested: FieldDemand,
    domain: Domain,
    observations: Int
  ): Either[DemandError, ResolvedDemand] =
    if observations < 0 then
      Left(DemandError.InvalidTimeBlock(0, observations, observations))
    else
      for
        rows <- resolveSpatial(requested.spatial, domain)
        columns <- resolveObservations(requested.observations, observations)
      yield fromIndices(rows, columns, domain.nElements, observations)

  def full(targetSize: Int, observations: Int): Either[DemandError, ResolvedDemand] =
    if targetSize < 0 then Left(DemandError.EmptySelection("target"))
    else if observations < 0 then Left(DemandError.InvalidTimeBlock(0, observations, observations))
    else Right(unsafeFull(targetSize, observations))

  private[spatial] def unsafeFull(targetSize: Int, observations: Int): ResolvedDemand =
    fromIndices(
      Vector.tabulate(targetSize)(identity),
      Vector.tabulate(observations)(identity),
      targetSize,
      observations
    )

  private[spatial] def fromIndices(
    rows: Vector[Int],
    observations: Vector[Int],
    targetSize: Int,
    rootObservationCount: Int
  ): ResolvedDemand =
    val rowSelection =
      if rows.length == targetSize && rows.indices.forall(i => rows(i) == i) then RowSelection.All
      else RowSelection.Rows(rows)
    val observationSelection =
      if observations.length == rootObservationCount && observations.indices.forall(i => observations(i) == i) then
        ObservationSelection.All
      else ObservationSelection.Indices(observations)
    val rowLabel = rows.mkString(",")
    val observationLabel = observations.mkString(",")
    ResolvedDemand(
      rowSelection = rowSelection,
      targetRows = rows,
      observationSelection = observationSelection,
      observationIndices = observations,
      targetSize = targetSize,
      rootObservationCount = rootObservationCount,
      fingerprint = DemandFingerprint.unsafe(s"demand-v1|rows=$rowLabel|observations=$observationLabel")
    )

private def resolveSpatial(
  requested: SpatialDemand,
  domain: Domain
): Either[DemandError, Vector[Int]] =
  requested match
    case SpatialDemand.Full =>
      Right(Vector.tabulate(domain.nElements)(identity))
    case SpatialDemand.Rows(indices) =>
      resolveIndices("row", indices, domain.nElements)
    case SpatialDemand.Vertices(indices) =>
      requireKind("vertex", domain, DomainKind.Surface)
        .flatMap(_ => resolveIndices("vertex", indices, domain.nElements))
    case SpatialDemand.Voxels(coords) =>
      volumeDimensions("voxel", domain).flatMap { dimensions =>
        if coords.isEmpty then Left(DemandError.EmptySelection("voxel"))
        else
          val rows = Vector.newBuilder[Int]
          val seen = scala.collection.mutable.HashSet.empty[Int]
          var i = 0
          var error = Option.empty[DemandError]
          while i < coords.length && error.isEmpty do
            val coord = coords(i)
            Indexing.gridToIndexChecked(dimensions, coord) match
              case Left(_) =>
                error = Some(DemandError.IndexOutOfBounds("voxel", i, domain.nElements))
              case Right(row) if seen.contains(row) =>
                error = Some(DemandError.DuplicateIndex("voxel", row))
              case Right(row) =>
                seen += row
                rows += row
            i += 1
          error.toLeft(rows.result())
      }
    case SpatialDemand.Roi(indices) =>
      resolveIndices("ROI", indices, domain.nElements)
    case SpatialDemand.Slice(axis, index) =>
      volumeDimensions("slice", domain).flatMap { dimensions =>
        val limit = dimensions(axis)
        if index < 0 || index >= limit then Left(DemandError.IndexOutOfBounds("slice", index, limit))
        else
          Right(
            Vector.tabulate(domain.nElements)(identity).filter { row =>
              Indexing.indexToGrid3D(dimensions, row)(axis) == index
            }
          )
      }
    case SpatialDemand.Mask(included) =>
      if included.length != domain.nElements then
        Left(DemandError.MaskLengthMismatch(domain.nElements, included.length))
      else
        val rows = included.indices.filter(included).toVector
        if rows.isEmpty then Left(DemandError.EmptySelection("mask"))
        else Right(rows)
    case SpatialDemand.Region(region) =>
      volumeDimensions("voxel region", domain).flatMap { dimensions =>
        if region.maxExclusive.x > dimensions.x ||
           region.maxExclusive.y > dimensions.y ||
           region.maxExclusive.z > dimensions.z then
          Left(DemandError.VoxelRegionOutOfBounds(region.maxExclusive, dimensions.toVector))
        else
          val rows = Vector.newBuilder[Int]
          var z = region.minInclusive.z
          while z < region.maxExclusive.z do
            var y = region.minInclusive.y
            while y < region.maxExclusive.y do
              var x = region.minInclusive.x
              while x < region.maxExclusive.x do
                rows += Indexing.gridToIndex3D(dimensions, x, y, z)
                x += 1
              y += 1
            z += 1
          Right(rows.result())
      }

private def resolveObservations(
  requested: ObservationDemand,
  observations: Int
): Either[DemandError, Vector[Int]] =
  requested match
    case ObservationDemand.All =>
      Right(Vector.tabulate(observations)(identity))
    case ObservationDemand.Block(block) =>
      resolveTimeBlock(block, observations)

private def resolveTimeBlock(
  block: TimeBlock,
  observations: Int
): Either[DemandError, Vector[Int]] =
  if block.start < 0 || block.length <= 0 || block.endExclusive > observations then
    Left(DemandError.InvalidTimeBlock(block.start, block.length, observations))
  else Right(Vector.range(block.start, block.endExclusive))

private def resolveIndices(
  label: String,
  indices: Vector[Int],
  limit: Int
): Either[DemandError, Vector[Int]] =
  if indices.isEmpty then Left(DemandError.EmptySelection(label))
  else
    val seen = scala.collection.mutable.HashSet.empty[Int]
    var i = 0
    var error = Option.empty[DemandError]
    while i < indices.length && error.isEmpty do
      val index = indices(i)
      if index < 0 || index >= limit then
        error = Some(DemandError.IndexOutOfBounds(label, index, limit))
      else if seen.contains(index) then
        error = Some(DemandError.DuplicateIndex(label, index))
      else seen += index
      i += 1
    error.toLeft(indices)

private def requireKind(
  selection: String,
  domain: Domain,
  expected: DomainKind
): Either[DemandError, Unit] =
  if domain.kind == expected then Right(())
  else Left(DemandError.DomainKindMismatch(selection, expected, domain.kind))

private def volumeDimensions(
  selection: String,
  domain: Domain
): Either[DemandError, scalafim.image.SpatialDims] =
  domain.geometry match
    case SamplingGeometry.Volume(space, _) => Right(space.spatialShape)
    case _ => Left(DemandError.DomainKindMismatch(selection, DomainKind.Volume, domain.kind))

private def selectionLabel(selection: SpatialDemand): String =
  selection match
    case SpatialDemand.Full => "full"
    case SpatialDemand.Rows(_) => "row"
    case SpatialDemand.Vertices(_) => "vertex"
    case SpatialDemand.Voxels(_) => "voxel"
    case SpatialDemand.Roi(_) => "ROI"
    case SpatialDemand.Slice(_, _) => "slice"
    case SpatialDemand.Mask(_) => "mask"
    case SpatialDemand.Region(_) => "voxel region"
