package scalafim.image

import SampleSpaces.*

import image4s.ImageMetadata
import image4s.Categorical
import image4s.Continuous
import image4s.ValueSemantics
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.locus.GridDomain
import locus4s.CenteredNeighborhoodSystem
import locus4s.NeighborhoodSystemError
import locus4s.Index
import locus4s.Region
import locus4s.Relation
import locus4s.RelationError
import locus4s.Selection
import locus4s.SelectionError
import locus4s.SpaceMismatch
import locus4s.data.Field
import ravel.DType
import ravel.NDArray
import scala.util.Random

enum ExactVolumeSearchlightError:
  case WrongSpace(error: SpaceMismatch)
  case InvalidRelation(error: RelationError)
  case NonEmptyOutsideCenters(pointOrdinal: Int)
  case MissingCenter(pointOrdinal: Int)
  case CenterExcluded(pointOrdinal: Int)
  case InvalidSelection(error: SelectionError)
  case InvalidSelectedImage(error: SelectedImageError)
  case InvalidWindow(error: SelectedVolumeWindowError)
  case InvalidScales(values: Vector[Double])
  case InvalidJitter(value: Double)
  case InvalidDrop(value: Double)
  case InvalidEdgeFraction(value: Double)

  def message: String =
    this match
      case WrongSpace(error) => error.message
      case InvalidRelation(error) => error.message
      case NonEmptyOutsideCenters(point) =>
        s"searchlight relation row $point is non-empty outside the center region"
      case MissingCenter(point) =>
        s"searchlight neighborhood at $point does not contain its center"
      case CenterExcluded(point) =>
        s"point $point is excluded by the requested field support"
      case InvalidSelection(error) => error.message
      case InvalidSelectedImage(error) => error.message
      case InvalidWindow(error) => error.message
      case InvalidScales(values) =>
        s"ellipsoid scales must contain three finite positive values; got $values"
      case InvalidJitter(value) =>
        s"ellipsoid jitter must be finite and non-negative; got $value"
      case InvalidDrop(value) =>
        s"blobby-ball drop probability must be finite and in [0, 1]; got $value"
      case InvalidEdgeFraction(value) =>
        s"blobby-ball edge fraction must be finite and in (0, 1]; got $value"

/** Reusable exact searchlight support. Geometry and selection construction are
  * workload preparation; applying image values retains only the compact Ravel
  * destination and this already-certified selection.
  */
final class PreparedVolumeWindow[S] private[image] (
    val selection: Selection[S],
    val center: Index[S],
    val centerPosition: Int
)

object ExactVolumeSearchlight:
  /** Validate an exact sparse neighborhood relation as centered searchlight
    * policy. Both relation ends must be the same live owner as `centers`.
    */
  def fromRelation[S](
      centers: Region[S],
      relation: Relation[S, S]
  ): Either[ExactVolumeSearchlightError, CenteredNeighborhoodSystem[?, S]] =
    if !centers.space.sameRuntimeOwnerAs(relation.from) then
      Left(
        ExactVolumeSearchlightError.WrongSpace(
          SpaceMismatch.between(centers.space, relation.from)
        )
      )
    else if !centers.space.sameRuntimeOwnerAs(relation.to) then
      Left(
        ExactVolumeSearchlightError.WrongSpace(
          SpaceMismatch.between(centers.space, relation.to)
        )
      )
    else
      val checked = centeredRows(centers): center =>
        relation.row(center).ordinalsInDomainOrder
      var ordinal = 0
      var invalidOutside = -1
      while ordinal < centers.space.size && invalidOutside < 0 do
        val point = centers.space.indexAtValidatedOrdinal(ordinal)
        if !centers.contains(point) && !relation.row(point).isEmpty then
          invalidOutside = ordinal
        ordinal += 1
      checked match
        case Left(ExactVolumeSearchlightError.MissingCenter(missing))
            if invalidOutside < 0 || missing < invalidOutside => checked
        case _ if invalidOutside >= 0 =>
          Left(ExactVolumeSearchlightError.NonEmptyOutsideCenters(invalidOutside))
        case _ => checked

  def prepare[C, S](
      searchlight: CenteredNeighborhoodSystem[C, S],
      center: Index[C],
      support: Option[Region[S]] = None
  ): Either[ExactVolumeSearchlightError, PreparedVolumeWindow[S]] =
    val ambientCenter = searchlight.center(center)
    val neighborhood = searchlight.neighborhood(center)
    for
      restricted <- support match
        case Some(region) if !searchlight.ambient.sameRuntimeOwnerAs(region.space) =>
          Left(ExactVolumeSearchlightError.WrongSpace(
            SpaceMismatch.between(searchlight.ambient, region.space)
          ))
        case _ => Right(support.fold(neighborhood)(neighborhood.intersect))
      _ <-
        if restricted.contains(ambientCenter) then Right(())
        else Left(ExactVolumeSearchlightError.CenterExcluded(ambientCenter.ordinal))
      selection <- Selection
        .fromRegion(restricted)
        .left
        .map(ExactVolumeSearchlightError.InvalidSelection.apply)
      centerPosition =
        var position = 0
        var found = -1
        while position < selection.size && found < 0 do
          val selectedPosition = selection.positions.indexAtValidatedOrdinal(position)
          if selection(selectedPosition).ordinal == ambientCenter.ordinal then found = position
          position += 1
        found
    yield new PreparedVolumeWindow(selection, ambientCenter, centerPosition)

  def metricBalls[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      radius: SearchlightRadius,
      centers: Region[S]
  ): Either[ExactVolumeSearchlightError, CenteredNeighborhoodSystem[?, S]] =
    if !domain.space.sameRuntimeOwnerAs(centers.space) then
      Left(
        ExactVolumeSearchlightError.WrongSpace(
          SpaceMismatch.between(domain.space, centers.space)
        )
      )
    else
      val distance = radius.millimeters
      val squaredRadius = distance * distance
      val deltas = physicalBallBounds(domain, distance, Vector(1.0, 1.0, 1.0))
      buildNeighborhoods(domain, centers, deltas): (dx, dy, dz) =>
        worldSquaredDistance(domain, dx, dy, dz) <= squaredRadius

  def metricBalls[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      radius: SearchlightRadius
  ): Either[ExactVolumeSearchlightError, CenteredNeighborhoodSystem[?, S]] =
    metricBalls(domain, radius, Region.whole(domain.space))

  /** Grid-axis-aligned boxes with half-width expressed in physical units. */
  def cubes[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      radius: SearchlightRadius,
      centers: Region[S]
  ): Either[ExactVolumeSearchlightError, CenteredNeighborhoodSystem[?, S]] =
    checkSpace(domain, centers.space).flatMap: _ =>
      val distance = radius.millimeters
      val deltas = Vector.tabulate(3): axis =>
        math.ceil(distance / axisSpacing(domain, axis)).toInt
      buildNeighborhoods(domain, centers, deltas)((_, _, _) => true)

  def cubes[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      radius: SearchlightRadius
  ): Either[ExactVolumeSearchlightError, CenteredNeighborhoodSystem[?, S]] =
    cubes(domain, radius, Region.whole(domain.space))

  /** Ellipsoids obtained by scaling the grid-axis contributions before
    * applying the grid's complete affine linear transform.
    *
    * Optional jitter is sampled once per center while constructing the
    * immutable relation. The resulting relation itself is deterministic and
    * contains no retained random state.
    */
  def ellipsoids[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      radius: SearchlightRadius,
      scales: Vector[Double],
      centers: Region[S],
      jitter: Double = 0.0,
      rng: Random = new Random(0L)
  ): Either[ExactVolumeSearchlightError, CenteredNeighborhoodSystem[?, S]] =
    if scales.length != 3 || scales.exists(value => !value.isFinite || value <= 0.0)
    then Left(ExactVolumeSearchlightError.InvalidScales(scales))
    else if !jitter.isFinite || jitter < 0.0 then
      Left(ExactVolumeSearchlightError.InvalidJitter(jitter))
    else
      checkSpace(domain, centers.space).flatMap: _ =>
        buildNeighborhoods(domain, centers): center =>
          val actualScales =
            if jitter == 0.0 then scales
            else
              scales.map: scale =>
                val candidate = scale * (1.0 + rng.nextGaussian() * jitter)
                if candidate.isFinite then math.max(candidate, 1e-12)
                else Double.MaxValue
          val distance = radius.millimeters
          val squaredRadius = distance * distance
          val deltas = physicalBallBounds(domain, distance, actualScales)
          boundedOrdinals(domain, center, deltas): (dx, dy, dz) =>
            worldSquaredDistance(
              domain,
              dx.toDouble * actualScales(0),
              dy.toDouble * actualScales(1),
              dz.toDouble * actualScales(2)
            ) <= squaredRadius

  def ellipsoids[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      radius: SearchlightRadius,
      scales: Vector[Double]
  ): Either[ExactVolumeSearchlightError, CenteredNeighborhoodSystem[?, S]] =
    ellipsoids(
      domain,
      radius,
      scales,
      Region.whole(domain.space)
    )

  /** Metric balls with randomized edge deletion, materialized once as an
    * exact relation. Centers are always retained.
    */
  def blobbyBalls[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      radius: SearchlightRadius,
      drop: Double,
      edgeFraction: Double,
      centers: Region[S],
      rng: Random = new Random(0L)
  ): Either[ExactVolumeSearchlightError, CenteredNeighborhoodSystem[?, S]] =
    if !drop.isFinite || drop < 0.0 || drop > 1.0 then
      Left(ExactVolumeSearchlightError.InvalidDrop(drop))
    else if !edgeFraction.isFinite || edgeFraction <= 0.0 || edgeFraction > 1.0
    then Left(ExactVolumeSearchlightError.InvalidEdgeFraction(edgeFraction))
    else
      checkSpace(domain, centers.space).flatMap: _ =>
        buildNeighborhoods(domain, centers): center =>
          val distance = radius.millimeters
          val squaredRadius = distance * distance
          val deltas = physicalBallBounds(domain, distance, Vector(1.0, 1.0, 1.0))
          val ball =
            boundedOrdinals(domain, center, deltas): (dx, dy, dz) =>
              worldSquaredDistance(domain, dx, dy, dz) <= squaredRadius
          val distances = ball.map: ordinal =>
            val target = Indexing.indexToGrid3D(domain.grid.shape, ordinal)
            math.sqrt(
              worldSquaredDistance(
                domain,
                target(0) - center.x,
                target(1) - center.y,
                target(2) - center.z
              )
            )
          val threshold =
            distances.sorted.apply(
              math.floor(edgeFraction * (distances.length - 1)).toInt
            )
          val centerOrdinal = ordinalOf(domain, center)
          ball.indices.collect:
            case index
                if ball(index) == centerOrdinal ||
                  distances(index) < threshold ||
                  rng.nextDouble() >= drop =>
              ball(index)

  def blobbyBalls[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      radius: SearchlightRadius,
      drop: Double,
      edgeFraction: Double,
      rng: Random
  ): Either[ExactVolumeSearchlightError, CenteredNeighborhoodSystem[?, S]] =
    blobbyBalls(
      domain,
      radius,
      drop,
      edgeFraction,
      Region.whole(domain.space),
      rng
    )

  /** Intersect every neighborhood with one exact support region.
    *
    * Centers are retained, so the operation fails when the supplied support
    * excludes any center instead of silently producing an invalid searchlight.
    */
  def restrictTargets[C, S](
      searchlight: CenteredNeighborhoodSystem[C, S],
      support: Region[S]
  ): Either[ExactVolumeSearchlightError, CenteredNeighborhoodSystem[C, S]] =
    if !searchlight.ambient.sameRuntimeOwnerAs(support.space) then
      Left(ExactVolumeSearchlightError.WrongSpace(
        SpaceMismatch.between(searchlight.ambient, support.space)
      ))
    else
      Relation.fromOrdinalRows(
        searchlight.centers,
        searchlight.ambient,
        searchlight.centers.indices.map: center =>
          searchlight.neighborhood(center).intersect(support).ordinalsInDomainOrder
      ).left.map(ExactVolumeSearchlightError.InvalidRelation.apply)
        .flatMap(membership => CenteredNeighborhoodSystem
          .from(searchlight.centerEmbedding, membership)
          .left.map(neighborhoodError))

  def materialize[F <: Frame[D3], C, S, A, Sem](
      domain: GridDomain[F, D3, S],
      searchlight: CenteredNeighborhoodSystem[C, S],
      center: Index[C],
      field: Field[S, A],
      support: Option[Region[S]] = None,
      label: String = ""
  )(using
      DType[A],
      ValueSemantics[A, Sem]
  ): Either[
    ExactVolumeSearchlightError,
    SelectedVolumeWindow[F, S, A, Sem]
  ] =
    for
      _ <- checkSpace(domain, searchlight.ambient)
      _ <- checkSpace(domain, field.space)
      prepared <- prepare(searchlight, center, support)
      data = NDArray.build[A, ravel.Rank[1]](
        ravel.Shape(prepared.selection.size)
      ): output =>
        var position = 0
        while position < prepared.selection.size do
          val selectedPosition =
            prepared.selection.positions.indexAtValidatedOrdinal(position)
          output.writeLinear(
            position,
            field(prepared.selection(selectedPosition))
          )
          position += 1
      selected <- SelectedVolume
        .create(
          domain,
          prepared.selection,
          data,
          ImageMetadata(label)
        )
        .left
        .map(ExactVolumeSearchlightError.InvalidSelectedImage.apply)
      window <- SelectedVolumeWindow
        .make(selected, prepared.center, prepared.centerPosition)
        .left
        .map(ExactVolumeSearchlightError.InvalidWindow.apply)
    yield window

  /** Apply one prepared searchlight directly to a native D3 image. This hot
    * path delegates dense-to-selected execution to image4s-locus and does not
    * rebuild the exact selection for every image or statistical map.
    */
  def materializePreparedVolume[
      F <: Frame[D3],
      S,
      A,
      Sem
  ](
      domain: GridDomain[F, D3, S],
      prepared: PreparedVolumeWindow[S],
      volume: SomeNeuroVolume[A, Sem],
      label: String = ""
  )(using
      DType[A],
      ValueSemantics[A, Sem]
  ): Either[
    ExactVolumeSearchlightError,
    SelectedVolumeWindow[F, S, A, Sem]
  ] =
    val labeled =
      if label.isEmpty then volume
      else SomeNeuroVolume.unsafeFromSampled(
        SomeNeuroVolume.sampled(volume).withMetadata(ImageMetadata(label))
      )
    for
      selected <- SelectedVolume
        .gather(domain, labeled, prepared.selection)
        .left
        .map(ExactVolumeSearchlightError.InvalidSelectedImage.apply)
      window <- SelectedVolumeWindow
        .make(
          selected,
          prepared.center,
          prepared.centerPosition
        )
        .left
        .map(ExactVolumeSearchlightError.InvalidWindow.apply)
    yield window

  def materializeVolume[F <: Frame[D3], C, S, A, Sem](
      domain: GridDomain[F, D3, S],
      searchlight: CenteredNeighborhoodSystem[C, S],
      center: Index[C],
      volume: SomeNeuroVolume[A, Sem],
      support: Option[Region[S]] = None,
      label: String = ""
  )(using
      DType[A],
      ValueSemantics[A, Sem]
  ): Either[
    ExactVolumeSearchlightError,
    SelectedVolumeWindow[F, S, A, Sem]
  ] =
    for
      _ <- checkSpace(domain, searchlight.ambient)
      prepared <- prepare(searchlight, center, support)
      window <- materializePreparedVolume(
        domain,
        prepared,
        volume,
        label
      )
    yield window

  /** Materialize a scalar-valued searchlight without asking inference to
    * choose between the continuous and categorical meanings available for
    * several primitive element types.
    */
  def materializeContinuous[F <: Frame[D3], C, S, A](
      domain: GridDomain[F, D3, S],
      searchlight: CenteredNeighborhoodSystem[C, S],
      center: Index[C],
      field: Field[S, A],
      support: Option[Region[S]] = None,
      label: String = ""
  )(using
      DType[A],
      ValueSemantics[A, Continuous]
  ): Either[
    ExactVolumeSearchlightError,
    SelectedVolumeWindow[F, S, A, Continuous]
  ] =
    materialize[F, C, S, A, Continuous](
      domain,
      searchlight,
      center,
      field,
      support,
      label
    )

  /** Materialize a label-valued searchlight with categorical semantics. */
  def materializeCategorical[F <: Frame[D3], C, S, A](
      domain: GridDomain[F, D3, S],
      searchlight: CenteredNeighborhoodSystem[C, S],
      center: Index[C],
      field: Field[S, A],
      support: Option[Region[S]] = None,
      label: String = ""
  )(using
      DType[A],
      ValueSemantics[A, Categorical]
  ): Either[
    ExactVolumeSearchlightError,
    SelectedVolumeWindow[F, S, A, Categorical]
  ] =
    materialize[F, C, S, A, Categorical](
      domain,
      searchlight,
      center,
      field,
      support,
      label
    )

  private def buildNeighborhoods[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      centers: Region[S],
      deltas: Vector[Int]
  )(
      include: (Int, Int, Int) => Boolean
  ): Either[ExactVolumeSearchlightError, CenteredNeighborhoodSystem[?, S]] =
    buildNeighborhoods(domain, centers): center =>
      boundedOrdinals(domain, center, deltas)(include)

  private def buildNeighborhoods[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      centers: Region[S]
  )(
      rowAt: VoxelCoord => IterableOnce[Int]
  ): Either[ExactVolumeSearchlightError, CenteredNeighborhoodSystem[?, S]] =
    centeredRows(centers): centerIndex =>
      val lattice = Indexing.indexToGrid3D(domain.grid.shape, centerIndex.ordinal)
      rowAt(VoxelCoord(lattice(0), lattice(1), lattice(2)))

  private def centeredRows[S](centers: Region[S])(
      rowAt: Index[S] => IterableOnce[Int]
  ): Either[ExactVolumeSearchlightError, CenteredNeighborhoodSystem[?, S]] =
    for
      selected <- Selection.fromRegion(centers)
        .left.map(ExactVolumeSearchlightError.InvalidSelection.apply)
      membership <- Relation.fromOrdinalRows(
        selected.positions,
        centers.space,
        selected.indices.map(rowAt)
      ).left.map(ExactVolumeSearchlightError.InvalidRelation.apply)
      centered <- CenteredNeighborhoodSystem.fromSelection(selected, membership)
        .left.map(neighborhoodError)
    yield centered

  private def neighborhoodError(error: NeighborhoodSystemError): ExactVolumeSearchlightError =
    error match
      case NeighborhoodSystemError.MissingEmbeddedCenter(_, ambient) =>
        ExactVolumeSearchlightError.MissingCenter(ambient)
      case NeighborhoodSystemError.MembershipCenterOwnerMismatch(mismatch) =>
        ExactVolumeSearchlightError.WrongSpace(mismatch)
      case NeighborhoodSystemError.MembershipAmbientOwnerMismatch(mismatch) =>
        ExactVolumeSearchlightError.WrongSpace(mismatch)

  private def boundedOrdinals[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      center: VoxelCoord,
      deltas: Vector[Int]
  )(
      include: (Int, Int, Int) => Boolean
  ): Vector[Int] =
    val shape = domain.grid.shape
    val targets = Vector.newBuilder[Int]
    var x = math.max(0, center.x - deltas(0))
    val maxX = math.min(shape(0) - 1, center.x + deltas(0))
    while x <= maxX do
      var y = math.max(0, center.y - deltas(1))
      val maxY = math.min(shape(1) - 1, center.y + deltas(1))
      while y <= maxY do
        var z = math.max(0, center.z - deltas(2))
        val maxZ = math.min(shape(2) - 1, center.z + deltas(2))
        while z <= maxZ do
          val dx = x - center.x
          val dy = y - center.y
          val dz = z - center.z
          if include(dx, dy, dz) then
            targets += ordinalOf(domain, VoxelCoord(x, y, z))
          z += 1
        y += 1
      x += 1
    targets.result()

  private def ordinalOf[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      voxel: VoxelCoord
  ): Int =
    Indexing.gridToIndex3D(
      domain.grid.shape,
      voxel.x,
      voxel.y,
      voxel.z
    )

  private def axisSpacing[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      axis: Int
  ): Double =
    val matrix = domain.grid.indexToFrame.matrix
    math.sqrt(
      matrix(0, axis) * matrix(0, axis) +
        matrix(1, axis) * matrix(1, axis) +
        matrix(2, axis) * matrix(2, axis)
    )

  private def physicalBallBounds[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      radius: Double,
      gridAxisScales: Vector[Double]
  ): Vector[Int] =
    val inverse = domain.grid.indexToFrame.inverse.matrix
    Vector.tabulate(3): axis =>
      val inverseRowNorm =
        math.sqrt(
          inverse(axis, 0) * inverse(axis, 0) +
            inverse(axis, 1) * inverse(axis, 1) +
            inverse(axis, 2) * inverse(axis, 2)
        )
      math.ceil(radius * inverseRowNorm / gridAxisScales(axis)).toInt

  private def worldSquaredDistance[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      dx: Double,
      dy: Double,
      dz: Double
  ): Double =
    val matrix = domain.grid.indexToFrame.matrix
    val wx = matrix(0, 0) * dx + matrix(0, 1) * dy + matrix(0, 2) * dz
    val wy = matrix(1, 0) * dx + matrix(1, 1) * dy + matrix(1, 2) * dz
    val wz = matrix(2, 0) * dx + matrix(2, 1) * dy + matrix(2, 2) * dz
    wx * wx + wy * wy + wz * wz

  private def checkSpace[F <: Frame[D3], S, T](
      domain: GridDomain[F, D3, S],
      actual: locus4s.FiniteDomain[T]
  ): Either[ExactVolumeSearchlightError, Unit] =
    if domain.space.sameRuntimeOwnerAs(actual) then Right(())
    else
      Left(
        ExactVolumeSearchlightError.WrongSpace(
          SpaceMismatch.between(domain.space, actual)
        )
      )
