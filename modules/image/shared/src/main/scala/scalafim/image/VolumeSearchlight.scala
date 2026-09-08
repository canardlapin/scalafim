package scalafim.image

import ravel.DType
import ravel.NDArray as RavelArray
import ravel.Shape
import locus4s.{
  CenteredNeighborhoodSystem,
  Index,
  NeighborhoodSystemError,
  Region,
  Relation,
  RelationError,
  Selection,
  SelectionError,
  SpaceMismatch
}
import locus4s.data.Field
import scalafim.locus.mismatch

enum VolumeSearchlightError:
  case WrongSpace(error: SpaceMismatch)
  case InvalidMembership(error: RelationError)
  case InvalidNeighborhood(error: NeighborhoodSystemError)
  case CenterExcluded(pointOrdinal: Int)
  case InvalidWindow(error: ROIVolWindowError)
  case InvalidSelection(error: SelectionError)

  def message: String =
    this match
      case WrongSpace(error) => error.message
      case InvalidMembership(error) => error.message
      case InvalidNeighborhood(error) => error.message
      case CenterExcluded(point) =>
        s"point $point is excluded by the requested field support"
      case InvalidWindow(error) => error.message
      case InvalidSelection(error) => error.message

object VolumeSearchlight:
  def metricBalls[S](
      domain: VolumeDomain[S],
      radius: SearchlightRadius,
      centers: Selection[S]
  ): Either[
    VolumeSearchlightError,
    CenteredNeighborhoodSystem[centers.I, S]
  ] =
    if !domain.finiteSpace.sameRuntimeOwnerAs(centers.space) then
      Left:
        VolumeSearchlightError.WrongSpace:
          mismatch(domain.finiteSpace, centers.space)
    else
      val rows = metricBallRows(domain, radius, centers.ordinals)
      Relation
        .fromOrdinalRows(
          centers.positions,
          domain.finiteSpace,
          rows.iterator.map(_.iterator)
        )
        .left
        .map(VolumeSearchlightError.InvalidMembership.apply)
        .flatMap: membership =>
          CenteredNeighborhoodSystem
            .fromSelection(centers, membership)
            .left
            .map(VolumeSearchlightError.InvalidNeighborhood.apply)

  def metricBalls[S](
      domain: VolumeDomain[S],
      radius: SearchlightRadius
  ): Either[VolumeSearchlightError, CenteredNeighborhoodSystem[S, S]] =
    val centerOrdinals = Array.tabulate(domain.finiteSpace.size)(identity)
    val rows = metricBallRows(domain, radius, centerOrdinals)
    Relation
      .fromOrdinalRows(
        domain.finiteSpace,
        domain.finiteSpace,
        rows.iterator.map(_.iterator)
      )
      .left
      .map(VolumeSearchlightError.InvalidMembership.apply)
      .flatMap: membership =>
        CenteredNeighborhoodSystem
          .fromIdentityCenters(membership)
          .left
          .map(VolumeSearchlightError.InvalidNeighborhood.apply)

  def materialize[C, S, A](
      domain: VolumeDomain[S],
      searchlight: CenteredNeighborhoodSystem[C, S],
      center: Index[C],
      field: Field[S, A],
      support: Option[Region[S]] = None,
      label: String = ""
  )(using DType[A]): Either[VolumeSearchlightError, ROIVolWindow[A]] =
    for
      _ <- checkSpace(domain, searchlight.ambient)
      _ <- checkSpace(domain, field.space)
      neighborhood = searchlight.neighborhood(center)
      ambientCenter = searchlight.center(center)
      // `support` shares this region's `S`, which *is* the proof that both are
      // indexed by the same domain, so the intersection is total — there is no
      // mismatch left to report. Cross-domain callers would use
      // `intersectChecked`.
      restricted = support.fold(neighborhood)(neighborhood.intersect)
      _ <-
        if restricted.contains(ambientCenter) then Right(())
        else Left(VolumeSearchlightError.CenterExcluded(ambientCenter.value))
      locusSelection <- scalafim.locus.Selection
        .fromRegion(restricted)
        .left
        .map(VolumeSearchlightError.InvalidSelection.apply)
      voxelSelection <- domain
        .voxelSelection(locusSelection)
        .left
        .map(VolumeSearchlightError.WrongSpace.apply)
      orderedPoints = locusSelection.indices.toVector
      data =
        RavelArray.fromSeq(
          Shape(orderedPoints.length),
          // `apply` is total: an `Index[S]` is already proof of membership in `S`.
          orderedPoints.map(point => field(point))
        )
      centerIndex = orderedPoints.indexWhere(_.value == ambientCenter.value)
      window <- ROIVolWindow
        .fromOwned(
          domain.volumeSpace.toNeuroSpace,
          ROICoords(voxelSelection.voxelCoords.map(_.toVector)),
          data,
          centerIndex,
          ambientCenter.value,
          label
        )
        .left
        .map(VolumeSearchlightError.InvalidWindow.apply)
    yield window

  private def metricBallRows[S](
      domain: VolumeDomain[S],
      radius: SearchlightRadius,
      centerOrdinals: Array[Int]
  ): Array[Array[Int]] =
    val shape = domain.volumeSpace.shape
    val spacing = domain.volumeSpace.toNeuroSpace.spacing
    val distance = radius.millimeters
    val squaredRadius = distance * distance
    val deltas = Vector.tabulate(3): axis =>
      math.ceil(distance / spacing(axis)).toInt
    val rows = Array.ofDim[Array[Int]](centerOrdinals.length)
    var centerIndex = 0
    while centerIndex < centerOrdinals.length do
      val center = Indexing.indexToGrid3D(shape, centerOrdinals(centerIndex))
      val targets = Array.newBuilder[Int]
      var x = math.max(0, center.x - deltas(0))
      val maxX = math.min(shape.x - 1, center.x + deltas(0))
      while x <= maxX do
        var y = math.max(0, center.y - deltas(1))
        val maxY = math.min(shape.y - 1, center.y + deltas(1))
        while y <= maxY do
          var z = math.max(0, center.z - deltas(2))
          val maxZ = math.min(shape.z - 1, center.z + deltas(2))
          while z <= maxZ do
            val dx = (x - center.x) * spacing(0)
            val dy = (y - center.y) * spacing(1)
            val dz = (z - center.z) * spacing(2)
            if dx * dx + dy * dy + dz * dz <= squaredRadius then
              targets += Indexing.gridToIndex3D(shape, x, y, z)
            z += 1
          y += 1
        x += 1
      rows(centerIndex) = targets.result()
      centerIndex += 1
    rows

  private def checkSpace[S, T](
      domain: VolumeDomain[S],
      actual: scalafim.locus.FiniteDomain[T]
  ): Either[VolumeSearchlightError, Unit] =
    if domain.finiteSpace.sameRuntimeOwnerAs(actual) then Right(())
    else
      Left:
        VolumeSearchlightError.WrongSpace:
          mismatch(domain.finiteSpace, actual)
