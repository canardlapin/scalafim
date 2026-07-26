package scalafim.image

import narr.NArray
import scala.reflect.ClassTag
import scalafim.locus.{
  CenteredSearchlight,
  CenteredSearchlightError as LocusCenteredSearchlightError,
  IndexedField,
  Point,
  Region,
  Relation,
  Searchlight as LocusSearchlight,
  SearchlightError as LocusSearchlightError,
  SpaceMismatch
}

enum VolumeSearchlightError:
  case WrongSpace(error: SpaceMismatch)
  case InvalidSearchlight(error: LocusSearchlightError)
  case NotCentered(error: LocusCenteredSearchlightError)
  case CenterUnavailable(pointOrdinal: Int)
  case CenterExcluded(pointOrdinal: Int)
  case InvalidWindow(error: ROIVolWindowError)

  def message: String =
    this match
      case WrongSpace(error) => error.message
      case InvalidSearchlight(error) => error.message
      case NotCentered(error) => error.message
      case CenterUnavailable(point) =>
        s"point $point is not an allowed searchlight center"
      case CenterExcluded(point) =>
        s"point $point is excluded by the requested field support"
      case InvalidWindow(error) => error.message

object VolumeSearchlight:
  def metricBalls[S](
      domain: VolumeDomain[S],
      radius: SearchlightRadius,
      centers: Region[S]
  ): Either[VolumeSearchlightError, CenteredSearchlight[S]] =
    if !domain.finiteSpace.sameIdentityAs(centers.space) then
      Left:
        VolumeSearchlightError.WrongSpace:
          SpaceMismatch(
            domain.finiteSpace.key,
            domain.finiteSpace.size,
            centers.space.key,
            centers.space.size
          )
    else
      val shape = domain.volumeSpace.shape
      val spacing = domain.volumeSpace.toNeuroSpace.spacing
      val distance = radius.millimeters
      val squaredRadius = distance * distance
      val deltas = Vector.tabulate(3): axis =>
        math.ceil(distance / spacing(axis)).toInt
      val rows = Array.fill(domain.finiteSpace.size)(Array.emptyIntArray)
      val centerOrdinals = centers.ordinalsInDomainOrder
      var centerIndex = 0
      while centerIndex < centerOrdinals.length do
        val centerOrdinal = centerOrdinals(centerIndex)
        val center = Indexing.indexToGrid3D(shape, centerOrdinal)
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
        rows(centerOrdinal) = targets.result()
        centerIndex += 1

      val relation =
        Relation
          .fromOrdinalRows(domain.finiteSpace, domain.finiteSpace, rows)
          .toOption
          .get
      LocusSearchlight
        .make(centers, relation)
        .left
        .map(VolumeSearchlightError.InvalidSearchlight.apply)
        .flatMap: searchlight =>
          CenteredSearchlight
            .validate(searchlight)
            .left
            .map(VolumeSearchlightError.NotCentered.apply)

  def metricBalls[S](
      domain: VolumeDomain[S],
      radius: SearchlightRadius
  ): Either[VolumeSearchlightError, CenteredSearchlight[S]] =
    metricBalls(domain, radius, Region.whole(domain.finiteSpace))

  def materialize[S, A](
      domain: VolumeDomain[S],
      searchlight: LocusSearchlight[S],
      center: Point[S],
      field: IndexedField[S, A],
      support: Option[Region[S]] = None,
      label: String = ""
  )(using ClassTag[A]): Either[VolumeSearchlightError, ROIVolWindow[A]] =
    for
      _ <- checkSpace(domain, searchlight.centers.space)
      _ <- checkSpace(domain, field.space)
      neighborhood <- searchlight
        .regionAt(center)
        .toRight(VolumeSearchlightError.CenterUnavailable(center.ordinal))
      restricted <- support match
        case Some(region) =>
          neighborhood
            .intersect(region)
            .left
            .map(VolumeSearchlightError.WrongSpace.apply)
        case None =>
          Right(neighborhood)
      _ <-
        if restricted.contains(center) then Right(())
        else Left(VolumeSearchlightError.CenterExcluded(center.ordinal))
      locusSelection = scalafim.locus.Selection.fromRegion(restricted)
      voxelSelection <- domain
        .voxelSelection(locusSelection)
        .left
        .map(VolumeSearchlightError.WrongSpace.apply)
      orderedPoints = locusSelection.points.toVector
      data =
        val owned = NArray.ofSize[A](orderedPoints.length)
        var i = 0
        while i < orderedPoints.length do
          owned(i) = field(orderedPoints(i))
          i += 1
        owned
      centerIndex = orderedPoints.indexWhere(_.ordinal == center.ordinal)
      window <- ROIVolWindow
        .fromOwned(
          domain.volumeSpace.toNeuroSpace,
          ROICoords(voxelSelection.voxelCoords.map(_.toVector)),
          data,
          centerIndex,
          center.ordinal,
          label
        )
        .left
        .map(VolumeSearchlightError.InvalidWindow.apply)
    yield window

  private def checkSpace[S, T](
      domain: VolumeDomain[S],
      actual: scalafim.locus.FiniteSpace[T]
  ): Either[VolumeSearchlightError, Unit] =
    if domain.finiteSpace.sameIdentityAs(actual) then Right(())
    else
      Left:
        VolumeSearchlightError.WrongSpace:
          SpaceMismatch(
            domain.finiteSpace.key,
            domain.finiteSpace.size,
            actual.key,
            actual.size
          )
