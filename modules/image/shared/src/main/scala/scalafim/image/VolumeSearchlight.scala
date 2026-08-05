package scalafim.image

import ravel.DType
import ravel.NDArray as RavelArray
import ravel.Shape
import scalafim.locus.{
  CenteredSearchlight,
  CenteredSearchlightError as LocusCenteredSearchlightError,
  IndexedField,
  Point,
  Region,
  Relation,
  Searchlight as LocusSearchlight,
  SearchlightError as LocusSearchlightError,
  SelectionError,
  SpaceMismatch,
  mismatch
}

enum VolumeSearchlightError:
  case WrongSpace(error: SpaceMismatch)
  case InvalidSearchlight(error: LocusSearchlightError)
  case NotCentered(error: LocusCenteredSearchlightError)
  case CenterUnavailable(pointOrdinal: Int)
  case CenterExcluded(pointOrdinal: Int)
  case InvalidWindow(error: ROIVolWindowError)
  case InvalidSelection(error: SelectionError)

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
      case InvalidSelection(error) => error.message

object VolumeSearchlight:
  def metricBalls[S](
      domain: VolumeDomain[S],
      radius: SearchlightRadius,
      centers: Region[S]
  ): Either[VolumeSearchlightError, CenteredSearchlight[S]] =
    if !domain.finiteSpace.sameRuntimeOwnerAs(centers.space) then
      Left:
        VolumeSearchlightError.WrongSpace:
          mismatch(domain.finiteSpace, centers.space)
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
          .fromOrdinalRows(
            domain.finiteSpace,
            domain.finiteSpace,
            rows.iterator.map(_.iterator)
          )
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
  )(using DType[A]): Either[VolumeSearchlightError, ROIVolWindow[A]] =
    for
      _ <- checkSpace(domain, searchlight.centers.space)
      _ <- checkSpace(domain, field.space)
      neighborhood <- searchlight
        .regionAt(center)
        .toRight(VolumeSearchlightError.CenterUnavailable(center.value))
      // `support` shares this region's `S`, which *is* the proof that both are
      // indexed by the same domain, so the intersection is total — there is no
      // mismatch left to report. Cross-domain callers would use
      // `intersectChecked`.
      restricted = support.fold(neighborhood)(neighborhood.intersect)
      _ <-
        if restricted.contains(center) then Right(())
        else Left(VolumeSearchlightError.CenterExcluded(center.value))
      locusSelection <- scalafim.locus.Selection
        .fromRegion(restricted)
        .left
        .map(VolumeSearchlightError.InvalidSelection.apply)
      voxelSelection <- domain
        .voxelSelection(locusSelection)
        .left
        .map(VolumeSearchlightError.WrongSpace.apply)
      orderedIndices = locusSelection.indices.toVector
      data =
        RavelArray.fromSeq(
          Shape(orderedIndices.length),
          orderedIndices.map(field.apply)
        )
      centerIndex = orderedIndices.indexWhere(_.value == center.value)
      window <- ROIVolWindow
        .fromOwned(
          domain.volumeSpace.toNeuroSpace,
          ROICoords(voxelSelection.voxelCoords.map(_.toVector)),
          data,
          centerIndex,
          center.value,
          label
        )
        .left
        .map(VolumeSearchlightError.InvalidWindow.apply)
    yield window

  private def checkSpace[S, T](
      domain: VolumeDomain[S],
      actual: scalafim.locus.FiniteDomain[T]
  ): Either[VolumeSearchlightError, Unit] =
    if domain.finiteSpace.sameRuntimeOwnerAs(actual) then Right(())
    else
      Left:
        VolumeSearchlightError.WrongSpace:
          mismatch(domain.finiteSpace, actual)
