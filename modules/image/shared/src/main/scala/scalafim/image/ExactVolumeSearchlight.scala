package scalafim.image

import image4s.ImageMetadata
import image4s.Categorical
import image4s.Continuous
import image4s.ValueSemantics
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.LatticeIndex
import image4s.locus.GridDomain
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

enum ExactVolumeSearchlightError:
  case WrongSpace(error: SpaceMismatch)
  case InvalidRelation(error: RelationError)
  case NonEmptyOutsideCenters(pointOrdinal: Int)
  case MissingCenter(pointOrdinal: Int)
  case CenterUnavailable(pointOrdinal: Int)
  case CenterExcluded(pointOrdinal: Int)
  case InvalidSelection(error: SelectionError)
  case InvalidSelectedImage(error: SelectedImageError)
  case InvalidWindow(error: SelectedVolumeWindowError)

  def message: String =
    this match
      case WrongSpace(error) => error.message
      case InvalidRelation(error) => error.message
      case NonEmptyOutsideCenters(point) =>
        s"searchlight relation row $point is non-empty outside the center region"
      case MissingCenter(point) =>
        s"searchlight neighborhood at $point does not contain its center"
      case CenterUnavailable(point) =>
        s"point $point is not an allowed searchlight center"
      case CenterExcluded(point) =>
        s"point $point is excluded by the requested field support"
      case InvalidSelection(error) => error.message
      case InvalidSelectedImage(error) => error.message
      case InvalidWindow(error) => error.message

/** Domain-specific policy pairing allowed centers with exact locus rows. */
final class VolumeNeighborhoods[S] private[image] (
    val centers: Region[S],
    val relation: Relation[S, S]
):
  def regionAt(center: Index[S]): Option[Region[S]] =
    Option.when(centers.contains(center))(relation.row(center))

object ExactVolumeSearchlight:
  /** Validate an exact sparse neighborhood relation as centered searchlight
    * policy. Both relation ends must be the same live owner as `centers`.
    */
  def fromRelation[S](
      centers: Region[S],
      relation: Relation[S, S]
  ): Either[ExactVolumeSearchlightError, VolumeNeighborhoods[S]] =
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
    else validateCentered(centers, relation)

  def metricBalls[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      radius: SearchlightRadius,
      centers: Region[S]
  ): Either[ExactVolumeSearchlightError, VolumeNeighborhoods[S]] =
    if !domain.space.sameRuntimeOwnerAs(centers.space) then
      Left(
        ExactVolumeSearchlightError.WrongSpace(
          SpaceMismatch.between(domain.space, centers.space)
        )
      )
    else
      val shape = domain.grid.shape
      val affine = domain.grid.indexToFrame
      val spacing = Vector.tabulate(3): axis =>
        math.sqrt(
          affine.matrix(0, axis) * affine.matrix(0, axis) +
            affine.matrix(1, axis) * affine.matrix(1, axis) +
            affine.matrix(2, axis) * affine.matrix(2, axis)
        )
      val distance = radius.millimeters
      val squaredRadius = distance * distance
      val deltas = Vector.tabulate(3): axis =>
        math.ceil(distance / spacing(axis)).toInt
      val rows = Array.fill(domain.space.size)(Array.emptyIntArray)
      val centerOrdinals = centers.ordinalsInDomainOrder
      var centerIndex = 0
      while centerIndex < centerOrdinals.length do
        val centerOrdinal = centerOrdinals(centerIndex)
        val centerLattice =
          domain.indexOfOrdinal(centerOrdinal).toOption.get
        val center = VoxelCoord(
          centerLattice.values(0),
          centerLattice.values(1),
          centerLattice.values(2)
        )
        val targets = Array.newBuilder[Int]
        var x = math.max(0, center.x - deltas(0))
        val maxX = math.min(shape(0) - 1, center.x + deltas(0))
        while x <= maxX do
          var y = math.max(0, center.y - deltas(1))
          val maxY = math.min(shape(1) - 1, center.y + deltas(1))
          while y <= maxY do
            var z = math.max(0, center.z - deltas(2))
            val maxZ = math.min(shape(2) - 1, center.z + deltas(2))
            while z <= maxZ do
              val dx = (x - center.x) * spacing(0)
              val dy = (y - center.y) * spacing(1)
              val dz = (z - center.z) * spacing(2)
              if dx * dx + dy * dy + dz * dz <= squaredRadius then
                val target =
                  LatticeIndex
                    .fromVector[D3](Vector(x, y, z))
                    .toOption
                    .get
                targets += domain.ordinalOf(target).toOption.get
              z += 1
            y += 1
          x += 1
        rows(centerOrdinal) = targets.result()
        centerIndex += 1

      Relation
        .fromOrdinalRows(
          domain.space,
          domain.space,
          rows.iterator.map(_.iterator)
        )
        .left
        .map(ExactVolumeSearchlightError.InvalidRelation.apply)
        .flatMap(fromRelation(centers, _))

  def metricBalls[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      radius: SearchlightRadius
  ): Either[ExactVolumeSearchlightError, VolumeNeighborhoods[S]] =
    metricBalls(domain, radius, Region.whole(domain.space))

  /** Intersect every neighborhood with one exact support region.
    *
    * Centers are retained, so the operation fails when the supplied support
    * excludes any center instead of silently producing an invalid searchlight.
    */
  def restrictTargets[S](
      searchlight: VolumeNeighborhoods[S],
      support: Region[S]
  ): Either[ExactVolumeSearchlightError, VolumeNeighborhoods[S]] =
    if !searchlight.centers.space.sameRuntimeOwnerAs(support.space) then
      Left(
        ExactVolumeSearchlightError.WrongSpace(
          SpaceMismatch.between(searchlight.centers.space, support.space)
        )
      )
    else
      Relation
        .fromOrdinalRows(
          searchlight.centers.space,
          searchlight.centers.space,
          Iterator.tabulate(searchlight.centers.space.size): ordinal =>
            val center =
              searchlight.centers.space.indexAtValidatedOrdinal(ordinal)
            searchlight.relation
              .row(center)
              .intersect(support)
              .ordinalsInDomainOrder
        )
        .left
        .map(ExactVolumeSearchlightError.InvalidRelation.apply)
        .flatMap(fromRelation(searchlight.centers, _))

  def materialize[F <: Frame[D3], S, A, Sem](
      domain: GridDomain[F, D3, S],
      searchlight: VolumeNeighborhoods[S],
      center: Index[S],
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
      _ <- checkSpace(domain, searchlight.centers.space)
      _ <- checkSpace(domain, field.space)
      neighborhood <- searchlight
        .regionAt(center)
        .toRight(
          ExactVolumeSearchlightError.CenterUnavailable(center.ordinal)
        )
      restricted = support.fold(neighborhood)(neighborhood.intersect)
      _ <-
        if restricted.contains(center) then Right(())
        else
          Left(
            ExactVolumeSearchlightError.CenterExcluded(center.ordinal)
          )
      selection <- Selection
        .fromRegion(restricted)
        .left
        .map(ExactVolumeSearchlightError.InvalidSelection.apply)
      indices = selection.indices.toVector
      data = NDArray.fromSeq(
        ravel.Shape(indices.length),
        indices.map(field.apply)
      )
      selected <- SelectedVolume
        .create(
          domain,
          selection,
          data,
          ImageMetadata(label)
        )
        .left
        .map(ExactVolumeSearchlightError.InvalidSelectedImage.apply)
      centerPosition = indices.indexWhere(_.ordinal == center.ordinal)
      window <- SelectedVolumeWindow
        .make(selected, center, centerPosition)
        .left
        .map(ExactVolumeSearchlightError.InvalidWindow.apply)
    yield window

  /** Materialize a scalar-valued searchlight without asking inference to
    * choose between the continuous and categorical meanings available for
    * several primitive element types.
    */
  def materializeContinuous[F <: Frame[D3], S, A](
      domain: GridDomain[F, D3, S],
      searchlight: VolumeNeighborhoods[S],
      center: Index[S],
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
    materialize[F, S, A, Continuous](
      domain,
      searchlight,
      center,
      field,
      support,
      label
    )

  /** Materialize a label-valued searchlight with categorical semantics. */
  def materializeCategorical[F <: Frame[D3], S, A](
      domain: GridDomain[F, D3, S],
      searchlight: VolumeNeighborhoods[S],
      center: Index[S],
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
    materialize[F, S, A, Categorical](
      domain,
      searchlight,
      center,
      field,
      support,
      label
    )

  private def validateCentered[S](
      centers: Region[S],
      relation: Relation[S, S]
  ): Either[ExactVolumeSearchlightError, VolumeNeighborhoods[S]] =
    var ordinal = 0
    var invalidOutside = -1
    var missingCenter = -1
    while ordinal < centers.space.size &&
        invalidOutside < 0 && missingCenter < 0
    do
      val point = centers.space.indexAtValidatedOrdinal(ordinal)
      val row = relation.row(point)
      if centers.contains(point) then
        if !row.contains(point) then missingCenter = ordinal
      else if !row.isEmpty then invalidOutside = ordinal
      ordinal += 1

    if invalidOutside >= 0 then
      Left(
        ExactVolumeSearchlightError.NonEmptyOutsideCenters(
          invalidOutside
        )
      )
    else if missingCenter >= 0 then
      Left(ExactVolumeSearchlightError.MissingCenter(missingCenter))
    else Right(new VolumeNeighborhoods(centers, relation))

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
