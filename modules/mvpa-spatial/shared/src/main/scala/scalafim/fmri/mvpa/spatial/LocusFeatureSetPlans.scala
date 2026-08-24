package scalafim.fmri.mvpa.spatial

import scalafim.fmri.mvpa.{
  FeatureSet,
  FeatureSetPlan,
  MvpaError,
  RoiId
}
import scalafim.locus.{
  CenteredSearchlight,
  Parcellation,
  Point,
  Region,
  Selection
}

/** Pure adapters from finite indexed spaces to MVPA algorithm plans.
  *
  * Geometry constructs regions, parcellations, and searchlights in their
  * owning modules. MVPA consumes only their finite indices and validated
  * evidence.
  */
object LocusFeatureSetPlans:
  def fromRegion[S](
      id: RoiId,
      region: Region[S],
      label: Option[String] = None
  ): Either[MvpaError, FeatureSet] =
    FeatureSet(
      id,
      region.ordinalsInDomainOrder.toVector,
      label = label
    )

  def fromSelection[S](
      id: RoiId,
      selection: Selection[S],
      label: Option[String] = None
  ): Either[MvpaError, FeatureSet] =
    FeatureSet(
      id,
      selection.ordinals.toVector,
      label = label
    )

  def fromParcellation[X, P](
      name: String,
      parcellation: Parcellation[X, P],
      label: Point[P] => Option[String] = (_: Point[P]) => None
  ): Either[MvpaError, FeatureSetPlan] =
    build(parcellation.parcels.indices): parcel =>
      fromRegion(
        RoiId(parcel.value),
        parcellation.fiber(parcel),
        label(parcel)
      )
    .flatMap(FeatureSetPlan.regional(name, _))

  def fromSearchlight[S](
      name: String,
      centered: CenteredSearchlight[S],
      label: Point[S] => Option[String] = (point: Point[S]) => Some(point.value.toString)
  ): Either[MvpaError, FeatureSetPlan] =
    val searchlight = centered.searchlight
    build(searchlight.centers.indicesInDomainOrder): center =>
      FeatureSet(
        RoiId(center.value),
        searchlight.neighborhoods
          .row(center)
          .ordinalsInDomainOrder
          .toVector,
        center = Some(center.value),
        label = label(center)
      )
    .flatMap(FeatureSetPlan.searchlight(name, _))

  private def build[A, B](
      items: Iterator[A]
  )(
      make: A => Either[MvpaError, B]
  ): Either[MvpaError, Vector[B]] =
    val result = Vector.newBuilder[B]
    while items.hasNext do
      make(items.next()) match
        case Right(value) => result += value
        case Left(error) => return Left(error)
    Right(result.result())
