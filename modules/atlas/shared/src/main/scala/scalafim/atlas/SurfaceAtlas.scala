package scalafim.atlas

import scalafim.surface.{
  FragmentedParcelPolicy,
  Hemisphere as SurfaceHemisphere,
  HemispherePair,
  LabelInfo,
  LabeledSurface,
  MeshTopology,
  ParcelContactMatrix,
  ParcelDistanceMatrix,
  ParcelDistanceMethod,
  ParcelUnit,
  SurfaceKind,
  SurfaceParcels,
  VertexId
}

final case class SurfaceAtlasPayload(labels: HemispherePair[LabeledSurface]):
  require(labels.left.geometry.hemisphere == SurfaceHemisphere.Left, "left surface atlas payload must use left hemisphere geometry")
  require(labels.right.geometry.hemisphere == SurfaceHemisphere.Right, "right surface atlas payload must use right hemisphere geometry")

  def left: LabeledSurface =
    labels.left

  def right: LabeledSurface =
    labels.right

  def surface(hemisphere: SurfaceHemisphere): LabeledSurface =
    hemisphere match
      case SurfaceHemisphere.Left => left
      case SurfaceHemisphere.Right => right
      case other => throw new IllegalArgumentException(s"surface atlas payload requires left or right hemisphere, got ${other.code}")

  def vertexCount(hemisphere: SurfaceHemisphere): Int =
    surface(hemisphere).geometry.vertexCount

  def presentLabelIds: Set[Int] =
    presentIds(left) ++ presentIds(right)

  def tableIds: Set[Int] =
    labelTableIds(left) ++ labelTableIds(right)

  private def presentIds(surface: LabeledSurface): Set[Int] =
    val out = scala.collection.mutable.Set.empty[Int]
    var i = 0
    while i < surface.labels.length do
      val id = surface.labels(i)
      if id != 0 then out += id
      i += 1
    out.toSet

  private def labelTableIds(surface: LabeledSurface): Set[Int] =
    surface.table.iterator.map(_.id).filter(_ != 0).toSet

final case class SurfaceAtlas(
  ref: AtlasRef,
  regions: RegionIndex,
  payload: SurfaceAtlasPayload,
  provenance: AtlasProvenance
) extends Atlas:
  require(ref.representation == AtlasRepresentation.Surface, "SurfaceAtlas requires surface representation")

  private val regionIdSet: Set[Int] =
    regions.ids.map(_.value).toSet

  private val payloadIdSet: Set[Int] =
    payload.presentLabelIds

  require(
    payloadIdSet == regionIdSet,
    AtlasError.InvalidRegionMetadata(
      s"surface payload labels must match region ids; missing=${regionIdSet.diff(payloadIdSet).toVector.sorted.mkString(",")} extra=${payloadIdSet.diff(regionIdSet).toVector.sorted.mkString(",")}"
    ).message
  )

  lazy val quotient: SurfaceAtlasQuotient =
    AtlasQuotient.surface(ref.coordSpace.value, ref.name, regions, payload)

  require(
    payload.tableIds.subsetOf(regionIdSet),
    AtlasError.InvalidRegionMetadata(
      s"surface label tables contain ids not present in regions: ${payload.tableIds.diff(regionIdSet).toVector.sorted.mkString(",")}"
    ).message
  )

  def left: LabeledSurface =
    payload.left

  def right: LabeledSurface =
    payload.right

  def surface(hemisphere: SurfaceHemisphere): LabeledSurface =
    payload.surface(hemisphere)

  def region(id: RegionId): Option[AtlasRegionMetadata] =
    regions.get(id)

  def region(label: String, hemisphere: Option[Hemisphere] = None): Vector[AtlasRegionMetadata] =
    regions.find(label, hemisphere)

  def labelIdAt(hemisphere: SurfaceHemisphere, vertex: VertexId): Option[RegionId] =
    surface(hemisphere).labelAt(vertex).filter(_ != 0).map(RegionId(_))

  def regionAt(hemisphere: SurfaceHemisphere, vertex: VertexId): Option[AtlasRegionMetadata] =
    labelIdAt(hemisphere, vertex).flatMap(regions.get)

  def labelInfo(hemisphere: SurfaceHemisphere, id: RegionId): Option[LabelInfo] =
    surface(hemisphere).info(id.value)

  def parcelUnits(
    hemisphere: SurfaceHemisphere,
    policy: FragmentedParcelPolicy = FragmentedParcelPolicy.Error,
    ignoredLabels: Set[Int] = Set(0)
  ): Vector[(AtlasRegionMetadata, ParcelUnit)] =
    val labeled = surface(hemisphere)
    val topology = MeshTopology.from(labeled.geometry.mesh)
    SurfaceParcels
      .units(labeled, topology, policy, ignoredLabels)
      .map(unit => regions.requireRegion(RegionId(unit.label)) -> unit)

  def boundaryContacts(
    hemisphere: SurfaceHemisphere,
    policy: FragmentedParcelPolicy = FragmentedParcelPolicy.Error,
    ignoredLabels: Set[Int] = Set(0)
  ): ParcelContactMatrix =
    val labeled = surface(hemisphere)
    SurfaceParcels.boundaryContacts(labeled, MeshTopology.from(labeled.geometry.mesh), policy, ignoredLabels)

  def distanceMatrix(
    hemisphere: SurfaceHemisphere,
    method: ParcelDistanceMethod = ParcelDistanceMethod.Centroid,
    policy: FragmentedParcelPolicy = FragmentedParcelPolicy.Error,
    ignoredLabels: Set[Int] = Set(0)
  ): ParcelDistanceMatrix =
    val labeled = surface(hemisphere)
    SurfaceParcels.distanceMatrix(labeled, MeshTopology.from(labeled.geometry.mesh), method, policy = policy, ignoredLabels = ignoredLabels)

object SurfaceAtlas:
  def apply(ref: AtlasRef, regions: RegionIndex, payload: SurfaceAtlasPayload): SurfaceAtlas =
    new SurfaceAtlas(ref, regions, payload, AtlasProvenance.fromRef(ref, regions))

  def fromLabeledSurfaces(
    ref: AtlasRef,
    regions: RegionIndex,
    left: LabeledSurface,
    right: LabeledSurface
  ): SurfaceAtlas =
    fromLabeledSurfaces(ref, regions, left, right, AtlasProvenance.fromRef(ref, regions))

  def fromLabeledSurfaces(
    ref: AtlasRef,
    regions: RegionIndex,
    left: LabeledSurface,
    right: LabeledSurface,
    provenance: AtlasProvenance
  ): SurfaceAtlas =
    SurfaceAtlas(ref, regions, SurfaceAtlasPayload(HemispherePair(left, right)), provenance)

enum SurfaceSamplingMethod:
  case NearestLabel, RibbonMode, RibbonMean

final case class SurfaceSamplingSpec(
  surfaceKind: SurfaceKind,
  method: SurfaceSamplingMethod,
  notes: Option[String] = None
)

object SurfaceSamplingSpec:
  val defaultParcel: SurfaceSamplingSpec =
    SurfaceSamplingSpec(
      surfaceKind = SurfaceKind.Midthickness,
      method = SurfaceSamplingMethod.NearestLabel,
      notes = Some("Planning value only; no volume/surface sampling is executed by scalafim-atlas.")
    )

enum VolumeSurfaceDirection:
  case VolumeToSurface, SurfaceToVolume

final case class VolumeSurfaceTransformPlan(
  direction: VolumeSurfaceDirection,
  from: AnySpaceId,
  to: AnySpaceId,
  route: TransformPlan,
  sampling: SurfaceSamplingSpec,
  dataKind: DataKind
):
  require(route.from == SpaceId.normalize(from), "volume/surface route source must match requested source")
  require(route.to == SpaceId.normalize(to), "volume/surface route target must match requested target")
  require(steps.exists(_.kind == bridgeKind), "volume/surface transform plan must include a bridge step")

  def steps: Vector[TransformStep] =
    route.steps

  def status: TransformStatus =
    route.status

  def confidence: Confidence =
    route.confidence

  def warnings: Vector[String] =
    route.warnings

  def isExecutable: Boolean =
    route.isExecutable

  private def bridgeKind: TransformKind =
    direction match
      case VolumeSurfaceDirection.VolumeToSurface => TransformKind.VolToSurf
      case VolumeSurfaceDirection.SurfaceToVolume => TransformKind.SurfToVol

object VolumeSurfaceTransformPlan:
  def volumeToSurface(
    fromVolumeSpace: VolumeOrUnknownSpaceId,
    toSurfaceSpace: SurfaceOrUnknownSpaceId,
    dataKind: DataKind = DataKind.Parcel,
    sampling: SurfaceSamplingSpec = SurfaceSamplingSpec.defaultParcel
  ): Either[AtlasError, VolumeSurfaceTransformPlan] =
    SpaceTransforms
      .plan(fromVolumeSpace, toSurfaceSpace, dataKind)
      .map(route =>
        VolumeSurfaceTransformPlan(
          VolumeSurfaceDirection.VolumeToSurface,
          SpaceId.normalize(fromVolumeSpace),
          SpaceId.normalize(toSurfaceSpace),
          route,
          sampling,
          dataKind
        )
      )

  def surfaceToVolume(
    fromSurfaceSpace: SurfaceOrUnknownSpaceId,
    toVolumeSpace: VolumeOrUnknownSpaceId,
    dataKind: DataKind = DataKind.Parcel,
    sampling: SurfaceSamplingSpec = SurfaceSamplingSpec.defaultParcel
  ): Either[AtlasError, VolumeSurfaceTransformPlan] =
    SpaceTransforms
      .plan(fromSurfaceSpace, toVolumeSpace, dataKind)
      .map(route =>
        VolumeSurfaceTransformPlan(
          VolumeSurfaceDirection.SurfaceToVolume,
          SpaceId.normalize(fromSurfaceSpace),
          SpaceId.normalize(toVolumeSpace),
          route,
          sampling,
          dataKind
        )
      )
