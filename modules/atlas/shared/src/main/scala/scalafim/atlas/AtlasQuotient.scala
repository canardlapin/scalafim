package scalafim.atlas

import narr.nArray2NArr
import scalafim.image.{ClusteredNeuroVol, VolumeDomain, VolumeSpace}
import scalafim.locus.{
  FiniteSpace,
  IndexedField,
  Parcellation,
  Point,
  Region as LocusRegion,
  Selection,
  SpaceKey,
  Surjection,
  TotalMap
}

trait AtlasNetworkAssignment[P]:
  type N
  val networkIds: IndexedField[N, NetworkId]
  val parcelToNetwork: Surjection[P, N]

  def networkPoint(id: NetworkId): Option[Point[N]] =
    networkIds.space.points.find(point => networkIds(point) == id)

trait AtlasNetworkParcellation[X]:
  type N
  val parcellation: Parcellation[X, N]
  val networkIds: IndexedField[N, NetworkId]

trait AtlasQuotient:
  type X
  type P

  val parcellation: Parcellation[X, P]
  val regionIds: IndexedField[P, RegionId]
  val metadata: IndexedField[P, AtlasRegionMetadata]
  val displayOrder: Selection[P]
  val networkAssignment: Option[AtlasNetworkAssignment[P]]

  final def support: LocusRegion[X] =
    parcellation.support

  final def parcelPoint(id: RegionId): Option[Point[P]] =
    regionIds.space.points.find(point => regionIds(point) == id)

  final def region(id: RegionId): Option[LocusRegion[X]] =
    parcelPoint(id).map(parcellation.fiber)

  final def networkParcellation: Option[AtlasNetworkParcellation[X]] =
    networkAssignment.map: assignment =>
      val coarsened =
        parcellation
          .coarsen(assignment.parcelToNetwork)
          .toOption
          .get
      new AtlasNetworkParcellation[X]:
        type N = assignment.N
        val parcellation: Parcellation[X, N] = coarsened
        val networkIds: IndexedField[N, NetworkId] = assignment.networkIds

  final def networkRegion(id: NetworkId): Option[LocusRegion[X]] =
    networkParcellation.flatMap: network =>
      network.networkIds.space.points
        .find(point => network.networkIds(point) == id)
        .map(network.parcellation.fiber)

trait VolumeAtlasQuotient extends AtlasQuotient:
  val domain: VolumeDomain[X]

trait SurfaceAtlasQuotient extends AtlasQuotient:
  val leftVertexCount: Int
  val rightVertexCount: Int

object AtlasQuotient:
  def volume(
      spatialSemanticId: String,
      atlasName: String,
      regions: RegionIndex,
      volume: ClusteredNeuroVol
  ): VolumeAtlasQuotient =
    final class Voxel
    final class Parcel

    val volumeSpace = VolumeSpace.fromSpatialPart(volume.space).toOption.get
    val volumeDomain =
      VolumeDomain.semantic[Voxel](
        SpaceKey.unsafe(
          s"scalafim:atlas:volume:$spatialSemanticId:${volumeSpace.hashCode}:${volumeSpace.nVoxels}"
        ),
        volumeSpace
      )
    val parcels =
      FiniteSpace
        .make[Parcel](
          SpaceKey.unsafe(
            s"scalafim:atlas:$atlasName:parcels:${regions.ids.map(_.value).mkString(",")}"
          ),
          regions.size
        )
        .toOption
        .get
    val parcelOrdinalById =
      regions.ids.zipWithIndex.toMap
    val dense = volume.toDense
    val assignments =
      Vector.tabulate(volumeDomain.finiteSpace.size): ordinal =>
        val id = dense.linear(ordinal)
        Option.when(id != 0)(parcelOrdinalById(RegionId(id)))
    val quotient =
      Parcellation
        .fromAssignments(volumeDomain.finiteSpace, parcels, assignments)
        .toOption
        .get
    val fields = parcelFields(parcels, regions)
    val networks = networkAssignment(parcels, regions)

    new VolumeAtlasQuotient:
      type X = Voxel
      type P = Parcel
      val domain: VolumeDomain[Voxel] = volumeDomain
      val parcellation: Parcellation[Voxel, Parcel] = quotient
      val regionIds: IndexedField[Parcel, RegionId] = fields._1
      val metadata: IndexedField[Parcel, AtlasRegionMetadata] = fields._2
      val displayOrder: Selection[Parcel] = fields._3
      val networkAssignment: Option[AtlasNetworkAssignment[Parcel]] = networks

  def surface(
      spatialSemanticId: String,
      atlasName: String,
      regions: RegionIndex,
      payload: SurfaceAtlasPayload
  ): SurfaceAtlasQuotient =
    final class Vertex
    final class Parcel

    val leftCount = payload.left.geometry.vertexCount
    val rightCount = payload.right.geometry.vertexCount
    val ambient =
      FiniteSpace
        .make[Vertex](
          SpaceKey.unsafe(
            s"scalafim:atlas:surface:$spatialSemanticId:${payload.left.geometry.mesh.topologyIdentity.stableKey}:${payload.right.geometry.mesh.topologyIdentity.stableKey}"
          ),
          leftCount + rightCount
        )
        .toOption
        .get
    val parcels =
      FiniteSpace
        .make[Parcel](
          SpaceKey.unsafe(
            s"scalafim:atlas:$atlasName:parcels:${regions.ids.map(_.value).mkString(",")}"
          ),
          regions.size
        )
        .toOption
        .get
    val parcelOrdinalById =
      regions.ids.zipWithIndex.toMap
    val assignments =
      Array.fill[Option[Int]](ambient.size)(None)

    assignSurface(
      payload.left,
      offset = 0,
      assignments,
      parcelOrdinalById
    )
    assignSurface(
      payload.right,
      offset = leftCount,
      assignments,
      parcelOrdinalById
    )

    val quotient =
      Parcellation
        .fromAssignments(ambient, parcels, assignments)
        .toOption
        .get
    val fields = parcelFields(parcels, regions)
    val networks = networkAssignment(parcels, regions)

    new SurfaceAtlasQuotient:
      type X = Vertex
      type P = Parcel
      val leftVertexCount: Int = leftCount
      val rightVertexCount: Int = rightCount
      val parcellation: Parcellation[Vertex, Parcel] = quotient
      val regionIds: IndexedField[Parcel, RegionId] = fields._1
      val metadata: IndexedField[Parcel, AtlasRegionMetadata] = fields._2
      val displayOrder: Selection[Parcel] = fields._3
      val networkAssignment: Option[AtlasNetworkAssignment[Parcel]] = networks

  private def parcelFields[P](
      parcels: FiniteSpace[P],
      regions: RegionIndex
  ): (
      IndexedField[P, RegionId],
      IndexedField[P, AtlasRegionMetadata],
      Selection[P]
  ) =
    (
      IndexedField.fromValues(parcels, regions.ids).toOption.get,
      IndexedField.fromValues(parcels, regions.regions).toOption.get,
      Selection.fromOrdinals(parcels, regions.regions.indices).toOption.get
    )

  private def networkAssignment[P](
      parcels: FiniteSpace[P],
      regions: RegionIndex
  ): Option[AtlasNetworkAssignment[P]] =
    val assigned = regions.regions.map(_.network)
    if assigned.exists(_.isEmpty) then None
    else
      final class Network
      val ids = assigned.flatten.distinct
      val networks =
        FiniteSpace
          .make[Network](
            SpaceKey.unsafe(
              s"${parcels.key.value}:networks:${ids.map(_.value).mkString(",")}"
            ),
            ids.length
          )
          .toOption
          .get
      val ordinalById = ids.zipWithIndex.toMap
      val mapping =
        TotalMap
          .fromTargetOrdinals(
            parcels,
            networks,
            assigned.flatten.map(ordinalById).toArray
          )
          .toOption
          .get
      val surjection =
        Surjection.validate(mapping).toOption.get
      val idField =
        IndexedField.fromValues(networks, ids).toOption.get
      Some:
        new AtlasNetworkAssignment[P]:
          type N = Network
          val networkIds: IndexedField[Network, NetworkId] = idField
          val parcelToNetwork: Surjection[P, Network] = surjection

  private def assignSurface(
      labeled: scalafim.surface.LabeledSurface,
      offset: Int,
      assignments: Array[Option[Int]],
      parcelOrdinalById: Map[RegionId, Int]
  ): Unit =
    var row = 0
    while row < labeled.indices.length do
      val label = labeled.labels(row)
      if label != 0 then
        assignments(offset + labeled.indices(row)) =
          Some(parcelOrdinalById(RegionId(label)))
      row += 1
