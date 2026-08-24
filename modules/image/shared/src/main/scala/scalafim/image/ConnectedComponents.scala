package scalafim.image

import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.locus.GridDomain
import locus4s.Region
import locus4s.SpaceMismatch

enum SpatialConnectivity3D:
  case Face6
  case FaceEdge18
  case FaceEdgeCorner26

final case class ConnectedComponentMetadata(
    label: String,
    cardinality: Int
):
  require(label.nonEmpty, "connected-component label must be non-empty")
  require(cardinality > 0, "connected-component cardinality must be positive")

enum ConnectedComponentsError:
  case WrongVoxelOwner(error: SpaceMismatch)
  case InvalidMask(error: MaskRegionError)
  case InvalidParcellation(error: VolumeParcellationError)

  def message: String =
    this match
      case WrongVoxelOwner(error) => error.message
      case InvalidMask(error) => error.message
      case InvalidParcellation(error) => error.message

object ConnectedComponents:
  def fromMask[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      mask: SomeMaskVolume,
      connectivity: SpatialConnectivity3D = SpatialConnectivity3D.FaceEdgeCorner26,
      parcelDomainName: String = "connected components"
  ): Either[
    ConnectedComponentsError,
    VolumeParcellationResolution[F, S, ConnectedComponentMetadata]
  ] =
    Mask
      .region(domain, mask)
      .left
      .map(ConnectedComponentsError.InvalidMask.apply)
      .flatMap: active =>
        fromRegion(domain, active, connectivity, parcelDomainName)

  def fromRegion[F <: Frame[D3], S, T](
      domain: GridDomain[F, D3, S],
      active: Region[T],
      connectivity: SpatialConnectivity3D = SpatialConnectivity3D.FaceEdgeCorner26,
      parcelDomainName: String = "connected components"
  ): Either[
    ConnectedComponentsError,
    VolumeParcellationResolution[F, S, ConnectedComponentMetadata]
  ] =
    Region
      .whole(domain.space)
      .intersectChecked(active)
      .left
      .map(ConnectedComponentsError.WrongVoxelOwner.apply)
      .flatMap: exactActive =>
        val components = findComponents(domain, exactActive, connectivity)
        val ordered =
          components.sortBy(component => (-component.length, component.head))
        val targetOrdinals =
          Array.fill[Option[Int]](domain.space.size)(None)
        val metadata =
          Vector.tabulate(ordered.length): componentOrdinal =>
            val members = ordered(componentOrdinal)
            var position = 0
            while position < members.length do
              targetOrdinals(members(position)) = Some(componentOrdinal)
              position += 1
            ConnectedComponentMetadata(
              s"Component_${componentOrdinal + 1}",
              members.length
            )
        VolumeParcellation
          .resolve(
            domain,
            parcelDomainName,
            metadata,
            targetOrdinals
          )
          .left
          .map(ConnectedComponentsError.InvalidParcellation.apply)

  private def findComponents[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      active: Region[S],
      connectivity: SpatialConnectivity3D
  ): Vector[Array[Int]] =
    val shape = domain.grid.shape
    val voxelCount = domain.space.size
    val activeFlags = Array.fill(voxelCount)(false)
    active.foreachIndex(index => activeFlags(index.ordinal) = true)
    val visited = Array.fill(voxelCount)(false)
    val queue = Array.ofDim[Int](active.cardinality)
    val components = Vector.newBuilder[Array[Int]]
    val neighborhood = offsets(connectivity)
    val plane = shape(1) * shape(2)

    val seeds = active.indicesInDomainOrder
    while seeds.hasNext do
      val seed = seeds.next().ordinal
      if !visited(seed) then
        val members = Array.newBuilder[Int]
        var head = 0
        var tail = 1
        queue(0) = seed
        visited(seed) = true
        while head < tail do
          val ordinal = queue(head)
          head += 1
          members += ordinal
          val x = ordinal / plane
          val withinPlane = ordinal % plane
          val y = withinPlane / shape(2)
          val z = withinPlane % shape(2)
          var neighbor = 0
          while neighbor < neighborhood.length do
            val (dx, dy, dz) = neighborhood(neighbor)
            val nx = x + dx
            val ny = y + dy
            val nz = z + dz
            if nx >= 0 && nx < shape(0) &&
              ny >= 0 && ny < shape(1) &&
              nz >= 0 && nz < shape(2)
            then
              val target = (nx * shape(1) + ny) * shape(2) + nz
              if activeFlags(target) && !visited(target) then
                visited(target) = true
                queue(tail) = target
                tail += 1
            neighbor += 1
        components += members.result()
    components.result()

  private def offsets(
      connectivity: SpatialConnectivity3D
  ): Vector[(Int, Int, Int)] =
    connectivity match
      case SpatialConnectivity3D.Face6 => face6
      case SpatialConnectivity3D.FaceEdge18 => faceEdge18
      case SpatialConnectivity3D.FaceEdgeCorner26 => faceEdgeCorner26

  private val face6: Vector[(Int, Int, Int)] =
    Vector(
      (-1, 0, 0),
      (1, 0, 0),
      (0, -1, 0),
      (0, 1, 0),
      (0, 0, -1),
      (0, 0, 1)
    )

  private val faceEdge18: Vector[(Int, Int, Int)] =
    face6 ++ Vector(
      (-1, -1, 0),
      (-1, 1, 0),
      (1, -1, 0),
      (1, 1, 0),
      (-1, 0, -1),
      (-1, 0, 1),
      (1, 0, -1),
      (1, 0, 1),
      (0, -1, -1),
      (0, -1, 1),
      (0, 1, -1),
      (0, 1, 1)
    )

  private val faceEdgeCorner26: Vector[(Int, Int, Int)] =
    val output = Vector.newBuilder[(Int, Int, Int)]
    var dx = -1
    while dx <= 1 do
      var dy = -1
      while dy <= 1 do
        var dz = -1
        while dz <= 1 do
          if dx != 0 || dy != 0 || dz != 0 then
            output += ((dx, dy, dz))
          dz += 1
        dy += 1
      dx += 1
    output.result()
