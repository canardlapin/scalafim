package scalafim.image

import image4s.geometry.{D3, Frame}
import locus4s.{PartialSurjection, Region, SpaceMismatch}

enum DilationMetric derives CanEqual:
  case GridEuclidean, WorldEuclidean

enum DilationTie derives CanEqual:
  case LeaveUnassigned, LowestParcelOrdinal

final case class DilationRadius private (value: Double):
  require(value.isFinite && value >= 0.0, "dilation radius must be finite and non-negative")

object DilationRadius:
  def from(value: Double): Either[VolumeDilationError, DilationRadius] =
    if value.isFinite && value >= 0.0 then Right(new DilationRadius(value))
    else Left(VolumeDilationError.InvalidRadius(value))

enum VolumeDilationError:
  case InvalidRadius(value: Double)
  case WrongMaskOwner(error: SpaceMismatch)
  case MaskExcludesSeed(voxelOrdinal: Int)
  case Assignment(error: VolumeParcellationError)

  def message: String = this match
    case InvalidRadius(value) => s"dilation radius must be finite and non-negative: $value"
    case WrongMaskOwner(error) => error.message
    case MaskExcludesSeed(ordinal) => s"dilation mask excludes assigned voxel $ordinal"
    case Assignment(error) => error.message

object VolumeDilation:
  /** Nearest-seed dilation. The mask constrains destinations, not paths; this
    * is a metric ball operation, not geodesic propagation through the mask.
    * Existing assignments are never replaced. Ties compare computed distances
    * exactly; multiple equally close seeds in the same parcel are not a tie.
    */
  def apply[F <: Frame[D3], S, P, M](
      source: VolumeParcellation[F, S, P, M],
      radius: DilationRadius,
      metric: DilationMetric,
      tie: DilationTie,
      mask: Region[S]
  ): Either[VolumeDilationError, PartialSurjection[S, P]] =
    if !source.domain.space.sameRuntimeOwnerAs(mask.space) then
      Left(VolumeDilationError.WrongMaskOwner(SpaceMismatch.between(source.domain.space, mask.space)))
    else
      source.support.indicesInDomainOrder.find(seed => !mask.contains(seed)) match
        case Some(seed) => Left(VolumeDilationError.MaskExcludesSeed(seed.ordinal))
        case None => dilate(source, radius, metric, tie, mask)

  private def dilate[F <: Frame[D3], S, P, M](
      source: VolumeParcellation[F, S, P, M], radius: DilationRadius,
      metric: DilationMetric, tie: DilationTie, mask: Region[S]
  ): Either[VolumeDilationError, PartialSurjection[S, P]] =
    val domain = source.domain
    val dims = domain.grid.shape
    val matrix = domain.grid.indexToFrame.matrix
    val inverse = domain.grid.indexToFrame.inverse.matrix
    val bounds = Vector.tabulate(3): axis =>
      val scale = metric match
        case DilationMetric.GridEuclidean => 1.0
        case DilationMetric.WorldEuclidean =>
          math.hypot(math.hypot(inverse(axis, 0), inverse(axis, 1)), inverse(axis, 2))
      math.min(dims(axis) - 1, math.ceil(radius.value * scale).toInt)
    val winners = Array.fill(domain.space.size)(-1)
    val distances = Array.fill(domain.space.size)(Double.PositiveInfinity)
    val ties = Array.fill(domain.space.size)(false)
    val seeds = source.support.indicesInDomainOrder
    while seeds.hasNext do
      val seed = seeds.next()
      val parcel = source.assignment(seed).get.ordinal
      val coords = Indexing.indexToGrid3D(dims, seed.ordinal)
      var x = math.max(0, coords(0) - bounds(0))
      while x <= math.min(dims(0) - 1, coords(0) + bounds(0)) do
        var y = math.max(0, coords(1) - bounds(1))
        while y <= math.min(dims(1) - 1, coords(1) + bounds(1)) do
          var z = math.max(0, coords(2) - bounds(2))
          while z <= math.min(dims(2) - 1, coords(2) + bounds(2)) do
            val ordinal = (x * dims(1) + y) * dims(2) + z
            val target = domain.space.indexAtValidatedOrdinal(ordinal)
            if mask.contains(target) && source.assignment(target).isEmpty then
              val dx = (x - coords(0)).toDouble
              val dy = (y - coords(1)).toDouble
              val dz = (z - coords(2)).toDouble
              val distance = metric match
                case DilationMetric.GridEuclidean => math.hypot(math.hypot(dx, dy), dz)
                case DilationMetric.WorldEuclidean =>
                  val wx = matrix(0, 0) * dx + matrix(0, 1) * dy + matrix(0, 2) * dz
                  val wy = matrix(1, 0) * dx + matrix(1, 1) * dy + matrix(1, 2) * dz
                  val wz = matrix(2, 0) * dx + matrix(2, 1) * dy + matrix(2, 2) * dz
                  math.hypot(math.hypot(wx, wy), wz)
              if distance <= radius.value then
                if distance < distances(ordinal) then
                  distances(ordinal) = distance
                  winners(ordinal) = parcel
                  ties(ordinal) = false
                else if distance == distances(ordinal) && winners(ordinal) != parcel then
                  ties(ordinal) = true
                  winners(ordinal) = math.min(winners(ordinal), parcel)
            z += 1
          y += 1
        x += 1
    PartialSurjection.fromOptionalTargetOrdinals(domain.space, source.parcels,
      domain.space.indices.map: voxel =>
        source.assignment(voxel).map(_.ordinal).orElse:
          Option.when(winners(voxel.ordinal) >= 0 &&
            (tie == DilationTie.LowestParcelOrdinal || !ties(voxel.ordinal)))(winners(voxel.ordinal))
    ).left.map(error => VolumeDilationError.Assignment(error match
      case e: locus4s.PartialMapError => VolumeParcellationError.InvalidPartialMap(e)
      case e: locus4s.CertifiedMapError => VolumeParcellationError.InvalidSurjection(e)
    ))
