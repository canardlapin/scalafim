package scalafim.image

import image4s.Categorical
import image4s.ImageMetadata
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.ValueSemantics
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.locus.GridDomain
import image4s.locus.GridDomainError
import image4s.geometry.GeometryError
import locus4s.CertifiedMapError
import locus4s.DomainError
import locus4s.FiniteDomain
import locus4s.Index
import locus4s.PartialMapError
import locus4s.PartialSurjection
import locus4s.Region
import locus4s.Relation
import locus4s.RelationError
import locus4s.SpaceMismatch
import locus4s.data.Field
import locus4s.data.FieldConstructionError
import locus4s.data.VectorField
import ravel.DType
import ravel.NDArray
import ravel.Rank
import ravel.Shape

enum VolumeParcellationError:
  case InvalidParcelDomain(error: DomainError)
  case InvalidPartialMap(error: PartialMapError)
  case InvalidSurjection(error: CertifiedMapError)
  case InvalidMetadata(error: FieldConstructionError)
  case WrongVoxelOwner(error: SpaceMismatch)
  case WrongParcelOwner(error: SpaceMismatch)
  case EmptyParcelDomain
  case InvalidImage(error: image4s.ImageError)

  def message: String =
    this match
      case InvalidParcelDomain(error) => error.message
      case InvalidPartialMap(error) => error.message
      case InvalidSurjection(error) => error.message
      case InvalidMetadata(error) => error.message
      case WrongVoxelOwner(error) => error.message
      case WrongParcelOwner(error) => error.message
      case EmptyParcelDomain => "a volume parcellation requires at least one parcel"
      case InvalidImage(error) => error.message

/** One exact partial assignment from a D3 voxel owner onto a parcel owner.
  *
  * `assignment` is the only source of support and fiber truth. Dense labels,
  * masks, and lookup tables are derived views/materializations rather than
  * retained parallel representations.
  */
final class VolumeParcellation[
    F <: Frame[D3],
    S,
    P,
    +M
] private (
    val domain: GridDomain[F, D3, S],
    val assignment: PartialSurjection[S, P],
    val parcelMetadata: Field[P, M],
    val imageMetadata: ImageMetadata
):
  def parcels: FiniteDomain[P] =
    assignment.to

  def support: Region[S] =
    assignment.support

  def parcelAt(voxel: Index[S]): Option[Index[P]] =
    assignment(voxel)

  def region(parcel: Index[P]): Region[S] =
    assignment.fiber(parcel)

  def fibers: Either[RelationError, Relation[P, S]] =
    assignment.fibers

  def metadataAt(parcel: Index[P]): M =
    parcelMetadata(parcel)

  /** Explicit dense categorical rendering with a caller-selected background. */
  def renderCategorical[Q, A](
      parcelValues: Field[Q, A],
      background: A,
      metadata: ImageMetadata = imageMetadata
  )(using
      DType[A],
      ValueSemantics[A, Categorical]
  ): Either[VolumeParcellationError, SomeLabelVolume[A]] =
    if !parcels.sameRuntimeOwnerAs(parcelValues.space) then
      Left(
        VolumeParcellationError.WrongParcelOwner(
          SpaceMismatch.between(parcels, parcelValues.space)
        )
      )
    else
      parcels.align(parcelValues.space) match
        case Left(_) =>
          Left(
            VolumeParcellationError.WrongParcelOwner(
              SpaceMismatch.between(parcels, parcelValues.space)
            )
          )
        case Right(alignment) =>
          val exactValues = parcelValues.rebind(alignment.reverse)
          val gridShape = domain.grid.shape
          val data =
            NDArray.build[A, Rank[3]](
              Shape(gridShape(0), gridShape(1), gridShape(2))
            ): output =>
              var voxelOrdinal = 0
              while voxelOrdinal < domain.space.size do
                val voxel =
                  domain.space.indexAtValidatedOrdinal(voxelOrdinal)
                val value =
                  assignment(voxel).fold(background)(exactValues.apply)
                output.writeLinear(voxelOrdinal, value)
                voxelOrdinal += 1
          val sampleSpace =
            SampleSpace.create(domain.grid, NonSpatialAxes.empty)
          NeuroVolume
            .categorical(sampleSpace, data, metadata)
            .left
            .map(VolumeParcellationError.InvalidImage.apply)
            .map(SomeNeuroVolume.eraseSpace)

sealed trait VolumeParcellationResolution[
    F <: Frame[D3],
    S,
    +M
]:
  type P
  val value: VolumeParcellation[F, S, P, M]

object VolumeParcellation:
  def create[F <: Frame[D3], S, T, P, Q, M](
      domain: GridDomain[F, D3, S],
      assignment: PartialSurjection[T, P],
      parcelMetadata: Field[Q, M],
      imageMetadata: ImageMetadata = ImageMetadata.empty
  ): Either[
    VolumeParcellationError,
    VolumeParcellation[F, S, P, M]
  ] =
    if !domain.space.sameRuntimeOwnerAs(assignment.from) then
      Left(
        VolumeParcellationError.WrongVoxelOwner(
          SpaceMismatch.between(domain.space, assignment.from)
        )
      )
    else if !assignment.to.sameRuntimeOwnerAs(parcelMetadata.space) then
      Left(
        VolumeParcellationError.WrongParcelOwner(
          SpaceMismatch.between(assignment.to, parcelMetadata.space)
        )
      )
    else
      for
        voxelAlignment <- domain.space
          .align(assignment.from)
          .left
          .map(_ =>
            VolumeParcellationError.WrongVoxelOwner(
              SpaceMismatch.between(domain.space, assignment.from)
            )
          )
        parcelAlignment <- assignment.to
          .align(parcelMetadata.space)
          .left
          .map(_ =>
            VolumeParcellationError.WrongParcelOwner(
              SpaceMismatch.between(
                assignment.to,
                parcelMetadata.space
              )
            )
          )
      yield new VolumeParcellation(
        domain,
        assignment.rebindFrom(voxelAlignment.reverse),
        parcelMetadata.rebind(parcelAlignment.reverse),
        imageMetadata
      )

  /** Create a process-local parcel owner and one certified assignment.
    * Optional target ordinals use `None` for unassigned/background voxels.
    */
  def resolve[F <: Frame[D3], S, M](
      domain: GridDomain[F, D3, S],
      parcelDomainName: String,
      metadata: Vector[M],
      targetOrdinals: IterableOnce[Option[Int]],
      imageMetadata: ImageMetadata = ImageMetadata.empty
  ): Either[
    VolumeParcellationError,
    VolumeParcellationResolution[F, S, M]
  ] =
    if metadata.isEmpty then
      Left(VolumeParcellationError.EmptyParcelDomain)
    else
      FiniteDomain
        .ephemeral(parcelDomainName, metadata.length)
        .left
        .map(VolumeParcellationError.InvalidParcelDomain.apply)
        .flatMap: packed =>
          type Parcel = packed.S
          val parcelDomain: FiniteDomain[Parcel] = packed.value
          VectorField
            .fromValues(parcelDomain, metadata)
            .left
            .map(VolumeParcellationError.InvalidMetadata.apply)
            .flatMap: metadataField =>
              PartialSurjection
                .fromOptionalTargetOrdinals(
                  domain.space,
                  parcelDomain,
                  targetOrdinals
                )
                .left
                .map(mappingError)
                .flatMap: assignment =>
                  create(
                    domain,
                    assignment,
                    metadataField,
                    imageMetadata
                  ).map: parcellation =>
                    new VolumeParcellationResolution[F, S, M]:
                      type P = Parcel
                      val value: VolumeParcellation[F, S, P, M] =
                        parcellation

  private def mappingError(
      error: PartialMapError | CertifiedMapError
  ): VolumeParcellationError =
    error match
      case partial: PartialMapError =>
        VolumeParcellationError.InvalidPartialMap(partial)
      case certified: CertifiedMapError =>
        VolumeParcellationError.InvalidSurjection(certified)

enum ParcelCentroidMethod:
  case CenterOfMass
  case GeometricMedian

enum VolumeParcellationGeometryError:
  case InvalidTolerance(value: Double)
  case InvalidMaximumIterations(value: Int)
  case Grid(error: GridDomainError)
  case Geometry(error: GeometryError)
  case InvalidField(error: FieldConstructionError)

  def message: String =
    this match
      case InvalidTolerance(value) =>
        s"centroid tolerance must be finite and positive, found $value"
      case InvalidMaximumIterations(value) =>
        s"centroid maximum iterations must be positive, found $value"
      case Grid(error) => error.message
      case Geometry(error) => error.message
      case InvalidField(error) => error.message

object VolumeParcellationGeometry:
  def centroids[F <: Frame[D3], S, P, M](
      parcellation: VolumeParcellation[F, S, P, M],
      method: ParcelCentroidMethod = ParcelCentroidMethod.CenterOfMass,
      frame: SpatialCoordinateFrame = SpatialCoordinateFrame.Grid,
      tolerance: Double = 1e-6,
      maxIterations: Int = 500
  ): Either[VolumeParcellationGeometryError, Field[P, Vector[Double]]] =
    if !tolerance.isFinite || tolerance <= 0.0 then
      Left(VolumeParcellationGeometryError.InvalidTolerance(tolerance))
    else if maxIterations <= 0 then
      Left(
        VolumeParcellationGeometryError.InvalidMaximumIterations(
          maxIterations
        )
      )
    else
      val output = Vector.newBuilder[Vector[Double]]
      var parcelOrdinal = 0
      var error = Option.empty[VolumeParcellationGeometryError]
      while parcelOrdinal < parcellation.parcels.size && error.isEmpty do
        val parcel =
          parcellation.parcels.indexAtValidatedOrdinal(parcelOrdinal)
        val points = Vector.newBuilder[Vector[Double]]
        val voxels = parcellation.region(parcel).indicesInDomainOrder
        while voxels.hasNext && error.isEmpty do
          val voxel = voxels.next()
          parcellation.domain.indexOf(voxel) match
            case Left(gridError) =>
              error = Some(
                VolumeParcellationGeometryError.Grid(gridError)
              )
            case Right(lattice) =>
              val grid = lattice.values.map(_.toDouble)
              frame match
                case SpatialCoordinateFrame.Grid => points += grid
                case SpatialCoordinateFrame.World =>
                  parcellation.domain.grid.indexToFrame.apply(grid) match
                    case Left(geometryError) =>
                      error = Some(
                        VolumeParcellationGeometryError.Geometry(
                          geometryError
                        )
                      )
                    case Right(world) => points += world
        if error.isEmpty then
          val parcelPoints = points.result()
          output +=
            (method match
              case ParcelCentroidMethod.CenterOfMass =>
                coordinateMean(parcelPoints)
              case ParcelCentroidMethod.GeometricMedian =>
                geometricMedian(
                  parcelPoints,
                  tolerance,
                  maxIterations
                ))
        parcelOrdinal += 1

      error match
        case Some(value) => Left(value)
        case None =>
          VectorField
            .fromValues(parcellation.parcels, output.result())
            .left
            .map(VolumeParcellationGeometryError.InvalidField.apply)

  private def coordinateMean(points: Vector[Vector[Double]]): Vector[Double] =
    val sums = Array.ofDim[Double](3)
    var point = 0
    while point < points.length do
      var axis = 0
      while axis < 3 do
        sums(axis) += points(point)(axis)
        axis += 1
      point += 1
    Vector.tabulate(3)(axis => sums(axis) / points.length.toDouble)

  private def geometricMedian(
      points: Vector[Vector[Double]],
      tolerance: Double,
      maxIterations: Int
  ): Vector[Double] =
    if points.length == 1 then points.head
    else
      val initial = coordinateMean(points)
      var x = initial(0)
      var y = initial(1)
      var z = initial(2)
      var iteration = 0
      var converged = false
      while iteration < maxIterations && !converged do
        var numeratorX = 0.0
        var numeratorY = 0.0
        var numeratorZ = 0.0
        var denominator = 0.0
        var coincident = -1
        var point = 0
        while point < points.length && coincident < 0 do
          val dx = points(point)(0) - x
          val dy = points(point)(1) - y
          val dz = points(point)(2) - z
          val distance = math.sqrt(dx * dx + dy * dy + dz * dz)
          if distance < 1e-12 then coincident = point
          else
            val weight = 1.0 / distance
            numeratorX += weight * points(point)(0)
            numeratorY += weight * points(point)(1)
            numeratorZ += weight * points(point)(2)
            denominator += weight
          point += 1

        if coincident >= 0 || denominator == 0.0 then
          val retained = points(math.max(coincident, 0))
          x = retained(0)
          y = retained(1)
          z = retained(2)
          converged = true
        else
          val nextX = numeratorX / denominator
          val nextY = numeratorY / denominator
          val nextZ = numeratorZ / denominator
          val dx = nextX - x
          val dy = nextY - y
          val dz = nextZ - z
          x = nextX
          y = nextY
          z = nextZ
          converged = math.sqrt(dx * dx + dy * dy + dz * dz) < tolerance
        iteration += 1
      Vector(x, y, z)
