package scalafim.surface

import narr.nArray2NArr
import scalafim.locus.*

enum SurfaceLocusError:
  case InvalidMeshDomain(error: SurfaceError)
  case TopologyMismatch(expected: String, actual: String)
  case NotFullField(expectedVertices: Int, actualVertices: Int)
  case WrongSpace(error: SpaceMismatch)

  def message: String =
    this match
      case InvalidMeshDomain(error) => error.message
      case TopologyMismatch(expected, actual) =>
        s"surface topology mismatch: expected $expected, found $actual"
      case NotFullField(expected, actual) =>
        s"full surface field requires $expected vertices, found $actual"
      case WrongSpace(error) => error.message

enum SurfaceIdentityBasis:
  case Semantic
  case StructuralCompatibility

sealed trait StructuralSurfaceVertex

final case class SurfaceRoiView[S, A](
    region: Region[S],
    values: Section[S, Option[A]],
    annotation: String
)

final class SurfaceLocusDomain[S] private (
    val geometry: SurfaceGeometry,
    val meshDomain: SurfaceMeshDomain,
    val finiteSpace: FiniteSpace[S],
    val identityBasis: SurfaceIdentityBasis
):
  def optionalField[A](
      field: SurfaceField[A]
  ): Either[SurfaceLocusError, IndexedField[S, Option[A]]] =
    checkGeometry(field.geometry).map: _ =>
      val values = field.vertexIds.zipWithIndex.map((vertex, row) => vertex.index -> field.data(row)).toMap
      IndexedField.tabulate(finiteSpace)(point => values.get(point.ordinal))

  def fullField[A](
      field: SurfaceField[A]
  ): Either[SurfaceLocusError, IndexedField[S, A]] =
    checkGeometry(field.geometry).flatMap: _ =>
      if field.size != finiteSpace.size then
        Left(SurfaceLocusError.NotFullField(finiteSpace.size, field.size))
      else
        val values = Array.ofDim[Any](finiteSpace.size)
        var row = 0
        while row < field.indices.length do
          values(field.indices(row)) = field.data(row)
          row += 1
        Right:
          IndexedField.tabulate(finiteSpace)(point => values(point.ordinal).asInstanceOf[A])

  def roi[A](
      surfaceRoi: SurfaceRoi[A]
  ): Either[SurfaceLocusError, SurfaceRoiView[S, A]] =
    checkGeometry(surfaceRoi.geometry).map: _ =>
      val region =
        Region.fromOrdinals(
          finiteSpace,
          Vector.tabulate(surfaceRoi.indices.length)(surfaceRoi.indices.apply)
        ).toOption.get
      val valuesByVertex =
        Vector
          .tabulate(surfaceRoi.indices.length): row =>
            surfaceRoi.indices(row) -> surfaceRoi.data(row)
          .toMap
      val optional =
        IndexedField.tabulate(finiteSpace)(point => valuesByVertex.get(point.ordinal))
      val section = optional.restrict(region).toOption.get
      SurfaceRoiView(region, section, surfaceRoi.label)

  def parcellation(
      labeled: LabeledSurface,
      ignoredLabels: Set[Int] = Set.empty
  ): Either[SurfaceLocusError, SurfaceParcellation[S]] =
    SurfaceParcellation.from(this, labeled, ignoredLabels)

  private[surface] def checkGeometry(
      actual: SurfaceGeometry
  ): Either[SurfaceLocusError, Unit] =
    SurfaceMeshDomain.from(actual).left.map(SurfaceLocusError.InvalidMeshDomain.apply).flatMap: actualDomain =>
      if meshDomain == actualDomain && geometry.mesh.hasSameTopology(actual.mesh) then
        Right(())
      else
        Left(SurfaceLocusError.TopologyMismatch(meshDomain.display, actualDomain.display))

object SurfaceLocusDomain:
  def semantic[S](
      semanticKey: SpaceKey,
      geometry: SurfaceGeometry
  ): Either[SurfaceLocusError, SurfaceLocusDomain[S]] =
    SurfaceMeshDomain
      .from(geometry)
      .left
      .map(SurfaceLocusError.InvalidMeshDomain.apply)
      .map: meshDomain =>
        val key =
          SpaceKey.unsafe(s"${semanticKey.value}:surface:${meshDomain.display}")
        new SurfaceLocusDomain(
          geometry,
          meshDomain,
          FiniteSpace.make[S](key, geometry.vertexCount).toOption.get,
          SurfaceIdentityBasis.Semantic
        )

  def structuralCompatibility(
      geometry: SurfaceGeometry
  ): Either[SurfaceLocusError, SurfaceLocusDomain[StructuralSurfaceVertex]] =
    SurfaceMeshDomain
      .from(geometry)
      .left
      .map(SurfaceLocusError.InvalidMeshDomain.apply)
      .map: meshDomain =>
        val key = SpaceKey.unsafe(s"scalafim:surface:structural:${meshDomain.display}")
        new SurfaceLocusDomain(
          geometry,
          meshDomain,
          FiniteSpace
            .make[StructuralSurfaceVertex](key, geometry.vertexCount)
            .toOption
            .get,
          SurfaceIdentityBasis.StructuralCompatibility
        )

trait SomeSurfaceLocusDomain:
  type S
  val value: SurfaceLocusDomain[S]

object SomeSurfaceLocusDomain:
  def semantic(
      semanticKey: SpaceKey,
      geometry: SurfaceGeometry
  ): Either[SurfaceLocusError, SomeSurfaceLocusDomain] =
    final class SurfaceVertex
    SurfaceLocusDomain.semantic[SurfaceVertex](semanticKey, geometry).map: domain =>
      new SomeSurfaceLocusDomain:
        type S = SurfaceVertex
        val value: SurfaceLocusDomain[SurfaceVertex] = domain

trait SurfaceParcellation[S]:
  type P
  val parcellation: Parcellation[S, P]
  val labelIds: IndexedField[P, Int]
  val metadata: IndexedField[P, Option[LabelInfo]]
  val displayOrder: Selection[P]

object SurfaceParcellation:
  def from[S](
      domain: SurfaceLocusDomain[S],
      labeled: LabeledSurface,
      ignoredLabels: Set[Int]
  ): Either[SurfaceLocusError, SurfaceParcellation[S]] =
    domain.checkGeometry(labeled.geometry).map: _ =>
      final class Parcel
      val labels =
        Vector.tabulate(labeled.labels.length)(labeled.labels.apply)
          .filterNot(ignoredLabels)
          .distinct
          .sorted
      val parcelSpace =
        FiniteSpace
          .make[Parcel](
            SpaceKey.unsafe(s"${domain.finiteSpace.key.value}:parcels:${labels.mkString(",")}"),
            labels.length
          )
          .toOption
          .get
      val ordinalByLabel = labels.zipWithIndex.toMap
      val assignments = Array.fill[Option[Int]](domain.finiteSpace.size)(None)
      var row = 0
      while row < labeled.indices.length do
        val label = labeled.labels(row)
        ordinalByLabel.get(label).foreach: parcel =>
          assignments(labeled.indices(row)) = Some(parcel)
        row += 1
      val quotient =
        Parcellation
          .fromAssignments(domain.finiteSpace, parcelSpace, assignments)
          .toOption
          .get
      val ids = IndexedField.fromValues(parcelSpace, labels).toOption.get
      val info =
        IndexedField.fromValues(parcelSpace, labels.map(labeled.info)).toOption.get
      val order =
        Selection.fromOrdinals(parcelSpace, labels.indices).toOption.get
      new SurfaceParcellation[S]:
        type P = Parcel
        val parcellation: Parcellation[S, Parcel] = quotient
        val labelIds: IndexedField[Parcel, Int] = ids
        val metadata: IndexedField[Parcel, Option[LabelInfo]] = info
        val displayOrder: Selection[Parcel] = order
