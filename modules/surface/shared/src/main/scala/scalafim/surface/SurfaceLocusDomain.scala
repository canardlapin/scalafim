package scalafim.surface

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

final case class SurfaceRoiView[S, A](
    region: Region[S],
    values: Section[S, Option[A]],
    annotation: String
)

final class SurfaceLocusDomain[S] private[surface] (
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
      // Total: `region` shares this field's `S`, so there is no mismatch case.
      val section = optional.restrict(region)
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
  def semantic(
      semanticKey: SpaceKey,
      geometry: SurfaceGeometry
  ): Either[SurfaceLocusError, SomeSurfaceLocusDomain] =
    SomeSurfaceLocusDomain.make(
      semanticKey,
      geometry,
      SurfaceIdentityBasis.Semantic
    )

  def structuralCompatibility(
      geometry: SurfaceGeometry
  ): Either[SurfaceLocusError, SomeSurfaceLocusDomain] =
    SurfaceMeshDomain
      .from(geometry)
      .left
      .map(SurfaceLocusError.InvalidMeshDomain.apply)
      .map: meshDomain =>
        SomeSurfaceLocusDomain.fromKey(
          SpaceKey.unsafe(s"scalafim:surface:structural:${meshDomain.display}"),
          geometry,
          meshDomain,
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
    make(semanticKey, geometry, SurfaceIdentityBasis.Semantic)

  private[surface] def make(
      semanticKey: SpaceKey,
      geometry: SurfaceGeometry,
      basis: SurfaceIdentityBasis
  ): Either[SurfaceLocusError, SomeSurfaceLocusDomain] =
    SurfaceMeshDomain
      .from(geometry)
      .left
      .map(SurfaceLocusError.InvalidMeshDomain.apply)
      .map: meshDomain =>
        val key =
          SpaceKey.unsafe(s"${semanticKey.value}:surface:${meshDomain.display}")
        fromKey(key, geometry, meshDomain, basis)

  private[surface] def fromKey(
      key: SpaceKey,
      geometry: SurfaceGeometry,
      meshDomain: SurfaceMeshDomain,
      basis: SurfaceIdentityBasis
  ): SomeSurfaceLocusDomain =
    val resolution =
      DomainFactory.unsafeRestore(key, geometry.vertexCount)
    new SomeSurfaceLocusDomain:
      type S = resolution.S
      val value: SurfaceLocusDomain[S] =
        new SurfaceLocusDomain(
          geometry,
          meshDomain,
          resolution.space,
          basis
        )

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
      val labels =
        Vector.tabulate(labeled.labels.length)(labeled.labels.apply)
          .filterNot(ignoredLabels)
          .distinct
          .sorted
      val parcelResolution =
        DomainFactory.unsafeRestore(
          SpaceKey.unsafe(
            s"${domain.finiteSpace.id.value}:parcels:${labels.mkString(",")}"
          ),
          labels.length
        )
      val parcelSpace = parcelResolution.space
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
        type P = parcelResolution.S
        val parcellation: Parcellation[S, P] = quotient
        val labelIds: IndexedField[P, Int] = ids
        val metadata: IndexedField[P, Option[LabelInfo]] = info
        val displayOrder: Selection[P] = order
