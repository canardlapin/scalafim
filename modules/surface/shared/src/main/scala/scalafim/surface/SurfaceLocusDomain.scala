package scalafim.surface

import locus4s.{DomainRegistry, PartialSurjection}
import scalafim.locus.*

enum SurfaceLocusError:
  case InvalidMeshDomain(error: SurfaceError)
  case TopologyMismatch(expected: String, actual: String)
  case NotFullField(expectedVertices: Int, actualVertices: Int)
  case WrongSpace(error: SpaceMismatch)
  case DomainRestoreFailed(error: DomainFactoryError)

  def message: String =
    this match
      case InvalidMeshDomain(error) => error.message
      case TopologyMismatch(expected, actual) =>
        s"surface topology mismatch: expected $expected, found $actual"
      case NotFullField(expected, actual) =>
        s"full surface field requires $expected vertices, found $actual"
      case WrongSpace(error) => error.message
      case DomainRestoreFailed(error) =>
        s"surface domain restoration failed: ${error.message}"

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
    val registry: DomainRegistry,
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

  def parcelAssignment(
      labeled: LabeledSurface,
      ignoredLabels: Set[Int] = Set.empty
  ): Either[SurfaceLocusError, SurfaceParcelAssignment[S]] =
    parcelAssignmentIn(registry, labeled, ignoredLabels)

  def parcelAssignmentIn(
      registry: DomainRegistry,
      labeled: LabeledSurface,
      ignoredLabels: Set[Int] = Set.empty
  ): Either[SurfaceLocusError, SurfaceParcelAssignment[S]] =
    SurfaceParcelAssignment.from(registry, this, labeled, ignoredLabels)

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
    semanticIn(DomainRegistry.empty, semanticKey, geometry)

  def semanticIn(
      registry: DomainRegistry,
      semanticKey: SpaceKey,
      geometry: SurfaceGeometry
  ): Either[SurfaceLocusError, SomeSurfaceLocusDomain] =
    SomeSurfaceLocusDomain.make(
      registry,
      semanticKey,
      geometry,
      SurfaceIdentityBasis.Semantic
    )

  def structuralCompatibility(
      geometry: SurfaceGeometry
  ): Either[SurfaceLocusError, SomeSurfaceLocusDomain] =
    structuralCompatibilityIn(DomainRegistry.empty, geometry)

  def structuralCompatibilityIn(
      registry: DomainRegistry,
      geometry: SurfaceGeometry
  ): Either[SurfaceLocusError, SomeSurfaceLocusDomain] =
    SurfaceMeshDomain
      .from(geometry)
      .left
      .map(SurfaceLocusError.InvalidMeshDomain.apply)
      .flatMap: meshDomain =>
        SomeSurfaceLocusDomain.fromKey(
          registry,
          SpaceKey.unsafe(s"scalafim:surface:structural:${meshDomain.display}"),
          geometry,
          meshDomain,
          SurfaceIdentityBasis.StructuralCompatibility
        )

trait SomeSurfaceLocusDomain:
  type S
  val registry: DomainRegistry
  val value: SurfaceLocusDomain[S]

object SomeSurfaceLocusDomain:
  def semantic(
      semanticKey: SpaceKey,
      geometry: SurfaceGeometry
  ): Either[SurfaceLocusError, SomeSurfaceLocusDomain] =
    semanticIn(DomainRegistry.empty, semanticKey, geometry)

  def semanticIn(
      registry: DomainRegistry,
      semanticKey: SpaceKey,
      geometry: SurfaceGeometry
  ): Either[SurfaceLocusError, SomeSurfaceLocusDomain] =
    make(registry, semanticKey, geometry, SurfaceIdentityBasis.Semantic)

  private[surface] def make(
      registry: DomainRegistry,
      semanticKey: SpaceKey,
      geometry: SurfaceGeometry,
      basis: SurfaceIdentityBasis
  ): Either[SurfaceLocusError, SomeSurfaceLocusDomain] =
    SurfaceMeshDomain
      .from(geometry)
      .left
      .map(SurfaceLocusError.InvalidMeshDomain.apply)
      .flatMap: meshDomain =>
        val key =
          SpaceKey.unsafe(s"${semanticKey.value}:surface:${meshDomain.display}")
        fromKey(registry, key, geometry, meshDomain, basis)

  private[surface] def fromKey(
      registry: DomainRegistry,
      key: SpaceKey,
      geometry: SurfaceGeometry,
      meshDomain: SurfaceMeshDomain,
      basis: SurfaceIdentityBasis
  ): Either[SurfaceLocusError, SomeSurfaceLocusDomain] =
    DomainFactory
      .restore(registry, key, geometry.vertexCount)
      .left
      .map(SurfaceLocusError.DomainRestoreFailed.apply)
      .map: resolution =>
        new SomeSurfaceLocusDomain:
          type S = resolution.S
          val registry: DomainRegistry = resolution.registry
          val value: SurfaceLocusDomain[S] =
            new SurfaceLocusDomain(
              geometry,
              meshDomain,
              resolution.space,
              resolution.registry,
              basis
            )

trait SurfaceParcelAssignment[S]:
  type P
  val registry: DomainRegistry
  val assignment: PartialSurjection[S, P]
  val labelIds: IndexedField[P, Int]
  val metadata: IndexedField[P, Option[LabelInfo]]
  val displayOrder: Selection[P]

object SurfaceParcelAssignment:
  def from[S](
      registry: DomainRegistry,
      domain: SurfaceLocusDomain[S],
      labeled: LabeledSurface,
      ignoredLabels: Set[Int]
  ): Either[SurfaceLocusError, SurfaceParcelAssignment[S]] =
    domain.checkGeometry(labeled.geometry).flatMap: _ =>
      val labels =
        Vector.tabulate(labeled.labels.length)(labeled.labels.apply)
          .filterNot(ignoredLabels)
          .distinct
          .sorted
      DomainFactory
        .restore(
          registry,
          SpaceKey.unsafe(
            s"${domain.finiteSpace.id.value}:parcels:${labels.mkString(",")}"
          ),
          labels.length
        )
        .left
        .map(SurfaceLocusError.DomainRestoreFailed.apply)
        .map: parcelResolution =>
          val parcelSpace = parcelResolution.space
          val ordinalByLabel = labels.zipWithIndex.toMap
          val assignments = Array.fill[Option[Int]](domain.finiteSpace.size)(None)
          var row = 0
          while row < labeled.indices.length do
            val label = labeled.labels(row)
            ordinalByLabel.get(label).foreach: parcel =>
              assignments(labeled.indices(row)) = Some(parcel)
            row += 1
          val assignmentValue =
            PartialSurjection
              .fromOptionalTargetOrdinals(domain.finiteSpace, parcelSpace, assignments)
              .toOption
              .get
          val ids = IndexedField.fromValues(parcelSpace, labels).toOption.get
          val info =
            IndexedField.fromValues(parcelSpace, labels.map(labeled.info)).toOption.get
          val order =
            Selection.fromOrdinals(parcelSpace, labels.indices).toOption.get
          new SurfaceParcelAssignment[S]:
            type P = parcelResolution.S
            val registry: DomainRegistry = parcelResolution.registry
            val assignment: PartialSurjection[S, P] = assignmentValue
            val labelIds: IndexedField[P, Int] = ids
            val metadata: IndexedField[P, Option[LabelInfo]] = info
            val displayOrder: Selection[P] = order
