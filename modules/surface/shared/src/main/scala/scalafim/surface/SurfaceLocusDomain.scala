package scalafim.surface

import locus4s.data.Field
import scalafim.locus.*

enum SurfaceLocusError:
  case InvalidMeshDomain(error: SurfaceError)
  case TopologyMismatch(expected: String, actual: String)
  case VertexOwnerMismatch(expectedFingerprint: String, actualFingerprint: String)
  case NotFullField(expectedVertices: Int, actualVertices: Int)
  case WrongSpace(error: SpaceMismatch)

  def message: String =
    this match
      case InvalidMeshDomain(error) => error.message
      case TopologyMismatch(expected, actual) =>
        s"surface topology mismatch: expected $expected, found $actual"
      case VertexOwnerMismatch(expected, actual) =>
        s"surface vertex owner mismatch: expected connectivity $expected, found $actual; " +
          "equal fingerprints do not authorize cross-owner indexing; canonicalize at ingestion"
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
    val finiteSpace: FiniteDomain[S],
    val identityBasis: SurfaceIdentityBasis,
    val identityKey: SpaceKey
):
  require(
    finiteSpace.sameRuntimeOwnerAs(geometry.mesh.topology.vertices),
    "surface locus domain must use topology.vertices as its exact owner"
  )

  /** Preferred spelling; `finiteSpace` remains as a compatibility alias. */
  def vertices: FiniteDomain[S] =
    finiteSpace

  def optionalField[A](
      field: SurfaceField[A]
  ): Either[SurfaceLocusError, Field[S, Option[A]]] =
    checkGeometry(field.geometry).flatMap: _ =>
      val source = field.locus
      if !finiteSpace.sameRuntimeOwnerAs(source.vertices) then
        Left(SurfaceLocusError.WrongSpace(mismatch(finiteSpace, source.vertices)))
      else
        val alignment =
          source.vertices
            .align(finiteSpace)
            .fold(
              _ => throw new IllegalStateException("validated surface owners did not align"),
              identity
            )
        Right(source.optionalValues.rebind(alignment))

  def fullField[A](
      field: SurfaceField[A]
  ): Either[SurfaceLocusError, Field[S, A]] =
    checkGeometry(field.geometry).flatMap: _ =>
      val source = field.locus
      source.fullValues match
        case None =>
          Left(SurfaceLocusError.NotFullField(finiteSpace.size, field.size))
        case Some(values) =>
          if !finiteSpace.sameRuntimeOwnerAs(source.vertices) then
            Left(SurfaceLocusError.WrongSpace(mismatch(finiteSpace, source.vertices)))
          else
            val alignment =
              source.vertices
                .align(finiteSpace)
                .fold(
                  _ => throw new IllegalStateException("validated surface owners did not align"),
                  identity
                )
            Right(values.rebind(alignment))

  def roi[A](
      surfaceRoi: SurfaceRoi[A]
  ): Either[SurfaceLocusError, SurfaceRoiView[S, A]] =
    checkGeometry(surfaceRoi.geometry).flatMap: _ =>
      val source = surfaceRoi.locus
      if !finiteSpace.sameRuntimeOwnerAs(source.vertices) then
        Left(SurfaceLocusError.WrongSpace(mismatch(finiteSpace, source.vertices)))
      else
        val alignment =
          source.vertices
            .align(finiteSpace)
            .fold(
              _ => throw new IllegalStateException("validated surface owners did not align"),
              identity
            )
        Right:
          SurfaceRoiView(
            source.support.rebind(alignment),
            source.section.rebind(alignment),
            surfaceRoi.label
          )

  def parcellation(
      labeled: LabeledSurface,
      ignoredLabels: Set[Int] = Set.empty
  ): Either[SurfaceLocusError, SurfaceParcellation[S]] =
    SurfaceParcellation.from(this, labeled, ignoredLabels)

  private[surface] def checkGeometry(
      actual: SurfaceGeometry
  ): Either[SurfaceLocusError, Unit] =
    SurfaceMeshDomain.from(actual).left.map(SurfaceLocusError.InvalidMeshDomain.apply).flatMap: actualDomain =>
      if geometry.hemisphere != actual.hemisphere then
        Left(SurfaceLocusError.TopologyMismatch(meshDomain.display, actualDomain.display))
      else if geometry.mesh.topology eq actual.mesh.topology then
        Right(())
      else
        Left:
          SurfaceLocusError.VertexOwnerMismatch(
            geometry.mesh.connectivityFingerprint.value,
            actual.mesh.connectivityFingerprint.value
          )

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
    val topology = geometry.mesh.topology
    new SomeSurfaceLocusDomain:
      type S = topology.Vertex
      val value: SurfaceLocusDomain[S] =
        new SurfaceLocusDomain(
          geometry,
          meshDomain,
          topology.vertices,
          basis,
          key
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
      val source = labeled.locus
      val alignment =
        source.vertices
          .align(domain.finiteSpace)
          .fold(
            _ => throw new IllegalStateException("validated label owners did not align"),
            identity
          )
      val labelsOnDomain = source.optionalValues.rebind(alignment)
      val labels =
        labeled.unsafeLabels.toVector
          .filterNot(ignoredLabels)
          .distinct
          .sorted
      val parcelResolution =
        DomainFactory.unsafeRestore(
          SpaceKey.unsafe(
            s"${domain.identityKey.value}:parcels:${labels.mkString(",")}"
          ),
          labels.length
        )
      val parcelSpace = parcelResolution.space
      val ordinalByLabel = labels.zipWithIndex.toMap
      val assignments = Array.fill[Option[Int]](domain.finiteSpace.size)(None)
      domain.finiteSpace.foreachIndex: vertex =>
        labelsOnDomain(vertex).flatMap(ordinalByLabel.get).foreach: parcel =>
          assignments(vertex.ordinal) = Some(parcel)
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
