package scalafim.fmri.mvpa.spatial

import locus4s.{Index, Region, Selection}
import multivar.core.SemanticSpace
import resample4s.core.{IndexSpace, Injection}
import scalafim.fmri.mvpa.AxisRef
import scalafim.fmri.mvpa.measurement.{MeasurementError, MeasurementFrame, MeasurementFrameDeclaration, MeasurementId, MeasurementLeg, PackedMeasurementEntry}
import scalafim.image.VolumeNeighborhoods
import scalafim.locus.CenteredSearchlight

/** Display and spatial support accompany a measurement, but do not contribute
  * to its scientific linear-map identity. */
final case class SpatialMeasurementRendition[S](
    center: Option[Index[S]],
    support: Region[S],
    label: Option[String] = None
)

/** One named spatial support. `Selection` retains its supplied order whereas
  * `Region` uses ambient domain order. */
enum SpatialMeasurementSupport[S]:
  case Regional(value: Region[S])
  case Selected(value: Selection[S])

  def space =
    this match
      case Regional(value) => value.space
      case Selected(value) => value.space

  def region =
    this match
      case Regional(value) => value
      case Selected(value) => value.region

  def ordinals: Array[Int] =
    this match
      case Regional(value) => value.ordinalsInDomainOrder
      case Selected(value) => value.ordinals

final case class SpatialMeasurementSite[S](
    id: MeasurementId,
    support: SpatialMeasurementSupport[S],
    center: Option[Index[S]] = None,
    label: Option[String] = None
)

enum SpatialMeasurementFrameError:
  case WrongRuntimeOwner(context: String, expected: String, actual: String)
  case EmptyFrame
  case DuplicateId(id: MeasurementId)
  case EmptySupport(id: MeasurementId)
  case InvalidLeg(id: MeasurementId, error: MeasurementError)

  def message: String =
    this match
      case WrongRuntimeOwner(context, expected, actual) =>
        s"$context belongs to $actual, expected live domain owner $expected"
      case EmptyFrame => "spatial measurement frame must contain at least one site"
      case DuplicateId(id) => s"spatial measurement frame repeats id '${id.value}'"
      case EmptySupport(id) => s"spatial measurement '${id.value}' has empty support"
      case InvalidLeg(id, error) => s"spatial measurement '${id.value}': ${error.message}"

/** Adapters from locus-owned supports to lazy, typed measurement frames.
  *
  * The runtime-owner comparison happens before any ordinal/support read. This
  * is deliberate: equal domain sizes or persistent labels never authorize a
  * spatial measurement against another live domain.
  */
object SpatialMeasurementFrames:
  def identity[K](
      source: AxisRef[K],
      declaration: MeasurementFrameDeclaration,
      id: MeasurementId,
      label: Option[String] = None
  ): MeasurementFrame[source.Id, K, SpatialMeasurementRendition[source.Locus]] =
    val bound = MeasurementFrameDeclaration(declaration.key, declaration.revision, Vector(
      "caller-declaration" -> declaration.fingerprint,
      "identity-measurement" -> id.value
    ))
    MeasurementFrame.lazyFrame(source, bound):
      val leg =
        MeasurementLeg
          .identity(source, id)
          .fold(error => throw new IllegalStateException(SpatialMeasurementFrameError.InvalidLeg(id, error).message), scala.Predef.identity)
      Iterator(
        PackedMeasurementEntry(
          leg,
          SpatialMeasurementRendition(None, Region.whole(source.locus), label)
        )
      )

  def regions[K, S](
      source: AxisRef[K] { type Locus = S },
      declaration: MeasurementFrameDeclaration,
      sites: Seq[SpatialMeasurementSite[S]]
  ): Either[SpatialMeasurementFrameError, MeasurementFrame[source.Id, K, SpatialMeasurementRendition[S]]] =
    val materialized = sites.toVector
    if materialized.isEmpty then Left(SpatialMeasurementFrameError.EmptyFrame)
    else
      var index = 0
      val ids = scala.collection.mutable.HashSet.empty[MeasurementId]
      while index < materialized.length do
        val site = materialized(index)
        if !source.locus.sameRuntimeOwnerAs(site.support.space) then
          return Left(ownerError("spatial support", source, site.support.space.toString))
        if site.support.ordinals.isEmpty then
          return Left(SpatialMeasurementFrameError.EmptySupport(site.id))
        if ids.contains(site.id) then return Left(SpatialMeasurementFrameError.DuplicateId(site.id))
        site.center match
          case Some(center) if center.ordinal < 0 || center.ordinal >= source.size =>
            return Left(ownerError("spatial center", source, center.toString))
          case _ => ()
        ids += site.id
        index += 1

      val ordered = materialized.sortBy(_.id.value)
      val population =
        IndexSpace.of(source.size).fold(error => throw new IllegalStateException(error.message), scala.Predef.identity)
      Right:
        MeasurementFrame.lazyFrame(source, declarationWithSupports(declaration, ordered)):
          ordered.iterator.map: site =>
            val injection =
              Injection
                .from(IArray.from(site.support.ordinals), population)
                .fold(error => throw new IllegalStateException(error.message), scala.Predef.identity)
            val leg =
              MeasurementLeg
                .hardSelection(source, site.id, injection)
                .fold(error => throw new IllegalStateException(SpatialMeasurementFrameError.InvalidLeg(site.id, error).message), scala.Predef.identity)
            PackedMeasurementEntry(
              leg,
              SpatialMeasurementRendition(site.center, site.support.region, site.label)
            )

  def volumeNeighborhoods[K, S](
      source: AxisRef[K] { type Locus = S },
      declaration: MeasurementFrameDeclaration,
      neighborhoods: VolumeNeighborhoods[S],
      label: Index[S] => Option[String] = (_: Index[S]) => None
  ): Either[SpatialMeasurementFrameError, MeasurementFrame[source.Id, K, SpatialMeasurementRendition[S]]] =
    if !source.locus.sameRuntimeOwnerAs(neighborhoods.centers.space) then
      Left(ownerError("volume neighborhood centers", source, neighborhoods.centers.space.toString))
    else if !source.locus.sameRuntimeOwnerAs(neighborhoods.relation.from) then
      Left(ownerError("volume neighborhood source relation", source, neighborhoods.relation.from.toString))
    else if !source.locus.sameRuntimeOwnerAs(neighborhoods.relation.to) then
      Left(ownerError("volume neighborhood target relation", source, neighborhoods.relation.to.toString))
    else
      val sites = Vector.newBuilder[SpatialMeasurementSite[S]]
      val centers = neighborhoods.centers.indicesInDomainOrder
      while centers.hasNext do
        val center = centers.next()
        neighborhoods.regionAt(center).foreach: support =>
          sites += SpatialMeasurementSite(
            MeasurementId.unsafe(stableId(source, "volume", center.ordinal)),
            SpatialMeasurementSupport.Regional(support),
            Some(center),
            label(center)
          )
      regions(source, declaration, sites.result())

  def surfacePatches[K, S](
      source: AxisRef[K] { type Locus = S },
      declaration: MeasurementFrameDeclaration,
      patches: CenteredSearchlight[S],
      label: Index[S] => Option[String] = (_: Index[S]) => None
  ): Either[SpatialMeasurementFrameError, MeasurementFrame[source.Id, K, SpatialMeasurementRendition[S]]] =
    val searchlight = patches.searchlight
    if !source.locus.sameRuntimeOwnerAs(searchlight.centers.space) then
      Left(ownerError("surface patch centers", source, searchlight.centers.space.toString))
    else if !source.locus.sameRuntimeOwnerAs(searchlight.neighborhoods.from) then
      Left(ownerError("surface patch source relation", source, searchlight.neighborhoods.from.toString))
    else if !source.locus.sameRuntimeOwnerAs(searchlight.neighborhoods.to) then
      Left(ownerError("surface patch target relation", source, searchlight.neighborhoods.to.toString))
    else
      val sites = Vector.newBuilder[SpatialMeasurementSite[S]]
      val centers = searchlight.centers.indicesInDomainOrder
      while centers.hasNext do
        val center = centers.next()
        searchlight.regionAt(center).foreach: support =>
          sites += SpatialMeasurementSite(
            MeasurementId.unsafe(stableId(source, "surface", center.ordinal)),
            SpatialMeasurementSupport.Regional(support),
            Some(center),
            label(center)
          )
      regions(source, declaration, sites.result())

  private def stableId[K](source: AxisRef[K], kind: String, ordinal: Int): String =
    val width = math.max(1, (source.size - 1).toString.length)
    val raw = ordinal.toString
    val padded = ("0" * (width - raw.length)) + raw
    s"$kind:${source.descriptor.coordinateSignature.value}:$padded"

  /** Bind a generator declaration to the exact linear supports it will create.
    * Site IDs and ordinal order are scientific map inputs; center and label are
    * rendition only and deliberately stay out of the manifest.
    */
  private def declarationWithSupports[S](
      declaration: MeasurementFrameDeclaration,
      sites: Vector[SpatialMeasurementSite[S]]
  ): MeasurementFrameDeclaration =
    val manifest = sites.map: site =>
      "spatial-support" -> supportManifest(site.id, site.support.ordinals)
    MeasurementFrameDeclaration(
      declaration.key,
      declaration.revision,
      Vector("caller-declaration" -> declaration.fingerprint) ++ manifest
    )

  private def supportManifest(id: MeasurementId, ordinals: Array[Int]): String =
    val builder = new StringBuilder
    builder.append(id.value.length).append(':').append(id.value)
    builder.append(':').append(ordinals.length)
    var index = 0
    while index < ordinals.length do
      builder.append(':').append(ordinals(index))
      index += 1
    builder.result()

  private def ownerError[K](context: String, source: AxisRef[K], actual: String): SpatialMeasurementFrameError =
    SpatialMeasurementFrameError.WrongRuntimeOwner(context, source.locus.toString, actual)
