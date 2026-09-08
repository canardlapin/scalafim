package scalafim.fmri.mvpa

import scalafim.atlas.AtlasRegionMetadata
import scalafim.atlas.AtlasRef
import scalafim.atlas.VolumeAtlas
import scalafim.image.GridMismatch
import scalafim.image.NeuroVol
import scalafim.image.SearchlightRadius
import scalafim.image.VolumeDomain
import scalafim.image.VolumeSearchlight
import scalafim.image.VolumeSearchlightError
import scalafim.image.VoxelRegion
import locus4s.CenteredNeighborhoodSystem
import locus4s.FiniteDomain
import locus4s.FiniteSpace
import locus4s.Index
import locus4s.Region as LocusRegion
import locus4s.Selection as LocusSelection
import scalafim.locus.Parcellation as LocusParcellation
import scalafim.surface.DistanceMetric
import scalafim.surface.LabeledSurface
import scalafim.surface.MeshTopology
import scalafim.surface.SurfaceLocusDomain
import scalafim.surface.SurfaceLocusError
import scalafim.surface.SurfaceSearchlight
import scalafim.surface.SurfaceSearchlightError
import resample4s.core.IndexSpace
import resample4s.core.Injection as OrdinalInjection

enum SpatialFrameError:
  case AxisIdentity(error: AxisIdentityError)
  case Axis(error: AxisRefError)
  case Measurement(error: MeasurementError)
  case WrongDomain(expected: String, actual: String)
  case EmptySupport(id: MeasurementId)
  case InvalidOrdinalPlan(detail: String)
  case InvalidRegion(detail: String)
  case Grid(error: GridMismatch)
  case VolumeSearchlight(error: VolumeSearchlightError)
  case SurfaceDomain(error: SurfaceLocusError)
  case SurfaceSearchlight(error: SurfaceSearchlightError)
  case InvalidVolumeLabel(label: Int)
  case MissingAtlasRegion(regionId: Int, label: String)
  case EmptyAtlas

  def message: String =
    this match
      case AxisIdentity(error)           => error.message
      case Axis(error)                   => error.message
      case Measurement(error)            => error.message
      case WrongDomain(expected, actual) =>
        s"spatial owner mismatch: expected $expected, found $actual"
      case EmptySupport(id) =>
        s"measurement '${id.value}' has empty spatial support"
      case InvalidOrdinalPlan(detail) =>
        s"invalid spatial ordinal plan: $detail"
      case InvalidRegion(detail) =>
        s"invalid spatial region: $detail"
      case Grid(error)               => error.message
      case VolumeSearchlight(error)  => error.message
      case SurfaceDomain(error)      => error.message
      case SurfaceSearchlight(error) => error.message
      case InvalidVolumeLabel(label) =>
        s"volume labels must be non-negative after background removal; found $label"
      case MissingAtlasRegion(regionId, label) =>
        s"atlas region $regionId ($label) has no covered voxels"
      case EmptyAtlas =>
        "atlas coverage policy removed every region"

/** The exact locus4s owner paired with the neural axis derived from its full persistent identity and declared
  * coordinate basis.
  */
final class IdentifiedLocusAxis[S] private[mvpa] (
    val domain: FiniteSpace[S],
    val features: AxisRef[FeatureId]
)

object SpatialAxes:
  def fromDomain[S](
      domain: FiniteSpace[S],
      basisKind: String,
      basisFields: Seq[(String, String)],
      units: Option[AxisUnits] = None
  ): Either[SpatialFrameError, IdentifiedLocusAxis[S]] =
    val key = domain.key
    val ownerFields = Vector(
      "locus-domain-id" -> key.id.value,
      "locus-domain-size" -> key.size.toString,
      "locus-domain-fingerprint" -> key.fingerprint.map(_.value).getOrElse("none")
    )
    val canonicalFields = (ownerFields ++ basisFields).sortBy(_._1)
    val digestWriter = CanonicalWriter()
    digestWriter.string("scalafim-mvpa-locus-axis/v1")
    digestWriter.rawFields(canonicalFields)
    val digest = AxisDigest.sha256Hex(digestWriter.result())
    val keys = Vector.tabulate(domain.size): ordinal =>
      FeatureId.unsafe(s"locus-$digest-$ordinal")

    for
      basis <- CoordinateBasis
        .apply(basisKind, canonicalFields)
        .left
        .map(SpatialFrameError.AxisIdentity.apply)
      provenance <- CoordinateProvenance
        .apply(
          s"locus-domain:${key.id.value}",
          key.fingerprint.map(_.value).getOrElse(s"size-${key.size}"),
          Vector(s"coordinate-basis-$digest")
        )
        .left
        .map(SpatialFrameError.AxisIdentity.apply)
      axis <- AxisRef
        .create(
          AxisId.unsafe(s"neural-$digest"),
          AxisPurpose.NeuralFeatures,
          keys,
          basis,
          units,
          AxisScale.nominal,
          provenance
        )
        .left
        .map(SpatialFrameError.Axis.apply)
    yield new IdentifiedLocusAxis(domain, axis)

  def volume[S](
      domain: VolumeDomain[S],
      units: Option[AxisUnits] = None
  ): Either[SpatialFrameError, IdentifiedLocusAxis[S]] =
    val space = domain.volumeSpace.toNeuroSpace
    val affine = space.trans
    val affineValues = Vector.newBuilder[String]
    var row = 0
    while row < affine.rows do
      var column = 0
      while column < affine.cols do
        affineValues += java.lang.Double.toHexString(affine(row, column))
        column += 1
      row += 1
    fromDomain(
      domain.finiteSpace,
      "volume-grid",
      Vector(
        "dimensions" -> space.spatialDims.mkString("x"),
        "affine-row-major" -> affineValues.result().mkString(","),
        "ordinal-layout" -> "x-fastest"
      ),
      units
    )

  def surface[S](
      domain: SurfaceLocusDomain[S],
      units: Option[AxisUnits] = None
  ): Either[SpatialFrameError, IdentifiedLocusAxis[S]] =
    fromDomain(
      domain.finiteSpace,
      "surface-topology",
      Vector(
        "mesh-domain" -> domain.meshDomain.display,
        "ordinal-layout" -> "vertex-index"
      ),
      units
    )

final case class RegionRendition[S](
    support: LocusRegion[S],
    label: Option[String]
)

final case class SelectionRendition[S](
    selection: LocusSelection[S],
    label: Option[String]
)

final case class SearchlightRendition[C, S](
    center: Index[C],
    ambientCenter: Index[S],
    support: LocusRegion[S],
    label: Option[String]
)

final case class AtlasRegionRendition[S](
    support: LocusRegion[S],
    atlas: AtlasRef,
    region: AtlasRegionMetadata
)

enum AtlasCoveragePolicy:
  case RequireEveryRegion
  case KeepCoveredRegions

object LocusFrames:
  def region[S](
      axis: IdentifiedLocusAxis[S],
      id: MeasurementId,
      support: LocusRegion[S],
      label: Option[String] = None
  ): Either[
    SpatialFrameError,
    MeasurementFrame[axis.features.Id, FeatureId, RegionRendition[S]]
  ] =
    regions(axis, Vector((id, support, label)))

  def regions[S](
      axis: IdentifiedLocusAxis[S],
      values: Seq[(MeasurementId, LocusRegion[S], Option[String])]
  ): Either[
    SpatialFrameError,
    MeasurementFrame[axis.features.Id, FeatureId, RegionRendition[S]]
  ] =
    val items = Vector.newBuilder[FrameItem[RegionRendition[S]]]
    val iterator = values.iterator
    while iterator.hasNext do
      val (id, support, label) = iterator.next()
      checkOwner(axis, support.space) match
        case Left(error) => return Left(error)
        case Right(_)    => ()
      if support.isEmpty then return Left(SpatialFrameError.EmptySupport(id))
      items += FrameItem(
        id,
        support.ordinalsInDomainOrder,
        RegionRendition(support, label)
      )
    buildFrame(axis, items.result())

  def selection[S](
      axis: IdentifiedLocusAxis[S],
      id: MeasurementId,
      selection: LocusSelection[S],
      label: Option[String] = None
  ): Either[
    SpatialFrameError,
    MeasurementFrame[axis.features.Id, FeatureId, SelectionRendition[S]]
  ] =
    checkOwner(axis, selection.space).flatMap: _ =>
      if selection.isEmpty then Left(SpatialFrameError.EmptySupport(id))
      else
        buildFrame(
          axis,
          Vector(FrameItem(id, selection.ordinals, SelectionRendition(selection, label)))
        )

  def parcellation[S, P](
      axis: IdentifiedLocusAxis[S],
      parcellation: LocusParcellation[S, P],
      label: Index[P] => Option[String] = (_: Index[P]) => None
  ): Either[
    SpatialFrameError,
    MeasurementFrame[axis.features.Id, FeatureId, RegionRendition[S]]
  ] =
    checkOwner(axis, parcellation.ambient).flatMap: _ =>
      val values = Vector.newBuilder[(MeasurementId, LocusRegion[S], Option[String])]
      val width = ordinalWidth(parcellation.parcels.size)
      parcellation.parcels.foreachIndex: parcel =>
        values += ((
          MeasurementId.unsafe(s"parcel-${padded(parcel.ordinal, width)}"),
          parcellation.fiber(parcel),
          label(parcel)
        ))
      regions(axis, values.result())

  def searchlights[C, S](
      axis: IdentifiedLocusAxis[S],
      neighborhoods: CenteredNeighborhoodSystem[C, S],
      label: Index[S] => Option[String] = (center: Index[S]) => Some(center.ordinal.toString)
  ): Either[
    SpatialFrameError,
    MeasurementFrame[axis.features.Id, FeatureId, SearchlightRendition[C, S]]
  ] =
    checkOwner(axis, neighborhoods.ambient).flatMap: _ =>
      val items = Vector.newBuilder[FrameItem[SearchlightRendition[C, S]]]
      val width = ordinalWidth(neighborhoods.ambient.size)
      neighborhoods.centers.foreachIndex: center =>
        val ambientCenter = neighborhoods.center(center)
        val support = neighborhoods.neighborhood(center)
        items += FrameItem(
          MeasurementId.unsafe(
            s"searchlight-${padded(ambientCenter.ordinal, width)}"
          ),
          support.ordinalsInDomainOrder,
          SearchlightRendition(
            center,
            ambientCenter,
            support,
            label(ambientCenter)
          )
        )
      buildFrame(axis, items.result())

  private[mvpa] final case class FrameItem[R](
      id: MeasurementId,
      ordinals: Array[Int],
      rendition: R
  )

  private[mvpa] def buildFrame[S, R](
      axis: IdentifiedLocusAxis[S],
      items: Vector[FrameItem[R]]
  ): Either[
    SpatialFrameError,
    MeasurementFrame[axis.features.Id, FeatureId, R]
  ] =
    val population = IndexSpace
      .of(axis.features.size)
      .left
      .map(error => SpatialFrameError.InvalidOrdinalPlan(error.message))
    population.flatMap: sourceSpace =>
      val entries = Vector.newBuilder[
        MeasurementEntry[axis.features.Id, FeatureId, FeatureId, R]
      ]
      val iterator = items.iterator
      var failure: Option[SpatialFrameError] = None
      while iterator.hasNext && failure.isEmpty do
        val item = iterator.next()
        if item.ordinals.isEmpty then failure = Some(SpatialFrameError.EmptySupport(item.id))
        else
          val encoded = IArray.unsafeFromArray(item.ordinals.clone())
          OrdinalInjection
            .from(encoded, sourceSpace)
            .left
            .map(error => SpatialFrameError.InvalidOrdinalPlan(error.message))
            .flatMap: injection =>
              Measurement
                .hardSelection(axis.features, item.id, injection)
                .left
                .map(SpatialFrameError.Measurement.apply)
            .fold(
              error => failure = Some(error),
              measurement =>
                val _ = entries += MeasurementEntry(measurement, item.rendition)
            )
      failure match
        case Some(error) => Left(error)
        case None        =>
          MeasurementFrame(axis.features)(entries.result()).left
            .map(SpatialFrameError.Measurement.apply)

  private[mvpa] def checkOwner[S, T](
      axis: IdentifiedLocusAxis[S],
      actual: FiniteDomain[T]
  ): Either[SpatialFrameError, Unit] =
    if axis.domain.sameRuntimeOwnerAs(actual) then Right(())
    else
      Left(
        SpatialFrameError.WrongDomain(
          axis.domain.descriptor.toString,
          actual.descriptor.toString
        )
      )

  private def ordinalWidth(size: Int): Int =
    math.max(1, math.max(0, size - 1).toString.length)

  private def padded(value: Int, width: Int): String =
    val raw = value.toString
    "0" * math.max(0, width - raw.length) + raw

object VolumeFrames:
  def roi[S](
      axis: IdentifiedLocusAxis[S],
      domain: VolumeDomain[S],
      id: MeasurementId,
      region: VoxelRegion,
      label: Option[String] = None
  ): Either[
    SpatialFrameError,
    MeasurementFrame[axis.features.Id, FeatureId, RegionRendition[S]]
  ] =
    for
      _ <- LocusFrames.checkOwner(axis, domain.finiteSpace)
      support <- domain.region(region).left.map(SpatialFrameError.Grid.apply)
      frame <- LocusFrames.region(axis, id, support, label)
    yield frame

  def labels[S](
      axis: IdentifiedLocusAxis[S],
      domain: VolumeDomain[S],
      values: NeuroVol[Int],
      background: Set[Int] = Set(0)
  ): Either[
    SpatialFrameError,
    MeasurementFrame[axis.features.Id, FeatureId, RegionRendition[S]]
  ] =
    for
      _ <- LocusFrames.checkOwner(axis, domain.finiteSpace)
      _ <- domain.indexedField(values).left.map(SpatialFrameError.Grid.apply)
      grouped <- groupedLabels(values, background)
      regions <- labelRegions(domain, grouped)
      frame <- LocusFrames.regions(axis, regions)
    yield frame

  def atlas[S](
      axis: IdentifiedLocusAxis[S],
      domain: VolumeDomain[S],
      atlas: VolumeAtlas,
      coverage: AtlasCoveragePolicy = AtlasCoveragePolicy.RequireEveryRegion
  ): Either[
    SpatialFrameError,
    MeasurementFrame[axis.features.Id, FeatureId, AtlasRegionRendition[S]]
  ] =
    for
      _ <- LocusFrames.checkOwner(axis, domain.finiteSpace)
      _ <- domain.indexedField(atlas.labelVolume).left.map(SpatialFrameError.Grid.apply)
      grouped <- groupedLabels(atlas.labelVolume, Set(0))
      frame <- atlasFrame(axis, domain, atlas, grouped, coverage)
    yield frame

  def metricSearchlights[S](
      axis: IdentifiedLocusAxis[S],
      domain: VolumeDomain[S],
      radius: SearchlightRadius
  ): Either[
    SpatialFrameError,
    MeasurementFrame[axis.features.Id, FeatureId, SearchlightRendition[S, S]]
  ] =
    for
      _ <- LocusFrames.checkOwner(axis, domain.finiteSpace)
      neighborhoods <- VolumeSearchlight
        .metricBalls(domain, radius)
        .left
        .map(SpatialFrameError.VolumeSearchlight.apply)
      frame <- LocusFrames.searchlights(axis, neighborhoods)
    yield frame

  def metricSearchlights[S](
      axis: IdentifiedLocusAxis[S],
      domain: VolumeDomain[S],
      radius: SearchlightRadius,
      centers: LocusSelection[S]
  ): Either[
    SpatialFrameError,
    MeasurementFrame[
      axis.features.Id,
      FeatureId,
      SearchlightRendition[centers.I, S]
    ]
  ] =
    for
      _ <- LocusFrames.checkOwner(axis, domain.finiteSpace)
      neighborhoods <- VolumeSearchlight
        .metricBalls(domain, radius, centers)
        .left
        .map(SpatialFrameError.VolumeSearchlight.apply)
      frame <- LocusFrames.searchlights(axis, neighborhoods)
    yield frame

  private def groupedLabels(
      values: NeuroVol[Int],
      background: Set[Int]
  ): Either[SpatialFrameError, Vector[(Int, Vector[Int])]] =
    val grouped = scala.collection.mutable.Map.empty[Int, scala.collection.mutable.ArrayBuffer[Int]]
    var ordinal = 0
    while ordinal < values.values.size do
      val label = values.linear(ordinal)
      if !background.contains(label) then
        if label < 0 then return Left(SpatialFrameError.InvalidVolumeLabel(label))
        grouped.getOrElseUpdate(label, scala.collection.mutable.ArrayBuffer.empty) += ordinal
      ordinal += 1
    Right(grouped.toVector.sortBy(_._1).map((label, support) => label -> support.toVector))

  private def labelRegions[S](
      domain: VolumeDomain[S],
      grouped: Vector[(Int, Vector[Int])]
  ): Either[
    SpatialFrameError,
    Vector[(MeasurementId, LocusRegion[S], Option[String])]
  ] =
    val out = Vector.newBuilder[(MeasurementId, LocusRegion[S], Option[String])]
    val iterator = grouped.iterator
    while iterator.hasNext do
      val (label, ordinals) = iterator.next()
      LocusRegion.fromOrdinals(domain.finiteSpace, ordinals) match
        case Left(error)   => return Left(SpatialFrameError.InvalidRegion(error.message))
        case Right(region) =>
          out += ((MeasurementId.unsafe(s"volume-label-$label"), region, Some(label.toString)))
    Right(out.result())

  private def atlasFrame[S](
      axis: IdentifiedLocusAxis[S],
      domain: VolumeDomain[S],
      atlas: VolumeAtlas,
      grouped: Vector[(Int, Vector[Int])],
      coverage: AtlasCoveragePolicy
  ): Either[
    SpatialFrameError,
    MeasurementFrame[axis.features.Id, FeatureId, AtlasRegionRendition[S]]
  ] =
    val byLabel = grouped.toMap
    val items = Vector.newBuilder[LocusFrames.FrameItem[AtlasRegionRendition[S]]]
    val iterator = atlas.regions.regions.iterator
    while iterator.hasNext do
      val region = iterator.next()
      byLabel.get(region.id.value) match
        case None if coverage == AtlasCoveragePolicy.RequireEveryRegion =>
          return Left(SpatialFrameError.MissingAtlasRegion(region.id.value, region.label))
        case None =>
          ()
        case Some(ordinals) =>
          LocusRegion.fromOrdinals(domain.finiteSpace, ordinals) match
            case Left(error) =>
              return Left(SpatialFrameError.InvalidRegion(error.message))
            case Right(support) =>
              items += LocusFrames.FrameItem(
                MeasurementId.unsafe(s"atlas-region-${region.id.value}"),
                support.ordinalsInDomainOrder,
                AtlasRegionRendition(support, atlas.ref, region)
              )
    val values = items.result()
    if values.isEmpty then Left(SpatialFrameError.EmptyAtlas)
    else LocusFrames.buildFrame(axis, values)

object SurfaceFrames:
  def labeled[S](
      axis: IdentifiedLocusAxis[S],
      domain: SurfaceLocusDomain[S],
      labeled: LabeledSurface,
      ignoredLabels: Set[Int] = Set.empty
  ): Either[
    SpatialFrameError,
    MeasurementFrame[axis.features.Id, FeatureId, RegionRendition[S]]
  ] =
    for
      _ <- LocusFrames.checkOwner(axis, domain.finiteSpace)
      parcels <- domain
        .parcellation(labeled, ignoredLabels)
        .left
        .map(SpatialFrameError.SurfaceDomain.apply)
      frame <-
        val values = Vector.newBuilder[(MeasurementId, LocusRegion[S], Option[String])]
        parcels.displayOrder.indices.foreach: parcel =>
          val labelId = parcels.labelIds(parcel)
          val label = parcels.metadata(parcel).map(_.name).orElse(Some(labelId.toString))
          values += ((
            MeasurementId.unsafe(s"surface-label-$labelId"),
            parcels.parcellation.fiber(parcel),
            label
          ))
        LocusFrames.regions(axis, values.result())
    yield frame

  def metricSearchlights[S](
      axis: IdentifiedLocusAxis[S],
      domain: SurfaceLocusDomain[S],
      topology: MeshTopology,
      radius: Double,
      metric: DistanceMetric = DistanceMetric.Geodesic
  ): Either[
    SpatialFrameError,
    MeasurementFrame[axis.features.Id, FeatureId, SearchlightRendition[S, S]]
  ] =
    for
      _ <- LocusFrames.checkOwner(axis, domain.finiteSpace)
      neighborhoods <- SurfaceSearchlight
        .metricBalls(domain, topology, radius, metric)
        .left
        .map(SpatialFrameError.SurfaceSearchlight.apply)
      frame <- LocusFrames.searchlights(axis, neighborhoods)
    yield frame

  def metricSearchlights[S](
      axis: IdentifiedLocusAxis[S],
      domain: SurfaceLocusDomain[S],
      topology: MeshTopology,
      radius: Double,
      centers: LocusSelection[S],
      metric: DistanceMetric
  ): Either[
    SpatialFrameError,
    MeasurementFrame[
      axis.features.Id,
      FeatureId,
      SearchlightRendition[centers.I, S]
    ]
  ] =
    for
      _ <- LocusFrames.checkOwner(axis, domain.finiteSpace)
      neighborhoods <- SurfaceSearchlight
        .metricBalls(domain, topology, radius, centers, metric)
        .left
        .map(SpatialFrameError.SurfaceSearchlight.apply)
      frame <- LocusFrames.searchlights(axis, neighborhoods)
    yield frame
