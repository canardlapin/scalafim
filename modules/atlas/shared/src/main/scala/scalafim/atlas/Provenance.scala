package scalafim.atlas

import scala.util.Try

final case class NonEmptyVector[+A] private (head: A, tail: Vector[A]):
  def toVector: Vector[A] =
    head +: tail

  def length: Int =
    1 + tail.length

  def map[B](f: A => B): NonEmptyVector[B] =
    NonEmptyVector(f(head), tail.map(f))

  def :+[B >: A](value: B): NonEmptyVector[B] =
    NonEmptyVector(head, tail :+ value)

object NonEmptyVector:
  def of[A](head: A, tail: A*): NonEmptyVector[A] =
    NonEmptyVector(head, tail.toVector)

  def fromVector[A](values: Vector[A]): Option[NonEmptyVector[A]] =
    values.headOption.map(head => NonEmptyVector(head, values.tail))

  def unsafe[A](values: Vector[A]): NonEmptyVector[A] =
    fromVector(values).getOrElse(throw new IllegalArgumentException("NonEmptyVector requires at least one value"))

final case class AtlasRelease(
  label: String,
  year: Option[Int] = None,
  version: Option[String] = None,
  commit: Option[String] = None,
  date: Option[String] = None
):
  require(label.trim.nonEmpty, "atlas release label must be non-empty")
  year.foreach(y => require(y >= 1900 && y <= 3000, "atlas release year must be plausible"))
  version.foreach(v => require(v.trim.nonEmpty, "atlas release version must be non-empty"))
  commit.foreach(c => require(c.trim.nonEmpty, "atlas release commit must be non-empty"))
  date.foreach(d => require(d.trim.nonEmpty, "atlas release date must be non-empty"))

  def summary: String =
    val details =
      Vector(
        year.map(y => s"year=$y"),
        version.map(v => s"version=$v"),
        Some(commit.fold("commit=unknown")(c => s"commit=$c")),
        date.map(d => s"date=$d")
      ).flatten
    if details.isEmpty then label else s"$label (${details.mkString(", ")})"

final case class AtlasIdentity(
  family: String,
  model: String,
  variant: Option[String] = None,
  release: Option[AtlasRelease] = None
):
  require(family.trim.nonEmpty, "atlas identity family must be non-empty")
  require(model.trim.nonEmpty, "atlas identity model must be non-empty")
  variant.foreach(v => require(v.trim.nonEmpty, "atlas identity variant must be non-empty"))

object AtlasIdentity:
  def fromRef(ref: AtlasRef): AtlasIdentity =
    val variant =
      Vector(ref.resolution, ref.density, ref.source).flatten match
        case Vector() => None
        case parts => Some(parts.mkString("/"))
    AtlasIdentity(ref.family, ref.model, variant, standardRelease(ref))

  private def standardRelease(ref: AtlasRef): Option[AtlasRelease] =
    AtlasRegistry.normalize(ref.family) match
      case "schaefer" =>
        Some(AtlasRelease("Schaefer2018 CBIG release", year = Some(2018)))
      case "glasser" =>
        Some(AtlasRelease("HCP-MMP1.0", year = Some(2016), version = Some("1.0")))
      case "brainnetome" =>
        Some(AtlasRelease("Brainnetome Atlas 246", year = Some(2016)))
      case "aseg" =>
        Some(AtlasRelease("neuroatlas bundled FreeSurfer ASEG extdata"))
      case _ =>
        None

final case class VoxelSize(xMm: Double, yMm: Double, zMm: Double):
  require(xMm > 0.0 && yMm > 0.0 && zMm > 0.0, "voxel size must be positive")

  def isIsotropic: Boolean =
    math.abs(xMm - yMm) < 1e-9 && math.abs(xMm - zMm) < 1e-9

  def label: String =
    if isIsotropic then s"${VoxelSize.format(xMm)}mm"
    else s"${VoxelSize.format(xMm)}x${VoxelSize.format(yMm)}x${VoxelSize.format(zMm)}mm"

object VoxelSize:
  def isotropic(mm: Double): VoxelSize =
    VoxelSize(mm, mm, mm)

  def parse(label: String): Option[VoxelSize] =
    val cleaned = label.trim.toLowerCase.stripSuffix("mm")
    if cleaned.contains("x") then
      cleaned.split("x").toVector match
        case Vector(x, y, z) =>
          for
            xd <- Try(x.toDouble).toOption
            yd <- Try(y.toDouble).toOption
            zd <- Try(z.toDouble).toOption
          yield VoxelSize(xd, yd, zd)
        case _ => None
    else Try(cleaned.toDouble).toOption.map(isotropic)

  private def format(value: Double): String =
    if value.isWhole then value.toInt.toString else value.toString

final case class GridDims(values: Vector[Int]):
  require(values.nonEmpty, "grid dimensions must be non-empty")
  require(values.forall(_ > 0), "grid dimensions must be positive")

final case class SurfaceDensity(label: String, verticesPerHemisphere: Option[Int] = None):
  require(label.trim.nonEmpty, "surface density label must be non-empty")
  verticesPerHemisphere.foreach(v => require(v > 0, "surface vertices per hemisphere must be positive"))

enum HemisphereCoverage:
  case LeftOnly, RightOnly, Bilateral, Unknown

enum SpatialSupport:
  case Volume(
    templateSpace: AnySpaceId,
    coordSpace: AnySpaceId,
    resolution: Option[VoxelSize],
    dimensions: Option[GridDims] = None
  )
  case Surface(
    templateSpace: AnySpaceId,
    density: SurfaceDensity,
    coverage: HemisphereCoverage = HemisphereCoverage.Bilateral
  )
  case Derived(templateSpace: AnySpaceId, coordSpace: AnySpaceId)

object SpatialSupport:
  def fromRef(ref: AtlasRef): SpatialSupport =
    ref.representation match
      case AtlasRepresentation.Volume =>
        SpatialSupport.Volume(
          ref.templateSpace,
          ref.coordSpace,
          ref.resolution.flatMap(VoxelSize.parse)
        )
      case AtlasRepresentation.Surface =>
        SpatialSupport.Surface(
          ref.templateSpace,
          SurfaceDensity(ref.density.getOrElse("unknown"))
        )
      case AtlasRepresentation.Derived =>
        SpatialSupport.Derived(ref.templateSpace, ref.coordSpace)

enum ArtifactRole(val legacy: String):
  case Descriptor extends ArtifactRole("descriptor")
  case ParcellationVolume extends ArtifactRole("parcellation_volume")
  case SurfaceAnnotation extends ArtifactRole("surface_annotation")
  case LabelTable extends ArtifactRole("label_table")
  case NetworkTable extends ArtifactRole("network_table")
  case Transform extends ArtifactRole("transform")
  case Geometry extends ArtifactRole("geometry")
  case Documentation extends ArtifactRole("documentation")
  case Other extends ArtifactRole("other")

object ArtifactRole:
  def fromLegacy(role: String): ArtifactRole =
    AtlasRegistry.normalize(role) match
      case "descriptor" => Descriptor
      case "parcellationvolume" | "volume" | "labelmap" => ParcellationVolume
      case "surfacelabels" | "surfaceannotation" | "annotation" => SurfaceAnnotation
      case "labeltable" | "lut" => LabelTable
      case "networktable" => NetworkTable
      case "transform" | "xfm" => Transform
      case "geometry" | "surfacegeometry" => Geometry
      case "documentation" | "docs" => Documentation
      case _ => Other

final case class Digest(algorithm: String, value: String):
  require(algorithm.trim.nonEmpty, "digest algorithm must be non-empty")
  require(value.trim.nonEmpty, "digest value must be non-empty")

object Digest:
  def sha256(value: String): Digest =
    Digest("sha256", value)

enum LicenseInfo:
  case Known(id: String)
  case Restricted(label: String, termsUri: Option[String] = None)
  case Unspecified(reason: String)
  case Missing

  def isAccounted: Boolean =
    this match
      case Missing => false
      case _ => true

  def legacyString: Option[String] =
    this match
      case Known(id) => Some(id)
      case Restricted(label, termsUri) =>
        Some(termsUri.fold(s"Restricted: $label")(uri => s"Restricted: $label ($uri)"))
      case Unspecified(reason) => Some(s"Unspecified: $reason")
      case Missing => None

  def summary: String =
    this match
      case Known(id) => id
      case Restricted(label, Some(uri)) => s"restricted: $label ($uri)"
      case Restricted(label, None) => s"restricted: $label"
      case Unspecified(reason) => s"unspecified: $reason"
      case Missing => "missing"

object LicenseInfo:
  def fromLegacy(value: Option[String]): LicenseInfo =
    value.map(_.trim).filter(_.nonEmpty) match
      case None => Missing
      case Some(text) if text.toLowerCase.startsWith("restricted:") =>
        Restricted(text.drop("restricted:".length).trim)
      case Some(text) if text.toLowerCase.startsWith("unspecified:") =>
        Unspecified(text.drop("unspecified:".length).trim)
      case Some(text) =>
        Known(text)

final case class SourceArtifact(
  id: String,
  role: ArtifactRole,
  sourceName: String,
  sourceRef: String,
  sourceUri: Option[String] = None,
  localPath: Option[String] = None,
  citationDoi: Option[String] = None,
  license: Option[String] = None,
  licenseInfo: LicenseInfo = LicenseInfo.Missing,
  digest: Option[Digest] = None,
  notes: Option[String] = None
):
  require(id.trim.nonEmpty, "source artifact id must be non-empty")
  require(sourceName.trim.nonEmpty, "source artifact sourceName must be non-empty")
  require(sourceRef.trim.nonEmpty, "source artifact sourceRef must be non-empty")

  def licenseStatus: LicenseInfo =
    licenseInfo match
      case LicenseInfo.Missing => LicenseInfo.fromLegacy(license)
      case other => other

  def withResolvedFile(path: String, fileDigest: Digest): SourceArtifact =
    require(path.trim.nonEmpty, "resolved artifact path must be non-empty")
    copy(localPath = Some(path), digest = Some(fileDigest))

  def toAtlasArtifact: AtlasArtifact =
    AtlasArtifact(
      role = role,
      sourceName = sourceName,
      sourceRef = sourceRef,
      sourceUrl = sourceUri,
      citationDoi = citationDoi,
      license = license.orElse(licenseStatus.legacyString),
      sha256 = digest.filter(_.algorithm == "sha256").map(_.value),
      notes = notes
    )

object SourceArtifact:
  def fromAtlasArtifact(artifact: AtlasArtifact): SourceArtifact =
    val role = artifact.role
    SourceArtifact(
      id = stableId(role.legacy, artifact.sourceRef),
      role = role,
      sourceName = artifact.sourceName,
      sourceRef = artifact.sourceRef,
      sourceUri = artifact.sourceUrl,
      citationDoi = artifact.citationDoi,
      license = artifact.license,
      licenseInfo = LicenseInfo.fromLegacy(artifact.license),
      digest = artifact.sha256.map(Digest.sha256),
      notes = artifact.notes
    )

  def descriptor(ref: AtlasRef): SourceArtifact =
    SourceArtifact(
      id = stableId("descriptor", ref.name),
      role = ArtifactRole.Descriptor,
      sourceName = ref.provenance.getOrElse(ref.family),
      sourceRef = ref.source.getOrElse(ref.model),
      licenseInfo = LicenseInfo.Unspecified("descriptor-only provenance; consult upstream source terms"),
      notes = ref.notes.orElse(ref.lineage)
    )

  private def stableId(parts: String*): String =
    val raw = parts.mkString(":")
    val normalized = raw.trim.toLowerCase.replaceAll("[^a-z0-9]+", "_").stripPrefix("_").stripSuffix("_")
    if normalized.nonEmpty then normalized else "artifact"

enum LabelEncoding:
  case VolumeIntegerLabels, SurfaceIntegerLabels, DerivedLabels

final case class LabelSchema(
  encoding: LabelEncoding,
  regionIds: Vector[RegionId],
  background: Option[Int] = Some(0),
  labelTableArtifactId: Option[String] = None,
  attributes: Map[String, String] = Map.empty
):
  require(regionIds.nonEmpty, "label schema must contain at least one region id")
  require(regionIds.distinct.length == regionIds.length, "label schema region ids must be unique")
  background.foreach(v => require(v >= 0, "background label id must be non-negative"))
  attributes.keys.foreach(k => require(k.trim.nonEmpty, "label schema attribute keys must be non-empty"))

object LabelSchema:
  def fromRegions(ref: AtlasRef, regions: RegionIndex, artifacts: Vector[SourceArtifact] = Vector.empty): LabelSchema =
    val encoding =
      ref.representation match
        case AtlasRepresentation.Volume => LabelEncoding.VolumeIntegerLabels
        case AtlasRepresentation.Surface => LabelEncoding.SurfaceIntegerLabels
        case AtlasRepresentation.Derived => LabelEncoding.DerivedLabels
    LabelSchema(
      encoding = encoding,
      regionIds = regions.ids.sorted,
      labelTableArtifactId = artifacts.find(_.role == ArtifactRole.LabelTable).map(_.id)
    )

final case class LabelTableSchema(name: String, columns: Vector[String]):
  require(name.trim.nonEmpty, "label table schema name must be non-empty")
  require(columns.nonEmpty, "label table schema must contain columns")
  require(columns.forall(_.trim.nonEmpty), "label table schema columns must be non-empty")

enum DerivationStep:
  case DeclaredDescriptor(details: String)
  case Loaded(artifactId: String)
  case ParsedLabels(artifactId: String, schema: LabelTableSchema)
  case ValidatedLabels(regionIds: Vector[RegionId])
  case FilteredLabels(kept: Vector[RegionId], dropped: Vector[RegionId])
  case Resampled(from: AnySpaceId, to: AnySpaceId, kind: TransformKind, status: TransformStatus, confidence: Confidence)
  case ProjectedVolumeToSurface(from: AnySpaceId, to: AnySpaceId, sampling: SurfaceSamplingSpec, status: TransformStatus)
  case LegacyHistory(
    action: String,
    fromTemplateSpace: AnySpaceId,
    toTemplateSpace: AnySpaceId,
    fromCoordSpace: AnySpaceId,
    toCoordSpace: AnySpaceId,
    status: TransformStatus,
    confidence: Confidence,
    details: String
  )

object DerivationStep:
  def fromHistory(step: AtlasHistoryStep): DerivationStep =
    LegacyHistory(
      action = step.action,
      fromTemplateSpace = step.fromTemplateSpace,
      toTemplateSpace = step.toTemplateSpace,
      fromCoordSpace = step.fromCoordSpace,
      toCoordSpace = step.toCoordSpace,
      status = step.status,
      confidence = step.confidence,
      details = step.details
    )

final case class Citation(doi: Option[String] = None, text: Option[String] = None):
  require(doi.exists(_.trim.nonEmpty) || text.exists(_.trim.nonEmpty), "citation requires DOI or text")

object Citation:
  def doi(value: String): Citation =
    Citation(doi = Some(value))

enum ProvenanceIssue:
  case MissingDigest(role: ArtifactRole)
  case MissingLicense(sourceName: String)
  case UncertainSpace(space: AnySpaceId)
  case UncertainConfidence
  case NoLoadedArtifact

final case class AtlasProvenance(
  identity: AtlasIdentity,
  support: SpatialSupport,
  labels: LabelSchema,
  sources: NonEmptyVector[SourceArtifact],
  derivation: Vector[DerivationStep],
  citations: Vector[Citation],
  confidence: Confidence
):
  def sourceArtifacts: Vector[SourceArtifact] =
    sources.toVector

  def validate(strict: Boolean = false): Vector[ProvenanceIssue] =
    val issues = Vector.newBuilder[ProvenanceIssue]
    support match
      case SpatialSupport.Volume(template, coord, _, _) =>
        if template == SpaceId.Unknown then issues += ProvenanceIssue.UncertainSpace(template)
        if coord == SpaceId.Unknown then issues += ProvenanceIssue.UncertainSpace(coord)
      case SpatialSupport.Surface(template, density, _) =>
        if template == SpaceId.Unknown then issues += ProvenanceIssue.UncertainSpace(template)
        if density.label == "unknown" then issues += ProvenanceIssue.UncertainSpace(template)
      case SpatialSupport.Derived(template, coord) =>
        if template == SpaceId.Unknown then issues += ProvenanceIssue.UncertainSpace(template)
        if coord == SpaceId.Unknown then issues += ProvenanceIssue.UncertainSpace(coord)

    if confidence == Confidence.Uncertain then issues += ProvenanceIssue.UncertainConfidence
    if !sourceArtifacts.exists(_.role != ArtifactRole.Descriptor) then issues += ProvenanceIssue.NoLoadedArtifact

    if strict then
      sourceArtifacts.foreach { source =>
        if source.digest.isEmpty then issues += ProvenanceIssue.MissingDigest(source.role)
        if !source.licenseStatus.isAccounted then issues += ProvenanceIssue.MissingLicense(source.sourceName)
      }

    issues.result()

  def withLabels(next: LabelSchema): AtlasProvenance =
    copy(labels = next)

  def mapSourceArtifacts(f: SourceArtifact => SourceArtifact): AtlasProvenance =
    copy(sources = sources.map(f))

  def withSourceArtifacts(next: Vector[SourceArtifact]): AtlasProvenance =
    copy(sources = NonEmptyVector.unsafe(next))

  def withDerivationStep(step: DerivationStep): AtlasProvenance =
    copy(derivation = derivation :+ step)

  def supportSummary: String =
    support match
      case SpatialSupport.Volume(template, coord, resolution, dimensions) =>
        val dims = dimensions.map(d => s", dims=${d.values.mkString("x")}").getOrElse("")
        s"volume template=${template.value}, coord=${coord.value}, resolution=${resolution.map(_.label).getOrElse("unknown")}$dims"
      case SpatialSupport.Surface(template, density, coverage) =>
        val vertices = density.verticesPerHemisphere.map(v => s", vertices/hemi=$v").getOrElse("")
        s"surface template=${template.value}, density=${density.label}, coverage=$coverage$vertices"
      case SpatialSupport.Derived(template, coord) =>
        s"derived template=${template.value}, coord=${coord.value}"

  def labelSummary: String =
    val ids = labels.regionIds.map(_.value)
    val range =
      if ids.isEmpty then "none"
      else s"${ids.min}-${ids.max}"
    s"${labels.encoding}, regions=${ids.length}, ids=$range, background=${labels.background.map(_.toString).getOrElse("none")}"

  def sourceSummary: Vector[String] =
    sourceArtifacts.map { artifact =>
      val path = artifact.localPath.fold("path=unresolved")(p => s"path=$p")
      val digest = artifact.digest.fold("sha256=missing")(d => s"${d.algorithm}=${d.value}")
      val license = s"license=${artifact.licenseStatus.summary}"
      s"${artifact.role.legacy}: ${artifact.sourceName}/${artifact.sourceRef}; $path; $digest; $license"
    }

  def summaryLines: Vector[String] =
    Vector(
      s"atlas: ${identity.family}:${identity.model}${identity.variant.fold("")(v => s" [$v]")}",
      s"release: ${identity.release.map(_.summary).getOrElse("unspecified")}",
      s"support: $supportSummary",
      s"labels: $labelSummary",
      s"confidence: $confidence"
    ) ++ sourceSummary

  def summary: String =
    summaryLines.mkString(System.lineSeparator())

object AtlasProvenance:
  def fromRef(ref: AtlasRef, regions: RegionIndex): AtlasProvenance =
    val artifacts = ref.artifacts.map(SourceArtifact.fromAtlasArtifact)
    val sources = NonEmptyVector.fromVector(artifacts).getOrElse(NonEmptyVector.of(SourceArtifact.descriptor(ref)))
    val loaded = artifacts.map(artifact => DerivationStep.Loaded(artifact.id))
    val history = ref.history.map(DerivationStep.fromHistory)
    val derivation =
      if loaded.nonEmpty || history.nonEmpty then loaded ++ history
      else Vector(DerivationStep.DeclaredDescriptor(ref.lineage.orElse(ref.notes).getOrElse(ref.name)))
    val citations =
      artifacts.flatMap(_.citationDoi).distinct.map(Citation.doi)

    AtlasProvenance(
      identity = AtlasIdentity.fromRef(ref),
      support = SpatialSupport.fromRef(ref),
      labels = LabelSchema.fromRegions(ref, regions, artifacts),
      sources = sources,
      derivation = derivation,
      citations = citations,
      confidence = ref.confidence
    )

  def loaded(ref: AtlasRef, regions: RegionIndex, declaredRegionIds: Iterable[RegionId]): AtlasProvenance =
    val base = fromRef(ref, regions)
    val declared = declaredRegionIds.toVector.distinct.sorted
    if declared.isEmpty then base
    else
      val kept = regions.ids.sorted
      val keptSet = kept.toSet
      val dropped = declared.filterNot(keptSet)
      if dropped.nonEmpty then base.withDerivationStep(DerivationStep.FilteredLabels(kept, dropped))
      else base.withDerivationStep(DerivationStep.ValidatedLabels(kept))
