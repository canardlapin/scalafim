package scalafim.atlas

sealed trait SpaceKind

object SpaceKind:
  sealed trait AnySpace extends SpaceKind
  sealed trait Volume extends AnySpace
  sealed trait Surface extends AnySpace
  sealed trait Unknown extends AnySpace

enum SpaceKindTag:
  case Volume, Surface, Unknown

type SpaceId = SpaceIdOf[SpaceKind.AnySpace]
type AnySpaceId = SpaceIdOf[SpaceKind.AnySpace]
type VolumeSpaceId = SpaceIdOf[SpaceKind.Volume]
type SurfaceSpaceId = SpaceIdOf[SpaceKind.Surface]
type UnknownSpaceId = SpaceIdOf[SpaceKind.Unknown]
type VolumeOrUnknownSpaceId = SpaceIdOf[SpaceKind.Volume | SpaceKind.Unknown]
type SurfaceOrUnknownSpaceId = SpaceIdOf[SpaceKind.Surface | SpaceKind.Unknown]

final case class SpaceIdOf[+K <: SpaceKind.AnySpace](value: String):
  require(value.trim.nonEmpty, "space id must be non-empty")

  override def toString: String = value

object SpaceId:
  def apply(value: String): SpaceId =
    SpaceIdOf[SpaceKind.AnySpace](value)

  val MNI152: VolumeSpaceId = volume("MNI152")
  val MNI305: VolumeSpaceId = volume("MNI305")
  val MNI152NLin6Asym: VolumeSpaceId = volume("MNI152NLin6Asym")
  val MNI152NLin2009cAsym: VolumeSpaceId = volume("MNI152NLin2009cAsym")
  val FsAverage: SurfaceSpaceId = surface("fsaverage")
  val FsAverage5: SurfaceSpaceId = surface("fsaverage5")
  val FsAverage6: SurfaceSpaceId = surface("fsaverage6")
  val FsLR32k: SurfaceSpaceId = surface("fsLR_32k")
  val Custom: UnknownSpaceId = unknown("custom")
  val Unknown: UnknownSpaceId = unknown("unknown")

  def volume(value: String): VolumeSpaceId =
    SpaceIdOf[SpaceKind.Volume](value)

  def surface(value: String): SurfaceSpaceId =
    SpaceIdOf[SpaceKind.Surface](value)

  def unknown(value: String): UnknownSpaceId =
    SpaceIdOf[SpaceKind.Unknown](value)

  def normalize(space: AnySpaceId): AnySpaceId =
    normalize(space.value)

  def normalize(space: String): AnySpaceId =
    val key = space.trim.toLowerCase.replace("-", "").replace(" ", "")
    key match
      case "mni152" => MNI152
      case "mni305" => MNI305
      case "mni152nlin6asym" => MNI152NLin6Asym
      case "mni152nlin2009casym" => MNI152NLin2009cAsym
      case "fsaverage" => FsAverage
      case "fsaverage5" => FsAverage5
      case "fsaverage6" => FsAverage6
      case "fslr" | "fslr32k" | "fslr_32k" => FsLR32k
      case "" => Unknown
      case other => unknown(space.trim)

  def kind(space: AnySpaceId): SpaceKindTag =
    normalize(space) match
      case MNI152 | MNI305 | MNI152NLin6Asym | MNI152NLin2009cAsym =>
        SpaceKindTag.Volume
      case FsAverage | FsAverage5 | FsAverage6 | FsLR32k =>
        SpaceKindTag.Surface
      case _ =>
        SpaceKindTag.Unknown

  def asVolume(space: AnySpaceId): Either[AtlasError, VolumeSpaceId] =
    normalize(space) match
      case MNI152 => Right(MNI152)
      case MNI305 => Right(MNI305)
      case MNI152NLin6Asym => Right(MNI152NLin6Asym)
      case MNI152NLin2009cAsym => Right(MNI152NLin2009cAsym)
      case other =>
        Left(AtlasError.SpaceKindMismatch(other, SpaceKindTag.Volume, kind(other)))

  def asSurface(space: AnySpaceId): Either[AtlasError, SurfaceSpaceId] =
    normalize(space) match
      case FsAverage => Right(FsAverage)
      case FsAverage5 => Right(FsAverage5)
      case FsAverage6 => Right(FsAverage6)
      case FsLR32k => Right(FsLR32k)
      case other =>
        Left(AtlasError.SpaceKindMismatch(other, SpaceKindTag.Surface, kind(other)))

enum AtlasRepresentation:
  case Volume, Surface, Derived

sealed trait AtlasRepresentationKind

object AtlasRepresentationKind:
  sealed trait AnyRepresentation extends AtlasRepresentationKind
  sealed trait Volume extends AnyRepresentation
  sealed trait Surface extends AnyRepresentation
  sealed trait Derived extends AnyRepresentation

type AtlasRef = AtlasRefOf[AtlasRepresentationKind.AnyRepresentation]
type AnyAtlasRef = AtlasRefOf[AtlasRepresentationKind.AnyRepresentation]
type VolumeAtlasRef = AtlasRefOf[AtlasRepresentationKind.Volume]
type SurfaceAtlasRef = AtlasRefOf[AtlasRepresentationKind.Surface]
type DerivedAtlasRef = AtlasRefOf[AtlasRepresentationKind.Derived]

enum Confidence:
  case Exact, High, Approximate, Uncertain

final case class AtlasArtifact(
  role: ArtifactRole,
  sourceName: String,
  sourceRef: String,
  sourceUrl: Option[String] = None,
  citationDoi: Option[String] = None,
  license: Option[String] = None,
  sha256: Option[String] = None,
  notes: Option[String] = None
):
  require(sourceName.trim.nonEmpty, "artifact sourceName must be non-empty")
  require(sourceRef.trim.nonEmpty, "artifact sourceRef must be non-empty")

  def roleLegacy: String =
    role.legacy

object AtlasArtifact:
  def fromLegacyRole(
    role: String,
    sourceName: String,
    sourceRef: String,
    sourceUrl: Option[String] = None,
    citationDoi: Option[String] = None,
    license: Option[String] = None,
    sha256: Option[String] = None,
    notes: Option[String] = None
  ): AtlasArtifact =
    AtlasArtifact(
      role = ArtifactRole.fromLegacy(role),
      sourceName = sourceName,
      sourceRef = sourceRef,
      sourceUrl = sourceUrl,
      citationDoi = citationDoi,
      license = license,
      sha256 = sha256,
      notes = notes
    )

final case class AtlasHistoryStep(
  action: String,
  fromTemplateSpace: AnySpaceId,
  toTemplateSpace: AnySpaceId,
  fromCoordSpace: AnySpaceId,
  toCoordSpace: AnySpaceId,
  status: TransformStatus,
  confidence: Confidence,
  details: String
):
  require(action.trim.nonEmpty, "history action must be non-empty")

final case class AtlasRefOf[+R <: AtlasRepresentationKind.AnyRepresentation](
  family: String,
  model: String,
  representation: AtlasRepresentation,
  templateSpace: AnySpaceId,
  coordSpace: AnySpaceId,
  resolution: Option[String] = None,
  density: Option[String] = None,
  provenance: Option[String] = None,
  source: Option[String] = None,
  lineage: Option[String] = None,
  confidence: Confidence = Confidence.Uncertain,
  notes: Option[String] = None,
  artifacts: Vector[AtlasArtifact] = Vector.empty,
  history: Vector[AtlasHistoryStep] = Vector.empty,
  parcelVariant: Option[String] = None
):
  require(family.trim.nonEmpty, "atlas family must be non-empty")
  require(model.trim.nonEmpty, "atlas model must be non-empty")
  parcelVariant.foreach(value =>
    require(value.trim.nonEmpty, "atlas parcel variant must be non-empty")
  )

  def name: String = s"$family:$model"

  def toProvenance(regions: RegionIndex): AtlasProvenance =
    AtlasProvenance.fromRef(this, regions)

object AtlasRef:
  def apply(
    family: String,
    model: String,
    representation: AtlasRepresentation,
    templateSpace: AnySpaceId,
    coordSpace: AnySpaceId,
    resolution: Option[String] = None,
    density: Option[String] = None,
    provenance: Option[String] = None,
    source: Option[String] = None,
    lineage: Option[String] = None,
    confidence: Confidence = Confidence.Uncertain,
    notes: Option[String] = None,
    artifacts: Vector[AtlasArtifact] = Vector.empty,
    history: Vector[AtlasHistoryStep] = Vector.empty,
    parcelVariant: Option[String] = None
  ): AtlasRef =
    AtlasRefOf[AtlasRepresentationKind.AnyRepresentation](
      family = family,
      model = model,
      representation = representation,
      templateSpace = templateSpace,
      coordSpace = coordSpace,
      resolution = resolution,
      density = density,
      provenance = provenance,
      source = source,
      lineage = lineage,
      confidence = confidence,
      notes = notes,
      artifacts = artifacts,
      history = history,
      parcelVariant = parcelVariant
    )

  def volume(
    family: String,
    model: String,
    templateSpace: VolumeOrUnknownSpaceId,
    coordSpace: VolumeOrUnknownSpaceId,
    resolution: Option[String] = None,
    provenance: Option[String] = None,
    source: Option[String] = None,
    lineage: Option[String] = None,
    confidence: Confidence = Confidence.Uncertain,
    notes: Option[String] = None,
    artifacts: Vector[AtlasArtifact] = Vector.empty,
    history: Vector[AtlasHistoryStep] = Vector.empty,
    parcelVariant: Option[String] = None
  ): VolumeAtlasRef =
    AtlasRefOf[AtlasRepresentationKind.Volume](
      family = family,
      model = model,
      representation = AtlasRepresentation.Volume,
      templateSpace = templateSpace,
      coordSpace = coordSpace,
      resolution = resolution,
      provenance = provenance,
      source = source,
      lineage = lineage,
      confidence = confidence,
      notes = notes,
      artifacts = artifacts,
      history = history,
      parcelVariant = parcelVariant
    )

  def surface(
    family: String,
    model: String,
    templateSpace: SurfaceOrUnknownSpaceId,
    coordSpace: SurfaceOrUnknownSpaceId,
    density: Option[String] = None,
    provenance: Option[String] = None,
    source: Option[String] = None,
    lineage: Option[String] = None,
    confidence: Confidence = Confidence.Uncertain,
    notes: Option[String] = None,
    artifacts: Vector[AtlasArtifact] = Vector.empty,
    history: Vector[AtlasHistoryStep] = Vector.empty,
    parcelVariant: Option[String] = None
  ): SurfaceAtlasRef =
    AtlasRefOf[AtlasRepresentationKind.Surface](
      family = family,
      model = model,
      representation = AtlasRepresentation.Surface,
      templateSpace = templateSpace,
      coordSpace = coordSpace,
      density = density,
      provenance = provenance,
      source = source,
      lineage = lineage,
      confidence = confidence,
      notes = notes,
      artifacts = artifacts,
      history = history,
      parcelVariant = parcelVariant
    )

  def derived(
    family: String,
    model: String,
    templateSpace: AnySpaceId,
    coordSpace: AnySpaceId,
    resolution: Option[String] = None,
    density: Option[String] = None,
    provenance: Option[String] = None,
    source: Option[String] = None,
    lineage: Option[String] = None,
    confidence: Confidence = Confidence.Uncertain,
    notes: Option[String] = None,
    artifacts: Vector[AtlasArtifact] = Vector.empty,
    history: Vector[AtlasHistoryStep] = Vector.empty,
    parcelVariant: Option[String] = None
  ): DerivedAtlasRef =
    AtlasRefOf[AtlasRepresentationKind.Derived](
      family = family,
      model = model,
      representation = AtlasRepresentation.Derived,
      templateSpace = templateSpace,
      coordSpace = coordSpace,
      resolution = resolution,
      density = density,
      provenance = provenance,
      source = source,
      lineage = lineage,
      confidence = confidence,
      notes = notes,
      artifacts = artifacts,
      history = history,
      parcelVariant = parcelVariant
    )
