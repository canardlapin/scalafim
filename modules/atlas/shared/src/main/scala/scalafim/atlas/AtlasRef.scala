package scalafim.atlas

enum SpaceKindTag:
  case Volume, Surface, Unknown

type VolumeSpaceId = SpaceId.Volume
type SurfaceSpaceId = SpaceId.Surface
type UnknownSpaceId = SpaceId.UnknownSpace
type VolumeOrUnknownSpaceId = VolumeSpaceId | UnknownSpaceId
type SurfaceOrUnknownSpaceId = SurfaceSpaceId | UnknownSpaceId

/** A classified space identifier. Only checked factories can construct one. */
sealed abstract class SpaceId private (val value: String, val kind: SpaceKindTag):
  final override def toString: String = value

  final override def equals(other: Any): Boolean =
    other match
      case that: SpaceId => value == that.value && kind == that.kind
      case _ => false

  final override def hashCode(): Int = (value, kind).hashCode

object SpaceId:
  final class Volume private[SpaceId] (value: String) extends SpaceId(value, SpaceKindTag.Volume)
  final class Surface private[SpaceId] (value: String) extends SpaceId(value, SpaceKindTag.Surface)
  final class UnknownSpace private[SpaceId] (value: String) extends SpaceId(value, SpaceKindTag.Unknown)

  val MNI152: VolumeSpaceId = new Volume("MNI152")
  val MNI305: VolumeSpaceId = new Volume("MNI305")
  val MNI152NLin6Asym: VolumeSpaceId = new Volume("MNI152NLin6Asym")
  val MNI152NLin2009cAsym: VolumeSpaceId = new Volume("MNI152NLin2009cAsym")
  val FsAverage: SurfaceSpaceId = new Surface("fsaverage")
  val FsAverage5: SurfaceSpaceId = new Surface("fsaverage5")
  val FsAverage6: SurfaceSpaceId = new Surface("fsaverage6")
  val FsLR32k: SurfaceSpaceId = new Surface("fsLR_32k")
  val FsLR59k: SurfaceSpaceId = new Surface("fsLR_59k")
  val FsLR164k: SurfaceSpaceId = new Surface("fsLR_164k")
  val Custom: UnknownSpaceId = new UnknownSpace("custom")
  val Unknown: UnknownSpaceId = new UnknownSpace("unknown")

  def from(value: String): Either[AtlasError, SpaceId] =
    if value.trim.isEmpty then Left(AtlasError.InvalidSpaceId("space id must be non-empty"))
    else Right(known(value).getOrElse(new UnknownSpace(value.trim)))

  def apply(value: String): SpaceId =
    from(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  def volumeFrom(value: String): Either[AtlasError, VolumeSpaceId] =
    from(value).flatMap:
      case space: Volume => Right(space)
      case space: UnknownSpace => Right(new Volume(space.value))
      case space => Left(AtlasError.SpaceKindMismatch(space, SpaceKindTag.Volume, space.kind))

  def surfaceFrom(value: String): Either[AtlasError, SurfaceSpaceId] =
    from(value).flatMap:
      case space: Surface => Right(space)
      case space: UnknownSpace => Right(new Surface(space.value))
      case space => Left(AtlasError.SpaceKindMismatch(space, SpaceKindTag.Surface, space.kind))

  def unknownFrom(value: String): Either[AtlasError, UnknownSpaceId] =
    from(value).flatMap:
      case space: UnknownSpace => Right(space)
      case space => Left(AtlasError.SpaceKindMismatch(space, SpaceKindTag.Unknown, space.kind))

  def volume(value: String): VolumeSpaceId =
    volumeFrom(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  def surface(value: String): SurfaceSpaceId =
    surfaceFrom(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  def unknown(value: String): UnknownSpaceId =
    unknownFrom(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  def normalize(space: SpaceId): SpaceId =
    known(space.value).filter(_.kind == space.kind).getOrElse(space)

  def normalize(value: String): SpaceId =
    if value.trim.isEmpty then Unknown else apply(value)

  def kind(space: SpaceId): SpaceKindTag = space.kind

  def asVolume(space: SpaceId): Either[AtlasError, VolumeSpaceId] =
    space match
      case value: Volume => Right(value)
      case other => Left(AtlasError.SpaceKindMismatch(other, SpaceKindTag.Volume, other.kind))

  def asSurface(space: SpaceId): Either[AtlasError, SurfaceSpaceId] =
    space match
      case value: Surface => Right(value)
      case other => Left(AtlasError.SpaceKindMismatch(other, SpaceKindTag.Surface, other.kind))

  private def known(value: String): Option[SpaceId] =
    value.trim.toLowerCase.replace("-", "").replace(" ", "") match
      case "mni152" => Some(MNI152)
      case "mni305" => Some(MNI305)
      case "mni152nlin6asym" => Some(MNI152NLin6Asym)
      case "mni152nlin2009casym" => Some(MNI152NLin2009cAsym)
      case "fsaverage" => Some(FsAverage)
      case "fsaverage5" => Some(FsAverage5)
      case "fsaverage6" => Some(FsAverage6)
      case "fslr" | "fslr32k" | "fslr_32k" => Some(FsLR32k)
      case "fslr59k" | "fslr_59k" => Some(FsLR59k)
      case "fslr164k" | "fslr_164k" => Some(FsLR164k)
      case "custom" => Some(Custom)
      case "unknown" => Some(Unknown)
      case _ => None

enum AtlasRepresentation:
  case Volume, Surface, Derived

type VolumeAtlasRef = AtlasRef.Volume
type SurfaceAtlasRef = AtlasRef.Surface
type DerivedAtlasRef = AtlasRef.Derived

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

final case class AtlasHistoryStep(
  action: String,
  fromTemplateSpace: SpaceId,
  toTemplateSpace: SpaceId,
  fromCoordSpace: SpaceId,
  toCoordSpace: SpaceId,
  status: TransformStatus,
  confidence: Confidence,
  details: String
):
  require(action.trim.nonEmpty, "history action must be non-empty")

final case class AtlasDetails(
  family: String,
  model: String,
  resolution: Option[String] = None,
  density: Option[String] = None,
  provenance: Option[String] = None,
  source: Option[String] = None,
  lineage: Option[String] = None,
  confidence: Confidence = Confidence.Uncertain,
  notes: Option[String] = None,
  artifacts: Vector[AtlasArtifact] = Vector.empty,
  history: Vector[AtlasHistoryStep] = Vector.empty,
  parcelVariant: Option[String] = None,
  parcelIdentity: ParcelIdentity = ParcelIdentity.SourceLabels
):
  require(family.trim.nonEmpty, "atlas family must be non-empty")
  require(model.trim.nonEmpty, "atlas model must be non-empty")
  parcelVariant.foreach(value => require(value.trim.nonEmpty, "atlas parcel variant must be non-empty"))

/** Representation and space kinds are fixed by the constructor and cannot be copied away. */
sealed abstract class AtlasRef private (val details: AtlasDetails):
  def representation: AtlasRepresentation
  def templateSpace: SpaceId
  def coordSpace: SpaceId
  def withDetails(f: AtlasDetails => AtlasDetails): AtlasRef

  final def family: String = details.family
  final def model: String = details.model
  final def resolution: Option[String] = details.resolution
  final def density: Option[String] = details.density
  final def provenance: Option[String] = details.provenance
  final def source: Option[String] = details.source
  final def lineage: Option[String] = details.lineage
  final def confidence: Confidence = details.confidence
  final def notes: Option[String] = details.notes
  final def artifacts: Vector[AtlasArtifact] = details.artifacts
  final def history: Vector[AtlasHistoryStep] = details.history
  final def parcelVariant: Option[String] = details.parcelVariant
  final def parcelIdentity: ParcelIdentity = details.parcelIdentity

  final def name: String = s"$family:$model"

  final def toProvenance(regions: RegionIndex): AtlasProvenance =
    AtlasProvenance.fromRef(this, regions)

  final override def equals(other: Any): Boolean =
    other match
      case that: AtlasRef =>
        details == that.details && representation == that.representation &&
          templateSpace == that.templateSpace && coordSpace == that.coordSpace
      case _ => false

  final override def hashCode(): Int =
    (details, representation, templateSpace, coordSpace).hashCode

  final override def toString: String = s"AtlasRef($name, $representation, $coordSpace)"

object AtlasRef:
  final class Volume private[AtlasRef] (
      details: AtlasDetails,
      val templateSpace: VolumeOrUnknownSpaceId,
      val coordSpace: VolumeOrUnknownSpaceId
  ) extends AtlasRef(details):
    val representation: AtlasRepresentation = AtlasRepresentation.Volume
    def withDetails(f: AtlasDetails => AtlasDetails): Volume =
      new Volume(f(details), templateSpace, coordSpace)

  final class Surface private[AtlasRef] (
      details: AtlasDetails,
      val templateSpace: SurfaceOrUnknownSpaceId,
      val coordSpace: SurfaceOrUnknownSpaceId
  ) extends AtlasRef(details):
    val representation: AtlasRepresentation = AtlasRepresentation.Surface
    def withDetails(f: AtlasDetails => AtlasDetails): Surface =
      new Surface(f(details), templateSpace, coordSpace)

  final class Derived private[AtlasRef] (
      details: AtlasDetails,
      val templateSpace: SpaceId,
      val coordSpace: SpaceId
  ) extends AtlasRef(details):
    val representation: AtlasRepresentation = AtlasRepresentation.Derived
    def withDetails(f: AtlasDetails => AtlasDetails): Derived =
      new Derived(f(details), templateSpace, coordSpace)

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
    parcelVariant: Option[String] = None,
    parcelIdentity: ParcelIdentity = ParcelIdentity.SourceLabels
  ): VolumeAtlasRef =
    new Volume(
      AtlasDetails(
        family = family,
        model = model,
        resolution = resolution,
        provenance = provenance,
        source = source,
        lineage = lineage,
        confidence = confidence,
        notes = notes,
        artifacts = artifacts,
        history = history,
        parcelVariant = parcelVariant,
        parcelIdentity = parcelIdentity
      ),
      templateSpace,
      coordSpace
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
    parcelVariant: Option[String] = None,
    parcelIdentity: ParcelIdentity = ParcelIdentity.SourceLabels
  ): SurfaceAtlasRef =
    new Surface(
      AtlasDetails(
        family = family,
        model = model,
        density = density,
        provenance = provenance,
        source = source,
        lineage = lineage,
        confidence = confidence,
        notes = notes,
        artifacts = artifacts,
        history = history,
        parcelVariant = parcelVariant,
        parcelIdentity = parcelIdentity
      ),
      templateSpace,
      coordSpace
    )

  def derived(
    family: String,
    model: String,
    templateSpace: SpaceId,
    coordSpace: SpaceId,
    resolution: Option[String] = None,
    density: Option[String] = None,
    provenance: Option[String] = None,
    source: Option[String] = None,
    lineage: Option[String] = None,
    confidence: Confidence = Confidence.Uncertain,
    notes: Option[String] = None,
    artifacts: Vector[AtlasArtifact] = Vector.empty,
    history: Vector[AtlasHistoryStep] = Vector.empty,
    parcelVariant: Option[String] = None,
    parcelIdentity: ParcelIdentity = ParcelIdentity.SourceLabels
  ): DerivedAtlasRef =
    new Derived(
      AtlasDetails(
        family = family,
        model = model,
        resolution = resolution,
        density = density,
        provenance = provenance,
        source = source,
        lineage = lineage,
        confidence = confidence,
        notes = notes,
        artifacts = artifacts,
        history = history,
        parcelVariant = parcelVariant,
        parcelIdentity = parcelIdentity
      ),
      templateSpace,
      coordSpace
    )

  /** Admit a dynamically described reference at a parsing boundary. */
  def checked(
      details: AtlasDetails,
      representation: AtlasRepresentation,
      templateSpace: SpaceId,
      coordSpace: SpaceId
  ): Either[AtlasError, AtlasRef] =
    representation match
      case AtlasRepresentation.Volume =>
        for
          template <- volumeSpace(templateSpace)
          coord <- volumeSpace(coordSpace)
        yield new Volume(details, template, coord)
      case AtlasRepresentation.Surface =>
        for
          template <- surfaceSpace(templateSpace)
          coord <- surfaceSpace(coordSpace)
        yield new Surface(details, template, coord)
      case AtlasRepresentation.Derived =>
        Right(new Derived(details, templateSpace, coordSpace))

  private def volumeSpace(space: SpaceId): Either[AtlasError, VolumeOrUnknownSpaceId] =
    space match
      case value: SpaceId.Volume => Right(value)
      case value: SpaceId.UnknownSpace => Right(value)
      case other => Left(AtlasError.SpaceKindMismatch(other, SpaceKindTag.Volume, other.kind))

  private def surfaceSpace(space: SpaceId): Either[AtlasError, SurfaceOrUnknownSpaceId] =
    space match
      case value: SpaceId.Surface => Right(value)
      case value: SpaceId.UnknownSpace => Right(value)
      case other => Left(AtlasError.SpaceKindMismatch(other, SpaceKindTag.Surface, other.kind))
