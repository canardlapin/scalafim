package scalafim.surface.reference

import scalafim.surface.*
import scalafim.surface.gifti.*

/** SHA-256 of an asset's exact bytes: 64 lowercase hexadecimal digits. */
opaque type AssetSha256 = String

object AssetSha256:
  def make(value: String): Either[ReferenceError, AssetSha256] =
    if value.length == 64 && value.forall(c => (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) then Right(value)
    else Left(ReferenceError.InvalidProvenance(s"sha256 must be 64 lowercase hexadecimal digits; got '$value'"))

  /** Digest of these bytes, computed with the portable SHA-256 on both platforms. */
  def of(bytes: Array[Byte]): AssetSha256 =
    SurfaceDigest.sha256Hex(bytes)

  extension (digest: AssetSha256)
    def value: String = digest

/** An asset identified by the SHA-256 of its exact bytes. */
sealed trait DeclaredAsset:
  def sha256: AssetSha256
  /** Short identifying name: the archive path or the declared asset name. */
  def label: String
  def display: String

/** Where an asset came from: its template, its path inside the TemplateFlow
  * archive (`tpl-<template>/...`), the catalog revision it was resolved
  * against, and the SHA-256 of its exact bytes.
  */
final case class AssetProvenance private (
  template: TemplateId,
  archivePath: String,
  catalogRevision: String,
  sha256: AssetSha256
) extends DeclaredAsset:
  def label: String = archivePath
  def display: String = s"$archivePath@$catalogRevision (sha256 ${sha256.value})"

object AssetProvenance:
  def make(template: TemplateId, archivePath: String, catalogRevision: String, sha256: String): Either[ReferenceError, AssetProvenance] =
    val prefix = s"tpl-${template.value}/"
    for
      _ <- Either.cond(archivePath.trim == archivePath && archivePath.startsWith(prefix) && archivePath.length > prefix.length,
        (), ReferenceError.InvalidProvenance(s"archive path must start with '$prefix' and name a file; got '$archivePath'"))
      _ <- Either.cond(!archivePath.contains('\\') && !archivePath.split('/').exists(p => p.isEmpty || p == "." || p == ".."),
        (), ReferenceError.InvalidProvenance(s"archive path must be a normalized relative path; got '$archivePath'"))
      _ <- Either.cond(catalogRevision.trim.nonEmpty && catalogRevision.trim == catalogRevision,
        (), ReferenceError.InvalidProvenance(s"catalog revision must be non-blank; got '$catalogRevision'"))
      digest <- AssetSha256.make(sha256)
    yield AssetProvenance(template, archivePath, catalogRevision, digest)

/** An asset outside the TemplateFlow archive, such as a group-result volume
  * or an exported analysis bundle, identified by name and digest.
  */
final case class DataAsset private (name: String, sha256: AssetSha256) extends DeclaredAsset:
  def label: String = name
  def display: String = s"$name (sha256 ${sha256.value})"

object DataAsset:
  def make(name: String, sha256: String): Either[ReferenceError, DataAsset] =
    if name.trim.isEmpty || name.trim != name then Left(ReferenceError.InvalidProvenance(s"asset name must be non-blank; got '$name'"))
    else AssetSha256.make(sha256).map(DataAsset(name, _))

/** Why an asset's coordinates are taken to be in a template frame. */
sealed trait FrameBasis:
  def display: String

object FrameBasis:
  /** A published reference, with a statement that claims only what the source
    * and its provenance support, e.g. that TemplateFlow tpl-fsLR (HCP Pipelines
    * templates, doi:10.1093/cercor/bhr291) surfaces are in MNI152NLin6Asym per
    * HCP convention.
    */
  final case class Literature private[FrameBasis] (doi: String, statement: String) extends FrameBasis:
    def display: String = s"doi:$doi: $statement"

  /** The frame follows from a recorded recipe applied to provenance-bound inputs. */
  final case class Derived private[FrameBasis] (recipe: String, inputs: Vector[DeclaredAsset]) extends FrameBasis:
    def display: String = s"derived by $recipe from ${inputs.map(_.label).mkString(", ")}"

  private val doiPattern = "10\\.[0-9]{4,9}/\\S+".r

  def literature(doi: String, statement: String): Either[ReferenceError, FrameBasis] =
    if !doiPattern.matches(doi) then Left(ReferenceError.InvalidFrameBasis(s"expected a bare DOI such as 10.1093/...; got '$doi'"))
    else if statement.trim.isEmpty then Left(ReferenceError.InvalidFrameBasis("literature statement must be non-blank"))
    else Right(Literature(doi, statement))

  def derived(recipe: String, inputs: Vector[DeclaredAsset]): Either[ReferenceError, FrameBasis] =
    if recipe.trim.isEmpty then Left(ReferenceError.InvalidFrameBasis("derivation recipe must be non-blank"))
    else if inputs.isEmpty then Left(ReferenceError.InvalidFrameBasis("derivation must name at least one input asset"))
    else Right(Derived(recipe, inputs))

/** The exact template frame of one asset's coordinates, with the basis for
  * that claim. This is the only source of an anatomy's frame: GIFTI
  * `DataSpace`/`TransformedSpace` codes are generic and never produce one.
  */
final case class FrameDeclaration private (frame: TemplateFrame, basis: FrameBasis, asset: DeclaredAsset):
  def display: String = s"${asset.label} in ${frame.display} (${basis.display})"

  /** Refuse bytes whose digest differs from the declared asset digest. */
  private[reference] def checkDigest(bytes: Array[Byte]): Either[ReferenceError, Unit] =
    val actual = AssetSha256.of(bytes)
    Either.cond(actual == asset.sha256, (), ReferenceError.DigestMismatch(asset.label, asset.sha256.value, actual.value))

object FrameDeclaration:
  def make(frame: TemplateFrame, basis: FrameBasis, asset: DeclaredAsset): Either[ReferenceError, FrameDeclaration] =
    basis match
      case FrameBasis.Derived(_, inputs) if inputs.contains(asset) =>
        Left(ReferenceError.InvalidFrameBasis(s"${asset.label} cannot be an input to its own frame derivation"))
      case _ => Right(FrameDeclaration(frame, basis, asset))

/** A surface whose frame declaration is bound to the exact bytes it was
  * decoded from. Platform loaders (`DeclaredSurfaceReader`) hash the bytes,
  * compare against `declaration.asset.sha256`, and only then decode those
  * same bytes; there is no public constructor.
  */
final class DeclaredSurface private (
  val declaration: FrameDeclaration,
  val geometry: SurfaceGeometry,
  val coordinates: GiftiCoordinateDeclaration
):
  def frame: TemplateFrame = declaration.frame

  override def toString: String = s"DeclaredSurface(${declaration.display}, ${geometry.kind.label})"

object DeclaredSurface:
  /** Bind a surface decoded from `bytes` to its declaration. Callers must
    * decode exactly these bytes; the digest is checked again here. The file's
    * own declared hemisphere and surface type must not contradict the
    * requested ones.
    */
  private[reference] def verified(
    declaration: FrameDeclaration,
    bytes: Array[Byte],
    decoded: DeclaredGiftiSurface
  ): Either[ReferenceError, DeclaredSurface] =
    for
      _ <- declaration.checkDigest(bytes)
      _ <- consistent(decoded)
    yield DeclaredSurface(declaration, decoded.geometry, decoded.coordinates)

  /** Test-only escape hatch: binds a declaration without seeing any bytes. */
  private[reference] def unsafeAssumeVerified(
    declaration: FrameDeclaration,
    geometry: SurfaceGeometry,
    coordinates: GiftiCoordinateDeclaration = GiftiCoordinateDeclaration(Vector.empty, None, None, None)
  ): DeclaredSurface =
    DeclaredSurface(declaration, geometry, coordinates)

  private def consistent(decoded: DeclaredGiftiSurface): Either[ReferenceError, Unit] =
    val geometry = decoded.geometry
    val coordinates = decoded.coordinates
    val hemisphere = coordinates.primaryStructure.collect:
      case GiftiPrimaryStructure.CortexLeft => Hemisphere.Left
      case GiftiPrimaryStructure.CortexRight => Hemisphere.Right
    val kinds: Option[Set[SurfaceKind]] = coordinates.secondaryStructure.collect:
      case GiftiSecondaryStructure.GrayWhite => Set(SurfaceKind.White)
      case GiftiSecondaryStructure.Pial => Set(SurfaceKind.Pial)
      case GiftiSecondaryStructure.MidThickness => Set(SurfaceKind.Midthickness)
    val anatomical = coordinates.geometricType.contains(GiftiGeometricType.Anatomical)
    if hemisphere.exists(_ != geometry.hemisphere) then
      Left(ReferenceError.DeclarationConflict(s"file declares ${coordinates.primaryStructure.get.code}; read as ${geometry.hemisphere}"))
    else if anatomical && kinds.exists(!_.contains(geometry.kind)) then
      Left(ReferenceError.DeclarationConflict(
        s"file declares ${coordinates.secondaryStructure.get.code}; read as ${geometry.kind.label}"))
    else if anatomical && DisplayForm.fromKind(geometry.kind).exists(!_.isInstanceOf[DisplayForm.Anatomical]) then
      Left(ReferenceError.DeclarationConflict(s"file declares Anatomical geometry; read as ${geometry.kind.label}"))
    else Right(())
