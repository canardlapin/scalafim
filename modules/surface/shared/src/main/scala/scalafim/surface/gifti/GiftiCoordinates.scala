package scalafim.surface.gifti

import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.GeometryError

/** A coordinate space as a GIFTI file declares it (`NIFTI_XFORM_*`).
  *
  * These are *generic* NIfTI space codes. `Talairach` and `Mni152` say only
  * that coordinates are "some Talairach-like" or "some MNI152-like" space; they
  * do not name a template variant, release or registration. Accordingly there
  * is deliberately no conversion from a declared space to
  * `scalafim.surface.reference.TemplateFrame`: an exact frame can only come from
  * a provenance-bound `FrameDeclaration`.
  */
enum GiftiDeclaredSpace(val code: String):
  case Unknown extends GiftiDeclaredSpace("NIFTI_XFORM_UNKNOWN")
  case ScannerAnatomical extends GiftiDeclaredSpace("NIFTI_XFORM_SCANNER_ANAT")
  case AlignedAnatomical extends GiftiDeclaredSpace("NIFTI_XFORM_ALIGNED_ANAT")
  case Talairach extends GiftiDeclaredSpace("NIFTI_XFORM_TALAIRACH")
  case Mni152 extends GiftiDeclaredSpace("NIFTI_XFORM_MNI_152")
  case Other(value: String) extends GiftiDeclaredSpace(value)

object GiftiDeclaredSpace:
  /** None when the element is absent or blank; unknown text is kept verbatim. */
  def fromText(value: Option[String]): Option[GiftiDeclaredSpace] =
    value.map(_.trim).filter(_.nonEmpty).map: raw =>
      raw.toUpperCase match
        case Unknown.code => Unknown
        case ScannerAnatomical.code => ScannerAnatomical
        case AlignedAnatomical.code => AlignedAnatomical
        case Talairach.code => Talairach
        case Mni152.code => Mni152
        case _ => Other(raw)

/** One `CoordinateSystemTransformMatrix`: the matrix maps `dataSpace`
  * coordinates to `transformedSpace` coordinates. The 16 row-major values are
  * finite (enforced by [[GiftiTransform]]); invertibility is checked only when
  * the matrix is used as an affine.
  */
final case class GiftiCoordinateSystem(
  dataSpace: Option[GiftiDeclaredSpace],
  transformedSpace: Option[GiftiDeclaredSpace],
  matrixRowMajor: Vector[Double]
):
  def affine: Either[GeometryError, Affine[D3]] =
    Affine.fromRowMajor[D3](matrixRowMajor)

object GiftiCoordinateSystem:
  def from(transform: GiftiTransform): GiftiCoordinateSystem =
    GiftiCoordinateSystem(
      GiftiDeclaredSpace.fromText(transform.dataSpace),
      GiftiDeclaredSpace.fromText(transform.transformedSpace),
      transform.matrixData
    )

enum GiftiGeometricType(val code: String):
  case Reconstruction extends GiftiGeometricType("Reconstruction")
  case Anatomical extends GiftiGeometricType("Anatomical")
  case Inflated extends GiftiGeometricType("Inflated")
  case VeryInflated extends GiftiGeometricType("VeryInflated")
  case Spherical extends GiftiGeometricType("Spherical")
  case SemiSpherical extends GiftiGeometricType("SemiSpherical")
  case Ellipsoid extends GiftiGeometricType("Ellipsoid")
  case Flat extends GiftiGeometricType("Flat")
  case Hull extends GiftiGeometricType("Hull")
  case Other(value: String) extends GiftiGeometricType(value)

object GiftiGeometricType:
  private val known = Vector(Reconstruction, Anatomical, Inflated, VeryInflated, Spherical, SemiSpherical, Ellipsoid,
    Flat, Hull)

  def fromText(value: Option[String]): Option[GiftiGeometricType] =
    value.map(_.trim).filter(_.nonEmpty).map(raw => known.find(_.code.equalsIgnoreCase(raw)).getOrElse(Other(raw)))

enum GiftiPrimaryStructure(val code: String):
  case CortexLeft extends GiftiPrimaryStructure("CortexLeft")
  case CortexRight extends GiftiPrimaryStructure("CortexRight")
  case CortexRightAndLeft extends GiftiPrimaryStructure("CortexRightAndLeft")
  case Cerebellum extends GiftiPrimaryStructure("Cerebellum")
  case Other(value: String) extends GiftiPrimaryStructure(value)

object GiftiPrimaryStructure:
  private val known = Vector(CortexLeft, CortexRight, CortexRightAndLeft, Cerebellum)

  def fromText(value: Option[String]): Option[GiftiPrimaryStructure] =
    value.map(_.trim).filter(_.nonEmpty).map(raw => known.find(_.code.equalsIgnoreCase(raw)).getOrElse(Other(raw)))

enum GiftiSecondaryStructure(val code: String):
  case GrayWhite extends GiftiSecondaryStructure("GrayWhite")
  case Pial extends GiftiSecondaryStructure("Pial")
  case MidThickness extends GiftiSecondaryStructure("MidThickness")
  case Other(value: String) extends GiftiSecondaryStructure(value)

object GiftiSecondaryStructure:
  private val known = Vector(GrayWhite, Pial, MidThickness)

  def fromText(value: Option[String]): Option[GiftiSecondaryStructure] =
    value.map(_.trim).filter(_.nonEmpty).map(raw => known.find(_.code.equalsIgnoreCase(raw)).getOrElse(Other(raw)))

/** What a GIFTI pointset declares about its own coordinates: every coordinate
  * system transform in file order, and the geometric type and anatomical
  * structure metadata. Structure and type are read from the pointset's
  * `MetaData`, falling back to the document-level `MetaData`.
  *
  * This is a record of the file's claims, not an admitted frame. It can never
  * yield a `TemplateFrame`; see [[GiftiDeclaredSpace]].
  */
final case class GiftiCoordinateDeclaration(
  coordinateSystems: Vector[GiftiCoordinateSystem],
  geometricType: Option[GiftiGeometricType],
  primaryStructure: Option[GiftiPrimaryStructure],
  secondaryStructure: Option[GiftiSecondaryStructure]
):
  /** Declared (data, transformed) space pairs in file order, skipping systems with neither. */
  def declaredSpaces: Vector[(Option[GiftiDeclaredSpace], Option[GiftiDeclaredSpace])] =
    coordinateSystems.collect:
      case system if system.dataSpace.nonEmpty || system.transformedSpace.nonEmpty =>
        (system.dataSpace, system.transformedSpace)

object GiftiCoordinateDeclaration:
  def fromPointSet(document: GiftiDocument, pointSet: GiftiDataArray): GiftiCoordinateDeclaration =
    def field(name: String): Option[String] =
      pointSet.metadata.get(name).filter(_.trim.nonEmpty).orElse(document.metadata.get(name))
    GiftiCoordinateDeclaration(
      pointSet.transforms.map(GiftiCoordinateSystem.from),
      GiftiGeometricType.fromText(field("GeometricType")),
      GiftiPrimaryStructure.fromText(field("AnatomicalStructurePrimary")),
      GiftiSecondaryStructure.fromText(field("AnatomicalStructureSecondary"))
    )

  def fromDocument(document: GiftiDocument): Either[GiftiError, GiftiCoordinateDeclaration] =
    document.pointSet
      .toRight(GiftiError.MissingDataArray(GiftiIntent.PointSet))
      .map(fromPointSet(document, _))

/** A surface read from GIFTI together with the file's coordinate declaration. */
final case class DeclaredGiftiSurface(
  geometry: scalafim.surface.SurfaceGeometry,
  coordinates: GiftiCoordinateDeclaration
)
