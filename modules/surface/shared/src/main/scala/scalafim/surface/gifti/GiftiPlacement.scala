package scalafim.surface.gifti

import image4s.geometry.{Affine, D3}
import scalafim.surface.SurfaceGeometry

/** A declared target convention, not proof of registration to an image/subject. */
enum GiftiTargetSpace(val code: String):
  case ScannerAnatomical extends GiftiTargetSpace("NIFTI_XFORM_SCANNER_ANAT")
  case AlignedAnatomical extends GiftiTargetSpace("NIFTI_XFORM_ALIGNED_ANAT")
  case Talairach extends GiftiTargetSpace("NIFTI_XFORM_TALAIRACH")
  case Mni152 extends GiftiTargetSpace("NIFTI_XFORM_MNI_152")

/** Caller-owned placement policy. No ordering of target spaces is implied. */
enum GiftiTransformSelection:
  /** No transforms means native/unplaced; exactly one recognized transform is
    * accepted. Multiple transforms or an unknown target are refused.
    */
  case Unambiguous
  /** Keep source coordinates unplaced, even when transform metadata is present. */
  case NativeCoordinates
  /** Exactly one transform to this target is required. */
  case Target(space: GiftiTargetSpace)
  /** Explicit selection from the document's original transform vector. */
  case Transform(index: Int)

enum GiftiPlacement:
  case NativeCoordinates
  case Transformed(source: GiftiTransform, target: GiftiTargetSpace)

/** Coordinates remain native; the selected matrix lives in surfaceToWorld.
  * Apply it exactly once. NativeCoordinates has identity placement and conveys
  * no world-frame evidence. Source transform metadata is retained in full.
  */
final case class GiftiDecodedSurface private[gifti] (
  geometry: SurfaceGeometry,
  placement: GiftiPlacement,
  sourceTransforms: Vector[GiftiTransform]
)

private[gifti] object GiftiPlacementResolver:
  def resolve(
    transforms: Vector[GiftiTransform],
    selection: GiftiTransformSelection
  ): Either[GiftiError, (Affine[D3], GiftiPlacement)] =
    def invalid(reason: String) = Left(GiftiError.InvalidDataArray(s"GIFTI placement: $reason"))
    def target(transform: GiftiTransform): Option[GiftiTargetSpace] =
      GiftiTargetSpace.values.find(space => transform.transformedSpace.exists(_.trim.toUpperCase == space.code))
    def selected(transform: GiftiTransform): Either[GiftiError, (Affine[D3], GiftiPlacement)] =
      Affine.fromRowMajor[D3](transform.matrixData).left.map(GiftiError.Geometry.apply).flatMap { affine =>
        target(transform) match
          case None => invalid("selected transform has an unknown or missing target space; request native coordinates explicitly to leave it unplaced")
          case Some(space) => Right((affine, GiftiPlacement.Transformed(transform, space)))
      }
    selection match
      case GiftiTransformSelection.NativeCoordinates => Right((Affine.identity[D3], GiftiPlacement.NativeCoordinates))
      case GiftiTransformSelection.Unambiguous =>
        if transforms.isEmpty then Right((Affine.identity[D3], GiftiPlacement.NativeCoordinates))
        else if transforms.size == 1 then selected(transforms.head)
        else invalid("multiple transforms require an explicit target or transform selection")
      case GiftiTransformSelection.Target(space) =>
        val eligible = transforms.filter(transform => target(transform).contains(space))
        if eligible.size == 1 then selected(eligible.head)
        else invalid(s"target ${space.code} has ${eligible.size} matching transforms; exactly one is required")
      case GiftiTransformSelection.Transform(index) =>
        transforms.lift(index) match
          case Some(transform) => selected(transform)
          case None => invalid(s"transform index $index is outside the ${transforms.size} source transforms")
