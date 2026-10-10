package scalafim.surface.gifti

import image4s.geometry.{Affine, D3}

/** A declared target space that a GIFTI transform may place coordinates into.
  *
  * This is the recognized subset of [[GiftiDeclaredSpace]]: `Unknown` and
  * unrecognized codes cannot be placement targets. A target is a declared
  * convention, not proof of registration to an image or subject.
  */
enum GiftiTargetSpace(val declared: GiftiDeclaredSpace):
  case ScannerAnatomical extends GiftiTargetSpace(GiftiDeclaredSpace.ScannerAnatomical)
  case AlignedAnatomical extends GiftiTargetSpace(GiftiDeclaredSpace.AlignedAnatomical)
  case Talairach extends GiftiTargetSpace(GiftiDeclaredSpace.Talairach)
  case Mni152 extends GiftiTargetSpace(GiftiDeclaredSpace.Mni152)

  def code: String = declared.code

object GiftiTargetSpace:
  def fromDeclared(space: GiftiDeclaredSpace): Option[GiftiTargetSpace] =
    values.find(_.declared == space)

/** Caller-owned placement policy. No ordering of target spaces is implied. */
enum GiftiTransformSelection:
  /** No transforms, or only exact identity matrices, leaves coordinates native.
    * Exactly one transform with a recognized target is applied. Anything else
    * (several transforms of which any is not identity, or a single non-identity
    * transform with an unknown/missing target) is refused.
    */
  case Unambiguous
  /** Keep source coordinates unplaced, even when transform metadata is present. */
  case NativeCoordinates
  /** Exactly one transform to this target is required. */
  case Target(space: GiftiTargetSpace)
  /** Explicit selection from the declaration's original transform vector. */
  case Transform(index: Int)

/** Which declared transform, if any, became `SurfaceGeometry.surfaceToWorld`. */
enum GiftiPlacement:
  /** Identity placement. Conveys no world-frame evidence. */
  case NativeCoordinates
  /** `GiftiCoordinateDeclaration.coordinateSystems(index)` was applied, once. */
  case Transformed(index: Int, system: GiftiCoordinateSystem, target: GiftiTargetSpace)

private[gifti] object GiftiPlacementResolver:
  private val identityRowMajor: Vector[Double] =
    Vector(1.0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)

  def resolve(
    systems: Vector[GiftiCoordinateSystem],
    selection: GiftiTransformSelection
  ): Either[GiftiError, (Affine[D3], GiftiPlacement)] =
    def invalid(reason: String) = Left(GiftiError.InvalidDataArray(s"GIFTI placement: $reason"))
    def target(system: GiftiCoordinateSystem): Option[GiftiTargetSpace] =
      system.transformedSpace.flatMap(GiftiTargetSpace.fromDeclared)
    def isIdentity(system: GiftiCoordinateSystem): Boolean =
      system.matrixRowMajor == identityRowMajor
    val native = Right((Affine.identity[D3], GiftiPlacement.NativeCoordinates))
    def selected(index: Int): Either[GiftiError, (Affine[D3], GiftiPlacement)] =
      val system = systems(index)
      system.affine.left.map(GiftiError.Geometry.apply).flatMap { affine =>
        target(system) match
          case None =>
            invalid(s"transform $index has an unknown or missing target space; request native coordinates explicitly to leave it unplaced")
          case Some(space) => Right((affine, GiftiPlacement.Transformed(index, system, space)))
      }
    selection match
      case GiftiTransformSelection.NativeCoordinates => native
      case GiftiTransformSelection.Unambiguous =>
        if systems.isEmpty then native
        else if systems.size == 1 && target(systems.head).nonEmpty then selected(0)
        else if systems.forall(isIdentity) then native
        else if systems.size == 1 then selected(0)
        else invalid(s"${systems.size} transforms, not all identity, require an explicit target or transform selection")
      case GiftiTransformSelection.Target(space) =>
        val eligible = systems.indices.filter(index => target(systems(index)).contains(space))
        if eligible.size == 1 then selected(eligible.head)
        else invalid(s"target ${space.code} has ${eligible.size} matching transforms; exactly one is required")
      case GiftiTransformSelection.Transform(index) =>
        if systems.indices.contains(index) then selected(index)
        else invalid(s"transform index $index is outside the ${systems.size} source transforms")
