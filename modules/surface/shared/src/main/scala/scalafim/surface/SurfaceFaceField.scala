package scalafim.surface

/** Owned, dense, frame-major observations on the exact ordered faces of a mesh.
  * Geometry coordinates may change without changing this sample association.
  */
final class SurfaceFaceField[A] private (
  val geometry: SurfaceGeometry,
  val frameCount: Int,
  private val values: Array[A]
):
  def faceCount: Int = geometry.faceCount

  def isCompatibleWith(other: SurfaceGeometry): Boolean =
    geometry.hasSameMeshDomain(other)

  def valueAt(face: FaceId, frame: Int = 0): Option[A] =
    if face.index >= faceCount || frame < 0 || frame >= frameCount then None
    else Some(values(frame * faceCount + face.index))

  private[scalafim] def valueAtUnsafe(face: Int, frame: Int): A =
    values(frame * faceCount + face)

object SurfaceFaceField:
  def make[A](
    geometry: SurfaceGeometry,
    values: Array[A],
    frameCount: Int = 1
  ): Either[SurfaceError, SurfaceFaceField[A]] =
    val expected = geometry.faceCount.toLong * frameCount.toLong
    if frameCount <= 0 then Left(SurfaceError.InvalidField("face frame count must be positive"))
    else if expected != values.length.toLong then
      Left(SurfaceError.InvalidField(s"expected $expected face values; got ${values.length}"))
    else geometry.meshDomainEither.map(_ => new SurfaceFaceField(geometry, frameCount, values.clone()))
