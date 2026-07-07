package scalafim.surface

import scalafim.image.DMat
import scala.util.control.NonFatal

final case class SurfaceGeometry private (
  mesh: TriangleMesh,
  hemisphere: Hemisphere,
  kind: SurfaceKind,
  surfaceToWorld: DMat
):
  def vertexCount: Int =
    mesh.vertexCount

  def faceCount: Int =
    mesh.faceCount

  def label: String =
    kind.label

  def domainEither: Either[SurfaceError, SurfaceDomain] =
    SurfaceDomain.fromTag(hemisphere, vertexCount)

  def domain: SurfaceDomain =
    domainEither.fold(error => throw new IllegalArgumentException(error.message), identity)

  def withSurfaceToWorld(transform: DMat): SurfaceGeometry =
    SurfaceGeometry(mesh, hemisphere, kind, transform)

object SurfaceGeometry:

  def apply(
    mesh: TriangleMesh,
    hemisphere: Hemisphere = Hemisphere.Unknown,
    kind: SurfaceKind = SurfaceKind.Custom("surface"),
    surfaceToWorld: DMat = DMat.eye(4)
  ): SurfaceGeometry =
    require(surfaceToWorld.rows == 4 && surfaceToWorld.cols == 4, "surfaceToWorld must be 4x4")
    new SurfaceGeometry(mesh, hemisphere, kind, surfaceToWorld)

  def readEither(
    mesh: TriangleMesh,
    hemisphere: Hemisphere = Hemisphere.Unknown,
    kind: SurfaceKind = SurfaceKind.Custom("surface"),
    surfaceToWorld: DMat = DMat.eye(4)
  ): Either[SurfaceError, SurfaceGeometry] =
    try scala.util.Right(SurfaceGeometry(mesh, hemisphere, kind, surfaceToWorld))
    catch case NonFatal(error) => scala.util.Left(SurfaceError.InvalidGeometry(SurfaceError.reason(error)))
