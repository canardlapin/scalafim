package scalafim.surface

import image4s.geometry.Affine
import image4s.geometry.D3
import scala.util.control.NonFatal

final case class SurfaceGeometry private (
  mesh: TriangleMesh,
  hemisphere: Hemisphere,
  kind: SurfaceKind,
  surfaceToWorld: Affine[D3]
):
  def vertexCount: Int =
    mesh.vertexCount

  def faceCount: Int =
    mesh.faceCount

  def label: String =
    kind.label

  def domainEither: Either[SurfaceError, SurfaceDomain] =
    SurfaceDomain.fromTag(hemisphere, vertexCount)

  def meshDomainEither: Either[SurfaceError, SurfaceMeshDomain] =
    SurfaceMeshDomain.from(this)

  def hasSameMeshDomain(other: SurfaceGeometry): Boolean =
    hemisphere == other.hemisphere && mesh.hasSameTopology(other.mesh)

  def domain: SurfaceDomain =
    domainEither.fold(error => throw new IllegalArgumentException(error.message), identity)

  def withSurfaceToWorld(transform: Affine[D3]): SurfaceGeometry =
    SurfaceGeometry(mesh, hemisphere, kind, transform)

object SurfaceGeometry:

  def apply(
    mesh: TriangleMesh,
    hemisphere: Hemisphere = Hemisphere.Unknown,
    kind: SurfaceKind = SurfaceKind.Custom("surface"),
    surfaceToWorld: Affine[D3] = Affine.identity[D3]
  ): SurfaceGeometry =
    new SurfaceGeometry(mesh, hemisphere, kind, surfaceToWorld)

  def readEither(
    mesh: TriangleMesh,
    hemisphere: Hemisphere = Hemisphere.Unknown,
    kind: SurfaceKind = SurfaceKind.Custom("surface"),
    surfaceToWorld: Affine[D3] = Affine.identity[D3]
  ): Either[SurfaceError, SurfaceGeometry] =
    try scala.util.Right(SurfaceGeometry(mesh, hemisphere, kind, surfaceToWorld))
    catch case NonFatal(error) => scala.util.Left(SurfaceError.InvalidGeometry(SurfaceError.reason(error)))
