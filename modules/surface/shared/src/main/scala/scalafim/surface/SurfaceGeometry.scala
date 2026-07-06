package scalafim.surface

import scalafim.image.DMat

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
