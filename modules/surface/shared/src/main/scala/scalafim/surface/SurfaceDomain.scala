package scalafim.surface

final case class SurfaceDomain private (
  hemisphere: CorticalHemisphere,
  vertexCount: Int
):
  def tag: Hemisphere =
    hemisphere.tag

  def contains(vertex: VertexId): Boolean =
    vertex.index < vertexCount

  def display: String =
    s"${hemisphere.code}:$vertexCount"

object SurfaceDomain:

  def apply(hemisphere: CorticalHemisphere, vertexCount: Int): SurfaceDomain =
    require(vertexCount > 0, "surface domain vertex count must be positive")
    new SurfaceDomain(hemisphere, vertexCount)

  def fromTag(hemisphere: Hemisphere, vertexCount: Int): Either[SurfaceError, SurfaceDomain] =
    hemisphere.toCortical.map(SurfaceDomain(_, vertexCount))
