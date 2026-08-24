package scalafim.surface

import scala.util.hashing.MurmurHash3

/** Legacy 16-character compatibility digest for scene and atlas keys.
  *
  * It is not a topology or vertex-owner authority. New runtime code uses the
  * exact mesh4s owner; structural ingestion uses mesh4s's versioned SHA-256
  * connectivity fingerprint followed by an ordered-incidence comparison.
  */
opaque type MeshTopologyIdentity = Long

object MeshTopologyIdentity:
  private[surface] def from(vertexCount: Int, faceIndices: Array[Int]): MeshTopologyIdentity =
    var primary = MurmurHash3.mix(0x3c074a61, vertexCount)
    var secondary = MurmurHash3.mix(0x1b873593, faceIndices.length)
    var index = 0
    while index < faceIndices.length do
      val value = faceIndices(index)
      primary = MurmurHash3.mix(primary, value)
      secondary = MurmurHash3.mix(secondary, value ^ index)
      index += 1
    val count = faceIndices.length + 1
    val high = MurmurHash3.finalizeHash(primary, count)
    val low = MurmurHash3.finalizeHash(secondary, count)
    (high.toLong << 32) | (low.toLong & 0xffffffffL)

  extension (identity: MeshTopologyIdentity)
    def stableKey: String =
      val raw = java.lang.Long.toHexString(identity)
      "0" * (16 - raw.length) + raw

/** A cortical vertex domain whose topology and vertex ordering are explicit.
  * Coordinates and surface kind are intentionally absent so corresponding
  * white, pial, and inflated geometries share a domain.
  */
final case class SurfaceMeshDomain private (
  hemisphere: CorticalHemisphere,
  vertexCount: Int,
  faceCount: Int,
  topology: MeshTopologyIdentity
):
  def display: String =
    s"${hemisphere.code}:$vertexCount:$faceCount:${topology.stableKey}"

object SurfaceMeshDomain:
  def from(geometry: SurfaceGeometry): Either[SurfaceError, SurfaceMeshDomain] =
    geometry.hemisphere.toCortical.map: hemisphere =>
      SurfaceMeshDomain(
        hemisphere = hemisphere,
        vertexCount = geometry.vertexCount,
        faceCount = geometry.faceCount,
        topology = geometry.mesh.topologyIdentity
      )
