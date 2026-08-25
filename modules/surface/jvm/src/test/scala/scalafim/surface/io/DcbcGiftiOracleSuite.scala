package scalafim.surface.io

import scalafim.surface.*

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.file.Files
import java.security.MessageDigest

class DcbcGiftiOracleSuite extends munit.FunSuite:

  test("JVM GIFTI ingestion matches the checksum-pinned independent 32k cortical oracle"):
    val resource = getClass.getResource(DcbcSurfaceOracle.ResourceName)
    assert(resource != null, s"missing ${DcbcSurfaceOracle.ResourceName}")
    val path = java.nio.file.Path.of(resource.toURI)
    val bytes = Files.readAllBytes(path)
    assertEquals(bytes.length, DcbcSurfaceOracle.ContainerBytes)
    assertEquals(sha256(bytes), DcbcSurfaceOracle.ContainerSha256)

    val geometry = GiftiSurfaceReader.read(path, Hemisphere.Left, SurfaceKind.Midthickness)
    assertEquals(DcbcSurfaceOracle.failures(geometry), Vector.empty[String])
    assertEquals(coordinateSha256(geometry), DcbcSurfaceOracle.CoordinateSha256)
    assertEquals(faceSha256(geometry), DcbcSurfaceOracle.FaceSha256)

  private def coordinateSha256(geometry: SurfaceGeometry): String =
    val bytes = ByteBuffer
      .allocate(geometry.vertexCount * 3 * java.lang.Float.BYTES)
      .order(ByteOrder.LITTLE_ENDIAN)
    var vertex = 0
    while vertex < geometry.vertexCount do
      val point = geometry.mesh.vertex(VertexId.unsafe(vertex))
      bytes.putFloat(point.x.toFloat)
      bytes.putFloat(point.y.toFloat)
      bytes.putFloat(point.z.toFloat)
      vertex += 1
    sha256(bytes.array())

  private def faceSha256(geometry: SurfaceGeometry): String =
    val bytes = ByteBuffer
      .allocate(geometry.faceCount * 3 * java.lang.Integer.BYTES)
      .order(ByteOrder.LITTLE_ENDIAN)
    var face = 0
    while face < geometry.faceCount do
      val triangle = geometry.mesh.face(FaceId.unsafe(face))
      bytes.putInt(triangle.a.index)
      bytes.putInt(triangle.b.index)
      bytes.putInt(triangle.c.index)
      face += 1
    sha256(bytes.array())

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(bytes)
      .iterator
      .map(value => f"${value & 0xff}%02x")
      .mkString
