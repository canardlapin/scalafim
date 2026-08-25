package scalafim.surface.io

import scalafim.surface.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array

class DcbcGiftiOracleJsSuite extends munit.FunSuite:

  test("Scala.js GIFTI byte ingestion matches the checksum-pinned independent 32k cortical oracle"):
    val bytes = nodeFs.applyDynamic("readFileSync")(fixturePath).asInstanceOf[Uint8Array]
    assertEquals(bytes.length, DcbcSurfaceOracle.ContainerBytes)
    assertEquals(sha256(bytes), DcbcSurfaceOracle.ContainerSha256)

    GiftiSurfaceReader
      .read(bytes, Hemisphere.Left, SurfaceKind.Midthickness)
      .map: result =>
        val geometry = result.fold(error => fail(error.message), identity)
        assertEquals(DcbcSurfaceOracle.failures(geometry), Vector.empty[String])
        assertEquals(coordinateSha256(geometry), DcbcSurfaceOracle.CoordinateSha256)
        assertEquals(faceSha256(geometry), DcbcSurfaceOracle.FaceSha256)

  private def coordinateSha256(geometry: SurfaceGeometry): String =
    val bytes = nodeBuffer.applyDynamic("allocUnsafe")(
      geometry.vertexCount * 3 * java.lang.Float.BYTES
    )
    var offset = 0
    var vertex = 0
    while vertex < geometry.vertexCount do
      val point = geometry.mesh.vertex(VertexId.unsafe(vertex))
      bytes.applyDynamic("writeFloatLE")(point.x, offset)
      bytes.applyDynamic("writeFloatLE")(point.y, offset + 4)
      bytes.applyDynamic("writeFloatLE")(point.z, offset + 8)
      offset += 12
      vertex += 1
    sha256(bytes)

  private def faceSha256(geometry: SurfaceGeometry): String =
    val bytes = nodeBuffer.applyDynamic("allocUnsafe")(
      geometry.faceCount * 3 * java.lang.Integer.BYTES
    )
    var offset = 0
    var face = 0
    while face < geometry.faceCount do
      val triangle = geometry.mesh.face(FaceId.unsafe(face))
      bytes.applyDynamic("writeInt32LE")(triangle.a.index, offset)
      bytes.applyDynamic("writeInt32LE")(triangle.b.index, offset + 4)
      bytes.applyDynamic("writeInt32LE")(triangle.c.index, offset + 8)
      offset += 12
      face += 1
    sha256(bytes)

  private def sha256(bytes: js.Any): String =
    nodeCrypto
      .applyDynamic("createHash")("sha256")
      .applyDynamic("update")(bytes)
      .applyDynamic("digest")("hex")
      .asInstanceOf[String]

  private def fixturePath: String =
    var current = js.Dynamic.global.process.applyDynamic("cwd")().asInstanceOf[String]
    var depth = 0
    while depth < 8 do
      val candidate = nodePath.applyDynamic("join")(
        current,
        DcbcSurfaceOracle.RepositoryRelativePath
      ).asInstanceOf[String]
      if nodeFs.applyDynamic("existsSync")(candidate).asInstanceOf[Boolean] then return candidate
      val parent = nodePath.applyDynamic("dirname")(current).asInstanceOf[String]
      if parent == current then depth = 8
      else
        current = parent
        depth += 1
    throw new IllegalStateException(
      s"could not locate ${DcbcSurfaceOracle.RepositoryRelativePath} from the Scala.js test working directory"
    )

  private lazy val nodeFs: js.Dynamic =
    js.Dynamic.global.require("node:fs")

  private lazy val nodePath: js.Dynamic =
    js.Dynamic.global.require("node:path")

  private lazy val nodeCrypto: js.Dynamic =
    js.Dynamic.global.require("node:crypto")

  private lazy val nodeBuffer: js.Dynamic =
    js.Dynamic.global.selectDynamic("Buffer")
