package scalafim.surface.scenarios

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.{Int8Array, Uint8Array}

@js.native
@JSImport("fs", JSImport.Namespace)
private object SurfaceNodeFs extends js.Object:
  def readFileSync(path: String): Uint8Array = js.native
  def existsSync(path: String): Boolean = js.native

@js.native
@JSImport("zlib", JSImport.Namespace)
private object SurfaceNodeZlib extends js.Object:
  def gunzipSync(data: Uint8Array): Uint8Array = js.native

@js.native
@JSImport("path", JSImport.Namespace)
private object SurfaceNodePath extends js.Object:
  def join(parts: String*): String = js.native
  def dirname(path: String): String = js.native

@js.native
@JSImport("crypto", JSImport.Namespace)
private object SurfaceNodeCrypto extends js.Object:
  def createHash(algorithm: String): SurfaceNodeHash = js.native

@js.native
private trait SurfaceNodeHash extends js.Object:
  def update(data: Uint8Array): SurfaceNodeHash = js.native
  def digest(encoding: String): String = js.native

private[scenarios] object SurfaceRealFixturePlatform:
  private val ResourceDir = "modules/surface/shared/src/test/resources"
  private lazy val root: String =
    var dir = js.Dynamic.global.process.cwd().asInstanceOf[String]
    var found: Option[String] = None
    var steps = 0
    while found.isEmpty && steps < 8 do
      val candidate = SurfaceNodePath.join(dir, ResourceDir)
      if SurfaceNodeFs.existsSync(candidate) then found = Some(candidate)
      else
        dir = SurfaceNodePath.dirname(dir)
        steps += 1
    found.getOrElse(throw new IllegalStateException(s"cannot locate $ResourceDir"))

  private def unsigned(bytes: Array[Byte]): Uint8Array =
    val result = new Uint8Array(bytes.length)
    bytes.indices.foreach(i => result(i) = (bytes(i) & 0xff).toShort)
    result

  def read(resource: String): Array[Byte] =
    val data = SurfaceNodeFs.readFileSync(SurfaceNodePath.join(root, resource))
    new Int8Array(data.buffer, data.byteOffset, data.length).toArray

  def gunzip(bytes: Array[Byte]): Array[Byte] =
    val data = SurfaceNodeZlib.gunzipSync(unsigned(bytes))
    new Int8Array(data.buffer, data.byteOffset, data.length).toArray

  def sha256(bytes: Array[Byte]): String =
    SurfaceNodeCrypto.createHash("sha256").update(unsigned(bytes)).digest("hex")
