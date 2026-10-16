package scalafim.surface.io

import java.nio.file.Files
import scalafim.surface.*

class FreeSurferMorphometryReaderSuite extends munit.FunSuite:
  test("public file reader binds scalar data and rejects opposite hemisphere and length"):
    val geometry = SurfaceGeometry(TriangleMesh.fromRows(
      Vector(Vector(-30.0,0.0,20.0), Vector(-29.0,0.0,20.0), Vector(-30.0,1.0,20.0)),
      Vector((0,1,2))), Hemisphere.Left, SurfaceKind.White)
    val directory = Files.createTempDirectory("scalafim-mni-morphometry-")
    val left = directory.resolve("lh.sulc")
    val right = directory.resolve("rh.sulc")
    try
      val bytes = Array(255,255,255, 0,0,0,3, 0,0,0,1, 0,0,0,1,
        192,32,0,0, 0,0,0,0, 63,160,0,0).map(_.toByte)
      val _ = Files.write(left, bytes)
      val _ = Files.write(right, bytes)
      assertEquals(FreeSurferMorphometryReader.read(left, geometry).data.toVector, Vector(-2.5,0.0,1.25))
      assert(FreeSurferMorphometryReader.readEither(right, geometry).isLeft)
      val _ = Files.write(left, bytes.dropRight(1))
      assert(FreeSurferMorphometryReader.readEither(left, geometry).isLeft)
    finally
      val _ = Files.deleteIfExists(left)
      val _ = Files.deleteIfExists(right)
      val _ = Files.deleteIfExists(directory)

/** Real-data command: bind source scalar files to reviewed MNI geometries and
  * emit a value digest for an independent decoder comparison.
  */
object FreeSurferMorphometryProbe:
  def main(args: Array[String]): Unit =
    require(args.length == 2, "Supply FreeSurfer scalar directory and reviewed MNI GIFTI directory")
    for hemisphere <- Vector("lh", "rh"); name <- Vector("sulc", "curv") do
      val geometry = GiftiSurfaceReader.read(java.nio.file.Path.of(args(1), s"$hemisphere.pial.surf.gii"))
      val field = FreeSurferMorphometryReader.read(java.nio.file.Path.of(args(0), s"$hemisphere.$name"), geometry, name)
      val digest = java.security.MessageDigest.getInstance("SHA-256")
      val buffer = java.nio.ByteBuffer.allocate(8).order(java.nio.ByteOrder.BIG_ENDIAN)
      field.data.foreach { value =>
        require(value.isFinite)
        buffer.clear()
        buffer.putDouble(value)
        digest.update(buffer.array())
      }
      val hash = digest.digest().map(b => f"${b & 0xff}%02x").mkString
      println(s"MORPHOMETRY $hemisphere.$name ${field.size} $hash ${field.data.min} ${field.data.max}")
    println("PASS: real cortical scalars bound to supplied MNI vertex identities")
