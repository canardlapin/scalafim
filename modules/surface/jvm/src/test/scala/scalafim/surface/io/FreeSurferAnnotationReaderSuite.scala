package scalafim.surface.io

import java.io.{DataOutputStream, FileOutputStream}
import java.nio.file.Path
import java.nio.file.Files

class FreeSurferAnnotationReaderSuite extends munit.FunSuite:
  test("decodes documented old-style big-endian annotation and colour table"):
    val file = Files.createTempFile("scalafim-annot-", ".annot")
    val out = DataOutputStream(FileOutputStream(file.toFile))
    try
      out.writeInt(3)
      out.writeInt(2); out.writeInt(0x030201)
      out.writeInt(0); out.writeInt(0x060504)
      out.writeInt(1); out.writeInt(0)
      out.writeInt(1)
      out.writeInt(1)
      cstring(out, "fixture")
      cstring(out, "area")
      out.writeInt(1); out.writeInt(2); out.writeInt(3); out.writeInt(42)
    finally out.close()
    try
      val decoded = FreeSurferAnnotationReader.read(file).fold(error => fail(error), identity)
      assertEquals(decoded.vertexAnnotations.toVector, Vector(0x060504, 0, 0x030201))
      assertEquals(decoded.table.map(_.id), Vector(0x030201))
      assertEquals(decoded.table.map(_.name), Vector("area"))
      assertEquals(decoded.table.map(_.color), Vector(Some("#010203")))
    finally Files.deleteIfExists(file)

  test("rejects duplicate vertex records"):
    val file = Files.createTempFile("scalafim-annot-duplicate-", ".annot")
    val out = DataOutputStream(FileOutputStream(file.toFile))
    try
      out.writeInt(2)
      out.writeInt(0); out.writeInt(1)
      out.writeInt(0); out.writeInt(2)
    finally out.close()
    try assertEquals(FreeSurferAnnotationReader.read(file), Left("duplicate annotation vertex index 0"))
    finally Files.deleteIfExists(file)

  private def cstring(out: DataOutputStream, value: String): Unit =
    val bytes = (value + "\u0000").getBytes("UTF-8")
    out.writeInt(bytes.length)
    out.write(bytes)
