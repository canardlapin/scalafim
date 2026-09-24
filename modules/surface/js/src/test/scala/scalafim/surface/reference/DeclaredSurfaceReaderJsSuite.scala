package scalafim.surface.reference

import scalafim.surface.*
import scalafim.surface.gifti.*
import scalafim.surface.io.GiftiSurfaceReader

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.typedarray.Uint8Array

class DeclaredSurfaceReaderJsSuite extends munit.FunSuite:
  private val nlin6 = TemplateFrame.unsafe("MNI152NLin6Asym", "templateflow-24.2.0")

  private def uint8(bytes: Array[Byte]): Uint8Array =
    val out = new Uint8Array(bytes.length)
    bytes.indices.foreach(i => out(i) = (bytes(i) & 0xff).toShort)
    out

  test("Scala.js loads a declared GIFTI only when its bytes match the declared digest"):
    val bytes = DeclaredGiftiFixture.bytes(DeclaredGiftiFixture.xml())
    val declaration = DeclaredGiftiFixture.declaration(nlin6, bytes)
    val edited = DeclaredGiftiFixture.bytes(DeclaredGiftiFixture.xml().replace("0 1 0</Data>", "0 2 0</Data>"))
    for
      accepted <- DeclaredSurfaceReader.read(uint8(bytes), declaration, Hemisphere.Left, SurfaceKind.Midthickness)
      conflict <- DeclaredSurfaceReader.read(uint8(bytes), declaration, Hemisphere.Right, SurfaceKind.Midthickness)
      mismatch <- DeclaredSurfaceReader.read(uint8(edited), declaration, Hemisphere.Left, SurfaceKind.Midthickness)
    yield
      val surface = accepted.fold(e => fail(e.message), s => s)
      assertEquals(surface.frame, nlin6)
      assertEquals(surface.geometry.vertexCount, 3)
      assertEquals(surface.coordinates.declaredSpaces,
        Vector((Some(GiftiDeclaredSpace.Talairach), Some(GiftiDeclaredSpace.Talairach))))
      assert(conflict.left.exists(_.isInstanceOf[ReferenceError.DeclarationConflict]))
      assert(mismatch.left.exists(_.isInstanceOf[ReferenceError.DigestMismatch]))

  test("Scala.js declared read path exposes coordinate metadata beside the geometry"):
    GiftiSurfaceReader.readDeclaredString(DeclaredGiftiFixture.xml(), Hemisphere.Left, SurfaceKind.Midthickness)
      .map { result =>
        val declared = result.fold(e => fail(e.message), d => d)
        assertEquals(declared.geometry.vertexCount, 3)
        assertEquals(declared.coordinates.primaryStructure, Some(GiftiPrimaryStructure.CortexLeft))
        assertEquals(declared.coordinates.geometricType, Some(GiftiGeometricType.Anatomical))
      }
