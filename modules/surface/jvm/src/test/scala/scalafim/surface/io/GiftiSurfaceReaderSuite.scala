package scalafim.surface.io

import scalafim.image.DMat
import scalafim.surface.*

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.Base64

class GiftiSurfaceReaderSuite extends munit.FunSuite:

  test("read extracts pointset, triangles, hemisphere, kind, and POINTSET transform"):
    val resource = getClass.getResource("/surface/tetra_lh_midthickness.surf.gii")
    assert(resource != null)

    val geom = GiftiSurfaceReader.read(java.nio.file.Path.of(resource.toURI))

    assertEquals(geom.vertexCount, 4)
    assertEquals(geom.faceCount, 2)
    assertEquals(geom.hemisphere, Hemisphere.Left)
    assertEquals(geom.kind, SurfaceKind.Midthickness)
    assertEquals(geom.mesh.face(FaceId(0)), Triangle(VertexId(0), VertexId(1), VertexId(2)))
    assertEqualsDouble(geom.surfaceToWorld(0, 3), 10.0, 1e-12)
    assertEqualsDouble(geom.surfaceToWorld(1, 3), 20.0, 1e-12)
    assertEqualsDouble(geom.surfaceToWorld(2, 3), 30.0, 1e-12)

  test("read uses identity transform when CoordinateSystemTransformMatrix is absent"):
    withGiftiFile("sub-01_hemi-R_pial.surf.gii", giftiXml(includeTransform = false)) { path =>
      val geom = GiftiSurfaceReader.read(path)

      assertEquals(geom.hemisphere, Hemisphere.Right)
      assertEquals(geom.kind, SurfaceKind.Pial)
      assertEquals(geom.surfaceToWorld, DMat.eye(4))
    }

  test("read rejects malformed POINTSET shapes with stable messages"):
    val xml = giftiXml(includeTransform = false).replace("Dim1=\"3\"", "Dim1=\"2\"")

    withGiftiFile("bad-pointset.surf.gii", xml) { path =>
      interceptMessage[IllegalArgumentException]("invalid GIFTI DataArray: GIFTI POINTSET array must have Dim1=3"):
        GiftiSurfaceReader.read(path)

      assert(GiftiSurfaceReader.readEither(path).left.exists(_.message.contains("GIFTI POINTSET array must have Dim1=3")))
    }

  test("read rejects malformed TRIANGLE data with stable messages"):
    val xml = giftiXml(includeTransform = false).replace("0 1 2", "0 1")

    withGiftiFile("bad-triangle.surf.gii", xml) { path =>
      interceptMessage[IllegalArgumentException]("GIFTI data decode failed: NIFTI_INTENT_TRIANGLE decoded 2 values but dimensions require 3"):
        GiftiSurfaceReader.read(path)
    }

  test("read rejects missing POINTSET and TRIANGLE arrays"):
    val noPointset = giftiXml(includeTransform = false).replace("NIFTI_INTENT_POINTSET", "NIFTI_INTENT_VECTOR")
    withGiftiFile("missing-pointset.surf.gii", noPointset) { path =>
      interceptMessage[IllegalArgumentException]("GIFTI document is missing NIFTI_INTENT_POINTSET DataArray"):
        GiftiSurfaceReader.read(path)
    }

    val noTriangle = giftiXml(includeTransform = false).replace("NIFTI_INTENT_TRIANGLE", "NIFTI_INTENT_VECTOR")
    withGiftiFile("missing-triangle.surf.gii", noTriangle) { path =>
      interceptMessage[IllegalArgumentException]("GIFTI document is missing NIFTI_INTENT_TRIANGLE DataArray"):
        GiftiSurfaceReader.read(path)
    }

  test("read rejects malformed transform matrices"):
    val xml = giftiXml(includeTransform = true).replace("0 0 0 1", "0 0 1")

    withGiftiFile("bad-transform.surf.gii", xml) { path =>
      interceptMessage[IllegalArgumentException]("invalid GIFTI DataArray: CoordinateSystemTransformMatrix must contain 16 values"):
        GiftiSurfaceReader.read(path)
    }

  test("read accepts explicit hemisphere and kind metadata"):
    withGiftiFile("anonymous.surf.gii", giftiXml(includeTransform = false)) { path =>
      val geom = GiftiSurfaceReader.read(path, Hemisphere.Both, SurfaceKind.Custom("custom-surface"))

      assertEquals(geom.hemisphere, Hemisphere.Both)
      assertEquals(geom.kind, SurfaceKind.Custom("custom-surface"))
    }

  test("read accepts Base64Binary pointset and triangle arrays"):
    val xml =
      giftiXml(includeTransform = false)
        .replace("Encoding=\"ASCII\"", "Encoding=\"Base64Binary\"")
        .replace("0 0 0\n      1 0 0\n      0 1 0", float32Base64(Vector(0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f)))
        .replace("0 1 2", int32Base64(Vector(0, 1, 2)))

    withGiftiFile("encoded.surf.gii", xml) { path =>
      val geom = GiftiSurfaceReader.read(path)

      assertEquals(geom.vertexCount, 3)
      assertEquals(geom.faceCount, 1)
      assertEquals(geom.mesh.face(FaceId(0)), Triangle(VertexId(0), VertexId(1), VertexId(2)))
    }

  test("read honors ColumnMajorOrder matrix payloads"):
    val xml =
      giftiXml(includeTransform = false)
        .replace("ArrayIndexingOrder=\"RowMajorOrder\"", "ArrayIndexingOrder=\"ColumnMajorOrder\"")
        .replace("0 0 0\n      1 0 0\n      0 1 0", "0 1 0\n      0 0 1\n      0 0 0")

    withGiftiFile("column-major.surf.gii", xml) { path =>
      val geom = GiftiSurfaceReader.read(path)

      assertEquals(geom.mesh.vertex(VertexId(0)).toVector, Vector(0.0, 0.0, 0.0))
      assertEquals(geom.mesh.vertex(VertexId(1)).toVector, Vector(1.0, 0.0, 0.0))
      assertEquals(geom.mesh.vertex(VertexId(2)).toVector, Vector(0.0, 1.0, 0.0))
    }

  private def withGiftiFile[A](name: String, contents: String)(f: java.nio.file.Path => A): A =
    val dir = Files.createTempDirectory("scalafim-gifti-")
    val path = dir.resolve(name)
    Files.writeString(path, contents, StandardCharsets.UTF_8)
    try f(path)
    finally
      Files.deleteIfExists(path)
      Files.deleteIfExists(dir)

  private def giftiXml(includeTransform: Boolean): String =
    val transform =
      if includeTransform then
        """<CoordinateSystemTransformMatrix>
          |  <MatrixData>
          |    1 0 0 1
          |    0 1 0 2
          |    0 0 1 3
          |    0 0 0 1
          |  </MatrixData>
          |</CoordinateSystemTransformMatrix>""".stripMargin
      else ""

    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<GIFTI Version="1.0" NumberOfDataArrays="2">
       |  <DataArray Intent="NIFTI_INTENT_POINTSET" DataType="NIFTI_TYPE_FLOAT32"
       |             ArrayIndexingOrder="RowMajorOrder" Dimensionality="2"
       |             Dim0="3" Dim1="3" Encoding="ASCII" Endian="LittleEndian">
       |    $transform
       |    <Data>
       |      0 0 0
       |      1 0 0
       |      0 1 0
       |    </Data>
       |  </DataArray>
       |  <DataArray Intent="NIFTI_INTENT_TRIANGLE" DataType="NIFTI_TYPE_INT32"
       |             ArrayIndexingOrder="RowMajorOrder" Dimensionality="2"
       |             Dim0="1" Dim1="3" Encoding="ASCII" Endian="LittleEndian">
       |    <Data>
       |      0 1 2
       |    </Data>
       |  </DataArray>
       |</GIFTI>
       |""".stripMargin

  private def float32Base64(values: Vector[Float]): String =
    val buffer = ByteBuffer.allocate(values.length * 4).order(ByteOrder.LITTLE_ENDIAN)
    values.foreach(buffer.putFloat)
    Base64.getMimeEncoder.encodeToString(buffer.array())

  private def int32Base64(values: Vector[Int]): String =
    val buffer = ByteBuffer.allocate(values.length * 4).order(ByteOrder.LITTLE_ENDIAN)
    values.foreach(buffer.putInt)
    Base64.getMimeEncoder.encodeToString(buffer.array())
