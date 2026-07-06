package scalafim.surface.io

import scalafim.surface.*

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.charset.StandardCharsets
import java.nio.file.Files

class FreeSurferSurfaceReaderSuite extends munit.FunSuite:

  test("readAscii handles std.8-style FreeSurfer ASCII surfaces"):
    val resource = getClass.getResource("/surface/mini_lh_smoothwm.asc")
    assert(resource != null)

    val geom = FreeSurferSurfaceReader.readAscii(java.nio.file.Path.of(resource.toURI))

    assertEquals(geom.vertexCount, 4)
    assertEquals(geom.faceCount, 2)
    assertEquals(geom.hemisphere, Hemisphere.Left)
    assertEquals(geom.kind, SurfaceKind.SmoothWm)
    assertEqualsDouble(geom.mesh.vertex(VertexId(3)).y, 1.0, 1e-12)
    assertEquals(geom.mesh.face(FaceId(1)).b, VertexId(3))

  test("read auto-detects ASCII and can infer right hemisphere"):
    withTempFile("sub-01_hemi-R_pial", ".asc") { path =>
      Files.writeString(
        path,
        """#!ascii version in FreeSurfer format
          |3 1
          |0 0 0
          |1 0 0
          |0 1 0
          |0 1 2
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val geom = FreeSurferSurfaceReader.read(path)

      assertEquals(geom.hemisphere, Hemisphere.Right)
      assertEquals(geom.kind, SurfaceKind.Pial)
      assertEquals(geom.mesh.face(FaceId(0)), Triangle(VertexId(0), VertexId(1), VertexId(2)))
    }

  test("readBinary loads triangle geometry and accepts explicit metadata"):
    withTempFile("surface", ".surf") { path =>
      writeBinarySurface(path)

      val geom =
        FreeSurferSurfaceReader.readBinary(
          path,
          hemisphere = Hemisphere.Right,
          kind = SurfaceKind.White
        )

      assertEquals(geom.vertexCount, 4)
      assertEquals(geom.faceCount, 2)
      assertEquals(geom.hemisphere, Hemisphere.Right)
      assertEquals(geom.kind, SurfaceKind.White)
      assertEqualsDouble(geom.mesh.vertex(VertexId(2)).y, 1.0, 1e-6)
      assertEquals(geom.mesh.face(FaceId(1)), Triangle(VertexId(1), VertexId(3), VertexId(2)))
    }

  test("readBinary rejects bad magic with a stable message"):
    withTempFile("bad", ".surf") { path =>
      writeBinarySurface(path, magic = 1)

      interceptMessage[IllegalArgumentException]("requirement failed: Unsupported FreeSurfer binary surface magic: 1"):
        FreeSurferSurfaceReader.readBinary(path)
    }

  test("readBinary rejects headers shorter than magic bytes"):
    withTempFile("tiny", ".surf") { path =>
      Files.write(path, Array[Byte](1, 2))

      interceptMessage[IllegalArgumentException]("requirement failed: File too small to be a valid FreeSurfer binary header"):
        FreeSurferSurfaceReader.readBinary(path)
    }

  test("readBinary rejects files without created-by and info lines"):
    withTempFile("nolines", ".surf") { path =>
      Files.write(path, Array[Byte](0xff.toByte, 0xff.toByte, 0xfe.toByte, 'x'.toByte))

      interceptMessage[IllegalArgumentException]("requirement failed: Unable to read FreeSurfer binary created-by line from FreeSurfer file"):
        FreeSurferSurfaceReader.readBinary(path)
    }

  test("readBinary rejects short reads with a stable message"):
    withTempFile("short", ".surf") { path =>
      writeBinarySurface(path, truncateBytes = 4)

      interceptMessage[IllegalArgumentException]("requirement failed: FreeSurfer binary surface ended before all vertices and faces were read"):
        FreeSurferSurfaceReader.readBinary(path)
    }

  test("readAscii rejects malformed rows with a stable message"):
    withTempFile("bad", ".asc") { path =>
      Files.writeString(
        path,
        """#!ascii version in FreeSurfer format
          |3 1
          |0 0
          |1 0 0
          |0 1 0
          |0 1 2
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      interceptMessage[IllegalArgumentException]("requirement failed: FreeSurfer ASCII vertex row 1 must contain 3 coordinates"):
        FreeSurferSurfaceReader.readAscii(path)
    }

  test("readAscii surfaces TriangleMesh face-index validation"):
    withTempFile("bad-face", ".asc") { path =>
      Files.writeString(
        path,
        """#!ascii version in FreeSurfer format
          |3 1
          |0 0 0
          |1 0 0
          |0 1 0
          |0 1 9
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      interceptMessage[IllegalArgumentException]("requirement failed: face indices out of range"):
        FreeSurferSurfaceReader.readAscii(path)
    }

  private def withTempFile[A](prefix: String, suffix: String)(f: java.nio.file.Path => A): A =
    val path = Files.createTempFile(prefix, suffix)
    try f(path)
    finally Files.deleteIfExists(path)

  private def writeBinarySurface(
    path: java.nio.file.Path,
    magic: Int = 16777214,
    truncateBytes: Int = 0
  ): Unit =
    val stamp = "created by scalafim test\ninfo\n".getBytes(StandardCharsets.US_ASCII)
    val coordinates =
      Vector(
        0.0f, 0.0f, 0.0f,
        1.0f, 0.0f, 0.0f,
        0.0f, 1.0f, 0.0f,
        1.0f, 1.0f, 0.0f
      )
    val faces =
      Vector(
        0, 1, 2,
        1, 3, 2
      )
    val bytes =
      ByteBuffer
        .allocate(3 + stamp.length + 8 + coordinates.length * 4 + faces.length * 4)
        .order(ByteOrder.BIG_ENDIAN)

    bytes.put(((magic >> 16) & 0xff).toByte)
    bytes.put(((magic >> 8) & 0xff).toByte)
    bytes.put((magic & 0xff).toByte)
    bytes.put(stamp)
    bytes.putInt(4)
    bytes.putInt(2)
    coordinates.foreach(bytes.putFloat)
    faces.foreach(bytes.putInt)

    val full = bytes.array()
    val out =
      if truncateBytes == 0 then full
      else full.take(full.length - truncateBytes)
    Files.write(path, out)
