package scalafim.surface.io

import scalafim.surface.*

import java.nio.ByteBuffer
import java.nio.ByteOrder
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array

class GiftiReaderJsSuite extends munit.FunSuite:

  test("Scala.js reads a generated fsaverage5-scale compressed GIFTI surface"):
    val fixture = gridFixture(columns = 129, rows = 81)
    val xml = surfaceXml(
      vertices = fixture.vertexCount,
      faces = fixture.faceCount,
      pointEncoding = "GZipBase64Binary",
      pointData = compressedBase64(float32Bytes(fixture.coordinates), "deflateSync"),
      triangleEncoding = "GZipBase64Binary",
      triangleData = compressedBase64(int32Bytes(fixture.faces), "gzipSync")
    )

    GiftiSurfaceReader
      .read(utf8Bytes(xml), Hemisphere.Left, SurfaceKind.Pial)
      .map { result =>
        val geometry = result.fold(error => fail(error.message), identity)
        val sample = geometry.mesh.vertex(VertexId(6550))

        assertEquals(geometry.vertexCount, 10449)
        assertEquals(geometry.faceCount, 20480)
        assertEquals(geometry.mesh.face(FaceId(20479)), Triangle(VertexId(10319), VertexId(10448), VertexId(10447)))
        assertEqualsDouble(sample.x, 100.0, 0.0)
        assertEqualsDouble(sample.y, 50.0, 0.0)
        assertEqualsDouble(sample.z, 3.0, 0.0)
        assertEqualsDouble(geometry.surfaceToWorld.matrix(0, 3), 10.0, 0.0)
        assertEqualsDouble(geometry.surfaceToWorld.matrix(1, 3), 20.0, 0.0)
        assertEqualsDouble(geometry.surfaceToWorld.matrix(2, 3), 30.0, 0.0)
      }

  test("Scala.js byte ingestion accepts an outer gzip stream"):
    val fixture = gridFixture(columns = 3, rows = 2)
    val xml = surfaceXml(
      vertices = fixture.vertexCount,
      faces = fixture.faceCount,
      pointEncoding = "ASCII",
      pointData = fixture.coordinates.mkString(" "),
      triangleEncoding = "ASCII",
      triangleData = fixture.faces.mkString(" ")
    )
    val bytes = nodeZlib.applyDynamic("gzipSync")(
      nodeBuffer.applyDynamic("from")(xml, "utf8")
    ).asInstanceOf[Uint8Array]

    GiftiSurfaceReader
      .read(bytes, Hemisphere.Right, SurfaceKind.Inflated)
      .map { result =>
        val geometry = result.fold(error => fail(error.message), identity)
        assertEquals(geometry.vertexCount, 6)
        assertEquals(geometry.faceCount, 4)
        assertEquals(geometry.hemisphere, Hemisphere.Right)
        assertEquals(geometry.kind, SurfaceKind.Inflated)
      }

  test("Scala.js bounds compressed payloads by their declared shape"):
    val fixture = gridFixture(columns = 3, rows = 2)
    val overlongCoordinates = fixture.coordinates ++ Array(99.0f)
    val xml = surfaceXml(
      vertices = fixture.vertexCount,
      faces = fixture.faceCount,
      pointEncoding = "GZipBase64Binary",
      pointData = compressedBase64(float32Bytes(overlongCoordinates), "deflateSync"),
      triangleEncoding = "ASCII",
      triangleData = fixture.faces.mkString(" ")
    )

    GiftiSurfaceReader
      .readString(xml, Hemisphere.Left, SurfaceKind.Pial)
      .map { result =>
        assert(
          result.left.exists(_.message.contains("decompressed data exceed the 72 byte limit")),
          result.left.map(_.message).getOrElse("compressed payload unexpectedly decoded")
        )
      }

  test("Scala.js label ingestion preserves LabelTable and NODE_INDEX semantics"):
    val fixture = gridFixture(columns = 3, rows = 2)
    val geometryXml = surfaceXml(
      vertices = fixture.vertexCount,
      faces = fixture.faceCount,
      pointEncoding = "ASCII",
      pointData = fixture.coordinates.mkString(" "),
      triangleEncoding = "ASCII",
      triangleData = fixture.faces.mkString(" ")
    )

    GiftiSurfaceReader
      .readString(geometryXml, Hemisphere.Left, SurfaceKind.Midthickness)
      .flatMap {
        case Left(error) => Future.failed(new AssertionError(error.message))
        case Right(geometry) =>
          GiftiSurfaceReader.readLabelsString(labelXml(), geometry, "sparse-labels")
      }
      .map { result =>
        val labeled = result.fold(error => fail(error.message), identity)
        assertEquals(labeled.labelAt(VertexId(0)), Some(7))
        assertEquals(labeled.labelAt(VertexId(4)), Some(9))
        assertEquals(labeled.labelAt(VertexId(2)), None)
        assertEquals(labeled.info(9).map(_.name), Some("Posterior"))
      }

  private final case class GridFixture(
    coordinates: Array[Float],
    faces: Array[Int]
  ):
    def vertexCount: Int = coordinates.length / 3
    def faceCount: Int = faces.length / 3

  private def gridFixture(columns: Int, rows: Int): GridFixture =
    require(columns >= 2 && rows >= 2)
    val coordinates = Array.ofDim[Float](columns * rows * 3)
    var row = 0
    while row < rows do
      var column = 0
      while column < columns do
        val vertex = row * columns + column
        val offset = vertex * 3
        coordinates(offset) = column.toFloat
        coordinates(offset + 1) = row.toFloat
        coordinates(offset + 2) = ((row + column) % 7).toFloat
        column += 1
      row += 1

    val faces = Array.ofDim[Int]((columns - 1) * (rows - 1) * 6)
    var offset = 0
    row = 0
    while row < rows - 1 do
      var column = 0
      while column < columns - 1 do
        val lowerLeft = row * columns + column
        val lowerRight = lowerLeft + 1
        val upperLeft = lowerLeft + columns
        val upperRight = upperLeft + 1
        faces(offset) = lowerLeft
        faces(offset + 1) = lowerRight
        faces(offset + 2) = upperLeft
        faces(offset + 3) = lowerRight
        faces(offset + 4) = upperRight
        faces(offset + 5) = upperLeft
        offset += 6
        column += 1
      row += 1
    GridFixture(coordinates, faces)

  private def surfaceXml(
    vertices: Int,
    faces: Int,
    pointEncoding: String,
    pointData: String,
    triangleEncoding: String,
    triangleData: String
  ): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<!DOCTYPE GIFTI SYSTEM "http://gifti.invalid/gifti.dtd">
       |<GIFTI Version="1.0" NumberOfDataArrays="2">
       |  <MetaData>
       |    <MD><Name><![CDATA[AnatomicalStructurePrimary]]></Name><Value><![CDATA[CortexLeft]]></Value></MD>
       |  </MetaData>
       |  <DataArray Intent="NIFTI_INTENT_POINTSET" DataType="NIFTI_TYPE_FLOAT32"
       |             ArrayIndexingOrder="RowMajorOrder" Dimensionality="2"
       |             Dim0="$vertices" Dim1="3" Encoding="$pointEncoding" Endian="LittleEndian">
       |    <CoordinateSystemTransformMatrix>
       |      <DataSpace>NIFTI_XFORM_ALIGNED_ANAT</DataSpace>
       |      <TransformedSpace>NIFTI_XFORM_TALAIRACH</TransformedSpace>
       |      <MatrixData>
       |        1 0 0 10
       |        0 1 0 20
       |        0 0 1 30
       |        0 0 0 1
       |      </MatrixData>
       |    </CoordinateSystemTransformMatrix>
       |    <Data>$pointData</Data>
       |  </DataArray>
       |  <DataArray Intent="NIFTI_INTENT_TRIANGLE" DataType="NIFTI_TYPE_INT32"
       |             ArrayIndexingOrder="RowMajorOrder" Dimensionality="2"
       |             Dim0="$faces" Dim1="3" Encoding="$triangleEncoding" Endian="LittleEndian">
       |    <Data>$triangleData</Data>
       |  </DataArray>
       |</GIFTI>
       |""".stripMargin

  private def labelXml(): String =
    """<?xml version="1.0" encoding="UTF-8"?>
      |<GIFTI Version="1.0" NumberOfDataArrays="2">
      |  <LabelTable>
      |    <Label Key="7" Red="1" Green="0" Blue="0" Alpha="1">Anterior</Label>
      |    <Label Key="9" Red="0" Green="0" Blue="1" Alpha="1">Posterior</Label>
      |  </LabelTable>
      |  <DataArray Intent="NIFTI_INTENT_NODE_INDEX" DataType="NIFTI_TYPE_INT32"
      |             ArrayIndexingOrder="RowMajorOrder" Dimensionality="1"
      |             Dim0="3" Encoding="ASCII" Endian="LittleEndian">
      |    <Data>0 1 4</Data>
      |  </DataArray>
      |  <DataArray Intent="NIFTI_INTENT_LABEL" DataType="NIFTI_TYPE_INT32"
      |             ArrayIndexingOrder="RowMajorOrder" Dimensionality="1"
      |             Dim0="3" Encoding="ASCII" Endian="LittleEndian">
      |    <Data>7 7 9</Data>
      |  </DataArray>
      |</GIFTI>
      |""".stripMargin

  private def float32Bytes(values: Array[Float]): Array[Byte] =
    val buffer = ByteBuffer.allocate(values.length * 4).order(ByteOrder.LITTLE_ENDIAN)
    var index = 0
    while index < values.length do
      buffer.putFloat(values(index))
      index += 1
    buffer.array()

  private def int32Bytes(values: Array[Int]): Array[Byte] =
    val buffer = ByteBuffer.allocate(values.length * 4).order(ByteOrder.LITTLE_ENDIAN)
    var index = 0
    while index < values.length do
      buffer.putInt(values(index))
      index += 1
    buffer.array()

  private def compressedBase64(bytes: Array[Byte], method: String): String =
    val compressed = nodeZlib.applyDynamic(method)(uint8Array(bytes))
    compressed.applyDynamic("toString")("base64").asInstanceOf[String]

  private def uint8Array(bytes: Array[Byte]): Uint8Array =
    val out = new Uint8Array(bytes.length)
    var index = 0
    while index < bytes.length do
      out(index) = ((bytes(index) & 0xff).toShort)
      index += 1
    out

  private def utf8Bytes(value: String): Uint8Array =
    nodeBuffer.applyDynamic("from")(value, "utf8").asInstanceOf[Uint8Array]

  private lazy val nodeZlib: js.Dynamic =
    js.Dynamic.global.require("node:zlib")

  private lazy val nodeBuffer: js.Dynamic =
    js.Dynamic.global.selectDynamic("Buffer")
