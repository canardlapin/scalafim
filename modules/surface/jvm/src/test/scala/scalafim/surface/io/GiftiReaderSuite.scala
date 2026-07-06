package scalafim.surface.io

import scalafim.surface.*
import scalafim.surface.gifti.*

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.util.Base64
import java.util.zip.GZIPOutputStream

class GiftiReaderSuite extends munit.FunSuite:

  test("parseString builds a typed GIFTI document with metadata and transforms"):
    val doc = GiftiReader.parseString(surfaceXml("ASCII", asciiPointset, "ASCII", asciiTriangles)).toOption.get

    assertEquals(doc.attributes("Version"), "1.0")
    assertEquals(doc.metadata("AnatomicalStructurePrimary"), "CortexLeft")
    assertEquals(doc.pointSet.map(_.intent), Some(GiftiIntent.PointSet))
    assertEquals(doc.triangles.map(_.intent), Some(GiftiIntent.Triangle))
    assertEquals(doc.pointSet.toVector.flatMap(_.transforms).head.matrixData(3), 10.0)

  test("ASCII data arrays decode to numeric pointsets and triangles"):
    val doc = GiftiReader.parseString(surfaceXml("ASCII", asciiPointset, "ASCII", asciiTriangles)).toOption.get

    assertEquals(GiftiReader.doubleData(doc.pointSet.get).toOption.get.toVector, Vector(0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0))
    assertEquals(GiftiReader.intData(doc.triangles.get).toOption.get.toVector, Vector(0, 1, 2))

  test("Base64Binary data arrays decode with declared endian"):
    val doc =
      GiftiReader
        .parseString(surfaceXml("Base64Binary", float32Base64(Vector(0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f)), "Base64Binary", int32Base64(Vector(0, 1, 2))))
        .toOption
        .get

    assertEquals(GiftiReader.doubleData(doc.pointSet.get).toOption.get.toVector, Vector(0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0))
    assertEquals(GiftiReader.intData(doc.triangles.get).toOption.get.toVector, Vector(0, 1, 2))

  test("GZipBase64Binary data arrays decode after inflation"):
    val points = gzipBase64(float32Bytes(Vector(0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f)))
    val triangles = gzipBase64(int32Bytes(Vector(0, 1, 2)))
    val doc = GiftiReader.parseString(surfaceXml("GZipBase64Binary", points, "GZipBase64Binary", triangles)).toOption.get

    assertEquals(GiftiReader.doubleData(doc.pointSet.get).toOption.get.toVector, Vector(0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0))
    assertEquals(GiftiReader.intData(doc.triangles.get).toOption.get.toVector, Vector(0, 1, 2))

  test("label GIFTI parses label table and builds LabeledSurface"):
    val doc = GiftiReader.parseString(labelXml()).toOption.get
    val geometry =
      SurfaceGeometry(
        TriangleMesh.fromRows(
          vertices = Vector(Vector(0.0, 0.0, 0.0), Vector(1.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0), Vector(0.0, 0.0, 1.0)),
          faces = Vector((0, 1, 2), (0, 1, 3))
        ),
        Hemisphere.Left,
        SurfaceKind.Midthickness
      )
    val labeled = GiftiSurfaceReader.labeledSurface(doc, geometry, "labels").toOption.get

    assertEquals(doc.labelTable.map(label => (label.key, label.name, label.colorHex)), Vector((1, "Positive", Some("#ff0000")), (2, "Negative", Some("#02ffff"))))
    assertEquals(labeled.labelAt(VertexId(0)), Some(1))
    assertEquals(labeled.labelAt(VertexId(3)), Some(2))
    assertEquals(labeled.info(2).map(_.name), Some("Negative"))

  test("external binary arrays are reported explicitly"):
    val doc =
      GiftiReader
        .parseString(surfaceXml("ExternalFileBinary", "", "ASCII", asciiTriangles, externalFile = Some("points.bin")))
        .toOption
        .get

    assertEquals(
      GiftiReader.doubleData(doc.pointSet.get).left.map(_.message),
      Left("unsupported GIFTI encoding: ExternalFileBinary")
    )

  test("malformed external offsets are data-array errors"):
    val xml =
      surfaceXml("ExternalFileBinary", "", "ASCII", asciiTriangles, externalFile = Some("points.bin"))
        .replace("ExternalFileOffset=\"0\"", "ExternalFileOffset=\"bad\"")

    assertEquals(
      GiftiReader.parseString(xml).left.map(_.message),
      Left("invalid GIFTI DataArray: DataArray ExternalFileOffset must be an integer")
    )

  test("malformed label keys are document errors"):
    val xml = labelXml().replace("Key=\"1\"", "Key=\"left\"")

    assertEquals(
      GiftiReader.parseString(xml).left.map(_.message),
      Left("invalid GIFTI document: Label Key must be an integer")
    )

  private val asciiPointset: String =
    "0 0 0 1 0 0 0 1 0"

  private val asciiTriangles: String =
    "0 1 2"

  private def surfaceXml(
    pointEncoding: String,
    pointData: String,
    triangleEncoding: String,
    triangleData: String,
    externalFile: Option[String] = None
  ): String =
    val external =
      externalFile.fold("")(name => s"""ExternalFileName="$name" ExternalFileOffset="0"""")
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<GIFTI Version="1.0" NumberOfDataArrays="2">
       |  <MetaData>
       |    <MD><Name>AnatomicalStructurePrimary</Name><Value>CortexLeft</Value></MD>
       |  </MetaData>
       |  <DataArray Intent="NIFTI_INTENT_POINTSET" DataType="NIFTI_TYPE_FLOAT32"
       |             ArrayIndexingOrder="RowMajorOrder" Dimensionality="2"
       |             Dim0="3" Dim1="3" Encoding="$pointEncoding" Endian="LittleEndian" $external>
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
       |             Dim0="1" Dim1="3" Encoding="$triangleEncoding" Endian="LittleEndian">
       |    <Data>$triangleData</Data>
       |  </DataArray>
       |</GIFTI>
       |""".stripMargin

  private def labelXml(): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<GIFTI Version="1.0" NumberOfDataArrays="1">
       |  <LabelTable>
       |    <Label Key="1" Red="1" Green="0" Blue="0" Alpha="1">Positive</Label>
       |    <Label Key="2" Red="0.008" Green="1" Blue="1" Alpha="1">Negative</Label>
       |  </LabelTable>
       |  <DataArray Intent="NIFTI_INTENT_LABEL" DataType="NIFTI_TYPE_INT32"
       |             ArrayIndexingOrder="RowMajorOrder" Dimensionality="1"
       |             Dim0="4" Encoding="ASCII" Endian="LittleEndian">
       |    <Data>1 1 2 2</Data>
       |  </DataArray>
       |</GIFTI>
       |""".stripMargin

  private def float32Base64(values: Vector[Float]): String =
    Base64.getMimeEncoder.encodeToString(float32Bytes(values))

  private def int32Base64(values: Vector[Int]): String =
    Base64.getMimeEncoder.encodeToString(int32Bytes(values))

  private def float32Bytes(values: Vector[Float]): Array[Byte] =
    val buffer = ByteBuffer.allocate(values.length * 4).order(ByteOrder.LITTLE_ENDIAN)
    values.foreach(buffer.putFloat)
    buffer.array()

  private def int32Bytes(values: Vector[Int]): Array[Byte] =
    val buffer = ByteBuffer.allocate(values.length * 4).order(ByteOrder.LITTLE_ENDIAN)
    values.foreach(buffer.putInt)
    buffer.array()

  private def gzipBase64(bytes: Array[Byte]): String =
    val out = new ByteArrayOutputStream()
    val gzip = new GZIPOutputStream(out)
    gzip.write(bytes)
    gzip.close()
    Base64.getMimeEncoder.encodeToString(out.toByteArray)
