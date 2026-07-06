package scalafim.surface.io

import scalafim.image.DMat
import scalafim.surface.Hemisphere
import scalafim.surface.SurfaceGeometry
import scalafim.surface.SurfaceKind
import scalafim.surface.TriangleMesh

import org.w3c.dom.Element
import org.w3c.dom.Node

import java.io.InputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import javax.xml.parsers.DocumentBuilderFactory

object GiftiSurfaceReader:

  def read(path: Path): SurfaceGeometry =
    read(path, FreeSurferSurfaceReader.inferHemisphere(path), FreeSurferSurfaceReader.inferKind(path))

  def read(path: Path, hemisphere: Hemisphere, kind: SurfaceKind): SurfaceGeometry =
    val doc = parseXml(path)
    val arrays = dataArrays(doc.getDocumentElement)
    val pointset =
      arrays.find(array => hasIntent(array, "POINTSET")).getOrElse {
        throw new IllegalArgumentException("GIFTI surface file must contain a POINTSET DataArray")
      }
    val triangle =
      arrays.find(array => hasIntent(array, "TRIANGLE")).getOrElse {
        throw new IllegalArgumentException("GIFTI surface file must contain a TRIANGLE DataArray")
      }

    val coordinates = readPointset(pointset)
    val faces = readTriangles(triangle)
    val transform = transformMatrix(pointset).getOrElse(DMat.eye(4))
    SurfaceGeometry(TriangleMesh.fromArrays(coordinates, faces), hemisphere, kind, transform)

  private def parseXml(path: Path): org.w3c.dom.Document =
    val factory = DocumentBuilderFactory.newInstance()
    factory.setNamespaceAware(false)
    val builder = factory.newDocumentBuilder()
    val input = open(path)
    try builder.parse(input)
    finally input.close()

  private def open(path: Path): InputStream =
    val base = Files.newInputStream(path)
    if path.getFileName.toString.endsWith(".gz") then new GZIPInputStream(base) else base

  private def dataArrays(root: Element): Vector[Element] =
    val nodes = root.getElementsByTagName("DataArray")
    Vector.tabulate(nodes.getLength)(i => nodes.item(i).asInstanceOf[Element])

  private def hasIntent(array: Element, label: String): Boolean =
    array.getAttribute("Intent").toUpperCase.contains(label)

  private def readPointset(array: Element): Array[Double] =
    val rows = dim(array, "Dim0", "GIFTI POINTSET Dim0")
    val cols = dim(array, "Dim1", "GIFTI POINTSET Dim1")
    require(cols == 3, "GIFTI POINTSET array must have Dim1=3")
    val values = numericData(array, "GIFTI POINTSET")
    require(values.length == rows * cols, "GIFTI POINTSET data length must equal Dim0*Dim1")
    values

  private def readTriangles(array: Element): Array[Int] =
    val rows = dim(array, "Dim0", "GIFTI TRIANGLE Dim0")
    val cols = dim(array, "Dim1", "GIFTI TRIANGLE Dim1")
    require(cols == 3, "GIFTI TRIANGLE array must have Dim1=3")
    val values = integerData(array, "GIFTI TRIANGLE")
    require(values.length == rows * cols, "GIFTI TRIANGLE data length must equal Dim0*Dim1")
    values

  private def transformMatrix(array: Element): Option[DMat] =
    childElement(array, "CoordinateSystemTransformMatrix").flatMap { xform =>
      childElement(xform, "MatrixData").map { matrixData =>
        val values = splitFields(matrixData.getTextContent).map(parseDouble(_, "GIFTI CoordinateSystemTransformMatrix"))
        require(values.length == 16, "GIFTI CoordinateSystemTransformMatrix must contain 16 values")
        DMat.fromRows(Vector.tabulate(4)(r => Vector.tabulate(4)(c => values(r * 4 + c))))
      }
    }

  private def numericData(array: Element, label: String): Array[Double] =
    ensureAsciiEncoding(array)
    splitFields(dataText(array, label)).map(parseDouble(_, label))

  private def integerData(array: Element, label: String): Array[Int] =
    ensureAsciiEncoding(array)
    splitFields(dataText(array, label)).map(parseInt(_, label))

  private def ensureAsciiEncoding(array: Element): Unit =
    val encoding = array.getAttribute("Encoding")
    require(
      encoding.isEmpty || encoding.equalsIgnoreCase("ASCII"),
      "GIFTI reader currently supports ASCII DataArray encoding only"
    )

  private def dataText(array: Element, label: String): String =
    childElement(array, "Data")
      .map(_.getTextContent)
      .getOrElse(throw new IllegalArgumentException(s"$label DataArray must contain Data"))

  private def dim(array: Element, name: String, label: String): Int =
    val value = array.getAttribute(name)
    require(value.nonEmpty, s"$label is required")
    parseInt(value, label)

  private def childElement(parent: Element, name: String): Option[Element] =
    var child = parent.getFirstChild
    while child != null do
      if child.getNodeType == Node.ELEMENT_NODE && child.getNodeName == name then
        return Some(child.asInstanceOf[Element])
      child = child.getNextSibling
    None

  private def splitFields(text: String): Array[String] =
    text.trim.split("\\s+").filter(_.nonEmpty)

  private def parseInt(value: String, label: String): Int =
    try value.toInt
    catch case _: NumberFormatException => throw new IllegalArgumentException(s"$label must contain integer values")

  private def parseDouble(value: String, label: String): Double =
    try value.toDouble
    catch case _: NumberFormatException => throw new IllegalArgumentException(s"$label must contain numeric values")
