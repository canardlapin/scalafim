package scalafim.surface.io

import scalafim.image.DMat
import scalafim.surface.*
import scalafim.surface.gifti.*

import java.nio.file.Path
import scala.util.control.NonFatal

object GiftiSurfaceReader:

  def read(path: Path): SurfaceGeometry =
    read(path, FreeSurferSurfaceReader.inferHemisphere(path), FreeSurferSurfaceReader.inferKind(path))

  def read(path: Path, hemisphere: Hemisphere, kind: SurfaceKind): SurfaceGeometry =
    unsafe {
      GiftiReader.read(path).flatMap(geometry(_, hemisphere, kind))
    }

  def readLabels(path: Path, geometry: SurfaceGeometry, label: String = ""): LabeledSurface =
    unsafe {
      GiftiReader.read(path).flatMap(labeledSurface(_, geometry, label))
    }

  def geometry(document: GiftiDocument, hemisphere: Hemisphere, kind: SurfaceKind): Either[GiftiError, SurfaceGeometry] =
    for
      pointset <- document.pointSet.toRight(GiftiError.MissingDataArray(GiftiIntent.PointSet))
      triangles <- document.triangles.toRight(GiftiError.MissingDataArray(GiftiIntent.Triangle))
      _ <- requireTriple(pointset, "GIFTI POINTSET array must have Dim1=3")
      _ <- requireTriple(triangles, "GIFTI TRIANGLE array must have Dim1=3")
      coordinates <- GiftiReader.doubleData(pointset)
      faces <- GiftiReader.intData(triangles)
      transform <- transformMatrix(pointset)
      surface <- buildGeometry(coordinates, faces, hemisphere, kind, transform)
    yield surface

  def labeledSurface(document: GiftiDocument, geometry: SurfaceGeometry, label: String = ""): Either[GiftiError, LabeledSurface] =
    for
      labelArray <- document.labels.toRight(GiftiError.MissingDataArray(GiftiIntent.Label))
      labels <- GiftiReader.intData(labelArray)
      indices <- labelIndices(document, labels.length, geometry.vertexCount)
      surface <- buildLabels(document, geometry, indices, labels, label)
    yield surface

  private def requireTriple(array: GiftiDataArray, message: String): Either[GiftiError, Unit] =
    if array.isTriple then Right(())
    else Left(GiftiError.InvalidDataArray(message))

  private def transformMatrix(pointset: GiftiDataArray): Either[GiftiError, DMat] =
    pointset.transforms.headOption match
      case None => Right(DMat.eye(4))
      case Some(transform) =>
        val values = transform.matrixData
        try Right(DMat.fromRows(Vector.tabulate(4)(row => Vector.tabulate(4)(col => values(row * 4 + col)))))
        catch case NonFatal(error) => Left(GiftiError.InvalidDataArray(error.getMessage))

  private def buildGeometry(
    coordinates: Array[Double],
    faces: Array[Int],
    hemisphere: Hemisphere,
    kind: SurfaceKind,
    transform: DMat
  ): Either[GiftiError, SurfaceGeometry] =
    try Right(SurfaceGeometry(TriangleMesh.fromArrays(coordinates, faces), hemisphere, kind, transform))
    catch case NonFatal(error) => Left(GiftiError.InvalidDataArray(error.getMessage))

  private def labelIndices(document: GiftiDocument, nLabels: Int, vertexCount: Int): Either[GiftiError, Vector[VertexId]] =
    document.nodeIndices match
      case Some(nodeArray) =>
        GiftiReader.intData(nodeArray).flatMap { indices =>
          if indices.length != nLabels then
            Left(GiftiError.InvalidDataArray("GIFTI NODE_INDEX length must match LABEL data length"))
          else buildVertexIds(indices)
        }
      case None =>
        if nLabels != vertexCount then
          Left(GiftiError.InvalidDataArray("GIFTI LABEL data length must match geometry vertex count when NODE_INDEX is absent"))
        else Right(Vector.tabulate(nLabels)(VertexId.apply))

  private def buildVertexIds(indices: Array[Int]): Either[GiftiError, Vector[VertexId]] =
    try Right(indices.toVector.map(VertexId.apply))
    catch case NonFatal(error) => Left(GiftiError.InvalidDataArray(error.getMessage))

  private def buildLabels(
    document: GiftiDocument,
    geometry: SurfaceGeometry,
    indices: Vector[VertexId],
    labels: Array[Int],
    label: String
  ): Either[GiftiError, LabeledSurface] =
    val tableById =
      document.labelTable.map { entry =>
        entry.key -> LabelInfo(entry.key, entry.name, entry.colorHex)
      }.toMap
    val table =
      labels.distinct.sorted.toVector.map { id =>
        tableById.getOrElse(id, LabelInfo(id, id.toString))
      }
    try Right(LabeledSurface.fromIndexed(geometry, indices, labels.toVector, table, label))
    catch case NonFatal(error) => Left(GiftiError.InvalidDataArray(error.getMessage))

  private def unsafe[A](result: Either[GiftiError, A]): A =
    result.fold(error => throw new IllegalArgumentException(error.message), identity)
