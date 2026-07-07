package scalafim.surface.io

import scalafim.image.DMat
import scalafim.surface.*
import scalafim.surface.gifti.*

import java.nio.file.Path
import scala.util.control.NonFatal

object GiftiSurfaceReader:

  def read(path: Path): SurfaceGeometry =
    read(path, FreeSurferSurfaceReader.inferHemisphere(path), FreeSurferSurfaceReader.inferKind(path))

  def readEither(path: Path): Either[SurfaceError, SurfaceGeometry] =
    catchRead(path)(read(path))

  def read(path: Path, hemisphere: Hemisphere, kind: SurfaceKind): SurfaceGeometry =
    unsafe {
      GiftiReader.read(path).flatMap(geometry(_, hemisphere, kind))
    }

  def readEither(path: Path, hemisphere: Hemisphere, kind: SurfaceKind): Either[SurfaceError, SurfaceGeometry] =
    catchRead(path)(read(path, hemisphere, kind))

  def readLabels(path: Path, geometry: SurfaceGeometry, label: String = ""): LabeledSurface =
    unsafe {
      GiftiReader.read(path).flatMap(labeledSurface(_, geometry, label))
    }

  def readLabelsEither(path: Path, geometry: SurfaceGeometry, label: String = ""): Either[SurfaceError, LabeledSurface] =
    catchRead(path)(readLabels(path, geometry, label))

  def geometry(document: GiftiDocument, hemisphere: Hemisphere, kind: SurfaceKind): Either[GiftiError, SurfaceGeometry] =
    for
      pointset <- document.pointSet.toRight(GiftiError.MissingDataArray(GiftiIntent.PointSet))
      triangles <- document.triangles.toRight(GiftiError.MissingDataArray(GiftiIntent.Triangle))
      _ <- requireDataArrayColumns(pointset, 3, "GIFTI POINTSET array must have Dim1=3")
      _ <- requireDataArrayColumns(triangles, 3, "GIFTI TRIANGLE array must have Dim1=3")
      coordinates <- GiftiReader.doubleMatrix(pointset)
      faces <- GiftiReader.intMatrix(triangles)
      _ <- requireColumns(coordinates, 3, "GIFTI POINTSET array must have Dim1=3")
      _ <- requireColumns(faces, 3, "GIFTI TRIANGLE array must have Dim1=3")
      transform <- transformMatrix(pointset)
      surface <- buildGeometry(coordinates, faces, hemisphere, kind, transform)
    yield surface

  def labeledSurface(document: GiftiDocument, geometry: SurfaceGeometry, label: String = ""): Either[GiftiError, LabeledSurface] =
    for
      labelArray <- document.labels.toRight(GiftiError.MissingDataArray(GiftiIntent.Label))
      labels <- GiftiReader.intVector(labelArray)
      indices <- labelIndices(document, labels.length, geometry.vertexCount)
      surface <- buildLabels(document, geometry, indices, labels, label)
    yield surface

  private def requireDataArrayColumns(array: GiftiDataArray, columns: Int, message: String): Either[GiftiError, Unit] =
    if array.rank == 2 && array.columns == columns then Right(())
    else Left(GiftiError.InvalidDataArray(message))

  private def requireColumns[A](payload: GiftiMatrix[A], columns: Int, message: String): Either[GiftiError, Unit] =
    if payload.columns == columns then Right(())
    else Left(GiftiError.InvalidDataArray(message))

  private def transformMatrix(pointset: GiftiDataArray): Either[GiftiError, DMat] =
    pointset.transforms.headOption match
      case None => Right(DMat.eye(4))
      case Some(transform) =>
        val values = transform.matrixData
        try Right(DMat.fromRows(Vector.tabulate(4)(row => Vector.tabulate(4)(col => values(row * 4 + col)))))
        catch case NonFatal(error) => Left(GiftiError.InvalidDataArray(error.getMessage))

  private def buildGeometry(
    coordinates: GiftiMatrix[Double],
    faces: GiftiMatrix[Int],
    hemisphere: Hemisphere,
    kind: SurfaceKind,
    transform: DMat
  ): Either[GiftiError, SurfaceGeometry] =
    try Right(SurfaceGeometry(TriangleMesh.fromArrays(coordinates.rowMajorValues.toArray, faces.rowMajorValues.toArray), hemisphere, kind, transform))
    catch case NonFatal(error) => Left(GiftiError.InvalidDataArray(error.getMessage))

  private def labelIndices(document: GiftiDocument, nLabels: Int, vertexCount: Int): Either[GiftiError, Vector[VertexId]] =
    document.nodeIndices match
      case Some(nodeArray) =>
        GiftiReader.intVector(nodeArray).flatMap { indices =>
          if indices.length != nLabels then
            Left(GiftiError.InvalidDataArray("GIFTI NODE_INDEX length must match LABEL data length"))
          else buildVertexIds(indices.values)
        }
      case None =>
        if nLabels != vertexCount then
          Left(GiftiError.InvalidDataArray("GIFTI LABEL data length must match geometry vertex count when NODE_INDEX is absent"))
        else Right(Vector.tabulate(nLabels)(VertexId.apply))

  private def buildVertexIds(indices: Vector[Int]): Either[GiftiError, Vector[VertexId]] =
    try Right(indices.map(VertexId.apply))
    catch case NonFatal(error) => Left(GiftiError.InvalidDataArray(error.getMessage))

  private def buildLabels(
    document: GiftiDocument,
    geometry: SurfaceGeometry,
    indices: Vector[VertexId],
    labels: GiftiVector[Int],
    label: String
  ): Either[GiftiError, LabeledSurface] =
    val tableById =
      document.labelTable.map { entry =>
        entry.key -> LabelInfo(entry.key, entry.name, entry.colorHex)
      }.toMap
    val table =
      labels.values.distinct.sorted.map { id =>
        tableById.getOrElse(id, LabelInfo(id, id.toString))
      }
    try Right(LabeledSurface.fromIndexed(geometry, indices, labels.values, table, label))
    catch case NonFatal(error) => Left(GiftiError.InvalidDataArray(error.getMessage))

  private def unsafe[A](result: Either[GiftiError, A]): A =
    result.fold(error => throw new IllegalArgumentException(error.message), identity)

  private def catchRead[A](path: Path)(body: => A): Either[SurfaceError, A] =
    try Right(body)
    catch case NonFatal(error) => Left(SurfaceError.ReadFailure(path.toString, SurfaceError.reason(error)))
