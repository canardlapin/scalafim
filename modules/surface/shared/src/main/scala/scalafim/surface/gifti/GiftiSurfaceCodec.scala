package scalafim.surface.gifti

import scalafim.image.DMat
import scalafim.surface.*

import scala.util.control.NonFatal

private[surface] object GiftiSurfaceCodec:

  final case class GeometryArrays(
    pointSet: GiftiDataArray,
    triangles: GiftiDataArray
  )

  final case class LabelArrays(
    labels: GiftiDataArray,
    nodeIndices: Option[GiftiDataArray]
  )

  def geometryArrays(document: GiftiDocument): Either[GiftiError, GeometryArrays] =
    for
      pointSet <- document.pointSet.toRight(GiftiError.MissingDataArray(GiftiIntent.PointSet))
      triangles <- document.triangles.toRight(GiftiError.MissingDataArray(GiftiIntent.Triangle))
      _ <- requireDataArrayColumns(pointSet, 3, "GIFTI POINTSET array must have Dim1=3")
      _ <- requireDataArrayColumns(triangles, 3, "GIFTI TRIANGLE array must have Dim1=3")
    yield GeometryArrays(pointSet, triangles)

  def labelArrays(document: GiftiDocument): Either[GiftiError, LabelArrays] =
    document.labels
      .toRight(GiftiError.MissingDataArray(GiftiIntent.Label))
      .map(LabelArrays(_, document.nodeIndices))

  def geometry(
    coordinates: GiftiMatrix[Double],
    faces: GiftiMatrix[Int],
    hemisphere: Hemisphere,
    kind: SurfaceKind
  ): Either[GiftiError, SurfaceGeometry] =
    for
      _ <- requireColumns(coordinates, 3, "GIFTI POINTSET array must have Dim1=3")
      _ <- requireColumns(faces, 3, "GIFTI TRIANGLE array must have Dim1=3")
      transform <- transformMatrix(coordinates.array)
      surface <- buildGeometry(coordinates, faces, hemisphere, kind, transform)
    yield surface

  def labeledSurface(
    document: GiftiDocument,
    geometry: SurfaceGeometry,
    labels: GiftiVector[Int],
    nodeIndices: Option[GiftiVector[Int]],
    label: String
  ): Either[GiftiError, LabeledSurface] =
    for
      indices <- labelIndices(
        hasNodeIndices = document.nodeIndices.nonEmpty,
        decodedNodeIndices = nodeIndices,
        nLabels = labels.length,
        vertexCount = geometry.vertexCount
      )
      surface <- buildLabels(document, geometry, indices, labels, label)
    yield surface

  private def requireDataArrayColumns(
    array: GiftiDataArray,
    columns: Int,
    message: String
  ): Either[GiftiError, Unit] =
    if array.rank == 2 && array.columns == columns then Right(())
    else Left(GiftiError.InvalidDataArray(message))

  private def requireColumns[A](
    payload: GiftiMatrix[A],
    columns: Int,
    message: String
  ): Either[GiftiError, Unit] =
    if payload.columns == columns then Right(())
    else Left(GiftiError.InvalidDataArray(message))

  private def transformMatrix(pointSet: GiftiDataArray): Either[GiftiError, DMat] =
    pointSet.transforms.headOption match
      case None => Right(DMat.eye(4))
      case Some(transform) =>
        val values = transform.matrixData
        try
          Right(
            DMat.fromRows(
              Vector.tabulate(4)(row => Vector.tabulate(4)(column => values(row * 4 + column)))
            )
          )
        catch case NonFatal(error) => Left(GiftiError.InvalidDataArray(error.getMessage))

  private def buildGeometry(
    coordinates: GiftiMatrix[Double],
    faces: GiftiMatrix[Int],
    hemisphere: Hemisphere,
    kind: SurfaceKind,
    transform: DMat
  ): Either[GiftiError, SurfaceGeometry] =
    try
      Right(
        SurfaceGeometry(
          TriangleMesh.fromArrays(
            coordinates.rowMajorValues.toArray,
            faces.rowMajorValues.toArray
          ),
          hemisphere,
          kind,
          transform
        )
      )
    catch case NonFatal(error) => Left(GiftiError.InvalidDataArray(error.getMessage))

  private def labelIndices(
    hasNodeIndices: Boolean,
    decodedNodeIndices: Option[GiftiVector[Int]],
    nLabels: Int,
    vertexCount: Int
  ): Either[GiftiError, Vector[VertexId]] =
    if hasNodeIndices then
      decodedNodeIndices match
        case Some(indices) =>
          if indices.length != nLabels then
            Left(GiftiError.InvalidDataArray("GIFTI NODE_INDEX length must match LABEL data length"))
          else buildVertexIds(indices.values)
        case None =>
          Left(GiftiError.InvalidDataArray("GIFTI NODE_INDEX data were not decoded"))
    else if nLabels != vertexCount then
      Left(
        GiftiError.InvalidDataArray(
          "GIFTI LABEL data length must match geometry vertex count when NODE_INDEX is absent"
        )
      )
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
