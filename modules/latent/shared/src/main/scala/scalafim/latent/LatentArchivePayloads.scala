package scalafim.latent

import scalafim.archive.{ArchiveError, ArchivePath}
import scalafim.archive.lna.{DatasetRole, LnaArchive, Payload, TemporalDctNorm, TransformDescriptor}
import scalafim.image.DMat
import scalafim.linalg.{CsrMatrix, DoubleMatrix, LinearMap}

private[latent] object LatentArchivePayloads:
  def byRole(desc: TransformDescriptor, role: DatasetRole): Either[ArchiveError, ArchivePath] =
    desc.datasets
      .find(_.role == role)
      .map(_.path)
      .toRight(ArchiveError.InvalidArchive(s"${desc.name} descriptor missing ${role.value} dataset"))

  def doubleMatrixPayload(
      archive: LnaArchive,
      path: ArchivePath,
      label: String
  ): Either[ArchiveError, DMat] =
    archive.payload(path) match
      case Some(Payload.DoubleMatrix(data, _)) => Right(data)
      case Some(_)                             => Left(ArchiveError.ShapeMismatch(s"$label payload is not a double matrix"))
      case None                                => Left(ArchiveError.MissingPayload(path))

  def optionalDoubleMatrix(
      archive: LnaArchive,
      desc: TransformDescriptor,
      role: DatasetRole,
      label: String
  ): Either[ArchiveError, Option[DMat]] =
    desc.datasets.find(_.role == role) match
      case None =>
        Right(None)
      case Some(ref) =>
        doubleMatrixPayload(archive, ref.path, label).map(Some(_))

  def intMatrixPayload(
      archive: LnaArchive,
      path: ArchivePath,
      label: String
  ): Either[ArchiveError, Payload.IntMatrix] =
    archive.payload(path) match
      case Some(payload: Payload.IntMatrix) => Right(payload)
      case Some(_)                          => Left(ArchiveError.ShapeMismatch(s"$label payload is not an integer matrix"))
      case None                             => Left(ArchiveError.MissingPayload(path))

  def doubleVectorPayload(
      archive: LnaArchive,
      path: ArchivePath,
      label: String
  ): Either[ArchiveError, Vector[Double]] =
    archive.payload(path) match
      case Some(Payload.DoubleVector(values, _)) => Right(values)
      case Some(_)                               => Left(ArchiveError.ShapeMismatch(s"$label payload is not a double vector"))
      case None                                  => Left(ArchiveError.MissingPayload(path))

  def optionalOffset(
      archive: LnaArchive,
      desc: TransformDescriptor,
      role: DatasetRole,
      label: String
  ): Either[ArchiveError, Option[Vector[Double]]] =
    desc.datasets.find(_.role == role) match
      case None => Right(None)
      case Some(ref) =>
        archive.payload(ref.path) match
          case Some(Payload.DoubleVector(values, _)) => Right(Some(values))
          case Some(_)                               => Left(ArchiveError.ShapeMismatch(s"$label payload is not a double vector"))
          case None                                  => Left(ArchiveError.MissingPayload(ref.path))

  def linearMapMatrix(map: LinearMap): Either[ArchiveError, DoubleMatrix] =
    map
      .forward(DoubleMatrix.eye(map.cols))
      .left
      .map(err => ArchiveError.InvalidArchive(err.message))

  def linearMapFromMatrix(matrix: DMat): Either[ArchiveError, LinearMap] =
    linearMapFromMatrix(toDoubleMatrix(matrix))

  def linearMapFromMatrix(matrix: DoubleMatrix): Either[ArchiveError, LinearMap] =
    val rows = scala.collection.mutable.ArrayBuffer.empty[Int]
    val cols = scala.collection.mutable.ArrayBuffer.empty[Int]
    val values = scala.collection.mutable.ArrayBuffer.empty[Double]
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        val value = matrix(row, col)
        if !value.isFinite then return Left(ArchiveError.InvalidArchive(s"linear map contains non-finite value at ${row},${col}"))
        if value != 0.0 then
          rows += row
          cols += col
          values += value
        col += 1
      row += 1
    CsrMatrix
      .fromTriplets(matrix.rows, matrix.cols, rows.toArray, cols.toArray, values.toArray)
      .left
      .map(err => ArchiveError.InvalidArchive(err.message))

  def toDMat(matrix: DoubleMatrix): DMat =
    DMat.fromRows(matrix.toRows)

  def toDoubleMatrix(matrix: DMat): DoubleMatrix =
    DoubleMatrix.fromRows(matrix.toRows)

  def archiveNorm(norm: DctNorm): TemporalDctNorm =
    norm match
      case DctNorm.Ortho => TemporalDctNorm.Ortho
      case DctNorm.None  => TemporalDctNorm.None

  def latentNorm(norm: TemporalDctNorm): DctNorm =
    norm match
      case TemporalDctNorm.Ortho => DctNorm.Ortho
      case TemporalDctNorm.None  => DctNorm.None

  def error(latentError: LatentError): ArchiveError =
    ArchiveError.InvalidArchive(latentError.message)

  def traverse[A, B](values: Iterable[A])(f: A => Either[ArchiveError, B]): Either[ArchiveError, Vector[B]] =
    val out = Vector.newBuilder[B]
    val it = values.iterator
    var failure = Option.empty[ArchiveError]
    while it.hasNext && failure.isEmpty do
      f(it.next()) match
        case Left(error)  => failure = Some(error)
        case Right(value) => out += value
    failure.fold(Right(out.result()))(Left(_))
