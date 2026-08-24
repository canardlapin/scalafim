package scalafim.surface.io

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

  def readEither(
    path: Path,
    hemisphere: Hemisphere,
    kind: SurfaceKind
  ): Either[SurfaceError, SurfaceGeometry] =
    catchRead(path)(read(path, hemisphere, kind))

  def readLabels(
    path: Path,
    geometry: SurfaceGeometry,
    label: String = ""
  ): LabeledSurface =
    unsafe {
      GiftiReader.read(path).flatMap(labeledSurface(_, geometry, label))
    }

  def readLabelsEither(
    path: Path,
    geometry: SurfaceGeometry,
    label: String = ""
  ): Either[SurfaceError, LabeledSurface] =
    catchRead(path)(readLabels(path, geometry, label))

  def geometry(
    document: GiftiDocument,
    hemisphere: Hemisphere,
    kind: SurfaceKind
  ): Either[GiftiError, SurfaceGeometry] =
    for
      arrays <- GiftiSurfaceCodec.geometryArrays(document)
      coordinates <- GiftiReader.doubleMatrix(arrays.pointSet)
      faces <- GiftiReader.intMatrix(arrays.triangles)
      surface <- GiftiSurfaceCodec.geometry(coordinates, faces, hemisphere, kind)
    yield surface

  def labeledSurface(
    document: GiftiDocument,
    geometry: SurfaceGeometry,
    label: String = ""
  ): Either[GiftiError, LabeledSurface] =
    for
      arrays <- GiftiSurfaceCodec.labelArrays(document)
      labels <- GiftiReader.intVector(arrays.labels)
      nodeIndices <- decodeNodeIndices(arrays.nodeIndices)
      surface <- GiftiSurfaceCodec.labeledSurface(document, geometry, labels, nodeIndices, label)
    yield surface

  private def decodeNodeIndices(
    array: Option[GiftiDataArray]
  ): Either[GiftiError, Option[GiftiVector[Int]]] =
    array match
      case Some(found) => GiftiReader.intVector(found).map(Some.apply)
      case None => Right(None)

  private def unsafe[A](result: Either[GiftiError, A]): A =
    result.fold(error => throw new IllegalArgumentException(error.message), identity)

  private def catchRead[A](path: Path)(body: => A): Either[SurfaceError, A] =
    try Right(body)
    catch case NonFatal(error) => Left(SurfaceError.ReadFailure(path.toString, SurfaceError.reason(error)))
