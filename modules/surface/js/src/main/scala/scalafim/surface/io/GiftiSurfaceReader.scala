package scalafim.surface.io

import scalafim.surface.*
import scalafim.surface.gifti.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js.typedarray.Uint8Array

object GiftiSurfaceReader:

  def read(
    bytes: Uint8Array,
    hemisphere: Hemisphere,
    kind: SurfaceKind
  ): Future[Either[GiftiError, SurfaceGeometry]] =
    GiftiReader.read(bytes).flatMap {
      case Left(error) => Future.successful(Left(error))
      case Right(document) => geometry(document, hemisphere, kind)
    }

  def readString(
    xml: String,
    hemisphere: Hemisphere,
    kind: SurfaceKind
  ): Future[Either[GiftiError, SurfaceGeometry]] =
    GiftiReader.parseString(xml) match
      case Left(error) => Future.successful(Left(error))
      case Right(document) => geometry(document, hemisphere, kind)

  def geometry(
    document: GiftiDocument,
    hemisphere: Hemisphere,
    kind: SurfaceKind
  ): Future[Either[GiftiError, SurfaceGeometry]] =
    GiftiSurfaceCodec.geometryArrays(document) match
      case Left(error) => Future.successful(Left(error))
      case Right(arrays) =>
        GiftiReader
          .doubleMatrix(arrays.pointSet)
          .zip(GiftiReader.intMatrix(arrays.triangles))
          .map { case (coordinatesResult, facesResult) =>
            for
              coordinates <- coordinatesResult
              faces <- facesResult
              surface <- GiftiSurfaceCodec.geometry(coordinates, faces, hemisphere, kind)
            yield surface
          }

  def readLabels(
    bytes: Uint8Array,
    geometry: SurfaceGeometry,
    label: String = ""
  ): Future[Either[GiftiError, LabeledSurface]] =
    GiftiReader.read(bytes).flatMap {
      case Left(error) => Future.successful(Left(error))
      case Right(document) => labeledSurface(document, geometry, label)
    }

  def readLabelsString(
    xml: String,
    geometry: SurfaceGeometry,
    label: String = ""
  ): Future[Either[GiftiError, LabeledSurface]] =
    GiftiReader.parseString(xml) match
      case Left(error) => Future.successful(Left(error))
      case Right(document) => labeledSurface(document, geometry, label)

  def labeledSurface(
    document: GiftiDocument,
    geometry: SurfaceGeometry,
    label: String = ""
  ): Future[Either[GiftiError, LabeledSurface]] =
    GiftiSurfaceCodec.labelArrays(document) match
      case Left(error) => Future.successful(Left(error))
      case Right(arrays) =>
        val labels = GiftiReader.intVector(arrays.labels)
        val nodeIndices = decodeNodeIndices(arrays.nodeIndices)
        labels.zip(nodeIndices).map { case (labelsResult, nodeIndicesResult) =>
          for
            decodedLabels <- labelsResult
            decodedNodeIndices <- nodeIndicesResult
            surface <- GiftiSurfaceCodec.labeledSurface(
              document,
              geometry,
              decodedLabels,
              decodedNodeIndices,
              label
            )
          yield surface
        }

  private def decodeNodeIndices(
    array: Option[GiftiDataArray]
  ): Future[Either[GiftiError, Option[GiftiVector[Int]]]] =
    array match
      case Some(found) => GiftiReader.intVector(found).map(_.map(Some.apply))
      case None => Future.successful(Right(None))
