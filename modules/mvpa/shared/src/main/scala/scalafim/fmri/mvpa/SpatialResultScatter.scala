package scalafim.fmri.mvpa

import scalafim.locus.FiniteDomain
import scalafim.locus.IndexedField
import scalafim.locus.IndexedFieldError
import scalafim.locus.ordinal

enum SpatialScatterError:
  case ResultCountMismatch(expected: Int, actual: Int)
  case CenterOutOfBounds(
      measurement: MeasurementId,
      centerPosition: Int,
      size: Int
  )
  case DuplicateCenter(
      centerPosition: Int,
      first: MeasurementId,
      second: MeasurementId
  )
  case MissingCenter(centerPosition: Int)
  case Field(error: IndexedFieldError)

  def message: String =
    this match
      case ResultCountMismatch(expected, actual) =>
        s"searchlight result contains $actual measurements, expected $expected center positions"
      case CenterOutOfBounds(measurement, centerPosition, size) =>
        s"measurement '${measurement.value}' renders to center $centerPosition outside [0, $size)"
      case DuplicateCenter(centerPosition, first, second) =>
        s"center $centerPosition is rendered by both '${first.value}' and '${second.value}'"
      case MissingCenter(centerPosition) =>
        s"searchlight result contains no rendition for center $centerPosition"
      case Field(error) => error.message

/** Spatial rendering is a consumer of typed rendition metadata. The generic measurement and result models know nothing
  * about centers or searchlights; this domain adapter restores compact center-domain order while retaining each local
  * success, rejection, failure, and execution receipt.
  */
object SpatialResultScatter:
  def searchlights[A, Rejection, Failure, C, S](
      centers: FiniteDomain[C],
      result: AnalysisResult[
        A,
        Rejection,
        Failure,
        SearchlightRendition[C, S]
      ]
  ): Either[
    SpatialScatterError,
    IndexedField[C, MeasurementOutcome[A, Rejection, Failure]]
  ] =
    if result.values.length != centers.size then
      Left(
        SpatialScatterError.ResultCountMismatch(
          centers.size,
          result.values.length
        )
      )
    else
      val byCenter = scala.collection.mutable.HashMap.empty[
        Int,
        (MeasurementId, MeasurementOutcome[A, Rejection, Failure])
      ]
      val iterator = result.values.iterator
      while iterator.hasNext do
        val value = iterator.next()
        val ordinal = value.rendition.center.ordinal
        if ordinal < 0 || ordinal >= centers.size then
          return Left(
            SpatialScatterError.CenterOutOfBounds(
              value.measurement.id,
              ordinal,
              centers.size
            )
          )
        byCenter.get(ordinal) match
          case Some((first, _)) =>
            return Left(
              SpatialScatterError.DuplicateCenter(
                ordinal,
                first,
                value.measurement.id
              )
            )
          case None =>
            byCenter.update(ordinal, value.measurement.id -> value.outcome)

      val ordered = Vector.newBuilder[MeasurementOutcome[A, Rejection, Failure]]
      var ordinal = 0
      while ordinal < centers.size do
        byCenter.get(ordinal) match
          case None               => return Left(SpatialScatterError.MissingCenter(ordinal))
          case Some((_, outcome)) => ordered += outcome
        ordinal += 1
      IndexedField
        .fromValues(centers, ordered.result())
        .left
        .map(SpatialScatterError.Field.apply)
