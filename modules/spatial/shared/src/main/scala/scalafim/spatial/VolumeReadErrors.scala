package scalafim.spatial

import neurosurf4s.routing.*

import neuroim4s.spatial.VolumeReadError

/** Attach functional source identity to the anatomical reader's typed refusal. */
private[spatial] object VolumeReadErrors:
  def apply(source: FieldSourceId)(error: VolumeReadError): SpatialError = error match
    case VolumeReadError.Unavailable(reason) => SpatialError.FieldSourceUnavailable(source, reason)
    case VolumeReadError.Stale(expected, actual) => SpatialError.FieldSourceStale(source, expected, actual)
    case VolumeReadError.ReadFailed(reason) => SpatialError.FieldSourceReadFailed(source, reason)
    case VolumeReadError.Admission(cause) => SpatialError.FieldSourceSampleSpaceAdmission(source, cause)
    case VolumeReadError.GridMismatch(cause) => SpatialError.FieldSourceGridMismatch(source, cause)
    case VolumeReadError.ShapeMismatch(expected, actual) => SpatialError.FieldSourceShapeMismatch(source, expected, actual)
    case VolumeReadError.ObservationMismatch(expected, actual) => SpatialError.FieldObservationMismatch(expected, actual)
    case VolumeReadError.IndexOutOfBounds(axis, index, limit) => SpatialError.FieldSourceIndexOutOfBounds(axis, index, limit)
    case VolumeReadError.DuplicateIndex(axis, index) => SpatialError.DuplicateFieldSourceIndex(axis, index)
    case VolumeReadError.BlockShapeMismatch(er, ar, ec, ac) => SpatialError.FieldSourceBlockShapeMismatch(source, er, ar, ec, ac)
    case VolumeReadError.RequestMismatch => SpatialError.FieldSourceRequestMismatch(source)
