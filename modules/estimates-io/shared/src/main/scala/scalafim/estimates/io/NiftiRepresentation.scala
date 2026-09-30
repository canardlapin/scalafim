package scalafim.estimates.io

import scalafim.estimates.*

enum NiftiStoredDatatype(val code: Int):
  case Float32 extends NiftiStoredDatatype(16)
  case Float64 extends NiftiStoredDatatype(64)

/** One observation per file; fourth-axis order is the product's target order.
  * The validity file has exactly the same shape and uses core uint8 codes.
  */
final case class NiftiRepresentation(
    product: ProductId,
    observation: ObservationId,
    values: FileReference,
    validity: FileReference,
    precision: NumericPrecision,
    slope: Double,
    intercept: Double,
    volumeOrder: Vector[EstimandId],
    selectedTransform: String,
    pairOrder: Vector[EstimandPair] = Vector.empty,
    qformAlternativeFrame: Option[String] = None,
    storedDatatype: Option[NiftiStoredDatatype] = None
):
  require((values.path.endsWith(".nii") || values.path.endsWith(".nii.gz")) &&
    (validity.path.endsWith(".nii") || validity.path.endsWith(".nii.gz")), "Core-NIfTI requires .nii or .nii.gz files")
  require(slope.isFinite && intercept.isFinite)
  require(volumeOrder.nonEmpty && volumeOrder.distinct.size == volumeOrder.size)
  require(pairOrder.isEmpty || pairOrder == volumeOrder.indices.toVector.flatMap(i => (i until volumeOrder.size).map(j => EstimandPair(volumeOrder(i), volumeOrder(j)))))
  require(selectedTransform == "scanner-sform", "only the qualified explicit scanner sform binding is admitted")
  require(qformAlternativeFrame.forall(frame => frame.nonEmpty && frame == frame.trim), "alternate qform frame must be a named frame")
