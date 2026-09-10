package scalafim.estimates.io

import scalafim.estimates.*

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
    pairOrder: Vector[EstimandPair] = Vector.empty
):
  require(values.path.endsWith(".nii") && validity.path.endsWith(".nii"), "bounded local representation requires uncompressed .nii")
  require(slope.isFinite && intercept.isFinite)
  require(volumeOrder.nonEmpty && volumeOrder.distinct.size == volumeOrder.size)
  require(pairOrder.isEmpty || pairOrder == volumeOrder.indices.toVector.flatMap(i => (i until volumeOrder.size).map(j => EstimandPair(volumeOrder(i), volumeOrder(j)))))
  require(selectedTransform == "scanner-sform", "only the qualified explicit scanner sform binding is admitted")
