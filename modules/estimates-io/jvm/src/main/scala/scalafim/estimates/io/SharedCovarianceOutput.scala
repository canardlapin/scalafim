package scalafim.estimates.io

import java.nio.charset.StandardCharsets.UTF_8
import scalafim.estimates.*

/** The only covariance coverage state is one entry per observation/pair. */
private[io] final class SharedCovarianceOutput(val product: ProductDescriptor, val observation: ObservationId):
  val values = new Array[Double](product.targets.width.toInt)
  val validity = Array.fill[Byte](values.length)(Validity.NotComputed.code)
  val coverage = new Array[Boolean](values.length)
  var remaining = values.length

  def table: SharedCovarianceTable =
    var ordinal = 0
    val axis = product.targets.estimands
    val rows = axis.indices.toVector.flatMap: first =>
      (first until axis.size).map: second =>
        val row = SharedCovarianceEntry(first, second, values(ordinal), Validity.fromCode(validity(ordinal)).toOption.get)
        ordinal += 1
        row
    SharedCovarianceTable(product.id, observation, axis, rows)

private[io] object SharedCovarianceOutput:
  /** Conservative UTF-8 serialization reservation, checked before pair allocation.
    * Each bounded row includes decimal indices, a finite Double and one code.
    */
  def reservedBytes(product: ProductDescriptor, observation: ObservationId): Long =
    val header = ujson.write(ujson.Obj("Schema" -> SharedCovarianceTable.schema, "WireVersion" -> SharedCovarianceTable.wireVersion,
      "Product" -> product.id.value, "Observation" -> observation.value,
      "Estimands" -> ujson.Arr.from(product.targets.estimands.map(_.value)), "Precision" -> "Float64",
      "ValidityBroadcast" -> "SupportedSamples", "Pairs" -> ujson.Arr()), indent = 2).getBytes(UTF_8).length
    header.toLong + 128L * product.targets.width + 1L
