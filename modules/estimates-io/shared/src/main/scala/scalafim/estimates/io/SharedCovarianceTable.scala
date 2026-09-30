package scalafim.estimates.io

import scalafim.estimates.*
import scala.util.control.NonFatal

final case class SharedCovarianceEntry(first: Int, second: Int, value: Double, validity: Validity):
  require(first >= 0 && second >= first)
  require(value.isFinite && (first != second || value >= 0.0))
  require(validity != Validity.OutsideSupport)

final case class SharedCovarianceTable(
    product: ProductId,
    observation: ObservationId,
    estimands: Vector[EstimandId],
    entries: Vector[SharedCovarianceEntry]
):
  require(estimands.nonEmpty && estimands.distinct.size == estimands.size)
  require(entries.size.toLong == estimands.size.toLong * (estimands.size.toLong + 1L) / 2L)
  private val ordered =
    var ordinal = 0
    var first = 0
    var valid = true
    while first < estimands.size do
      var second = first
      while second < estimands.size do
        valid &&= entries(ordinal).first == first && entries(ordinal).second == second
        ordinal += 1
        second += 1
      first += 1
    valid
  require(ordered, "table must contain the exact ordered upper triangle")

  def agrees(other: SharedCovarianceTable): Boolean =
    estimands == other.estimands && entries.zip(other.entries).forall: (a, b) =>
      a.first == b.first && a.second == b.second && a.validity == b.validity &&
        java.lang.Double.doubleToLongBits(a.value) == java.lang.Double.doubleToLongBits(b.value)

  def validate(representation: SharedCovarianceRepresentation): Either[EstimateError, Unit] =
    if product == representation.product && observation == representation.observation && estimands == representation.estimands then Right(())
    else Left(EstimateError.Integrity("shared table identity or ordered estimands disagree with its representation"))

object SharedCovarianceTable:
  val schema = "scalafim-estimates-shared-normalized-upper-triangle-1"
  val wireVersion = "1.0.0"

  def encode(table: SharedCovarianceTable): String =
    ujson.write(ujson.Obj("Schema" -> schema, "WireVersion" -> wireVersion,
      "Product" -> table.product.value, "Observation" -> table.observation.value,
      "Estimands" -> ujson.Arr.from(table.estimands.map(_.value)), "Precision" -> "Float64",
      "ValidityBroadcast" -> "SupportedSamples",
      "Pairs" -> ujson.Arr.from(table.entries.map(row => ujson.Obj(
        "First" -> row.first, "Second" -> row.second, "Value" -> row.value, "Validity" -> row.validity.code.toInt)))), indent = 2) + "\n"

  def decode(text: String, maximumPairs: Int): Either[EstimateError, SharedCovarianceTable] =
    try
      val value = ujson.read(text)
      require(value.obj.keySet == Set("Schema", "WireVersion", "Product", "Observation", "Estimands", "Precision", "ValidityBroadcast", "Pairs"))
      require(value("Schema").str == schema && value("WireVersion").str == wireVersion)
      require(value("Precision").str == "Float64" && value("ValidityBroadcast").str == "SupportedSamples")
      val axis = value("Estimands").arr.toVector.map(v => EstimandId(v.str))
      val expected = axis.size.toLong * (axis.size.toLong + 1L) / 2L
      require(expected <= maximumPairs && value("Pairs").arr.size.toLong == expected, "shared table pair budget or coverage differs")
      def integer(v: ujson.Value): Int =
        val n = v.num
        require(n.isFinite && n >= 0 && n <= Int.MaxValue && n == math.floor(n))
        n.toInt
      val entries = value("Pairs").arr.toVector.map: row =>
        require(row.obj.keySet == Set("First", "Second", "Value", "Validity"))
        val code = integer(row("Validity"))
        require(code <= 5)
        val status = Validity.fromCode(code.toByte).fold(e => throw new IllegalArgumentException(e.message), identity)
        SharedCovarianceEntry(integer(row("First")), integer(row("Second")), row("Value").num, status)
      Right(SharedCovarianceTable(ProductId(value("Product").str), ObservationId(value("Observation").str), axis, entries))
    catch case NonFatal(error) => Left(EstimateError.Integrity(Option(error.getMessage).getOrElse("invalid shared covariance table")))
