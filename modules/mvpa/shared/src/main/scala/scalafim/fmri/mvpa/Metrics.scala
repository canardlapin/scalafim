package scalafim.fmri.mvpa

final case class MetricVector private (names: Vector[String], values: Vector[Double]):
  require(names.nonEmpty, "metric vector must be non-empty")
  require(names.length == values.length, "metric names and values must have the same length")
  require(names.forall(_.nonEmpty), "metric names must be non-empty")

  def apply(name: String): Option[Double] =
    val idx = names.indexOf(name)
    if idx < 0 then None else Some(values(idx))

object MetricVector:
  def apply(values: (String, Double)*): MetricVector =
    from(values)

  def from(values: Seq[(String, Double)]): MetricVector =
    require(values.nonEmpty, "metric vector must be non-empty")
    new MetricVector(values.map(_._1.trim).toVector, values.map(_._2).toVector)
