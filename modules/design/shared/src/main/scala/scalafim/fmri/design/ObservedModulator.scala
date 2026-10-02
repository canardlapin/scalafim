package scalafim.fmri.design

/** Pure within-partition preprocessing for one continuous modulator.
  * Non-finite inputs are missing. Values in the result are parallel to
  * `retainedIndices`; a drop policy therefore changes both vectors together.
  */
object ObservedModulator:
  enum Centering:
    case None, Global, ByCell

  enum Scaling:
    case Raw, ZScore, StandardDeviation

  enum Error:
    case CellLengthMismatch(values: Int, cells: Int)
    case MissingValues(indices: Vector[Int])
    case UnsupportedMissingPolicy(policy: MissingValuePolicy)
    case ZeroRequiresCentering
    case NonFiniteTransform(index: Int)
    case NonFiniteStatistic(name: String)

    def message: String = this match
      case CellLengthMismatch(values, cells) => s"modulator has $values values but $cells cell assignments"
      case MissingValues(indices) => s"modulator has missing values at input rows ${indices.mkString(", ")}"
      case UnsupportedMissingPolicy(policy) => s"missing-value policy '${policy.label}' is not supported by observed modulator preprocessing"
      case ZeroRequiresCentering => "zero contribution requires explicit centering unless z-score scaling supplies global centering"
      case NonFiniteTransform(index) => s"modulator transform became non-finite at input row $index"
      case NonFiniteStatistic(name) => s"modulator $name is non-finite"

  final case class GroupReceipt(cell: Option[CellKey], observedIndices: Vector[Int], mean: Option[Double])
  final case class Receipt(
      observedIndices: Vector[Int],
      observedMean: Option[Double],
      observedSampleStandardDeviation: Option[Double],
      scale: Double,
      degenerateScale: Boolean,
      groups: Vector[GroupReceipt]
  )
  final case class Result(values: Vector[Double], retainedIndices: Vector[Int], receipt: Receipt):
    require(values.length == retainedIndices.length, "transformed values must align with retained indices")

  def prepare(
      values: Vector[Double],
      cells: Vector[CellKey],
      centering: Centering,
      scaling: Scaling,
      missing: MissingValuePolicy
  ): Either[Error, Result] =
    if values.length != cells.length then Left(Error.CellLengthMismatch(values.length, cells.length))
    else missing match
      case MissingValuePolicy.ImputeConstant(_) => Left(Error.UnsupportedMissingPolicy(missing))
      case _ => prepareFinite(values, cells, centering, scaling, missing)

  private def prepareFinite(
      values: Vector[Double],
      cells: Vector[CellKey],
      centering: Centering,
      scaling: Scaling,
      missing: MissingValuePolicy
  ): Either[Error, Result] =
    val missingIndices = values.indices.filter(index => !values(index).isFinite).toVector
    if missing == MissingValuePolicy.Reject && missingIndices.nonEmpty then Left(Error.MissingValues(missingIndices))
    else if missing == MissingValuePolicy.ZeroContribution && centering == Centering.None && scaling != Scaling.ZScore then
      Left(Error.ZeroRequiresCentering)
    else
      val retained = missing match
        case MissingValuePolicy.DropFromTerm => values.indices.filter(index => values(index).isFinite).toVector
        case _ => values.indices.toVector
      val observed = retained.filter(index => values(index).isFinite)
      val centered = Array.fill(values.length)(0.0)
      observed.foreach(index => centered(index) = values(index))
      val effectiveCentering =
        if centering == Centering.None && scaling == Scaling.ZScore then Centering.Global
        else centering
      val groups = group(observed, cells, effectiveCentering)
      val calculatedGroups = groups.map { (cell, indices) =>
        val mean = if effectiveCentering == Centering.None then None else average(indices.map(centered))
        (cell, indices, mean)
      }
      if calculatedGroups.exists(_._3.exists(!_.isFinite)) then Left(Error.NonFiniteStatistic("centering mean"))
      else
        calculatedGroups.foreach { (_, indices, mean) =>
          mean.foreach { value => indices.foreach(index => centered(index) -= value) }
        }
        val transformed = observed.map(centered)
        val mean = average(transformed)
        val sampleSd = standardDeviation(transformed, mean)
        if mean.exists(!_.isFinite) then Left(Error.NonFiniteStatistic("observed mean"))
        else if sampleSd.exists(!_.isFinite) then Left(Error.NonFiniteStatistic("observed sample standard deviation"))
        else
          val needsScale = scaling != Scaling.Raw
          val degenerate = needsScale && sampleSd.forall(_ <= 0.0)
          val scale = if needsScale then sampleSd.filter(_ > 0.0).getOrElse(1.0) else 1.0
          val scaled = observed.map(index => index -> (centered(index) / scale))
          scaled.find((_, value) => !value.isFinite) match
            case Some((index, _)) => Left(Error.NonFiniteTransform(index))
            case None =>
              scaled.foreach { (index, value) => centered(index) = value }
              val output = retained.map { index =>
                if values(index).isFinite then centered(index) else 0.0
              }
              val receipts = calculatedGroups.map { (cell, indices, groupMean) =>
                GroupReceipt(cell, indices, groupMean)
              }
              Right(Result(output, retained, Receipt(observed, mean, sampleSd, scale, degenerate, receipts)))

  private def group(indices: Vector[Int], cells: Vector[CellKey], centering: Centering): Vector[(Option[CellKey], Vector[Int])] = centering match
    case Centering.None => Vector(None -> indices)
    case Centering.Global => Vector(None -> indices)
    case Centering.ByCell =>
      val grouped = scala.collection.mutable.LinkedHashMap.empty[CellKey, Vector[Int]]
      indices.foreach(index => grouped.updateWith(cells(index))(previous => Some(previous.getOrElse(Vector.empty) :+ index)))
      grouped.toVector.map((cell, rows) => Some(cell) -> rows)

  private def average(values: Vector[Double]): Option[Double] =
    if values.isEmpty then None else Some(values.sum / values.size.toDouble)

  private def standardDeviation(values: Vector[Double], mean: Option[Double]): Option[Double] =
    if values.isEmpty then None
    else if values.size == 1 then Some(0.0)
    else
      val center = mean.getOrElse(0.0)
      Some(math.sqrt(values.map(value => (value - center) * (value - center)).sum / (values.size - 1).toDouble))
