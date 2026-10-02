package scalafim.fmri.mvpa

import gale.linalg.DMat

enum RdmMethod:
  case SquaredEuclidean(normalizeByFeatures: Boolean = false)
  case Euclidean
  case Correlation

  def label: String =
    this match
      case SquaredEuclidean(false) => "squared_euclidean"
      case SquaredEuclidean(true) => "squared_euclidean_normalized"
      case Euclidean => "euclidean"
      case Correlation => "correlation"

  def compute(matrix: DMat): Either[MvpaError, RdmVector] =
    this match
      case SquaredEuclidean(normalizeByFeatures) =>
        Rdm.squaredEuclidean(matrix, normalizeByFeatures)
      case Euclidean =>
        Rdm.euclidean(matrix)
      case Correlation =>
        Rdm.correlation(matrix)

opaque type RsaItemId = String

object RsaItemId:
  def apply(value: String): Either[MvpaError, RsaItemId] =
    checkedRsaId("RSA item", value)

  def unsafe(value: String): RsaItemId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: RsaItemId)
    inline def value: String = id

final case class LabeledRdm private (
    items: Vector[RsaItemId],
    rdm: RdmVector
):
  require(items.length == rdm.items, "labeled RDM item count must match RDM item count")
  require(items.distinct.length == items.length, "labeled RDM item labels must be unique")

  def labels: Vector[String] =
    items.map(_.value)

  def alignTo(targetItems: Vector[RsaItemId], label: String): Either[MvpaError, RdmVector] =
    if targetItems.distinct.length != targetItems.length then
      Left(MvpaError.InvalidRdmInput("target RDM item labels must be unique"))
    else if targetItems.toSet != items.toSet then
      Left(MvpaError.InvalidRdmInput(s"$label item labels do not match observed labels"))
    else
      val sourceIndex = items.zipWithIndex.toMap
      val matrix = new Array[Double](items.length * items.length)
      val pairs = Rdm.pairIndices(items.length)
      var pair = 0
      while pair < pairs.length do
        val (row, col) = pairs(pair)
        val value = rdm.values(pair)
        matrix(row * items.length + col) = value
        matrix(col * items.length + row) = value
        pair += 1

      val outPairs = Rdm.pairIndices(targetItems.length)
      val out = new Array[Double](outPairs.length)
      pair = 0
      while pair < outPairs.length do
        val (row, col) = outPairs(pair)
        val sourceRow = sourceIndex(targetItems(row))
        val sourceCol = sourceIndex(targetItems(col))
        out(pair) = matrix(sourceRow * items.length + sourceCol)
        pair += 1
      Right(RdmVector.unsafe(targetItems.length, out.toVector))

object LabeledRdm:
  def apply(items: Seq[String], rdm: RdmVector): Either[MvpaError, LabeledRdm] =
    val out = Vector.newBuilder[RsaItemId]
    val vector = items.toVector
    var index = 0
    while index < vector.length do
      RsaItemId(vector(index)) match
        case Right(item) =>
          out += item
        case Left(error) =>
          return Left(error)
      index += 1
    fromItemIds(out.result(), rdm)

  def fromItemIds(items: Seq[RsaItemId], rdm: RdmVector): Either[MvpaError, LabeledRdm] =
    val itemVector = items.toVector
    if itemVector.length != rdm.items then
      Left(MvpaError.InvalidRdmInput(s"labeled RDM item count ${itemVector.length} != RDM item count ${rdm.items}"))
    else if itemVector.distinct.length != itemVector.length then
      Left(MvpaError.InvalidRdmInput("labeled RDM item labels must be unique"))
    else if rdm.values.exists(value => !value.isFinite) then
      Left(MvpaError.InvalidRdmInput("labeled RDM contains non-finite values"))
    else Right(new LabeledRdm(itemVector, rdm))

  def unsafe(items: Seq[String], rdm: RdmVector): LabeledRdm =
    apply(items, rdm).fold(error => throw new IllegalArgumentException(error.message), identity)

  def unsafeFromItemIds(items: Seq[RsaItemId], rdm: RdmVector): LabeledRdm =
    fromItemIds(items, rdm).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class RdmModel private (
    name: String,
    labeledRdm: LabeledRdm
):
  def itemIds: Vector[RsaItemId] =
    labeledRdm.items

  def items: Vector[String] =
    labeledRdm.labels

  def rdm: RdmVector =
    labeledRdm.rdm

  def alignTo(targetItems: Vector[RsaItemId]): Either[MvpaError, RdmVector] =
    labeledRdm.alignTo(targetItems, s"model RDM '$name'")

object RdmModel:
  def apply(name: String, items: Seq[String], rdm: RdmVector): Either[MvpaError, RdmModel] =
    val trimmedName = name.trim
    if trimmedName.isEmpty then Left(MvpaError.InvalidRdmInput("RDM model name must be non-empty"))
    else LabeledRdm(items, rdm).map(labeled => new RdmModel(trimmedName, labeled))

  def fromLabeled(name: String, labeledRdm: LabeledRdm): Either[MvpaError, RdmModel] =
    val trimmedName = name.trim
    if trimmedName.isEmpty then Left(MvpaError.InvalidRdmInput("RDM model name must be non-empty"))
    else Right(new RdmModel(trimmedName, labeledRdm))

  def unsafe(name: String, items: Seq[String], rdm: RdmVector): RdmModel =
    apply(name, items, rdm).fold(error => throw new IllegalArgumentException(error.message), identity)

trait RdmScorer:
  def name: String
  def score(observed: RdmVector, model: RdmVector): Either[MvpaError, Double]
  def score(observed: LabeledRdm, model: RdmVector): Either[MvpaError, Double] =
    score(observed.rdm, model)

  def score(
      observedItems: Vector[String],
      observed: RdmVector,
      model: RdmVector
  ): Either[MvpaError, Double] =
    LabeledRdm(observedItems, observed).flatMap(labeled => score(labeled, model))

object RdmScorer:
  object Pearson extends RdmScorer:
    override val name: String = "Pearson"

    override def score(observed: RdmVector, model: RdmVector): Either[MvpaError, Double] =
      validatePair(observed, model, name).flatMap(_ => pearsonValues(observed.values, model.values, name))

  object Spearman extends RdmScorer:
    override val name: String = "Spearman"

    override def score(observed: RdmVector, model: RdmVector): Either[MvpaError, Double] =
      for
        _ <- validatePair(observed, model, name)
        observedRanks <- averageRanks(observed.values, name)
        modelRanks <- averageRanks(model.values, name)
        value <- pearsonValues(observedRanks, modelRanks, name)
      yield value

  final class PartialPearson private (val controls: Vector[RdmModel]) extends RdmScorer:
    override val name: String = "PartialPearson"

    override def score(observed: RdmVector, model: RdmVector): Either[MvpaError, Double] =
      Left(MvpaError.InvalidRdmInput("partial Pearson RDM scorer requires observed item labels"))

    override def score(observed: LabeledRdm, model: RdmVector): Either[MvpaError, Double] =
      if observed.items.distinct.length != observed.items.length then
        Left(MvpaError.InvalidRdmInput("partial Pearson RDM scorer requires unique observed item labels"))
      else if observed.items.length != observed.rdm.items then
        Left(MvpaError.InvalidRdmInput("partial Pearson observed item labels do not match observed RDM"))
      else
        val aligned = Vector.newBuilder[RdmVector]
        var i = 0
        var error: MvpaError | Null = null
        while i < controls.length && error == null do
          controls(i).alignTo(observed.items) match
            case Right(rdm) => aligned += rdm
            case Left(e) => error = e
          i += 1

        error match
          case null =>
            validatePair(observed.rdm, model, name).flatMap(_ => partialPearsonValues(observed.rdm, model, aligned.result()))
          case e => Left(e)

  object PartialPearson:
    def apply(controls: Seq[RdmModel]): Either[MvpaError, PartialPearson] =
      val vector = controls.toVector
      if vector.isEmpty then Left(MvpaError.InvalidRdmInput("partial Pearson RDM scorer requires at least one control model"))
      else if vector.map(_.name).distinct.length != vector.length then
        Left(MvpaError.InvalidRdmInput("partial Pearson control model names must be unique"))
      else Right(new PartialPearson(vector))

    def unsafe(controls: Seq[RdmModel]): PartialPearson =
      apply(controls).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def validatePair(observed: RdmVector, model: RdmVector, scorerName: String): Either[MvpaError, Unit] =
    if observed.items != model.items then
      Left(MvpaError.InvalidRdmInput(s"observed RDM items ${observed.items} != model RDM items ${model.items}"))
    else Right(())

  private def scoringFailure(error: RsaScoreFailure, scorerName: String): MvpaError = error match
    case RsaScoreFailure.InsufficientData(_) => MvpaError.InvalidRdmInput(s"$scorerName RDM scorer requires at least two distances")
    case RsaScoreFailure.NonFiniteInput(_) => MvpaError.InvalidRdmInput(s"$scorerName RDM scorer requires finite distances")
    case RsaScoreFailure.ZeroVariance => MvpaError.InvalidRdmInput(s"$scorerName RDM scorer is undefined for zero-variance distances")
    case RsaScoreFailure.ShapeMismatch(_, _) => MvpaError.InvalidRdmInput(s"$scorerName RDM scorer requires equal-length distance vectors")

  private def pearsonValues(observed: Vector[Double], model: Vector[Double], scorerName: String): Either[MvpaError, Double] =
    RsaScoreKernels.pearson(observed, model).left.map(scoringFailure(_, scorerName))

  private def averageRanks(values: Vector[Double], scorerName: String): Either[MvpaError, Vector[Double]] =
    RsaScoreKernels.averageRanks(values).left.map(scoringFailure(_, scorerName))

  private def partialPearsonValues(
      observed: RdmVector,
      model: RdmVector,
      controls: Vector[RdmVector]
  ): Either[MvpaError, Double] =
    if controls.isEmpty then
      Left(MvpaError.InvalidRdmInput("partial Pearson RDM scorer requires at least one control RDM"))
    else if observed.values.length < controls.length + 2 then
      Left(MvpaError.InvalidRdmInput("partial Pearson RDM scorer requires more distances than controls"))
    else
      var i = 0
      while i < controls.length do
        val control = controls(i)
        if control.items != observed.items then
          return Left(MvpaError.InvalidRdmInput("partial Pearson control RDM item count does not match observed RDM"))
        i += 1

      for
        observedCentered <- centered(observed.values, "partial Pearson observed")
        modelCentered <- centered(model.values, "partial Pearson model")
        controlMatrix <- centeredControlMatrix(controls, observed.values.length)
        observedResidual <- residualize(observedCentered, controlMatrix, observed.values.length, controls.length)
        modelResidual <- residualize(modelCentered, controlMatrix, observed.values.length, controls.length)
        value <- pearsonCentered(observedResidual, modelResidual, "PartialPearson")
      yield value

  private def centered(values: Vector[Double], label: String): Either[MvpaError, Array[Double]] =
    var sum = 0.0
    var i = 0
    while i < values.length do
      val value = values(i)
      if !value.isFinite then
        return Left(MvpaError.InvalidRdmInput(s"$label contains non-finite distances"))
      sum += value
      i += 1
    val mean = sum / values.length
    val out = new Array[Double](values.length)
    i = 0
    while i < values.length do
      out(i) = values(i) - mean
      i += 1
    Right(out)

  private def centeredControlMatrix(
      controls: Vector[RdmVector],
      distances: Int
  ): Either[MvpaError, Array[Double]] =
    val matrix = new Array[Double](distances * controls.length)
    var control = 0
    while control < controls.length do
      centered(controls(control).values, "partial Pearson control") match
        case Right(centeredControl) =>
          var i = 0
          while i < distances do
            matrix(i * controls.length + control) = centeredControl(i)
            i += 1
        case Left(error) =>
          return Left(error)
      control += 1
    Right(matrix)

  private def residualize(
      values: Array[Double],
      controls: Array[Double],
      distances: Int,
      controlCount: Int
  ): Either[MvpaError, Array[Double]] =
    val gram = new Array[Double](controlCount * controlCount)
    val rhs = new Array[Double](controlCount)
    var row = 0
    while row < controlCount do
      var col = 0
      while col < controlCount do
        var sum = 0.0
        var i = 0
        while i < distances do
          sum += controls(i * controlCount + row) * controls(i * controlCount + col)
          i += 1
        gram(row * controlCount + col) = sum
        col += 1

      var rhsSum = 0.0
      var i = 0
      while i < distances do
        rhsSum += controls(i * controlCount + row) * values(i)
        i += 1
      rhs(row) = rhsSum
      row += 1

    solveLinearSystem(gram, rhs, controlCount).map { coefficients =>
      val out = values.clone()
      var i = 0
      while i < distances do
        var fitted = 0.0
        var control = 0
        while control < controlCount do
          fitted += controls(i * controlCount + control) * coefficients(control)
          control += 1
        out(i) -= fitted
        i += 1
      out
    }

  private def solveLinearSystem(
      matrix: Array[Double],
      rhs: Array[Double],
      size: Int
  ): Either[MvpaError, Array[Double]] =
    val a = matrix.clone()
    val b = rhs.clone()
    val tolerance = 1e-12
    var pivot = 0
    while pivot < size do
      var pivotRow = pivot
      var pivotAbs = math.abs(a(pivot * size + pivot))
      var row = pivot + 1
      while row < size do
        val candidate = math.abs(a(row * size + pivot))
        if candidate > pivotAbs then
          pivotAbs = candidate
          pivotRow = row
        row += 1

      if pivotAbs <= tolerance then
        return Left(MvpaError.InvalidRdmInput("partial Pearson control RDMs are rank deficient"))

      if pivotRow != pivot then
        var col = 0
        while col < size do
          val tmp = a(pivot * size + col)
          a(pivot * size + col) = a(pivotRow * size + col)
          a(pivotRow * size + col) = tmp
          col += 1
        val tmp = b(pivot)
        b(pivot) = b(pivotRow)
        b(pivotRow) = tmp

      val scale = a(pivot * size + pivot)
      var col = 0
      while col < size do
        a(pivot * size + col) /= scale
        col += 1
      b(pivot) /= scale

      row = 0
      while row < size do
        if row != pivot then
          val factor = a(row * size + pivot)
          col = 0
          while col < size do
            a(row * size + col) -= factor * a(pivot * size + col)
            col += 1
          b(row) -= factor * b(pivot)
        row += 1
      pivot += 1
    Right(b)

  private def pearsonCentered(
      observed: Array[Double],
      model: Array[Double],
      scorerName: String
  ): Either[MvpaError, Double] =
    var numerator = 0.0
    var observedSs = 0.0
    var modelSs = 0.0
    var i = 0
    while i < observed.length do
      numerator += observed(i) * model(i)
      observedSs += observed(i) * observed(i)
      modelSs += model(i) * model(i)
      i += 1

    val denom = math.sqrt(observedSs * modelSs)
    if denom <= 0.0 then Left(MvpaError.InvalidRdmInput(s"$scorerName RDM scorer is undefined for zero-variance residual distances"))
    else Right(numerator / denom)

private def checkedRsaId(kind: String, value: String): Either[MvpaError, String] =
  val trimmed = value.trim
  if trimmed.isEmpty then Left(MvpaError.InvalidRdmInput(s"$kind id must be non-empty"))
  else Right(trimmed)
