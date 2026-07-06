package scalafim.fmri.mvpa

import scalafim.linalg.DoubleMatrix

enum RdmRows:
  case Samples
  case ClassMeans

  def label: String =
    this match
      case Samples => "samples"
      case ClassMeans => "class_means"

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

  def compute(matrix: DoubleMatrix): Either[MvpaError, RdmVector] =
    this match
      case SquaredEuclidean(normalizeByFeatures) =>
        Rdm.squaredEuclidean(matrix, normalizeByFeatures)
      case Euclidean =>
        Rdm.euclidean(matrix)
      case Correlation =>
        Rdm.correlation(matrix)

final case class RdmModel private (
    name: String,
    items: Vector[String],
    rdm: RdmVector
):
  def alignTo(targetItems: Vector[String]): Either[MvpaError, RdmVector] =
    if targetItems.distinct.length != targetItems.length then
      Left(MvpaError.InvalidRdmInput("target RDM item labels must be unique"))
    else if targetItems.toSet != items.toSet then
      Left(MvpaError.InvalidRdmInput(s"model RDM '$name' item labels do not match observed labels"))
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

object RdmModel:
  def apply(name: String, items: Seq[String], rdm: RdmVector): Either[MvpaError, RdmModel] =
    val trimmedName = name.trim
    val trimmedItems = items.map(_.trim).toVector
    if trimmedName.isEmpty then Left(MvpaError.InvalidRdmInput("RDM model name must be non-empty"))
    else if trimmedItems.exists(_.isEmpty) then Left(MvpaError.InvalidRdmInput("RDM model item labels must be non-empty"))
    else if trimmedItems.distinct.length != trimmedItems.length then Left(MvpaError.InvalidRdmInput("RDM model item labels must be unique"))
    else if trimmedItems.length != rdm.items then
      Left(MvpaError.InvalidRdmInput(s"RDM model item count ${trimmedItems.length} != RDM item count ${rdm.items}"))
    else if rdm.values.exists(value => !value.isFinite) then Left(MvpaError.InvalidRdmInput("RDM model contains non-finite values"))
    else Right(new RdmModel(trimmedName, trimmedItems, rdm))

  def unsafe(name: String, items: Seq[String], rdm: RdmVector): RdmModel =
    apply(name, items, rdm).fold(error => throw new IllegalArgumentException(error.message), identity)

trait RdmScorer:
  def name: String
  def score(observed: RdmVector, model: RdmVector): Either[MvpaError, Double]
  def score(
      observedItems: Vector[String],
      observed: RdmVector,
      model: RdmVector
  ): Either[MvpaError, Double] =
    score(observed, model)

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

    override def score(
        observedItems: Vector[String],
        observed: RdmVector,
        model: RdmVector
    ): Either[MvpaError, Double] =
      if observedItems.distinct.length != observedItems.length then
        Left(MvpaError.InvalidRdmInput("partial Pearson RDM scorer requires unique observed item labels"))
      else if observedItems.length != observed.items then
        Left(MvpaError.InvalidRdmInput("partial Pearson observed item labels do not match observed RDM"))
      else
        val aligned = Vector.newBuilder[RdmVector]
        var i = 0
        var error: MvpaError | Null = null
        while i < controls.length && error == null do
          controls(i).alignTo(observedItems) match
            case Right(rdm) => aligned += rdm
            case Left(e) => error = e
          i += 1

        error match
          case null =>
            validatePair(observed, model, name).flatMap(_ => partialPearsonValues(observed, model, aligned.result()))
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

  private def pearsonValues(
      observed: Vector[Double],
      model: Vector[Double],
      scorerName: String
  ): Either[MvpaError, Double] =
    if observed.length != model.length then
      Left(MvpaError.InvalidRdmInput(s"$scorerName RDM scorer requires equal-length distance vectors"))
    else if observed.length < 2 then
      Left(MvpaError.InvalidRdmInput(s"$scorerName RDM scorer requires at least two distances"))
    else
      var observedSum = 0.0
      var modelSum = 0.0
      var i = 0
      while i < observed.length do
        val x = observed(i)
        val y = model(i)
        if !x.isFinite || !y.isFinite then
          return Left(MvpaError.InvalidRdmInput(s"$scorerName RDM scorer requires finite distances"))
        observedSum += x
        modelSum += y
        i += 1

      val observedMean = observedSum / observed.length
      val modelMean = modelSum / model.length
      var numerator = 0.0
      var observedSs = 0.0
      var modelSs = 0.0
      i = 0
      while i < observed.length do
        val xo = observed(i) - observedMean
        val ym = model(i) - modelMean
        numerator += xo * ym
        observedSs += xo * xo
        modelSs += ym * ym
        i += 1

      val denom = math.sqrt(observedSs * modelSs)
      if denom <= 0.0 then Left(MvpaError.InvalidRdmInput(s"$scorerName RDM scorer is undefined for zero-variance distances"))
      else Right(numerator / denom)

  private def averageRanks(values: Vector[Double], scorerName: String): Either[MvpaError, Vector[Double]] =
    var i = 0
    while i < values.length do
      if !values(i).isFinite then
        return Left(MvpaError.InvalidRdmInput(s"$scorerName RDM scorer requires finite distances"))
      i += 1

    val indexed = values.zipWithIndex.sortBy(_._1)
    val ranks = new Array[Double](values.length)
    var start = 0
    while start < indexed.length do
      var end = start + 1
      while end < indexed.length && indexed(end)._1 == indexed(start)._1 do
        end += 1
      val rank = (start.toDouble + 1.0 + end.toDouble) / 2.0
      var cursor = start
      while cursor < end do
        ranks(indexed(cursor)._2) = rank
        cursor += 1
      start = end
    Right(ranks.toVector)

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

final case class RdmAnalysis(
    method: RdmMethod,
    rows: RdmRows = RdmRows.Samples,
    storeRdm: Boolean = true
) extends RoiAnalysis:
  override def name: String = s"rdm_${method.label}_${rows.label}"
  override val minFeatures: Int = 1

  override def evaluate(roi: PatternMatrix, context: RoiContext): Either[MvpaError, RoiAnalysisResult] =
    for
      observed <- RdmAnalysisSupport.observedPatterns(roi, context, rows)
      rdm <- method.compute(observed.matrix)
    yield
      val payload =
        if storeRdm then Some(RoiPayload.Rdm(observed.items, rdm))
        else None
      RoiAnalysisResult(RdmAnalysisSupport.rdmMetrics(rdm, observed.matrix.cols), payload)

final case class CrossnobisAnalysis(
    normalizeByFeatures: Boolean = true,
    storeRdm: Boolean = true
) extends RoiAnalysis:
  override def name: String =
    if normalizeByFeatures then "crossnobis_normalized" else "crossnobis"
  override val minFeatures: Int = 1

  override def evaluate(roi: PatternMatrix, context: RoiContext): Either[MvpaError, RoiAnalysisResult] =
    val folds = context.folds.toRight(MvpaError.InvalidRdmInput("crossnobis analysis requires a fold plan"))
    for
      plan <- folds
      partitioned <- PartitionMeansBuilder.fromPatterns(roi, context.response, plan)
    yield
      val rdm = Rdm.crossnobisDistances(partitioned.means, normalizeByFeatures)
      val payload =
        if storeRdm then Some(RoiPayload.Rdm(partitioned.items, rdm))
        else None
      val metrics =
        RdmAnalysisSupport.rdmMetricPairs(rdm, partitioned.means.features) :+
          ("Folds" -> partitioned.means.folds.toDouble)
      RoiAnalysisResult(MetricVector.from(metrics), payload)

final case class RsaAnalysis(
    method: RdmMethod,
    models: Vector[RdmModel],
    rows: RdmRows = RdmRows.ClassMeans,
    scorer: RdmScorer = RdmScorer.Pearson,
    storeObservedRdm: Boolean = false
) extends RoiAnalysis:
  require(models.nonEmpty, "RSA analysis requires at least one model")
  require(models.map(_.name).distinct.length == models.length, "RSA model names must be unique")

  override def name: String = s"rsa_${method.label}_${rows.label}_${scorer.name.toLowerCase}"
  override val minFeatures: Int = 1

  override def evaluate(roi: PatternMatrix, context: RoiContext): Either[MvpaError, RoiAnalysisResult] =
    for
      observed <- RdmAnalysisSupport.observedPatterns(roi, context, rows)
      observedRdm <- method.compute(observed.matrix)
      scores <- scoreModels(observed.items, observedRdm)
    yield
      val scoreMetrics = scores.map(score => s"${score.modelName}.${scorer.name}" -> score.value)
      val metrics = RdmAnalysisSupport.rdmMetricPairs(observedRdm, observed.matrix.cols) ++ scoreMetrics
      val payload =
        if storeObservedRdm then Some(RoiPayload.Rsa(observed.items, Some(observedRdm), scores))
        else Some(RoiPayload.Rsa(observed.items, None, scores))
      RoiAnalysisResult(MetricVector.from(metrics), payload)

  private def scoreModels(items: Vector[String], observed: RdmVector): Either[MvpaError, Vector[RsaScore]] =
    val out = Vector.newBuilder[RsaScore]
    var error: MvpaError | Null = null
    var i = 0
    while i < models.length && error == null do
      val model = models(i)
      val scored =
        for
          aligned <- model.alignTo(items)
          value <- scorer.score(observed, aligned)
        yield RsaScore(model.name, value)
      scored match
        case Right(score) => out += score
        case Left(e) => error = e
      i += 1
    error match
      case null => Right(out.result())
      case e => Left(e)

final case class ObservedPatterns(items: Vector[String], matrix: DoubleMatrix)

private[mvpa] object RdmAnalysisSupport:
  def observedPatterns(
      roi: PatternMatrix,
      context: RoiContext,
      rows: RdmRows
  ): Either[MvpaError, ObservedPatterns] =
    rows match
      case RdmRows.Samples =>
        Right(ObservedPatterns(roi.sampleIndices.map(_.value.toString), roi.value))
      case RdmRows.ClassMeans =>
        for
          labels <- Classification.categorical(context.response, roi.samples)
          summary <- Classification.classSummary(roi, labels)
        yield ObservedPatterns(summary.classes.map(_.value), summary.means)

  def rdmMetrics(rdm: RdmVector, features: Int): MetricVector =
    MetricVector.from(rdmMetricPairs(rdm, features))

  def rdmMetricPairs(rdm: RdmVector, features: Int): Vector[(String, Double)] =
    Vector(
      "Items" -> rdm.items.toDouble,
      "Pairs" -> rdm.values.length.toDouble,
      "Features" -> features.toDouble,
      "MeanDistance" -> mean(rdm.values)
    )

  private def mean(values: Vector[Double]): Double =
    var sum = 0.0
    var i = 0
    while i < values.length do
      sum += values(i)
      i += 1
    sum / values.length
