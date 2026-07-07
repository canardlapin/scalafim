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

opaque type RsaBlockId = String

object RsaBlockId:
  def apply(value: String): Either[MvpaError, RsaBlockId] =
    checkedRsaId("RSA block", value)

  def unsafe(value: String): RsaBlockId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: RsaBlockId)
    inline def value: String = id

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

final case class SamplewiseRsaDesign private (
    model: RdmModel,
    sampleItems: Vector[RsaItemId],
    blocks: Vector[RsaBlockId],
    modelItemIndices: Vector[Int]
):
  require(sampleItems.length == blocks.length, "sample items and blocks must have the same length")
  require(sampleItems.length == modelItemIndices.length, "sample items and model index lookup must have the same length")

  def samples: Int =
    sampleItems.length

  private[mvpa] inline def referenceDistance(row: Int, col: Int): Double =
    model.rdm.unsafeDistance(modelItemIndices(row), modelItemIndices(col))

object SamplewiseRsaDesign:
  def apply(
      model: RdmModel,
      sampleItems: Seq[String],
      blocks: Seq[String]
  ): Either[MvpaError, SamplewiseRsaDesign] =
    val itemIds = Vector.newBuilder[RsaItemId]
    val rawItems = sampleItems.toVector
    val rawBlocks = blocks.toVector
    if rawItems.length != rawBlocks.length then
      Left(MvpaError.InvalidRdmInput(s"sample item count ${rawItems.length} != block count ${rawBlocks.length}"))
    else if rawItems.length < 2 then Left(MvpaError.InvalidRdmInput("samplewise RSA requires at least two samples"))
    else
      var i = 0
      while i < rawItems.length do
        RsaItemId(rawItems(i)) match
          case Right(id) =>
            itemIds += id
          case Left(error) =>
            return Left(error)
        i += 1

      val blockIds = Vector.newBuilder[RsaBlockId]
      i = 0
      while i < rawBlocks.length do
        RsaBlockId(rawBlocks(i)) match
          case Right(id) =>
            blockIds += id
          case Left(error) =>
            return Left(error)
        i += 1

      val parsedItems = itemIds.result()
      val parsedBlocks = blockIds.result()
      if parsedBlocks.map(_.value).distinct.length < 2 then
        Left(MvpaError.InvalidRdmInput("samplewise RSA requires at least two blocks"))
      else
        val modelIndex = model.items.zipWithIndex.toMap
        val indices = Vector.newBuilder[Int]
        indices.sizeHint(parsedItems.length)
        i = 0
        while i < parsedItems.length do
          modelIndex.get(parsedItems(i).value) match
            case Some(index) =>
              indices += index
            case None =>
              return Left(MvpaError.InvalidRdmInput(s"sample item '${parsedItems(i).value}' is not present in model RDM '${model.name}'"))
          i += 1
        Right(new SamplewiseRsaDesign(model, parsedItems, parsedBlocks, indices.result()))

  def unsafe(model: RdmModel, sampleItems: Seq[String], blocks: Seq[String]): SamplewiseRsaDesign =
    apply(model, sampleItems, blocks).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class SamplewiseRsaScore(
    sample: SampleIndex,
    item: RsaItemId,
    block: RsaBlockId,
    value: Double
)

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

trait RowSimilarity:
  def name: String
  private[mvpa] def score(observed: Array[Double], model: Array[Double], length: Int): Either[MvpaError, Option[Double]]

object RowSimilarity:
  object Pearson extends RowSimilarity:
    override val name: String = "Pearson"

    private[mvpa] override def score(
        observed: Array[Double],
        model: Array[Double],
        length: Int
    ): Either[MvpaError, Option[Double]] =
      pearson(observed, model, length, name)

  object Spearman extends RowSimilarity:
    override val name: String = "Spearman"

    private[mvpa] override def score(
        observed: Array[Double],
        model: Array[Double],
        length: Int
    ): Either[MvpaError, Option[Double]] =
      for
        observedRanks <- ranks(observed, length, name)
        modelRanks <- ranks(model, length, name)
        value <- pearson(observedRanks, modelRanks, length, name)
      yield value

  private def pearson(
      observed: Array[Double],
      model: Array[Double],
      length: Int,
      scorerName: String
  ): Either[MvpaError, Option[Double]] =
    if length < 2 then Right(None)
    else
      var observedSum = 0.0
      var modelSum = 0.0
      var i = 0
      while i < length do
        val x = observed(i)
        val y = model(i)
        if !x.isFinite || !y.isFinite then
          return Left(MvpaError.InvalidRdmInput(s"$scorerName row similarity requires finite distances"))
        observedSum += x
        modelSum += y
        i += 1

      val observedMean = observedSum / length
      val modelMean = modelSum / length
      var numerator = 0.0
      var observedSs = 0.0
      var modelSs = 0.0
      i = 0
      while i < length do
        val xo = observed(i) - observedMean
        val ym = model(i) - modelMean
        numerator += xo * ym
        observedSs += xo * xo
        modelSs += ym * ym
        i += 1

      val denom = math.sqrt(observedSs * modelSs)
      if denom <= 0.0 then Right(None)
      else Right(Some(numerator / denom))

  private def ranks(values: Array[Double], length: Int, scorerName: String): Either[MvpaError, Array[Double]] =
    val indexed = new Array[(Double, Int)](length)
    var i = 0
    while i < length do
      val value = values(i)
      if !value.isFinite then
        return Left(MvpaError.InvalidRdmInput(s"$scorerName row similarity requires finite distances"))
      indexed(i) = (value, i)
      i += 1

    val sorted = indexed.toVector.sortBy(_._1)
    val out = new Array[Double](length)
    var start = 0
    while start < length do
      var end = start + 1
      while end < length && sorted(end)._1 == sorted(start)._1 do
        end += 1
      val rank = (start.toDouble + 1.0 + end.toDouble) / 2.0
      var cursor = start
      while cursor < end do
        out(sorted(cursor)._2) = rank
        cursor += 1
      start = end
    Right(out)

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
        if storeRdm then Some(RoiPayload.Rdm(LabeledRdm.unsafeFromItemIds(observed.items, rdm)))
        else None
      RoiAnalysisResult(RdmAnalysisSupport.rdmMetrics(rdm, observed.matrix.cols), payload)

final case class CrossnobisAnalysis(
    normalizeByFeatures: Boolean = true,
    storeRdm: Boolean = true
) extends FoldRequiredRoiAnalysis:
  override def name: String =
    if normalizeByFeatures then "crossnobis_normalized" else "crossnobis"
  override val minFeatures: Int = 1
  override def missingFoldsError: MvpaError =
    MvpaError.InvalidRdmInput("crossnobis analysis requires a fold plan")

  override def evaluateFolded(roi: PatternMatrix, context: FoldedRoiContext): Either[MvpaError, RoiAnalysisResult] =
    for
      partitioned <- PartitionMeansBuilder.fromPatterns(roi, context.response, context.foldPlan)
    yield
      val rdm = Rdm.crossnobisDistances(partitioned.means, normalizeByFeatures)
      val payload =
        if storeRdm then Some(RoiPayload.Rdm(LabeledRdm.unsafe(partitioned.items, rdm)))
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
      labeledObserved = LabeledRdm.unsafeFromItemIds(observed.items, observedRdm)
      scores <- scoreModels(labeledObserved)
    yield
      val scoreMetrics = scores.map(score => s"${score.modelName}.${scorer.name}" -> score.value)
      val metrics = RdmAnalysisSupport.rdmMetricPairs(observedRdm, observed.matrix.cols) ++ scoreMetrics
      val payload =
        if storeObservedRdm then Some(RoiPayload.Rsa(Some(labeledObserved), scores))
        else Some(RoiPayload.Rsa(None, scores))
      RoiAnalysisResult(MetricVector.from(metrics), payload)

  private def scoreModels(observed: LabeledRdm): Either[MvpaError, Vector[RsaScore]] =
    val out = Vector.newBuilder[RsaScore]
    var error: MvpaError | Null = null
    var i = 0
    while i < models.length && error == null do
      val model = models(i)
      val scored =
        for
          aligned <- model.alignTo(observed.items)
          value <- scorer.score(observed, aligned)
        yield RsaScore(model.name, value)
      scored match
        case Right(score) => out += score
        case Left(e) => error = e
      i += 1
    error match
      case null => Right(out.result())
      case e => Left(e)

final case class SamplewiseRsaAnalysis(
    design: SamplewiseRsaDesign,
    method: RdmMethod = RdmMethod.Correlation,
    scorer: RowSimilarity = RowSimilarity.Pearson,
    storeScores: Boolean = false
) extends RoiAnalysis:
  override def name: String = s"samplewise_rsa_${method.label}_${scorer.name.toLowerCase}"
  override val minFeatures: Int =
    method match
      case RdmMethod.Correlation => 2
      case _ => 1

  override def evaluate(roi: PatternMatrix, context: RoiContext): Either[MvpaError, RoiAnalysisResult] =
    if roi.samples != design.samples then
      Left(MvpaError.InvalidRdmInput(s"samplewise RSA design samples ${design.samples} != ROI samples ${roi.samples}"))
    else
      for
        observed <- method.compute(roi.value)
        scores <- SamplewiseRsaAnalysis.scoreRows(observed, design, scorer)
      yield
        val validScores = scores.map(_.value).filter(_.isFinite)
        val mean =
          if validScores.isEmpty then Double.NaN
          else validScores.sum / validScores.length
        val metrics =
          MetricVector(
            "SamplewiseRsa" -> mean,
            "ValidScores" -> validScores.length.toDouble,
            "Samples" -> design.samples.toDouble,
            "Features" -> roi.features.toDouble
          )
        val payload =
          if storeScores then Some(RoiPayload.SamplewiseRsa(design.model.name, scores))
          else None
        RoiAnalysisResult(metrics, payload)

object SamplewiseRsaAnalysis:
  private[mvpa] def scoreRows(
      observed: RdmVector,
      design: SamplewiseRsaDesign,
      scorer: RowSimilarity
  ): Either[MvpaError, Vector[SamplewiseRsaScore]] =
    if observed.items != design.samples then
      Left(MvpaError.InvalidRdmInput(s"observed RDM items ${observed.items} != samplewise RSA design samples ${design.samples}"))
    else
      val out = Vector.newBuilder[SamplewiseRsaScore]
      out.sizeHint(design.samples)
      val observedRow = new Array[Double](design.samples)
      val referenceRow = new Array[Double](design.samples)
      var row = 0
      while row < design.samples do
        var length = 0
        var col = 0
        while col < design.samples do
          if design.blocks(col) != design.blocks(row) then
            observedRow(length) = observed.unsafeDistance(row, col)
            referenceRow(length) = design.referenceDistance(row, col)
            length += 1
          col += 1

        val value =
          scorer.score(observedRow, referenceRow, length) match
            case Right(Some(score)) =>
              score
            case Right(None) =>
              Double.NaN
            case Left(error) =>
              return Left(error)
        out += SamplewiseRsaScore(SampleIndex.unsafe(row), design.sampleItems(row), design.blocks(row), value)
        row += 1
      Right(out.result())

final case class ObservedPatterns(items: Vector[RsaItemId], matrix: DoubleMatrix)

private[mvpa] object RdmAnalysisSupport:
  def observedPatterns(
      roi: PatternMatrix,
      context: RoiContext,
      rows: RdmRows
  ): Either[MvpaError, ObservedPatterns] =
    rows match
      case RdmRows.Samples =>
        Right(ObservedPatterns(roi.sampleIndices.map(index => RsaItemId.unsafe(index.value.toString)), roi.value))
      case RdmRows.ClassMeans =>
        for
          labels <- Classification.categorical(context.response, roi.samples)
          summary <- Classification.classSummary(roi, labels)
        yield ObservedPatterns(summary.classes.map(label => RsaItemId.unsafe(label.value)), summary.means)

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

private def checkedRsaId(kind: String, value: String): Either[MvpaError, String] =
  val trimmed = value.trim
  if trimmed.isEmpty then Left(MvpaError.InvalidRdmInput(s"$kind id must be non-empty"))
  else Right(trimmed)
