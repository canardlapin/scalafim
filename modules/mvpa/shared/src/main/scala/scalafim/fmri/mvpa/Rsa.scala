package scalafim.fmri.mvpa

import gale.linalg.{DMat, Matrix, QROptions, QRPivoting}

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

  /** Partial correlation after projecting out the centered control RDMs.
    * Controls are normalized before pivoted QR; diagonal pivots at most 1e-12
    * are refused as rank deficient. Residual norms at most 1e-12 relative to
    * the centered outcome are refused as numerically zero. Units do not set
    * either threshold, and no control is silently dropped.
    */
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
        observedCentered <- normalizedCentered(observed.values, "partial Pearson observed")
        modelCentered <- normalizedCentered(model.values, "partial Pearson model")
        controlMatrix <- centeredControlMatrix(controls, observed.values.length)
        value <- partialCorrelation(observedCentered, modelCentered, controlMatrix)
      yield value

  // Rank is assessed after each centered column has unit Euclidean norm.
  // This cutoff is therefore independent of RDM units; unresolved directions
  // are refused rather than silently removed from the requested control set.
  private val PartialRankTolerance = 1e-12
  private val PartialResidualTolerance = 1e-12

  private def normalizedCentered(values: Vector[Double], label: String): Either[MvpaError, Array[Double]] =
    var maximum = 0.0
    var i = 0
    while i < values.length do
      val value = values(i)
      if !value.isFinite then
        return Left(MvpaError.InvalidRdmInput(s"$label contains non-finite distances"))
      maximum = math.max(maximum, math.abs(value))
      i += 1
    val out = new Array[Double](values.length)
    if maximum > 0.0 then
      // Scale before centering so neither the mean nor the squared norm
      // overflows or underflows solely because of the choice of units.
      var sum = 0.0
      i = 0
      while i < values.length do
        out(i) = values(i) / maximum
        sum += out(i)
        i += 1
      val mean = sum / values.length
      var squaredNorm = 0.0
      i = 0
      while i < values.length do
        out(i) -= mean
        squaredNorm += out(i) * out(i)
        i += 1
      if squaredNorm > 0.0 then
        val norm = math.sqrt(squaredNorm)
        i = 0
        while i < values.length do
          out(i) /= norm
          i += 1
    Right(out)

  private def centeredControlMatrix(
      controls: Vector[RdmVector],
      distances: Int
  ): Either[MvpaError, DMat] =
    val matrix = Matrix.newBuilder(distances, controls.length)
    var control = 0
    while control < controls.length do
      normalizedCentered(controls(control).values, "partial Pearson control") match
        case Right(centeredControl) =>
          var i = 0
          while i < distances do
            matrix(i, control) = centeredControl(i)
            i += 1
        case Left(error) =>
          return Left(error)
      control += 1
    Right(matrix.result())

  private def partialCorrelation(
      observed: Array[Double],
      model: Array[Double],
      controls: DMat
  ): Either[MvpaError, Double] =
    val qr = controls.qr(QROptions(QRPivoting.Column, Some(PartialRankTolerance)))
    if !qr.diagnostics.rank.contains(controls.cols) then
      Left(MvpaError.InvalidRdmInput("partial Pearson control RDMs are rank deficient"))
    else
      val responses = Matrix.tabulate(controls.rows, 2)((row, col) => if col == 0 then observed(row) else model(row))
      qr.applyQT(responses)
        .left.map(error => MvpaError.InvalidRdmInput(s"partial Pearson projection failed: ${error.getMessage}"))
        .flatMap { transformed =>
          // The trailing Q-transformed coordinates are the residuals in an
          // orthonormal basis. Their dot product/norms equal those in the
          // original coordinates; forming X'X or solving for beta is unnecessary.
          var numerator = 0.0
          var observedSs = 0.0
          var modelSs = 0.0
          var row = controls.cols
          while row < controls.rows do
            val x = transformed(row, 0)
            val y = transformed(row, 1)
            numerator += x * y
            observedSs += x * x
            modelSs += y * y
            row += 1
          val minimumSs = PartialResidualTolerance * PartialResidualTolerance
          if !observedSs.isFinite || !modelSs.isFinite || !numerator.isFinite then
            Left(MvpaError.InvalidRdmInput("partial Pearson projection produced non-finite residual distances"))
          else if observedSs <= minimumSs || modelSs <= minimumSs then
            Left(MvpaError.InvalidRdmInput("PartialPearson RDM scorer is undefined for zero-variance residual distances (relative norm <= 1e-12)"))
          else
            val correlation = numerator / (math.sqrt(observedSs) * math.sqrt(modelSs))
            Right(math.max(-1.0, math.min(1.0, correlation)))
        }

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
) extends DenseRoiAnalysis:
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
) extends FoldRequiredDenseRoiAnalysis:
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
) extends DenseRoiAnalysis:
  require(models.nonEmpty, "RSA analysis requires at least one model")
  require(models.map(_.name).distinct.length == models.length, "RSA model names must be unique")

  override def name: String = s"rsa_${method.label}_${rows.label}_${scorer.name.toLowerCase}"
  override val minFeatures: Int = 1

  override def evaluate(roi: PatternMatrix, context: RoiContext): Either[MvpaError, RoiAnalysisResult] =
    for
      observed <- RdmAnalysisSupport.observedPatterns(roi, context, rows)
      observedRdm <- method.compute(observed.matrix)
      labeledObserved = LabeledRdm.unsafeFromItemIds(observed.items, observedRdm)
      scores <- RsaAnalysisSupport.scoreModels(models, scorer, labeledObserved)
    yield
      val scoreMetrics = scores.map(score => s"${score.modelName}.${scorer.name}" -> score.value)
      val metrics = RdmAnalysisSupport.rdmMetricPairs(observedRdm, observed.matrix.cols) ++ scoreMetrics
      val payload =
        if storeObservedRdm then Some(RoiPayload.Rsa(Some(labeledObserved), scores))
        else Some(RoiPayload.Rsa(None, scores))
      RoiAnalysisResult(MetricVector.from(metrics), payload)

private[mvpa] object RsaAnalysisSupport:
  def scoreModels(
      models: Vector[RdmModel],
      scorer: RdmScorer,
      observed: LabeledRdm
  ): Either[MvpaError, Vector[RsaScore]] =
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
) extends DenseRoiAnalysis:
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

final case class ObservedPatterns(items: Vector[RsaItemId], matrix: DMat)

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
