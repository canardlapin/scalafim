package scalafim.fmri.mvpa

import gale.linalg.{CholeskyOptions, DMat, Matrix}

/** Legacy row-identified prediction for P6/P7 numeric kernels; expires with M3.13. */
final case class ClassificationPrediction(
    classes: Vector[ClassLabel],
    probabilities: DMat,
    sampleIndices: Vector[SampleIndex]
):
  require(classes.nonEmpty, "prediction classes must be non-empty")
  require(probabilities.cols == classes.length, "probability columns must match classes")
  require(probabilities.rows == sampleIndices.length, "probability rows must match sample indices")

  def predicted: Vector[ClassLabel] =
    val out = Vector.newBuilder[ClassLabel]
    out.sizeHint(probabilities.rows)
    var row = 0
    while row < probabilities.rows do
      var bestCol = 0
      var best = probabilities(row, 0)
      var col = 1
      while col < probabilities.cols do
        val value = probabilities(row, col)
        if value > best then
          best = value
          bestCol = col
        col += 1
      out += classes(bestCol)
      row += 1
    out.result()

/** A dense categorical result has a fixed class-column order and no row identity. */
final class CategoricalProbabilities private (
    val classes: Vector[ClassLabel],
    val probabilities: DMat
):
  val predicted: Vector[ClassLabel] =
    Vector.tabulate(probabilities.rows): row =>
      var best = 0
      var column = 1
      while column < probabilities.cols do
        if probabilities(row, column) > probabilities(row, best) then best = column
        column += 1
      classes(best)

object CategoricalProbabilities:
  def from(classes: Vector[ClassLabel], probabilities: DMat): Either[MvpaError, CategoricalProbabilities] =
    if classes.isEmpty || classes.distinct.length != classes.length || probabilities.cols != classes.length then
      Left(MvpaError.InvalidClassifierInput("probability columns require distinct non-empty classes"))
    else
      Classification.validateFinite(probabilities, "class probabilities").flatMap: _ =>
        var row = 0
        var invalid = false
        while row < probabilities.rows && !invalid do
          var column = 0
          var total = 0.0
          while column < probabilities.cols do
            val value = probabilities(row, column)
            if value < 0.0 || value > 1.0 then invalid = true
            total += value
            column += 1
          if math.abs(total - 1.0) > 1e-12 then invalid = true
          row += 1
        if invalid then Left(MvpaError.InvalidClassifierInput("class probabilities must be normalized rows in [0, 1]"))
        else Right(new CategoricalProbabilities(classes, probabilities))

trait Classifier:
  type Model <: ClassifierModel
  def name: String
  def minFeatures: Int = 1
  def fit(train: DMat, labels: Vector[ClassLabel]): Either[MvpaError, Model]

trait ClassifierModel:
  def classifierName: String
  def classes: Vector[ClassLabel]
  def predict(test: DMat): Either[MvpaError, CategoricalProbabilities]

enum FeatureScaling:
  case None
  case ZScore
  case DiagonalShrinkage(alpha: ShrinkageAlpha = ShrinkageAlpha.unsafe(0.1))

object FeatureScaling:
  def diagonalShrinkage(alpha: Double = 0.1): Either[MvpaError, FeatureScaling] =
    ShrinkageAlpha(alpha).map(value => FeatureScaling.DiagonalShrinkage(value))

  def unsafeDiagonalShrinkage(alpha: Double = 0.1): FeatureScaling =
    FeatureScaling.DiagonalShrinkage(ShrinkageAlpha.unsafe(alpha))

final case class CorrelationCentroidClassifier() extends Classifier:
  override type Model = CorrelationCentroidModel
  override val name: String = "correlation_centroid"
  override val minFeatures: Int = 2

  override def fit(train: DMat, labels: Vector[ClassLabel]): Either[MvpaError, CorrelationCentroidModel] =
    for
      _ <- if train.cols >= minFeatures then Right(()) else Left(MvpaError.InvalidClassifierInput("correlation centroid requires at least two features"))
      _ <- Classification.validateFinite(train, "training data")
      summary <- Classification.classSummary(train, labels)
    yield
      CorrelationCentroidModel(
        classes = summary.classes,
        centroids = summary.means
      )

final case class CorrelationCentroidModel(
    classes: Vector[ClassLabel],
    centroids: DMat
) extends ClassifierModel:
  override val classifierName: String = "correlation_centroid"

  override def predict(test: DMat): Either[MvpaError, CategoricalProbabilities] =
    if test.cols != centroids.cols then
      Left(MvpaError.MatrixShapeMismatch(s"test feature count ${test.cols} != model feature count ${centroids.cols}"))
    else
      Classification.validateFinite(test, "test data").flatMap { _ =>
        CategoricalProbabilities.from(classes, Classification.softmax(Classification.rowCorrelationScores(test, centroids)))
      }

final case class SwiftCentroidClassifier(
    scaling: FeatureScaling = FeatureScaling.ZScore
) extends Classifier:
  override type Model = SwiftCentroidModel
  override val name: String = "swift_centroid"
  override val minFeatures: Int = 1

  override def fit(train: DMat, labels: Vector[ClassLabel]): Either[MvpaError, SwiftCentroidModel] =
    for
      _ <- Classification.validateFinite(train, "training data")
      _ <- Classification.validateScaling(scaling)
      model <-
        val scaler = Classification.Scaler.fit(train, scaling)
        val scaled = scaler.transform(train)
        Classification.classSummary(scaled, labels).map { summary =>
          SwiftCentroidModel(
            classes = summary.classes,
            centroids = summary.means,
            priors = summary.priors,
            scaler = scaler
          )
        }
    yield model

final case class SwiftCentroidModel(
    classes: Vector[ClassLabel],
    centroids: DMat,
    priors: Vector[Double],
    scaler: Classification.Scaler
) extends ClassifierModel:
  override val classifierName: String = "swift_centroid"

  override def predict(test: DMat): Either[MvpaError, CategoricalProbabilities] =
    if test.cols != centroids.cols then
      Left(MvpaError.MatrixShapeMismatch(s"test feature count ${test.cols} != model feature count ${centroids.cols}"))
    else
      Classification.validateFinite(test, "test data").flatMap { _ =>
        val scaled = scaler.transform(test)
        CategoricalProbabilities.from(classes, Classification.softmax(Classification.linearCentroidScores(scaled, centroids, priors)))
      }

final class RidgeLdaClassifier private (val penalty: RidgePenalty) extends Classifier:
  override type Model = RidgeLdaModel
  def gamma: Double =
    penalty.value

  override val name: String = "ridge_lda"
  override val minFeatures: Int = 1

  override def fit(train: DMat, labels: Vector[ClassLabel]): Either[MvpaError, RidgeLdaModel] =
    for
      _ <- Classification.validateFinite(train, "training data")
      model <- Classification.classSummary(train, labels).flatMap { summary =>
        val sigma0 = Classification.pooledResidualCrossproduct(train, labels, summary)
        val sigmaBuilder = Matrix.newBuilder(sigma0.rows, sigma0.cols)
        var row = 0
        while row < sigma0.rows do
          var col = 0
          while col < sigma0.cols do
            sigmaBuilder(row, col) = sigma0(row, col) + (if row == col then gamma else 0.0)
            col += 1
          row += 1
        val sigma = sigmaBuilder.result()
        sigma.cholesky(CholeskyOptions(1e-12)).left.map(error => MvpaError.ClassifierFitFailed(name, error.getMessage)).flatMap { factor =>
          val meansT = Classification.transpose(summary.means)
          factor.solve(meansT).left.map(error => MvpaError.ClassifierFitFailed(name, error.getMessage)).map { invSigmaMeans =>
            val linConst = new Array[Double](summary.classes.length)
            var klass = 0
            while klass < summary.classes.length do
              var dot = 0.0
              var feature = 0
              while feature < train.cols do
                dot += summary.means(klass, feature) * invSigmaMeans(feature, klass)
                feature += 1
              linConst(klass) = -0.5 * dot + math.log(math.max(summary.priors(klass), 1e-300))
              klass += 1

            RidgeLdaModel(
              classes = summary.classes,
              invSigmaMeans = invSigmaMeans,
              linearConstants = linConst.toVector
            )
          }
        }
      }
    yield model

final case class RidgeLdaModel(
    classes: Vector[ClassLabel],
    invSigmaMeans: DMat,
    linearConstants: Vector[Double]
) extends ClassifierModel:
  override val classifierName: String = "ridge_lda"

  override def predict(test: DMat): Either[MvpaError, CategoricalProbabilities] =
    if test.cols != invSigmaMeans.rows then
      Left(MvpaError.MatrixShapeMismatch(s"test feature count ${test.cols} != model feature count ${invSigmaMeans.rows}"))
    else
      Classification.validateFinite(test, "test data").flatMap { _ =>
        val rawScores = test * invSigmaMeans
        val scores = Matrix.newBuilder(rawScores.rows, rawScores.cols)
        var row = 0
        while row < rawScores.rows do
          var klass = 0
          while klass < rawScores.cols do
            scores(row, klass) = rawScores(row, klass) + linearConstants(klass)
            klass += 1
          row += 1
        CategoricalProbabilities.from(classes, Classification.softmax(scores.result()))
      }

object RidgeLdaClassifier:
  def apply(gamma: Double = 1e-2): RidgeLdaClassifier =
    RidgePenalty(gamma).fold(error => throw new IllegalArgumentException(error.message), value => new RidgeLdaClassifier(value))

  def fromPenalty(gamma: RidgePenalty): RidgeLdaClassifier =
    new RidgeLdaClassifier(gamma)

object Classification:
  private val Eps = 1e-12

  final case class ClassSummary(
      classes: Vector[ClassLabel],
      counts: Vector[Int],
      priors: Vector[Double],
      means: DMat
  )

  def validateFinite(matrix: DMat, label: String): Either[MvpaError, Unit] =
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        if !matrix(row, col).isFinite then
          return Left(MvpaError.InvalidClassifierInput(s"$label contains non-finite values"))
        col += 1
      row += 1
    Right(())

  def validateScaling(scaling: FeatureScaling): Either[MvpaError, Unit] =
    scaling match
      case FeatureScaling.DiagonalShrinkage(alpha) if !alpha.value.isFinite || alpha.value < 0.0 || alpha.value > 1.0 =>
        Left(MvpaError.InvalidClassifierInput("diagonal shrinkage alpha must be finite and in [0, 1]"))
      case _ =>
        Right(())

  final case class Scaler(means: Array[Double], scales: Array[Double]):
    def transform(matrix: DMat): DMat =
      require(matrix.cols == means.length, "matrix columns must match scaler length")
      val out = Matrix.newBuilder(matrix.rows, matrix.cols)
      var row = 0
      while row < matrix.rows do
        var col = 0
        while col < matrix.cols do
          out(row, col) = (matrix(row, col) - means(col)) / scales(col)
          col += 1
        row += 1
      out.result()

  object Scaler:
    def fit(matrix: DMat, scaling: FeatureScaling): Scaler =
      val means = new Array[Double](matrix.cols)
      val scales = new Array[Double](matrix.cols)
      var col = 0
      while col < matrix.cols do
        var sum = 0.0
        var row = 0
        while row < matrix.rows do
          sum += matrix(row, col)
          row += 1
        val mean = sum / matrix.rows
        means(col) = scaling match
          case FeatureScaling.None => 0.0
          case _ => mean

        var ss = 0.0
        row = 0
        while row < matrix.rows do
          val centered = matrix(row, col) - mean
          ss += centered * centered
          row += 1
        scales(col) = math.sqrt(ss / math.max(1, matrix.rows - 1))
        col += 1

      scaling match
        case FeatureScaling.None =>
          java.util.Arrays.fill(scales, 1.0)
        case FeatureScaling.ZScore =>
          floorScales(scales)
        case FeatureScaling.DiagonalShrinkage(alpha) =>
          require(alpha.value.isFinite && alpha.value >= 0.0 && alpha.value <= 1.0, "diagonal shrinkage alpha must be finite and in [0, 1]")
          val positive = scales.filter(_ > Eps)
          val target =
            if positive.isEmpty then 1.0
            else
              val sorted = positive.sorted
              sorted(sorted.length / 2)
          col = 0
          while col < scales.length do
            scales(col) = math.sqrt((1.0 - alpha.value) * scales(col) * scales(col) + alpha.value * target * target)
            col += 1
          floorScales(scales)

      Scaler(means, scales)

    private def floorScales(scales: Array[Double]): Unit =
      var i = 0
      while i < scales.length do
        if !scales(i).isFinite || scales(i) <= Eps then scales(i) = 1.0
        i += 1

  def categorical(response: Response, samples: Int): Either[MvpaError, Vector[ClassLabel]] =
    response.validate(samples).flatMap {
      case Response.Categorical(labels) => Right(labels)
      case Response.Probabilistic(_) | Response.Continuous(_) =>
        Left(MvpaError.InvalidClassifierInput("this classifier requires hard categorical response labels"))
    }

  def classSummary(data: DMat, labels: Vector[ClassLabel]): Either[MvpaError, ClassSummary] =
    if labels.length != data.rows then Left(MvpaError.ResponseLengthMismatch(data.rows, labels.length))
    else
      val classes = labels.distinct
      if classes.length < 2 then Left(MvpaError.SingleClassResponse)
      else
        val classIndex = classes.zipWithIndex.map { case (label, index) => label.value -> index }.toMap
        val counts = Array.fill(classes.length)(0)
        val sums = new Array[Double](classes.length * data.cols)
        var row = 0
        while row < data.rows do
          val klass = classIndex(labels(row).value)
          counts(klass) += 1
          var feature = 0
          while feature < data.cols do
            sums(klass * data.cols + feature) += data(row, feature)
            feature += 1
          row += 1

        if counts.exists(_ == 0) then Left(MvpaError.InvalidClassifierInput("every class must have at least one training sample"))
        else
          var klass = 0
          while klass < classes.length do
            var feature = 0
            while feature < data.cols do
              sums(klass * data.cols + feature) /= counts(klass)
              feature += 1
            klass += 1
          val means = Matrix.newBuilder(classes.length, data.cols)
          klass = 0
          while klass < classes.length do
            var feature = 0
            while feature < data.cols do
              means(klass, feature) = sums(klass * data.cols + feature)
              feature += 1
            klass += 1
          val total = counts.sum.toDouble
          Right(
            ClassSummary(
              classes = classes,
              counts = counts.toVector,
              priors = counts.map(_ / total).toVector,
              means = means.result()
            )
          )

  def pooledResidualCrossproduct(data: DMat, labels: Vector[ClassLabel], summary: ClassSummary): DMat =
    val classIndex = summary.classes.zipWithIndex.map { case (label, index) => label.value -> index }.toMap
    val out = Matrix.newBuilder(data.cols, data.cols)
    var row = 0
    while row < data.rows do
      val klass = classIndex(labels(row).value)
      var left = 0
      while left < data.cols do
        val residualLeft = data(row, left) - summary.means(klass, left)
        var right = 0
        while right < data.cols do
          val residualRight = data(row, right) - summary.means(klass, right)
          out(left, right) = out(left, right) + residualLeft * residualRight
          right += 1
        left += 1
      row += 1
    out.result()

  def rowCorrelationScores(data: DMat, centroids: DMat): DMat =
    val out = Matrix.newBuilder(data.rows, centroids.rows)
    val centroidMeans = new Array[Double](centroids.rows)
    val centroidNorms = new Array[Double](centroids.rows)
    var klass = 0
    while klass < centroids.rows do
      val mean = rowMean(centroids, klass)
      centroidMeans(klass) = mean
      centroidNorms(klass) = rowNorm(centroids, klass, mean)
      klass += 1

    var row = 0
    while row < data.rows do
      val mean = rowMean(data, row)
      val norm = rowNorm(data, row, mean)
      klass = 0
      while klass < centroids.rows do
        var dot = 0.0
        var col = 0
        while col < data.cols do
          dot += (data(row, col) - mean) * (centroids(klass, col) - centroidMeans(klass))
          col += 1
        out(row, klass) = dot / math.max(Eps, norm * centroidNorms(klass))
        klass += 1
      row += 1
    out.result()

  def linearCentroidScores(data: DMat, centroids: DMat, priors: Vector[Double]): DMat =
    require(centroids.rows == priors.length, "centroid rows must match priors")
    val out = Matrix.newBuilder(data.rows, centroids.rows)
    val norms = new Array[Double](centroids.rows)
    var klass = 0
    while klass < centroids.rows do
      var ss = 0.0
      var col = 0
      while col < centroids.cols do
        val value = centroids(klass, col)
        ss += value * value
        col += 1
      norms(klass) = ss
      klass += 1

    var row = 0
    while row < data.rows do
      klass = 0
      while klass < centroids.rows do
        var dot = 0.0
        var col = 0
        while col < data.cols do
          dot += data(row, col) * centroids(klass, col)
          col += 1
        out(row, klass) =
          dot - 0.5 * norms(klass) + math.log(math.max(priors(klass), 1e-300))
        klass += 1
      row += 1
    out.result()

  def softmax(scores: DMat): DMat =
    val out = Matrix.newBuilder(scores.rows, scores.cols)
    var row = 0
    while row < scores.rows do
      var maxScore = scores(row, 0)
      var col = 1
      while col < scores.cols do
        maxScore = math.max(maxScore, scores(row, col))
        col += 1
      var sum = 0.0
      col = 0
      while col < scores.cols do
        val value = math.exp(scores(row, col) - maxScore)
        out(row, col) = value
        sum += value
        col += 1
      col = 0
      while col < scores.cols do
        out(row, col) = out(row, col) / sum
        col += 1
      row += 1
    out.result()

  def accuracy(prediction: ClassificationPrediction, labels: Vector[ClassLabel]): Either[MvpaError, Double] =
    if prediction.probabilities.rows == 0 then Left(MvpaError.InvalidClassifierInput("accuracy requires at least one prediction"))
    else prediction.sampleIndices.find(index => index.value < 0 || index.value >= labels.length) match
      case Some(index) =>
        Left(MvpaError.FoldIndexOutOfBounds("prediction", index.value, labels.length))
      case None =>
        val predicted = prediction.predicted
        var correct = 0
        var row = 0
        while row < predicted.length do
          if predicted(row) == labels(prediction.sampleIndices(row).value) then correct += 1
          row += 1
        Right(correct.toDouble / predicted.length)

  /** Reorders a model-local probability matrix to the frozen response order.
    * Cross-validation fits may encounter the same classes in a different row
    * order, but a public OOF result has one class-column convention.
    */
  def reorderProbabilities(
      classes: Vector[ClassLabel],
      probabilities: DMat,
      expectedClasses: Vector[ClassLabel]
  ): Either[MvpaError, DMat] =
    if probabilities.cols != classes.length then
      Left(MvpaError.MatrixShapeMismatch(s"probability columns ${probabilities.cols} != class count ${classes.length}"))
    else predictionClassColumns(classes, expectedClasses).map: columns =>
      val out = Matrix.newBuilder(probabilities.rows, expectedClasses.length)
      var row = 0
      while row < probabilities.rows do
        var klass = 0
        while klass < expectedClasses.length do
          out(row, klass) = probabilities(row, columns(klass))
          klass += 1
        row += 1
      out.result()

  private def predictionClassColumns(
      predictionClasses: Vector[ClassLabel],
      expectedClasses: Vector[ClassLabel]
  ): Either[MvpaError, Array[Int]] =
    if predictionClasses.distinct.length != predictionClasses.length then
      Left(MvpaError.InvalidClassifierInput("classifier prediction classes contain duplicates"))
    else
      val classIndex = predictionClasses.zipWithIndex.map { case (label, index) => label.value -> index }.toMap
      val expectedSet = expectedClasses.map(_.value).toSet
      if classIndex.keySet != expectedSet then
        Left(MvpaError.InvalidClassifierInput("classifier prediction classes did not match the response classes"))
      else
        val columns = new Array[Int](expectedClasses.length)
        var klass = 0
        while klass < expectedClasses.length do
          columns(klass) = classIndex(expectedClasses(klass).value)
          klass += 1
        Right(columns)

  def transpose(matrix: DMat): DMat =
    matrix.t

  private def rowMean(matrix: DMat, row: Int): Double =
    var sum = 0.0
    var col = 0
    while col < matrix.cols do
      sum += matrix(row, col)
      col += 1
    sum / matrix.cols

  private def rowNorm(matrix: DMat, row: Int, mean: Double): Double =
    var ss = 0.0
    var col = 0
    while col < matrix.cols do
      val centered = matrix(row, col) - mean
      ss += centered * centered
      col += 1
    math.sqrt(ss)
