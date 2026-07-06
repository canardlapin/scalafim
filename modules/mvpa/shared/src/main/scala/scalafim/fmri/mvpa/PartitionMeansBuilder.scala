package scalafim.fmri.mvpa

final case class PartitionMeanPatterns(
    classes: Vector[ClassLabel],
    means: PartitionMeans
):
  def items: Vector[String] =
    classes.map(_.value)

object PartitionMeansBuilder:
  def fromPatterns(
      data: PatternMatrix,
      response: Response,
      folds: FoldPlan
  ): Either[MvpaError, PartitionMeanPatterns] =
    for
      _ <- validateFolds(data, folds)
      _ <- Classification.validateFinite(data.value, "partition data")
      labels <- Classification.categorical(response, data.samples)
      result <- build(data, labels, folds)
    yield result

  private def validateFolds(data: PatternMatrix, folds: FoldPlan): Either[MvpaError, Unit] =
    if folds.samples != data.samples then
      Left(MvpaError.ResponseLengthMismatch(data.samples, folds.samples))
    else Right(())

  private def build(
      data: PatternMatrix,
      labels: Vector[ClassLabel],
      folds: FoldPlan
  ): Either[MvpaError, PartitionMeanPatterns] =
    val classes = labels.distinct
    val classIndex = classes.zipWithIndex.map { case (label, index) => label.value -> index }.toMap
    val counts = new Array[Int](folds.folds.length * classes.length)
    val sums = new Array[Double](folds.folds.length * classes.length * data.features)

    var foldIndex = 0
    while foldIndex < folds.folds.length do
      val fold = folds.folds(foldIndex)
      var rowIndex = 0
      while rowIndex < fold.test.length do
        val sample = fold.test(rowIndex).value
        val condition = classIndex(labels(sample).value)
        counts(foldIndex * classes.length + condition) += 1
        var feature = 0
        while feature < data.features do
          sums((foldIndex * classes.length + condition) * data.features + feature) +=
            data.value.dataArray(sample * data.features + feature)
          feature += 1
        rowIndex += 1
      foldIndex += 1

    var missingFold = -1
    var missingCondition = -1
    foldIndex = 0
    while foldIndex < folds.folds.length && missingFold < 0 do
      var condition = 0
      while condition < classes.length && missingFold < 0 do
        if counts(foldIndex * classes.length + condition) == 0 then
          missingFold = foldIndex
          missingCondition = condition
        condition += 1
      foldIndex += 1

    if missingFold >= 0 then
      Left(
        MvpaError.InvalidRdmInput(
          s"crossnobis fold '${folds.folds(missingFold).id}' has no samples for class '${classes(missingCondition).value}'"
        )
      )
    else
      foldIndex = 0
      while foldIndex < folds.folds.length do
        var condition = 0
        while condition < classes.length do
          val count = counts(foldIndex * classes.length + condition).toDouble
          var feature = 0
          while feature < data.features do
            sums((foldIndex * classes.length + condition) * data.features + feature) /= count
            feature += 1
          condition += 1
        foldIndex += 1

      PartitionMeans(classes.length, data.features, folds.folds.length, sums).map { means =>
        PartitionMeanPatterns(classes, means)
      }
