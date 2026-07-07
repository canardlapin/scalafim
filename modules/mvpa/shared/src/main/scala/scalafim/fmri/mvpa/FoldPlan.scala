package scalafim.fmri.mvpa

final case class Fold private (
    id: String,
    train: Vector[SampleIndex],
    test: Vector[SampleIndex]
):
  require(id.nonEmpty, "fold id must be non-empty")
  require(train.nonEmpty && test.nonEmpty, "fold train/test sets must be non-empty")

object Fold:
  def apply(id: String, train: Seq[Int], test: Seq[Int]): Either[MvpaError, Fold] =
    val trimmed = id.trim
    if trimmed.isEmpty || train.isEmpty || test.isEmpty then Left(MvpaError.EmptyFold(id))
    else
      val trainIdx = train.map(SampleIndex.apply).toVector
      val testIdx = test.map(SampleIndex.apply).toVector
      if trainIdx.map(_.value).toSet.intersect(testIdx.map(_.value).toSet).nonEmpty then
        Left(MvpaError.FoldTrainTestOverlap(trimmed))
      else
        Right(new Fold(trimmed, trainIdx, testIdx))

  def unsafe(id: String, train: Seq[Int], test: Seq[Int]): Fold =
    apply(id, train, test).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class FoldPlan private (folds: Vector[Fold], samples: Int):
  require(folds.nonEmpty, "fold plan must be non-empty")
  require(samples > 1, "fold plan requires at least two samples")

object FoldPlan:
  def apply(folds: Seq[Fold], samples: Int): Either[MvpaError, FoldPlan] =
    val foldVector = folds.toVector
    if foldVector.isEmpty then Left(MvpaError.EmptyFoldPlan)
    else if samples <= 1 then Left(MvpaError.InvalidSampleAxis("fold plan requires at least two samples"))
    else
      foldVector
        .flatMap(fold => fold.train ++ fold.test)
        .find(index => index.value < 0 || index.value >= samples)
        .map(index => Left(MvpaError.FoldIndexOutOfBounds("fold", index.value, samples)))
        .getOrElse(Right(new FoldPlan(foldVector, samples)))

  def unsafe(folds: Seq[Fold], samples: Int): FoldPlan =
    apply(folds, samples).fold(error => throw new IllegalArgumentException(error.message), identity)

  def alignTo(axis: SampleAxis, folds: FoldPlan): Either[MvpaError, FoldPlan] =
    if folds.samples == axis.samples then Right(folds)
    else Left(MvpaError.ResponseLengthMismatch(axis.samples, folds.samples))

  def leaveOneBlockOut(blocks: Seq[Int]): Either[MvpaError, FoldPlan] =
    val blockVector = blocks.toVector
    val samples = blockVector.length
    if samples == 0 then Left(MvpaError.EmptyResponse)
    else
      val ids = blockVector.distinct.sorted
      val folded = ids.map { block =>
        val test = blockVector.zipWithIndex.collect { case (b, index) if b == block => index }
        val train = blockVector.indices.filterNot(test.toSet).toVector
        Fold(block.toString, train, test)
      }
      folded.collectFirst { case Left(error) => error } match
        case Some(error) => Left(error)
        case None => FoldPlan(folded.collect { case Right(fold) => fold }, samples)
