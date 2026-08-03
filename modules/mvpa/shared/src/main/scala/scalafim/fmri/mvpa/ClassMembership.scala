package scalafim.fmri.mvpa

import gale.linalg.{DMat, Matrix}

enum ClassMembershipKind:
  case HardLabels
  case Simplex

/** A validated sample-by-class target matrix.
  *
  * Rows lie on the probability simplex. Hard labels are represented by one-hot
  * rows, while genuinely soft targets retain their distinct construction kind.
  * This is a target contract, not a claim that a decoder's scores are
  * probabilities.
  */
final class ClassMembership private (
    val classes: Vector[ClassLabel],
    val values: DMat,
    val kind: ClassMembershipKind
):
  require(classes.length >= 2, "class membership requires at least two classes")
  require(classes.distinct.length == classes.length, "class membership classes must be unique")
  require(values.rows > 0, "class membership requires at least one sample")
  require(values.cols == classes.length, "membership columns must match classes")

  def samples: Int = values.rows
  def classCount: Int = classes.length

  def argmaxLabels: Vector[ClassLabel] =
    Vector.tabulate(samples)(argmaxLabelAt)

  private[mvpa] def argmaxLabelAt(sample: Int): ClassLabel =
    require(sample >= 0 && sample < samples, "membership sample index out of bounds")
    var bestClass = 0
    var bestValue = values(sample, 0)
    var klass = 1
    while klass < classCount do
      val candidate = values(sample, klass)
      if candidate > bestValue then
        bestValue = candidate
        bestClass = klass
      klass += 1
    classes(bestClass)

  private[mvpa] def selectPositions(positions: IndexedSeq[Int]): DMat =
    val selected = Matrix.newBuilder(positions.length, classCount)
    var row = 0
    while row < positions.length do
      val sourceRow = positions(row)
      require(sourceRow >= 0 && sourceRow < samples, "membership selection index out of bounds")
      var klass = 0
      while klass < classCount do
        selected(row, klass) = values(sourceRow, klass)
        klass += 1
      row += 1
    selected.result()

  private[mvpa] def subset(positions: IndexedSeq[Int]): ClassMembership =
    new ClassMembership(classes, selectPositions(positions), kind)

object ClassMembership:
  private val SimplexTolerance = 1e-10

  def hard(labels: Seq[ClassLabel]): Either[MvpaError, ClassMembership] =
    val labelVector = labels.toVector
    if labelVector.isEmpty then Left(MvpaError.EmptyResponse)
    else if labelVector.exists(_.value.trim.isEmpty) then
      Left(MvpaError.InvalidClassMembership("hard class labels must be non-empty"))
    else
      val classes = labelVector.distinct
      if classes.length < 2 then Left(MvpaError.SingleClassResponse)
      else
        val classIndex = classes.zipWithIndex.map { case (label, index) => label.value -> index }.toMap
        val memberships = Matrix.newBuilder(labelVector.length, classes.length)
        var sample = 0
        while sample < labelVector.length do
          memberships(sample, classIndex(labelVector(sample).value)) = 1.0
          sample += 1
        Right(new ClassMembership(classes, memberships.result(), ClassMembershipKind.HardLabels))

  def simplex(
      classes: Seq[ClassLabel],
      values: DMat
  ): Either[MvpaError, ClassMembership] =
    val classVector = classes.toVector
    if classVector.length < 2 then Left(MvpaError.SingleClassResponse)
    else if classVector.exists(_.value.trim.isEmpty) then
      Left(MvpaError.InvalidClassMembership("simplex class labels must be non-empty"))
    else if classVector.distinct.length != classVector.length then
      Left(MvpaError.InvalidClassMembership("simplex class labels must be unique"))
    else if values.rows == 0 then Left(MvpaError.EmptyResponse)
    else if values.cols != classVector.length then
      Left(
        MvpaError.InvalidClassMembership(
          s"membership columns ${values.cols} do not match class count ${classVector.length}"
        )
      )
    else
      var sample = 0
      while sample < values.rows do
        var total = 0.0
        var klass = 0
        while klass < values.cols do
          val value = values(sample, klass)
          if !value.isFinite || value < 0.0 || value > 1.0 then
            return Left(
              MvpaError.InvalidClassMembership(
                s"membership at sample $sample, class $klass must be finite and in [0, 1]"
              )
            )
          total += value
          klass += 1
        if math.abs(total - 1.0) > SimplexTolerance then
          return Left(
            MvpaError.InvalidClassMembership(
              s"membership row $sample sums to $total instead of 1"
            )
          )
        sample += 1

      var klass = 0
      while klass < values.cols do
        var mass = 0.0
        sample = 0
        while sample < values.rows do
          mass += values(sample, klass)
          sample += 1
        if mass <= SimplexTolerance then
          return Left(
            MvpaError.InvalidClassMembership(
              s"class ${classVector(klass).value} has no membership mass"
            )
          )
        klass += 1

      Right(new ClassMembership(classVector, values, ClassMembershipKind.Simplex))

  private[mvpa] def fromResponse(response: Response): Either[MvpaError, ClassMembership] =
    response match
      case Response.Categorical(labels)       => hard(labels)
      case Response.Probabilistic(membership) => Right(membership)
      case Response.Continuous(_) =>
        Left(MvpaError.InvalidClassMembership("classification requires categorical or probabilistic targets"))
