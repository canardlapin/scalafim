package scalafim.fmri.mvpa

import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.DoubleLinearOperator
import gale.linalg.MutableDVec
import org.scalacheck.Gen
import org.scalacheck.Shrink
import org.scalacheck.Test
import org.scalacheck.rng.Seed

private[mvpa] enum MvpaLawProfile(
    val successfulTests: Int,
    val maximumSize: Int
):
  case PullRequest extends MvpaLawProfile(successfulTests = 48, maximumSize = 28)
  case Calibration extends MvpaLawProfile(successfulTests = 300, maximumSize = 96)

private[mvpa] object MvpaLawProfile:
  val current: MvpaLawProfile =
    sys.env.get("SCALAFIM_LAW_PROFILE") match
      case None | Some("") | Some("pr") | Some("pull-request") =>
        MvpaLawProfile.PullRequest
      case Some("calibration") =>
        MvpaLawProfile.Calibration
      case Some(other) =>
        throw new IllegalArgumentException(
          s"SCALAFIM_LAW_PROFILE must be 'pull-request' or 'calibration', got '$other'"
        )

  val initialSeed: String =
    val configured =
      sys.env
        .get("SCALAFIM_LAW_SEED")
        .orElse:
          sys.env
            .get("SCALAFIM_LAW_SEED_LONG")
            .map: raw =>
              val value = raw.toLongOption.getOrElse:
                throw new IllegalArgumentException(
                  "SCALAFIM_LAW_SEED_LONG must be a signed 64-bit integer"
                )
              Seed(value).toBase64
        .getOrElse(Seed(0x6d767061L).toBase64)
    require(
      Seed.fromBase64(configured).isSuccess,
      "SCALAFIM_LAW_SEED must be a ScalaCheck base64 seed"
    )
    configured

/** One deterministic execution policy for every generated MVPA law. The same seed reproduces a shrunken counterexample
  * on the JVM and Scala.js.
  */
private[mvpa] trait MvpaGeneratedLawSuite extends munit.ScalaCheckSuite:
  private val profile = MvpaLawProfile.current

  override def scalaCheckInitialSeed: String =
    MvpaLawProfile.initialSeed

  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(profile.successfulTests)
      .withMinSize(0)
      .withMaxSize(profile.maximumSize)
      .withWorkers(1)

private[mvpa] final case class ReindexLawCase(
    size: Int,
    salt: Int,
    permutation: Vector[Int],
    selection: Vector[Int],
    injection: Vector[Int],
    draw: Vector[Int]
):
  require(size > 0)
  require(permutation.sorted == (0 until size).toVector)
  require(selection.nonEmpty && selection == selection.sorted && selection.distinct == selection)
  require(selection.forall(position => position >= 0 && position < size))
  require(injection.nonEmpty && injection.distinct == injection)
  require(injection.forall(position => position >= 0 && position < size))
  require(draw.length >= 2 && draw.forall(position => position >= 0 && position < size))
  require(draw.distinct.length < draw.length)

private[mvpa] final case class EvidenceLawCase(
    rowCount: Int,
    columnCount: Int,
    salt: Int,
    cells: Vector[Int],
    rightWeights: Vector[Int],
    rowScores: Vector[Int],
    rowSelection: Vector[Int],
    rowDraw: Vector[Int],
    supportPositions: Vector[Int],
    supportWeights: Vector[Int],
    localCount: Int,
    projection: Vector[Int]
):
  require(rowCount > 0 && columnCount > 0 && localCount > 0)
  require(cells.length == rowCount * columnCount)
  require(rightWeights.length == columnCount)
  require(rowScores.length == rowCount)
  require(rowSelection.nonEmpty && rowSelection == rowSelection.sorted)
  require(rowSelection.distinct == rowSelection)
  require(rowSelection.forall(position => position >= 0 && position < rowCount))
  require(rowDraw.nonEmpty && rowDraw.forall(position => position >= 0 && position < rowCount))
  require(supportPositions.nonEmpty && supportPositions == supportPositions.sorted)
  require(supportPositions.distinct == supportPositions)
  require(supportPositions.forall(position => position >= 0 && position < columnCount))
  require(supportPositions.length == supportWeights.length)
  require(supportWeights.forall(_ != 0))
  require(projection.length == localCount * columnCount)

private[mvpa] final case class MutationLawCase(
    size: Int,
    salt: Int,
    mapping: Vector[Int]
):
  require(size >= 2)
  require(mapping.sorted == (0 until size).toVector)
  require(mapping != (0 until size).toVector)

/** A hostile residual-moment domain. Every value carries both a proper rank deficiency and a scale separation large
  * enough to exercise numerical conditioning without changing the positive-semidefinite estimand.
  */
private[mvpa] final case class ResidualCapabilityLawCase(
    size: Int,
    salt: Int,
    deficientRank: Int,
    conditionExponent: Int
):
  require(size >= 2)
  require(deficientRank > 0 && deficientRank < size)
  require(conditionExponent >= 6 && conditionExponent <= 12)

/** A two-class open-learner domain. Shrinking must retain at least one observation from each class and a positive class
  * separation.
  */
private[mvpa] final case class LearnerLawCase(
    repetitionsPerClass: Int,
    featureCount: Int,
    separation: Int,
    salt: Int
):
  require(repetitionsPerClass > 0)
  require(featureCount > 0)
  require(separation > 0)

private[mvpa] object MvpaLawGenerators:
  private val nonZeroInt =
    Gen.oneOf(-4, -3, -2, -1, 1, 2, 3, 4)

  private def dimension(maximum: Int): Gen[Int] =
    Gen.frequency(
      3 -> Gen.const(1),
      7 -> Gen.choose(2, maximum)
    )

  private def picked(count: Int, size: Int): Gen[Vector[Int]] =
    Gen.pick(count, (0 until size).toVector).map(_.toVector)

  val reindexCase: Gen[ReindexLawCase] =
    for
      size <- dimension(8)
      salt <- Gen.choose(1, 1000000)
      permutation <- picked(size, size)
      selectionSize <- Gen.choose(1, size)
      selection <- picked(selectionSize, size).map(_.sorted)
      injectionSize <- Gen.choose(1, size)
      injection = permutation.take(injectionSize)
      drawSize <- Gen.choose(2, size + 3)
      repeated <- Gen.choose(0, size - 1)
      remainder <- Gen.listOfN(drawSize - 2, Gen.choose(0, size - 1))
      draw = Vector(repeated, repeated) ++ remainder
    yield ReindexLawCase(
      size,
      salt,
      permutation,
      selection,
      injection,
      draw
    )

  val evidenceCase: Gen[EvidenceLawCase] =
    for
      rows <- dimension(7)
      columns <- dimension(7)
      salt <- Gen.choose(1, 1000000)
      cells <- Gen.listOfN(rows * columns, Gen.choose(-8, 8)).map(_.toVector)
      rightWeights <- Gen.listOfN(columns, Gen.choose(-5, 5)).map(_.toVector)
      rowScores <- Gen.listOfN(rows, Gen.choose(-5, 5)).map(_.toVector)
      selectionSize <- Gen.choose(1, rows)
      rowSelection <- picked(selectionSize, rows).map(_.sorted)
      drawSize <- Gen.choose(1, rows + 3)
      rowDraw <- Gen.listOfN(drawSize, Gen.choose(0, rows - 1)).map(_.toVector)
      supportSize <- Gen.choose(1, columns)
      supportPositions <- picked(supportSize, columns).map(_.sorted)
      supportWeights <- Gen.listOfN(supportSize, nonZeroInt).map(_.toVector)
      localCount <- Gen.choose(1, 3)
      projection <- Gen.listOfN(localCount * columns, Gen.choose(-4, 4)).map(_.toVector)
    yield EvidenceLawCase(
      rows,
      columns,
      salt,
      cells,
      rightWeights,
      rowScores,
      rowSelection,
      rowDraw,
      supportPositions,
      supportWeights,
      localCount,
      projection
    )

  val mutationCase: Gen[MutationLawCase] =
    for
      size <- Gen.choose(2, 8)
      salt <- Gen.choose(1, 1000000)
      shift <- Gen.choose(1, size - 1)
    yield MutationLawCase(
      size,
      salt,
      Vector.tabulate(size)(position => (position + shift) % size)
    )

  val residualCapabilityCase: Gen[ResidualCapabilityLawCase] =
    for
      size <- Gen.choose(2, 6)
      salt <- Gen.choose(1, 1000000)
      deficientRank <- Gen.choose(1, size - 1)
      conditionExponent <- Gen.choose(6, 12)
    yield ResidualCapabilityLawCase(
      size,
      salt,
      deficientRank,
      conditionExponent
    )

  val learnerCase: Gen[LearnerLawCase] =
    for
      repetitions <- Gen.choose(1, 5)
      features <- Gen.choose(1, 6)
      separation <- Gen.choose(1, 8)
      salt <- Gen.choose(1, 1000000)
    yield LearnerLawCase(repetitions, features, separation, salt)

  given Shrink[ReindexLawCase] = Shrink.withLazyList: value =>
    val candidates = Vector.newBuilder[ReindexLawCase]
    if value.salt != 1 then candidates += value.copy(salt = 1)
    if value.selection.length > 1 then candidates += value.copy(selection = value.selection.dropRight(1))
    if value.injection.length > 1 then candidates += value.copy(injection = value.injection.dropRight(1))
    if value.draw.length > 2 then candidates += value.copy(draw = value.draw.dropRight(1))
    if value.size > 1 then
      val smaller = math.max(1, value.size / 2)
      val reverse = Vector.tabulate(smaller)(position => smaller - position - 1)
      candidates += ReindexLawCase(
        smaller,
        1,
        reverse,
        Vector(0),
        reverse,
        Vector.fill(math.min(2, smaller + 1))(0)
      )
    LazyList.from(candidates.result().distinct)

  given Shrink[EvidenceLawCase] = Shrink.withLazyList: value =>
    val candidates = Vector.newBuilder[EvidenceLawCase]
    if value.salt != 1 then candidates += value.copy(salt = 1)
    val halved = value.copy(
      cells = value.cells.map(_ / 2),
      rightWeights = value.rightWeights.map(_ / 2),
      rowScores = value.rowScores.map(_ / 2),
      projection = value.projection.map(_ / 2)
    )
    if halved != value then candidates += halved
    if value.rowCount > 1 then candidates += resize(value, math.max(1, value.rowCount / 2), value.columnCount)
    if value.columnCount > 1 then candidates += resize(value, value.rowCount, math.max(1, value.columnCount / 2))
    if value.rowCount > 1 || value.columnCount > 1 || value.localCount > 1 then candidates += resize(value, 1, 1)
    LazyList.from(candidates.result().distinct)

  given Shrink[MutationLawCase] = Shrink.withLazyList: value =>
    if value.size == 2 && value.salt == 1 then LazyList.empty
    else
      LazyList(
        MutationLawCase(2, 1, Vector(1, 0)),
        value.copy(salt = 1)
      ).distinct

  given Shrink[ResidualCapabilityLawCase] = Shrink.withLazyList: value =>
    val candidates = Vector.newBuilder[ResidualCapabilityLawCase]
    if value.salt != 1 then candidates += value.copy(salt = 1)
    if value.conditionExponent != 6 then candidates += value.copy(conditionExponent = 6)
    if value.deficientRank > 1 then candidates += value.copy(deficientRank = 1)
    if value.size > 2 then
      val smaller = math.max(2, value.size / 2)
      candidates += ResidualCapabilityLawCase(
        smaller,
        1,
        math.min(value.deficientRank, smaller - 1),
        6
      )
    LazyList.from(candidates.result().distinct)

  given Shrink[LearnerLawCase] = Shrink.withLazyList: value =>
    val candidates = Vector.newBuilder[LearnerLawCase]
    if value.salt != 1 then candidates += value.copy(salt = 1)
    if value.repetitionsPerClass > 1 then candidates += value.copy(repetitionsPerClass = 1)
    if value.featureCount > 1 then candidates += value.copy(featureCount = 1)
    if value.separation > 1 then candidates += value.copy(separation = 1)
    LazyList.from(candidates.result().distinct)

  private def resize(
      value: EvidenceLawCase,
      rows: Int,
      columns: Int
  ): EvidenceLawCase =
    val cells = Vector.tabulate(rows * columns): flat =>
      val row = flat / columns
      val column = flat % columns
      value.cells(row * value.columnCount + column)
    val rowSelection =
      value.rowSelection.filter(_ < rows) match
        case Vector() => Vector(0)
        case kept     => kept
    val rowDraw =
      value.rowDraw.filter(_ < rows) match
        case Vector() => Vector(0)
        case kept     => kept
    val retainedSupport = value.supportPositions.zip(value.supportWeights).filter(_._1 < columns)
    val support =
      if retainedSupport.nonEmpty then retainedSupport
      else Vector(0 -> value.supportWeights.head)
    EvidenceLawCase(
      rows,
      columns,
      1,
      cells,
      value.rightWeights.take(columns),
      value.rowScores.take(rows),
      rowSelection,
      rowDraw,
      support.map(_._1),
      support.map(_._2),
      1,
      Vector.tabulate(columns)(column => value.projection(column))
    )

private[mvpa] object MvpaLawFixtures:
  private def admitted[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => throw new IllegalStateException(error.toString)

  def sampleAxis(
      size: Int,
      salt: Int,
      basisKind: String = "generated-trial-table"
  ): AxisRef[SampleId] =
    val token = Math.floorMod(salt, 1000001)
    val keys = Vector.tabulate(size): position =>
      admitted(
        SampleId(
          f"sub-${token % 97}%02d/run-${(position * 7 + token) % 19}%02d/trial-${101 + position * 23}%04d"
        )
      )
    admitted(
      AxisRef.create(
        admitted(AxisId(s"generated-samples-$token")),
        AxisPurpose.Samples,
        keys,
        admitted(CoordinateBasis(basisKind, Seq("ordering" -> "declared-not-ordinal"))),
        None,
        AxisScale.nominal,
        admitted(CoordinateProvenance("mvpa-law-generator", s"seed-$token"))
      )
    )

  def featureAxis(
      size: Int,
      salt: Int,
      purpose: AxisPurpose = AxisPurpose.NeuralFeatures,
      basisKind: String = "generated-neural-coordinates"
  ): AxisRef[FeatureId] =
    val token = Math.floorMod(salt, 1000001)
    val keys = Vector.tabulate(size): position =>
      admitted(
        FeatureId(
          f"mesh-${token % 89}%02d/vertex-${211 + position * 29}%05d"
        )
      )
    admitted(
      AxisRef.create(
        admitted(AxisId(s"generated-features-$token-${purpose.value}")),
        purpose,
        keys,
        admitted(
          CoordinateBasis(
            basisKind,
            Seq("coordinate-order" -> "explicit")
          )
        ),
        Some(admitted(AxisUnits("activation"))),
        AxisScale.nominal,
        admitted(CoordinateProvenance("mvpa-law-generator", s"seed-$token"))
      )
    )

  def effectAxis(size: Int, salt: Int): AxisRef[AxisKey] =
    val token = Math.floorMod(salt, 1000001)
    val keys = Vector.tabulate(size): position =>
      admitted(AxisKey(s"condition-${token % 71}-${position + 1}"))
    admitted(
      AxisRef.create(
        admitted(AxisId(s"generated-effects-$token")),
        AxisPurpose.Effects,
        keys,
        admitted(CoordinateBasis("generated-effect-contrasts")),
        None,
        admitted(AxisScale.named("interval")),
        admitted(CoordinateProvenance("mvpa-law-generator", s"seed-$token"))
      )
    )

  def diagonal(values: Vector[Double]): DMat =
    GaleTestMatrix.fromRows(
      Vector.tabulate(values.length): row =>
        Vector.tabulate(values.length): column =>
          if row == column then values(row) else 0.0
    )

  def matrix(value: EvidenceLawCase): DMat =
    fromFlat(value.rowCount, value.columnCount, value.cells.map(_.toDouble))

  def projection(value: EvidenceLawCase): DMat =
    fromFlat(value.localCount, value.columnCount, value.projection.map(_.toDouble))

  def column(values: Vector[Int]): DMat =
    GaleTestMatrix.fromRows(values.map(value => Vector(value.toDouble)))

  def fromFlat(rows: Int, columns: Int, values: Vector[Double]): DMat =
    GaleTestMatrix.fromRows(
      Vector.tabulate(rows): row =>
        Vector.tabulate(columns): column =>
          values(row * columns + column)
    )

  def selectRows(matrix: DMat, positions: Vector[Int]): DMat =
    GaleTestMatrix.fromRows(
      positions.map: sourceRow =>
        Vector.tabulate(matrix.cols)(column => matrix(sourceRow, column))
    )

  def rightMultiply(matrix: DMat, weights: DMat): DMat =
    GaleTestMatrix.fromRows(
      Vector.tabulate(matrix.rows): row =>
        Vector.tabulate(weights.cols): output =>
          var total = 0.0
          var column = 0
          while column < matrix.cols do
            total += matrix(row, column) * weights(column, output)
            column += 1
          total
    )

  def transposeMultiply(matrix: DMat, scores: DMat): DMat =
    GaleTestMatrix.fromRows(
      Vector.tabulate(matrix.cols): column =>
        Vector.tabulate(scores.cols): output =>
          var total = 0.0
          var row = 0
          while row < matrix.rows do
            total += matrix(row, column) * scores(row, output)
            row += 1
          total
    )

  def measure(matrix: DMat, projection: DMat): DMat =
    GaleTestMatrix.fromRows(
      Vector.tabulate(matrix.rows): row =>
        Vector.tabulate(projection.rows): local =>
          var total = 0.0
          var column = 0
          while column < matrix.cols do
            total += matrix(row, column) * projection(local, column)
            column += 1
          total
    )

  def sameMatrix(left: DMat, right: DMat, tolerance: Double = 1e-10): Boolean =
    if left.rows != right.rows || left.cols != right.cols then false
    else
      var equal = true
      var row = 0
      while row < left.rows && equal do
        var column = 0
        while column < left.cols && equal do
          val difference = math.abs(left(row, column) - right(row, column))
          equal = difference <= tolerance
          column += 1
        row += 1
      equal

  def indices(values: Vector[Int]): IArray[Int] =
    IArray.unsafeFromArray(values.toArray)

  final class MatrixFree private (matrix: DMat) extends DoubleLinearOperator:
    var forwardCalls: Int = 0
    var transposeCalls: Int = 0

    override def rows: Int =
      matrix.rows

    override def cols: Int =
      matrix.cols

    override def applyTo(input: DVec, output: MutableDVec): Unit =
      forwardCalls += 1
      var row = 0
      while row < rows do
        var total = 0.0
        var column = 0
        while column < cols do
          total += matrix(row, column) * input(column)
          column += 1
        output(row) = total
        row += 1

    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
      transposeCalls += 1
      var column = 0
      while column < cols do
        var total = 0.0
        var row = 0
        while row < rows do
          total += matrix(row, column) * input(row)
          row += 1
        output(column) = total
        column += 1

  object MatrixFree:
    def apply(matrix: DMat): MatrixFree =
      new MatrixFree(matrix)

private[mvpa] final case class ReindexObservation(
    parentFingerprint: String,
    childFingerprint: String,
    keys: Vector[String],
    identityKeys: Vector[String],
    positions: Vector[Int],
    values: Vector[Int]
)

private[mvpa] trait ReindexSubject:
  def observe(value: MutationLawCase): ReindexObservation

private[mvpa] object PublicReindexSubject extends ReindexSubject:
  override def observe(value: MutationLawCase): ReindexObservation =
    val axis = MvpaLawFixtures.sampleAxis(value.size, value.salt)
    val relation = axis
      .reorder(value.mapping)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val column = Column(axis, Vector.tabulate(value.size)(identity))
      .fold(error => throw new IllegalStateException(error.message), identity)
      .reindex(relation)
      .fold(error => throw new IllegalStateException(error.message), identity)
    ReindexObservation(
      axis.identity.fingerprint.value,
      relation.child.identity.fingerprint.value,
      relation.child.keys.map(_.value),
      relation.child.identity.orderedKeys.map(_.value),
      relation.reindexing.toVector,
      column.toVector
    )

private[mvpa] object IdentityDroppingSubject extends ReindexSubject:
  override def observe(value: MutationLawCase): ReindexObservation =
    val observed = PublicReindexSubject.observe(value)
    observed.copy(childFingerprint = observed.parentFingerprint)

private[mvpa] object HiddenReorderingSubject extends ReindexSubject:
  override def observe(value: MutationLawCase): ReindexObservation =
    val observed = PublicReindexSubject.observe(value)
    val order = observed.positions.indices.sortBy(observed.positions)
    observed.copy(
      keys = order.map(observed.keys).toVector,
      identityKeys = order.map(observed.identityKeys).toVector,
      positions = order.map(observed.positions).toVector,
      values = order.map(observed.values).toVector
    )

private[mvpa] object ReindexLawCourt:
  def violations(
      value: MutationLawCase,
      observed: ReindexObservation
  ): Vector[String] =
    val axis = MvpaLawFixtures.sampleAxis(value.size, value.salt)
    val expectedKeys = value.mapping.map(position => axis.keys(position).value)
    val expectedValues = value.mapping
    Vector(
      Option.when(observed.childFingerprint == observed.parentFingerprint)("identity-loss"),
      Option.when(observed.positions != value.mapping)("source-order"),
      Option.when(observed.keys != expectedKeys)("key-order"),
      Option.when(observed.identityKeys != expectedKeys)("identity-key-order"),
      Option.when(observed.values != expectedValues)("value-order")
    ).flatten
