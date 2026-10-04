package scalafim.fmri.mvpa.dataset.predictive

import alder.kernel.DataFingerprint
import multivar.core.SpaceRole
import resample4s.core.{DigestAlgorithm, Labels}
import resample4s.designs.FixedPartitions
import scalafim.fmri.mvpa.*

class AlderOperatorRidgeSuite extends munit.FunSuite:
  given DigestAlgorithm = DigestAlgorithm.fnv1a64
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private val values = Vector(
    Vector(2.0, 1.0), Vector(-2.0, -1.0), Vector(-1.0, -2.0),
    Vector(1.0, 2.0), Vector(2.5, 0.5), Vector(-2.5, -0.5)
  )
  private val target = Vector(0.0, 1.0, 1.0, 0.0, 0.0, 1.0)
  private val coding = right(SwiftTargetCoding(Vector(0.0 -> "left", 1.0 -> "right")))

  private def fixture(input: Vector[Vector[Double]] = values): (AlderOperatorRidgeRows, ValidationDesign[?, ?, resample4s.core.Coverage.ExactOnce]) =
    val keys = input.indices.map(i => s"sample:$i").toVector
    val axis = right(AxisRef.fromStableKeys("operator-samples", SpaceRole.Samples, keys, "trial", "none", "one"))
    val mapping = right(NativeAxisMapping.fromAxis(axis, keys.indices.map(i => 900L + i).toVector, DataFingerprint.external("operator-ridge-suite")))
    val matrix = PatternMatrix.fromRows(input)
    val operator = right(PatternOperator.fromMatrix(matrix))
    val admitted = right(AlderOperatorRidge.admit(operator, target, mapping, right(MaterializationBudget(64L))))
    val design = right(ValidationDesign.bind(axis, right(FixedPartitions.once(right(Labels.retained(IArray.unsafeFromArray(Array(0, 0, 1, 1, 2, 2)))))), ScientificSeed.fromLong(19L)))
    (admitted, design)

  test("operator ridge completes Alder fold fits without materializing a dense feature table"):
    val (rows, design) = fixture()
    val result = right(AlderOperatorRidge.crossValidate(rows, design, coding, right(OperatorRidgeConfig(penalty = 0.5, tolerance = 1e-10, maxIterations = 1000))))
    assertEquals(result.prediction.scores.rows, values.length)
    assertEquals(result.prediction.scores.cols, coding.classes.length)
    assertEquals(result.admission.targetCells, values.length.toLong)
    assert(result.fits.forall(_.model.receipt.patternProvenance.origin == PatternOperatorOrigin.Dense))
    assert(result.fits.forall(_.model.receipt.features == 2))

  test("held-out operator values do not change fitted coefficients"):
    val (baselineRows, design) = fixture()
    val changed = values.updated(0, Vector(200.0, -100.0)).updated(1, Vector(-200.0, 100.0))
    val (changedRows, changedDesign) = fixture(changed)
    val config = right(OperatorRidgeConfig(penalty = 0.5, tolerance = 1e-10, maxIterations = 1000))
    val baseline = right(AlderOperatorRidge.crossValidate(baselineRows, design, coding, config))
    val perturbed = right(AlderOperatorRidge.crossValidate(changedRows, changedDesign, coding, config))
    val baselineFold = baseline.fits.find(_.unit.fold == 0).get.model
    val perturbedFold = perturbed.fits.find(_.unit.fold == 0).get.model
    (0 until baselineFold.coefficients.rows).foreach: row =>
      (0 until baselineFold.coefficients.cols).foreach: col =>
        assertEqualsDouble(baselineFold.coefficients(row, col), perturbedFold.coefficients(row, col), 1e-10)

  test("hard-label fold class order is explicitly aligned to coding order"):
    val (rows, design) = fixture()
    val config = right(OperatorRidgeConfig(penalty = 0.5, tolerance = 1e-10, maxIterations = 1000))
    val actual = right(AlderOperatorRidge.crossValidate(rows, design, coding, config))
    assertEquals(actual.fits.head.model.classes, coding.classes.reverse)
    actual.fits.foreach: fold =>
      val unit = right(design.at(fold.unit))
      val positions = unit.assessment.ordinals.toVector
      val restricted = right(rows.operator.selectRowPositions(positions))
      val expected = right(fold.model.predict(restricted))
      positions.zipWithIndex.foreach: (position, local) =>
        coding.classes.zipWithIndex.foreach: (label, column) =>
          assertEqualsDouble(actual.prediction.scores(position, column), expected.scores(local, expected.classes.indexOf(label)), 1e-10)

  test("soft target admission preserves simplex fits and refuses a different coding order"):
    val (hard, design) = fixture()
    val membership = right(ClassMembership.simplex(coding.classes, gale.linalg.DMat.dense(6, 2, target.flatMap(value => if value == 0.0 then Vector(0.8, 0.2) else Vector(0.1, 0.9)))))
    val soft = right(AlderOperatorRidge.admitSoft(hard.operator, membership, hard.mapping, right(MaterializationBudget(100))))
    val config = right(OperatorRidgeConfig(penalty = 0.5, tolerance = 1e-10, maxIterations = 1000))
    val actual = right(AlderOperatorRidge.crossValidate(soft, design, coding, config))
    assert(actual.fits.forall(_.model.classes == coding.classes))
    actual.fits.foreach: fold =>
      val unit = right(design.at(fold.unit))
      val positions = unit.analysis.ordinals.toVector
      val selected = right(soft.operator.selectRowPositions(positions))
      val targets = right(ClassMembership.simplex(coding.classes, gale.linalg.DMat.tabulate(positions.length, 2)((row, column) => membership.values(positions(row), column))))
      val expected = right(OperatorRidge.fit(selected, targets, config))
      (0 until expected.coefficients.rows).foreach: row =>
        (0 until expected.coefficients.cols).foreach: column =>
          assertEqualsDouble(fold.model.coefficients(row, column), expected.coefficients(row, column), 1e-10)
    val reversed = right(SwiftTargetCoding(Vector(1.0 -> "right", 0.0 -> "left")))
    assert(AlderOperatorRidge.crossValidate(soft, design, reversed, config).isLeft)
