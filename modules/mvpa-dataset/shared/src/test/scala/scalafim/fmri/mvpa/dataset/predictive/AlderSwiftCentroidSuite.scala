package scalafim.fmri.mvpa.dataset.predictive

import alder.kernel.DataFingerprint
import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import resample4s.core.{DigestAlgorithm, IndexSpace, Injection, Labels}
import resample4s.designs.FixedPartitions
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.measurement.{MeasurementId, MeasurementLeg}
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class AlderSwiftCentroidSuite extends munit.FunSuite:
  given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private def right[A](value: Either[?, A]): A =
    value.fold(error => fail(error.toString), identity)

  // R-generated frozen M0 fixture: generate_migration_parity.R, commit
  // 528c302e454697055bc9af31c9a6eca684f019e3,
  // JSON053e50710a241e6eacee4e532bf0d398d02a84536bacf5bf5db6ca444144d04d.
  private val values = Vector(
    Vector(2.0, 1.0, 0.0), Vector(-2.0, -1.0, 0.0),
    Vector(-1.5, -1.0, 1.0), Vector(1.5, 1.0, -1.0), Vector(0.8, 0.5, 0.0),
    Vector(2.2, 0.7, 1.0), Vector(-2.0, -0.4, -1.0),
    Vector(-0.7, -0.2, 0.5), Vector(-1.8, -1.2, 0.0)
  )
  private val target = Vector(0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0)
  private val runs = Vector(0, 0, 1, 1, 1, 2, 2, 2, 2)
  private val frozenB = Vector(0.93360327508833352, 0.041840832422621453,
    0.36120276152878011, 0.81713721780146087, 0.87686005915306098,
    0.46949030787242568, 0.14408805126964574, 0.050354240185370026,
    0.011793534313393831)
  private val coding = right(SwiftTargetCoding(Vector(0.0 -> "b", 1.0 -> "a")))

  private def source: EvidenceSource =
    val id = SourceId.unsafe("swift-fixture")
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe("swift-root"), id)))

  private final class Fixture(
      input: Vector[Vector[Double]] = values,
      outcomes: Vector[Double] = target,
      blocks: Vector[Int] = runs
  ):
    val keys = input.indices.map(i => s"original:$i").toVector
    val axis = right(AxisRef.fromStableKeys("samples", SpaceRole.Samples, keys, "trial", "none", "one"))
    val neural = right(AxisRef.fromStableKeys("neural", SpaceRole.Observed,
      input.head.indices.map(i => s"voxel:$i").toVector, "voxel", "psc", "raw"))
    val response = right(AxisRef.fromStableKeys("target", SpaceRole.Observed, Vector("class-code"), "class", "none", "code"))
    val matrix = DMat.dense(input.length, input.head.length, input.flatten)
    val targets = right(MultiResponse.fromDense(axis, response,
      DMat.dense(input.length, 1, outcomes), ValueIdentity.source(ValueId.unsafe("target-values")), source))
    val mapping = right(NativeAxisMapping.fromAxis(axis,
      keys.indices.map(i => 100L - i.toLong * 3L).toVector, DataFingerprint.external("swift-fixture-v1")))
    val design = right(ValidationDesign.bind(axis,
      right(FixedPartitions.once(right(Labels.retained(IArray.unsafeFromArray(blocks.toArray))))), ScientificSeed.fromLong(23L)))
    var calls = 0
    val operator = new DoubleLinearOperator:
      val rows = matrix.rows
      val cols = matrix.cols
      def applyTo(x: DVec, into: MutableDVec): Unit =
        calls += 1
        matrix.applyTo(x, into)
      override def transposeApplyTo(x: DVec, into: MutableDVec): Unit =
        calls += 1
        matrix.transposeApplyTo(x, into)
    def observations(native: Boolean): Observations[axis.Id, neural.Id] =
      if native then right(Observations.fromOperator(axis, neural, operator,
        ValueIdentity.source(ValueId.unsafe("input-values")), source))
      else right(Observations.fromDense(axis, neural, matrix,
        ValueIdentity.source(ValueId.unsafe("input-values")), source))
    def rows(native: Boolean = false, selected: Vector[Int] = input.head.indices.toVector, cells: Long = 10000L): Either[AlderPredictiveAdmissionError, AlderMaterializedRows[String]] =
      val selection = right(Injection.from(IArray.unsafeFromArray(selected.toArray), right(IndexSpace.of(neural.size))))
      val leg = right(MeasurementLeg.hardSelection(neural, MeasurementId.unsafe("local"), selection))
      AlderPredictiveAdmission.nativeMeasurement(observations(native), leg, targets,
        keys, DataFingerprint.external("swift-metadata"), mapping,
        right(NativeReadPolicy(math.max(selected.length, 1), right(MaterializationBudget(cells)))))
    def evaluate(native: Boolean = false, selected: Vector[Int] = input.head.indices.toVector): AlderSwiftCentroidResult =
      right(AlderSwiftCentroid.crossValidate(right(rows(native, selected)), design, coding))

  test("keyed exact-once LORO preserves frozen R probabilities and pooled denominator"):
    val fixture = new Fixture
    val result = fixture.evaluate()
    assertEquals(result.classes.map(_.value), Vector("b", "a"))
    assertEquals(result.rows.map(_.stableKey), fixture.keys)
    assertEquals(result.rows.map(_.stableKey).distinct.length, 9)
    result.rows.foreach(row => assert(row.assessments.forall(fit => !fit.trainingStableKeys.contains(row.stableKey))))
    frozenB.indices.foreach(i => assertEqualsDouble(result.probabilities(i, 0), frozenB(i), 1e-12))
    assertEquals(result.rows.map(_.predicted.value), Vector("b", "a", "a", "b", "b", "a", "a", "a", "a"))
    assertEquals(result.assessment.correct, 6L)
    assertEquals(result.assessment.samples, 9L)
    assertEquals(result.assessment.confusion, Vector(Vector(2L, 2L), Vector(1L, 4L)))
    assertEqualsDouble(result.assessment.accuracy, 2.0 / 3.0, 1e-12)
    assert(math.abs(result.assessment.accuracy - 13.0 / 18.0) > 0.05)
    assertEquals(result.fits.length, 3)
    assertEquals(result.validationReceipt, fixture.design.receipt)
    val separatePreparation = right(CrossFitDesign.bind(fixture.axis, fixture.design.design, ScientificSeed.fromLong(23L)))
    assert(result.validationReceipt.seed != separatePreparation.receipt.seed)
    result.fits.foreach(fit => assertEquals(fit.audit.component.id.render, "scalafim.swift-centroid"))

  test("held-out payload perturbation cannot change its training-fitted scales or other assessment rows"):
    val baseline = new Fixture().evaluate()
    val perturbed = new Fixture(values.updated(0, Vector(300.0, 200.0, 100.0))).evaluate()
    assertEquals(perturbed.fits.head.means, baseline.fits.head.means)
    assertEquals(perturbed.fits.head.scales, baseline.fits.head.scales)
    assertEquals(perturbed.fits.head.priors, baseline.fits.head.priors)
    assertEquals(perturbed.fits.head.trainingStableKeys, baseline.fits.head.trainingStableKeys)
    assertEqualsDouble(perturbed.probabilities(1, 0), baseline.probabilities(1, 0), 1e-12)
    val training = values.drop(2)
    training.head.indices.foreach: col =>
      val mean = training.map(_(col)).sum / training.length
      val sd = math.sqrt(training.map(row => math.pow(row(col) - mean, 2)).sum / (training.length - 1))
      assertEqualsDouble(baseline.fits.head.means(col), mean, 1e-12)
      assertEqualsDouble(baseline.fits.head.scales(col), sd, 1e-12)

  test("dense and operator whole-domain ROI and searchlight measurements agree"):
    val fixture = new Fixture
    for selected <- Vector(Vector(0, 1, 2), Vector(2, 0), Vector(1, 2)) do
      val dense = fixture.evaluate(selected = selected)
      val native = fixture.evaluate(native = true, selected = selected)
      dense.rows.indices.foreach: row =>
        dense.classes.indices.foreach(col => assertEqualsDouble(native.probabilities(row, col), dense.probabilities(row, col), 1e-12))
      assertEquals(native.rows.map(_.predicted), dense.rows.map(_.predicted))
      assertEquals(native.nativeRead.get.observationsIdentity.columns.size, selected.length)
      assertEquals(native.materialization, dense.materialization)
    assert(fixture.calls > 0)

  test("missing training class and unknown target refuse explicitly"):
    val missing = new Fixture(Vector(Vector(1.0), Vector(2.0), Vector(-1.0), Vector(-2.0)),
      Vector(0.0, 0.0, 1.0, 1.0), Vector(0, 0, 1, 1))
    assert(AlderSwiftCentroid.crossValidate(right(missing.rows()), missing.design, coding).left.toOption.exists {
      case AlderSwiftCentroidError.MissingClass(_) => true
      case _ => false
    })
    val unknown = new Fixture(outcomes = target.updated(8, 17.0))
    assert(AlderSwiftCentroid.crossValidate(right(unknown.rows()), unknown.design, coding).isLeft)
    assert(SwiftTargetCoding(Vector(Double.NaN -> "b", 1.0 -> "a")).isLeft)
    assert(SwiftTargetCoding(Vector(0.0 -> " ", 1.0 -> "a")).isLeft)

  test("foreign ordered axes and budgets are refused before operator calls"):
    val fixture = new Fixture
    assert(fixture.rows(native = true, cells = 1L).isLeft)
    assertEquals(fixture.calls, 0)
    val reversed = right(AxisRef.fromStableKeys("samples", SpaceRole.Samples, fixture.keys.reverse, "trial", "none", "one"))
    val reversedDesign = right(ValidationDesign.bind(reversed,
      right(FixedPartitions.once(right(Labels.retained(IArray.unsafeFromArray(runs.toArray))))), ScientificSeed.fromLong(23L)))
    assert(AlderSwiftCentroid.crossValidate(right(fixture.rows()), reversedDesign, coding).isLeft)

  test("repeated exact assessments average native Swift probabilities by stable row"):
    val fixture = new Fixture
    val first = right(Labels.retained(IArray.unsafeFromArray(runs.toArray)))
    val second = right(Labels.retained(IArray(0, 1, 2, 0, 1, 2, 0, 1, 2)))
    val repeated = right(ValidationDesign.bind(fixture.axis, right(FixedPartitions.repeated(IArray(first, second))), ScientificSeed.fromLong(23L)))
    val alternate = right(ValidationDesign.bind(fixture.axis, right(FixedPartitions.once(second)), ScientificSeed.fromLong(23L)))
    val a = right(AlderSwiftCentroid.crossValidate(right(fixture.rows()), fixture.design, coding))
    val b = right(AlderSwiftCentroid.crossValidate(right(fixture.rows()), alternate, coding))
    val combined = right(AlderSwiftCentroid.crossValidate(right(fixture.rows()), repeated, coding))
    assertEquals(combined.rows.map(_.stableKey), fixture.keys)
    assertEquals(combined.fits.length, a.fits.length + b.fits.length)
    assert((0 until a.probabilities.rows).exists(row => math.abs(a.probabilities(row, 0) - b.probabilities(row, 0)) > 1e-8))
    combined.rows.foreach: row =>
      assertEquals(row.assessments.map(_.unit.repeat), Vector(0, 1))
      assertEquals(row.assessments.length, 2)
      row.assessments.foreach: contribution =>
        assert(!contribution.trainingStableKeys.contains(row.stableKey))
        assertEquals(contribution.trainingStableKeys, combined.fits.find(_.unit == contribution.unit).get.trainingStableKeys)
    combined.rows.indices.foreach: row =>
      coding.classes.indices.foreach: column =>
        assertEqualsDouble(combined.probabilities(row, column), (a.probabilities(row, column) + b.probabilities(row, column)) / 2.0, 1e-12)

  test("native Swift refuses non-finite probabilities arising from finite unscaled inputs"):
    val fixture = new Fixture(Vector(Vector(1e200), Vector(-1e200), Vector(0.0), Vector(0.0)), Vector(0.0, 1.0, 0.0, 1.0), Vector(0, 0, 1, 1))
    val result = AlderSwiftCentroid.crossValidate(right(fixture.rows()), fixture.design, coding, FeatureScaling.None)
    assert(result.left.toOption.exists {
      case AlderSwiftCentroidError.Mvpa(error) => error.message.contains("non-finite")
      case _ => false
    })
