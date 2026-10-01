package scalafim.fmri.mvpa.dataset.predictive

import alder.kernel.*
import cats.Id
import cats.data.EitherT
import gale.linalg.DMat
import multivar.core.SpaceRole
import resample4s.core.{DigestAlgorithm, Labels}
import resample4s.designs.FixedPartitions
import scalafim.fmri.mvpa.{AxisRef, CrossFitDesign, ScientificSeed, ValidationDesign}

class AlderRidgeRegressionSuite extends munit.FunSuite:
  given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private def right[A](value: Either[?, A]): A =
    value.fold(error => fail(error.toString), identity)

  private val tolerance = 1e-10

  // Independent oracle: numpy closed-form centred ridge,
  //   w = (Xc'Xc + lambda I)^-1 Xc'yc,  b = mean(y) - mean(X) w,
  // unit row weights and an unscaled penalty (Alder's dense Gale objective),
  // with leave-one-run-out outer and inner designs, the inner design
  // restricted to outer-training rows, and lambda chosen by the pooled inner
  // mean squared error (first minimum in grid order).
  private val x = Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.5)
  private val y = Vector(2.0, 3.5, 3.0, 6.0, 5.5, 7.0)
  private val x2 = Vector(0.5, -1.0, 2.0, 0.0, 1.5, -0.5)
  private val y2 = Vector(1.0, 0.0, 2.0, 1.5, 3.0, 2.5)
  private val runs = Array(0, 0, 1, 1, 2, 2)
  private val nativeIds = Vector(60L, 10L, 50L, 20L, 40L, 30L)

  private final class Fixture(
      inputs: Vector[Vector[Double]],
      targets: Vector[Vector[Double]],
      responseNames: Vector[String],
      crossLabels: Array[Int] = Array(0, 1, 0, 1, 0, 1)
  ):
    val keys = Vector.tabulate(6)(i => s"s$i")
    val axis = right(AxisRef.fromStableKeys("samples", SpaceRole.Samples, keys, "trial", "none", "one"))
    val response = right(AxisRef.fromStableKeys("responses", SpaceRole.Observed, responseNames, "response", "none", "value"))
    val mapping = right(NativeAxisMapping.fromAxis(axis, nativeIds, DataFingerprint.external("ridge-fixture-v1")))
    val rows = right(AlderPredictiveAdmission.materialized(axis.descriptor,
      DMat.dense(6, inputs.head.length, inputs.flatten), DMat.dense(6, targets.head.length, targets.flatten),
      keys, mapping, right(MaterializationBudget(10000L))))
    val outer = right(ValidationDesign.bind(axis, right(FixedPartitions.once(right(Labels.retained(IArray.unsafeFromArray(runs.clone()))))), ScientificSeed.fromLong(23L)))
    val inner = right(ValidationDesign.bind(axis, right(FixedPartitions.once(right(Labels.retained(IArray.unsafeFromArray(runs.clone()))))), ScientificSeed.fromLong(29L)))
    val crossFit = right(CrossFitDesign.bind(axis, right(FixedPartitions.once(right(Labels.retained(IArray.unsafeFromArray(crossLabels.clone()))))), ScientificSeed.fromLong(31L)))
    def runKeys(run: Int): Vector[String] = Vector(keys(2 * run), keys(2 * run + 1))

  private def scalar(targets: Vector[Double] = y) =
    new Fixture(x.map(Vector(_)), targets.map(Vector(_)), Vector("y"))

  private def multi =
    new Fixture(x.zip(x2).map((a, b) => Vector(a, b)), y.zip(y2).map((a, b) => Vector(a, b)), Vector("y-a", "y-b"))

  // Independent centered NumPy oracle, frozen in weighted-oracle.py/receipt.
  // Equal block masses: one signal coordinate, two replicated nuisance targets.
  // Uniform coordinates choose lambda0.1 in run0; balanced blocks choose2.0.
  private val noise = Vector(-1.1791991921116334, 2.0404033221829017, 5.0606605697068066, 3.0774566558513454, -3.0732932711703533, -2.3873681815539065)
  private def weightedFixture(signal: Vector[Double] = y, copies: Int = 2) =
    new Fixture(x.map(Vector(_)), signal.zip(noise).map((s, n) => s +: Vector.fill(copies)(n)),
      Vector("signal") ++ Vector.tabulate(copies)(i => s"noise-$i"))

  private def balanced(f: Fixture, mass: Double = 1.0): RidgeTargetGeometry =
    right(RidgeTargetGeometry.declared(f.response, Vector(
      right(RidgeResponseBlock("signal-block", Vector("signal"), mass)),
      right(RidgeResponseBlock("noise-block", f.response.toRecord.stableKeys.filter(_.startsWith("noise-")), mass))
    ), "fixed equal block utility"))

  private def weightedRun(f: Fixture, geometry: RidgeTargetGeometry, penalties: Vector[Double] = Vector(0.1, 2.0, 10.0)) =
    right(AlderRidgeRegression.crossValidate(f.rows, f.outer, f.inner, f.response, Vector("x"),
      right(RidgePenaltyGrid(penalties)), budget(), targetGeometry = Some(geometry)))

  test("block-balanced shared penalty and OOF values match the independent weighted oracle"):
    val f = weightedFixture()
    val geometry = balanced(f)
    val result = weightedRun(f, geometry)
    val uniform = weightedRun(f, right(RidgeTargetGeometry.uniform(f.response)))
    val expectedPenalty = Vector(2.0, 10.0, 2.0)
    val expectedLoss = Vector(
      Vector(22.719600129544546, 22.684339711818836, 24.779614116044982),
      Vector(61.21870437905212, 15.36763654877306, 12.24876024519346),
      Vector(18.679870919715547, 8.452611425569525, 9.242020423719236))
    val expected = Vector(
      Vector(2.6366906474820144, 7.252065244369771, 7.252065244369771),
      Vector(3.3920863309352516, 5.436147644049413, 5.436147644049413),
      Vector(4.147368421052631, -0.9117649772089131, -0.9117649772089131),
      Vector(4.711578947368421, -1.2927239427358486, -1.2927239427358486),
      Vector(5.678571428571429, 5.069513337373934, 5.069513337373934),
      Vector(6.9107142857142865, 6.761323136453881, 6.761323136453881))
    assertEquals(geometry.coordinateWeights, Vector(1.0, 0.5, 0.5))
    assertEquals(geometry.normalizedWeights, Vector(0.5, 0.25, 0.25))
    (0 until 3).foreach: run =>
      val fold = foldFor(result, f.runKeys(run))
      assertEqualsDouble(fold.selection.selectedPenalty, expectedPenalty(run), 0.0)
      assertEquals(fold.selection.loss, RidgeSelectionLoss.TargetWeightedMeanSquaredError)
      assertEquals(fold.selection.assessmentAppearances, 4L)
      assertEqualsDouble(fold.selection.lossDenominator, 4.0, 0.0)
      fold.selection.pooledLossByPenalty.zip(expectedLoss(run)).foreach((actual, reference) => assertEqualsDouble(actual, reference, tolerance))
      fold.selection.normalizedSquaredErrorByPenalty.zip(expectedLoss(run)).foreach((actual, reference) => assertEqualsDouble(actual, 4.0 * reference, tolerance))
      assert(fold.model.targetGeometry eq geometry)
      assert(fold.selection.targetGeometry eq geometry)
      assertEquals(fold.model.responseAxis, f.response.descriptor)
      val direct = right(fold.model.predict(Array(x(2 * run))))
      assert(direct.targetGeometry eq geometry)
      assertEquals(direct.responseAxis, f.response.descriptor)
      direct.values.zip(expected(2 * run)).foreach((actual, reference) => assertEqualsDouble(actual, reference, tolerance))
    assertEqualsDouble(foldFor(uniform, f.runKeys(0)).selection.selectedPenalty, 0.1, 0.0)
    result.rows.zip(expected).foreach: (row, reference) =>
      assert(row.targetGeometry eq geometry)
      assertEquals(row.responseAxis, f.response.descriptor)
      row.predicted.zip(reference).foreach((actual, expectedValue) => assertEqualsDouble(actual, expectedValue, tolerance))
    assert(result.targetGeometry eq geometry)
    result.pooled.meanSquaredError match
      case RegressionMetric.Defined(value) => assertEqualsDouble(value, 24.235238359030046, tolerance)
      case other => fail(other.toString)
    result.pooled.rSquared match
      case RegressionMetric.Defined(value) => assertEqualsDouble(value, -3.000731541170433, tolerance)
      case other => fail(other.toString)

  test("fixed-lambda fits retain raw units while target geometry changes plan identity"):
    val f = weightedFixture()
    val weighted = weightedRun(f, balanced(f), Vector(2.0))
    val uniform = weightedRun(f, right(RidgeTargetGeometry.uniform(f.response)), Vector(2.0))
    assertEquals(weighted.rows.map(_.predicted), uniform.rows.map(_.predicted))
    weighted.folds.zip(uniform.folds).foreach: (w, u) =>
      assertEquals(w.model.targets.map(w.model.weights), u.model.targets.map(u.model.weights))
      assertNotEquals(w.audit.plan, u.audit.plan)
      assertNotEquals(w.model.targetGeometry.identity, u.model.targetGeometry.identity)

  test("block duplication and common mass rescaling preserve target utility"):
    val f = weightedFixture()
    val duplicate = weightedFixture(copies = 3)
    val before = weightedRun(f, balanced(f))
    val after = weightedRun(duplicate, balanced(duplicate))
    val rescaled = weightedRun(f, balanced(f, mass = 1e100))
    (0 until 3).foreach: run =>
      val original = foldFor(before, f.runKeys(run)).selection
      Vector(foldFor(after, duplicate.runKeys(run)).selection, foldFor(rescaled, f.runKeys(run)).selection).foreach: selection =>
        assertEqualsDouble(selection.selectedPenalty, original.selectedPenalty, 0.0)
        selection.pooledLossByPenalty.zip(original.pooledLossByPenalty).foreach((actual, reference) => assertEqualsDouble(actual, reference, tolerance))
    after.rows.zip(before.rows).foreach((actual, reference) => actual.predicted.take(3).zip(reference.predicted).foreach((a, b) => assertEqualsDouble(a, b, tolerance)))

  test("consistent target permutation preserves block-weighted selection and raw predictions"):
    val original = weightedFixture()
    val permutation = Vector(2, 0, 1)
    val names = original.response.toRecord.stableKeys
    val permuted = new Fixture(x.map(Vector(_)), y.zip(noise).map((s, n) => permutation.map(Vector(s, n, n))), permutation.map(names))
    val before = weightedRun(original, balanced(original))
    val after = weightedRun(permuted, balanced(permuted))
    (0 until 3).foreach: run =>
      assertEqualsDouble(foldFor(after, permuted.runKeys(run)).selection.selectedPenalty, foldFor(before, original.runKeys(run)).selection.selectedPenalty, 0.0)
    after.rows.zip(before.rows).foreach: (a, b) =>
      assertEquals(a.targets, permutation.map(b.targets))
      a.predicted.zip(permutation.map(b.predicted)).foreach((actual, reference) => assertEqualsDouble(actual, reference, tolerance))

  test("weighted refit retains geometry in its receipt, model and prediction and matches the independent oracle"):
    val f = weightedFixture()
    val geometry = balanced(f)
    val refit = right(AlderRidgeRegression.refit(f.rows, f.inner, f.response, Vector("x"), right(RidgePenaltyGrid(Vector(0.1, 2.0, 10.0))),
      budget(), RidgeRefitAuthorization.Declared("final weighted model"), targetGeometry = Some(geometry)))
    assert(refit.targetGeometry eq geometry)
    assert(refit.receipt.selection.targetGeometry eq geometry)
    assertEquals(refit.responseAxis, f.response.descriptor)
    assertEqualsDouble(refit.model.penalty, 10.0, 0.0)
    refit.receipt.selection.pooledLossByPenalty.zip(Vector(32.39049623721055, 24.147353750879955, 15.424147404353137))
      .foreach((actual, reference) => assertEqualsDouble(actual, reference, tolerance))
    val prediction = right(refit.predict(Array(2.5)))
    assert(prediction.targetGeometry eq geometry)
    assertEquals(prediction.responseAxis, f.response.descriptor)
    prediction.values.zip(Vector(3.8455172413793104, 1.062121043796112, 1.062121043796112)).foreach((actual, reference) => assertEqualsDouble(actual, reference, tolerance))

  test("fixed target utility cannot use outer assessment targets to select its model"):
    val baseline = weightedFixture()
    val perturbed = weightedFixture(y.updated(5, 999.0))
    val before = weightedRun(baseline, balanced(baseline))
    val after = weightedRun(perturbed, balanced(perturbed))
    val held = baseline.runKeys(2)
    assertEquals(foldFor(after, held).selection.pooledLossByPenalty, foldFor(before, held).selection.pooledLossByPenalty)
    assertEquals(after.rows.filter(row => held.contains(row.stableKey)).map(_.predicted), before.rows.filter(row => held.contains(row.stableKey)).map(_.predicted))

  test("target block geometry refuses gaps, overlaps, foreign keys, bad masses and reordered axes before fitting"):
    val f = weightedFixture()
    def invalid[A](value: Either[AlderRidgeRegressionError, A]): Unit = value match
      case Left(AlderRidgeRegressionError.InvalidTargetGeometry(_)) => ()
      case other => fail(s"expected InvalidTargetGeometry, got $other")
    invalid(RidgeResponseBlock(" ", Vector("signal"), 1.0))
    invalid(RidgeResponseBlock("empty", Vector.empty, 1.0))
    invalid(RidgeResponseBlock("duplicate", Vector("signal", "signal"), 1.0))
    Vector(0.0, -1.0, Double.NaN, Double.PositiveInfinity).foreach(mass => invalid(RidgeResponseBlock("bad", Vector("signal"), mass)))
    val signal = right(RidgeResponseBlock("signal", Vector("signal"), 1.0))
    val noiseBlock = right(RidgeResponseBlock("noise", Vector("noise-0", "noise-1"), 1.0))
    invalid(RidgeTargetGeometry.declared(f.response, Vector(signal), "gap"))
    invalid(RidgeTargetGeometry.declared(f.response, Vector(signal, noiseBlock, right(RidgeResponseBlock("overlap", Vector("signal"), 1.0))), "overlap"))
    invalid(RidgeTargetGeometry.declared(f.response, Vector(signal, right(RidgeResponseBlock("foreign", Vector("noise-0", "foreign"), 1.0))), "foreign"))
    invalid(RidgeTargetGeometry.declared(f.response, Vector(signal, noiseBlock), " "))
    invalid(RidgeTargetGeometry.declared(f.response, Vector(right(RidgeResponseBlock("signal", Vector("signal"), Double.MaxValue)),
      right(RidgeResponseBlock("noise", Vector("noise-0", "noise-1"), Double.MaxValue))), "overflow"))
    invalid(RidgeTargetGeometry.declared(f.response, Vector(signal, right(RidgeResponseBlock("noise", Vector("noise-0", "noise-1"), Double.MinPositiveValue))), "underflow"))
    val foreign = right(AxisRef.fromStableKeys("responses", SpaceRole.Observed, Vector("noise-1", "signal", "noise-0"), "response", "none", "value"))
    val foreignGeometry = right(RidgeTargetGeometry.declared(foreign, Vector(signal, noiseBlock), "reordered"))
    val encoder = new MeanTargetEncoder
    AlderRidgeRegression.crossValidate(f.rows, f.outer, f.inner, f.response, Vector("x", "fold-target-mean"), right(RidgePenaltyGrid(Vector(2.0))),
      budget(), preparation = Some(new RidgeCrossFitPreparation(f.crossFit, encoder)), targetGeometry = Some(foreignGeometry)) match
      case Left(AlderRidgeRegressionError.TargetGeometryAxisMismatch(expected, actual)) =>
        assertEquals(expected, f.response.descriptor.stableKey)
        assertEquals(actual, foreign.descriptor.stableKey)
      case other => fail(s"expected TargetGeometryAxisMismatch, got $other")
    assertEquals(encoder.fitted, Vector.empty)

  private def budget(cells: Long = 100000L) = right(RidgeSolveBudget(cells))

  private def foldFor(result: AlderRidgeRegressionResult, assessed: Vector[String]): RidgeOuterFold =
    result.folds.find(_.assessmentKeys == assessed).getOrElse(fail(s"no outer fold assessing $assessed"))

  test("scalar nested ridge matches the independent oracle and keeps selection inside outer training") {
    val f = scalar()
    val result = right(AlderRidgeRegression.crossValidate(f.rows, f.outer, f.inner, f.response, Vector("x"), right(RidgePenaltyGrid(Vector(0.5, 4.0))), budget()))
    val expectedLosses = Vector(
      Vector(2.074796597633136, 2.380279595478882),
      Vector(0.30346246301775154, 7.591902348139337),
      Vector(1.4453125, 2.7638888888888893)
    )
    val expectedWeights = Vector(0.9130434782608696, 0.8297213622291022, 1.0454545454545454)
    val expectedIntercepts = Vector(1.1521739130434776, 1.4922600619195046, 1.0113636363636367)
    (0 until 3).foreach: run =>
      val fold = foldFor(result, f.runKeys(run))
      val selection = fold.selection
      assertEqualsDouble(selection.selectedPenalty, 0.5, 0.0)
      selection.pooledLossByPenalty.zip(expectedLosses(run)).foreach((actual, expected) => assertEqualsDouble(actual, expected, tolerance))
      assertEqualsDouble(right(fold.model.weights("y").toRight("missing")).head._2, expectedWeights(run), tolerance)
      assertEqualsDouble(right(fold.model.intercept("y").toRight("missing")), expectedIntercepts(run), tolerance)
      assertEquals(selection.trainingKeys.toSet, f.keys.toSet -- f.runKeys(run))
      assertEquals(fold.trainingKeys.toSet, f.keys.toSet -- f.runKeys(run))
      selection.units.foreach: unit =>
        assert((unit.analysisKeys ++ unit.assessmentKeys).forall(key => !f.runKeys(run).contains(key)))
      assertEquals(selection.units.length, 2)
      assertEquals(selection.skippedUnits.length, 1)
    val expected = Vector(2.0652173913043472, 2.978260869565217, 3.9814241486068114, 4.811145510835914, 6.238636363636363, 7.806818181818182)
    assertEquals(result.rows.map(_.stableKey), f.keys)
    expected.indices.foreach(row => assertEqualsDouble(result.predictions(row, 0), expected(row), tolerance))
    assertEquals(result.sampleAxis, f.axis.descriptor)
    assertEquals(result.responseAxis, f.response.descriptor)
    assertEquals(result.solve.execution, RidgeExecution.BoundedMaterialized)
    assertEquals(result.materialization, f.rows.receipt)
  }

  test("multiresponse ridge shares one penalty, preserves response order, and matches per-output oracles") {
    val f = multi
    val grid = right(RidgePenaltyGrid(Vector(0.1, 2.0, 10.0)))
    val result = right(AlderRidgeRegression.crossValidate(f.rows, f.outer, f.inner, f.response, Vector("x1", "x2"), grid, budget()))
    assertEquals(result.targets, Vector("y-a", "y-b"))
    assertEquals(result.features, Vector("x1", "x2"))
    assertEquals(result.outputs.map(_.target), Vector("y-a", "y-b"))
    val expectedLosses = Vector(
      Vector(1.3774997026763682, 1.4493454095702882, 2.116044926303854),
      Vector(6.855168605446339, 7.424555371073925, 8.353951132135128),
      Vector(2.723374087017727, 2.771170967836644, 2.9505376228465723)
    )
    val expectedWeights = Vector(
      Vector(Vector(0.5530973451327433, -0.7747177296307599), Vector(0.5420353982300886, 0.5666386939273728)),
      Vector(Vector(0.8565389094223077, -0.21201458154017516), Vector(0.40706799655713627, 0.6748181526673754)),
      Vector(Vector(1.2149557197505636, -0.5950322276371641), Vector(0.2627993502069906, 0.5462977519257979))
    )
    (0 until 3).foreach: run =>
      val fold = foldFor(result, f.runKeys(run))
      assertEqualsDouble(fold.selection.selectedPenalty, 0.1, 0.0)
      assertEqualsDouble(fold.model.penalty, 0.1, 0.0)
      assertEquals(fold.model.targets, Vector("y-a", "y-b"))
      assertEquals(fold.model.outputs.map(_.target), Vector("y-a", "y-b"))
      fold.selection.pooledLossByPenalty.zip(expectedLosses(run)).foreach((actual, expected) => assertEqualsDouble(actual, expected, tolerance))
      Vector("y-a", "y-b").zipWithIndex.foreach: (target, output) =>
        val weights = right(fold.model.weights(target).toRight("missing"))
        assertEquals(weights.map(_._1), Vector("x1", "x2"))
        weights.map(_._2).zip(expectedWeights(run)(output)).foreach((actual, expected) => assertEqualsDouble(actual, expected, tolerance))
      fold.model.outputs.foreach(fit => assertEquals(fit.audit.component.id, ComponentId("alder.ridge")))
    val expected = Vector(
      Vector(3.563701556301495, 0.14346200793408537), Vector(5.278875495880378, -0.1644606347268852),
      Vector(3.5671358412232292, 2.6358665384031186), Vector(4.847703913725887, 1.6932982296255041),
      Vector(5.9929780432846, 2.396583346433999), Vector(9.005476078184774, 1.6981868678928893)
    )
    expected.indices.foreach: row =>
      expected(row).indices.foreach(column => assertEqualsDouble(result.predictions(row, column), expected(row)(column), tolerance))
      assertEquals(result.rows(row).predicted.length, 2)
  }

  test("perturbing an outer assessment target cannot change that fold's selection or predictions") {
    val baseline = scalar()
    val perturbed = scalar(y.updated(5, 100.0))
    val grid = right(RidgePenaltyGrid(Vector(0.5, 4.0)))
    val before = right(AlderRidgeRegression.crossValidate(baseline.rows, baseline.outer, baseline.inner, baseline.response, Vector("x"), grid, budget()))
    val after = right(AlderRidgeRegression.crossValidate(perturbed.rows, perturbed.outer, perturbed.inner, perturbed.response, Vector("x"), grid, budget()))
    val heldOut = baseline.runKeys(2)
    assertEquals(foldFor(after, heldOut).selection.pooledLossByPenalty, foldFor(before, heldOut).selection.pooledLossByPenalty)
    assertEqualsDouble(foldFor(after, heldOut).selection.selectedPenalty, foldFor(before, heldOut).selection.selectedPenalty, 0.0)
    Vector(4, 5).foreach(row => assertEqualsDouble(after.predictions(row, 0), before.predictions(row, 0), 0.0))
    assertNotEquals(foldFor(after, baseline.runKeys(0)).selection.pooledLossByPenalty, foldFor(before, baseline.runKeys(0)).selection.pooledLossByPenalty)
  }

  private final class MeanTargetEncoder extends FoldEncoder[Id, Array[Double], Array[Double], String, Array[Double]]:
    val fitted = scala.collection.mutable.ArrayBuffer.empty[Set[Long]]
    type State = Double
    type FitError = Nothing
    type RunError = Nothing
    def fit[U <: Use.Fit](data: NonEmptyData[U, Example[Array[Double], Array[Double], String]])(using context: FitContext): FitResult[Id, Nothing, Trained[Double]] =
      val seen = data.data.foldRows(Vector.empty[(Long, Double)])((rows, id, example) => rows :+ (id.value -> example.target(0)))
      fitted += seen.map(_._1).toSet
      EitherT.right(context.complete(seen.map(_._2).sum / seen.length.toDouble, data,
        ComponentDescriptor(ComponentId("scalafim.test.mean-target"), ComponentVersion("1"), AuditValue.record(), BackendFingerprint("test", "1", AuditValue.record()))))
    def encode(state: Double, input: Array[Double]): Either[Failure[Nothing], Array[Double]] = Right(Array(input(0), state))

  private def sortedSets(sets: Iterable[Set[Long]]): Vector[Vector[Long]] =
    sets.map(_.toVector.sorted).toVector.sortBy(_.mkString(","))

  test("target-aware preparation is refitted inside every scored training scope and never sees held rows") {
    val f = scalar()
    val encoder = new MeanTargetEncoder
    val penalties = Vector(0.5, 4.0)
    val result = right(AlderRidgeRegression.crossValidate(f.rows, f.outer, f.inner, f.response, Vector("x", "fold-target-mean"),
      right(RidgePenaltyGrid(penalties)), budget(), Some(new RidgeCrossFitPreparation(f.crossFit, encoder))))
    val idByKey = f.keys.zip(nativeIds).toMap
    def id(ordinal: Int): Long = nativeIds(ordinal)
    // Per outer fold: for every penalty and each usable inner unit (assessing
    // run r, analysing the remaining run o), Alder cross-fits on {2o, 2o+1}
    // with alternating preparation folds, then serves from both rows; the
    // final model repeats that on the outer training rows.
    assertEquals(encoder.fitted.length, 3 * (penalties.length * 2 * 3 + 3))
    result.folds.zipWithIndex.foreach: (fold, index) =>
      val heldRun = (0 until 3).find(run => f.runKeys(run) == fold.assessmentKeys).getOrElse(fail("unknown outer fold"))
      val others = (0 until 3).filter(_ != heldRun)
      val training = others.flatMap(run => Vector(id(2 * run), id(2 * run + 1))).toSet
      val inner = for
        _ <- penalties
        run <- others
        other = others.find(_ != run).get
        set <- Vector(Set(id(2 * other + 1)), Set(id(2 * other)), Set(id(2 * other), id(2 * other + 1)))
      yield set
      val outer = Vector(training.filter(value => nativeIds.indexOf(value) % 2 == 1), training.filter(value => nativeIds.indexOf(value) % 2 == 0), training)
      val group = encoder.fitted.slice(15 * index, 15 * index + 15)
      assertEquals(sortedSets(group), sortedSets(inner ++ outer))
      group.foreach(ids => assert(fold.assessmentKeys.map(idByKey).forall(value => !ids.contains(value))))
      fold.selection.units.foreach: unit =>
        val receipt = unit.preparation.getOrElse(fail("missing inner preparation receipt"))
        assertEquals(receipt.assignment.map(_._1).sorted, unit.analysisKeys.sorted)
        assert(unit.assessmentKeys.forall(key => !unit.analysisKeys.contains(key)))
        assertEquals(unit.auditByPenalty.length, penalties.length)
      val receipt = fold.crossFit.getOrElse(fail("missing cross-fit receipt"))
      assertEquals(receipt.assignment.map(_._1).sorted, fold.trainingKeys.sorted)
      assertEquals(receipt.retainedUnits.length, 2)
      assertEquals(receipt.design, f.crossFit.receipt)
      assertEquals(fold.model.features, Vector("x", "fold-target-mean"))
    assertEquals(result.crossFitDesign, Some(f.crossFit.receipt))
    assertEquals(result.outerDesign, f.outer.receipt)
    assertEquals(result.innerDesign, f.inner.receipt)
  }

  test("perturbing an inner assessment target cannot change that inner unit's prepared predictions") {
    val grid = right(RidgePenaltyGrid(Vector(0.5, 4.0)))
    def run(targets: Vector[Double]) =
      val f = scalar(targets)
      val result = right(AlderRidgeRegression.crossValidate(f.rows, f.outer, f.inner, f.response, Vector("x", "fold-target-mean"),
        grid, budget(), Some(new RidgeCrossFitPreparation(f.crossFit, new MeanTargetEncoder))))
      (f, result)
    val (f, before) = run(y)
    val (_, after) = run(y.updated(5, 100.0))
    def unitAssessing(result: AlderRidgeRegressionResult, assessed: Vector[String]): RidgeInnerUnitReceipt =
      foldFor(result, f.runKeys(0)).selection.units.find(_.assessmentKeys == assessed).getOrElse(fail(s"no inner unit assessing $assessed"))
    // Within outer fold 0, s5 is an inner assessment row of the unit holding
    // out run 2; that unit's encoder and ridge see only run 1.
    assertEquals(unitAssessing(after, f.runKeys(2)).predictedByPenalty, unitAssessing(before, f.runKeys(2)).predictedByPenalty)
    assertNotEquals(unitAssessing(after, f.runKeys(1)).predictedByPenalty, unitAssessing(before, f.runKeys(1)).predictedByPenalty)
  }

  test("cross-fit preparation that leaves one fold inside an outer training set is refused") {
    val f = new Fixture(x.map(Vector(_)), y.map(Vector(_)), Vector("y"), crossLabels = Array(0, 0, 0, 0, 1, 1))
    val refused = AlderRidgeRegression.crossValidate(f.rows, f.outer, f.inner, f.response, Vector("x", "fold-target-mean"),
      right(RidgePenaltyGrid(Vector(0.5))), budget(), Some(new RidgeCrossFitPreparation(f.crossFit, new MeanTargetEncoder)))
    refused match
      case Left(AlderRidgeRegressionError.CrossFitTooFewFolds(_, 1)) => ()
      case other => fail(s"expected CrossFitTooFewFolds(_, 1), got $other")
  }

  test("regression metrics are typed: constant references and empty assessments have no R-squared") {
    val defined = RegressionAssessment.output("y", Vector(1.0, 2.0, 3.0), Vector(1.0, 2.0, 4.0))
    assertEquals(defined.assessed, 3)
    defined.meanSquaredError match
      case RegressionMetric.Defined(value) => assertEqualsDouble(value, 1.0 / 3.0, tolerance)
      case other => fail(s"expected defined MSE, got $other")
    defined.rSquared match
      case RegressionMetric.Defined(value) => assertEqualsDouble(value, 0.5, tolerance)
      case other => fail(s"expected defined R-squared, got $other")
    val constant = RegressionAssessment.output("y", Vector(2.0, 2.0), Vector(1.0, 3.0))
    assertEquals(constant.rSquared, RegressionMetric.UndefinedConstantReference)
    assertEquals(constant.meanSquaredError, RegressionMetric.Defined(1.0))
    val empty = RegressionAssessment.output("y", Vector.empty, Vector.empty)
    assertEquals(empty.meanSquaredError, RegressionMetric.UndefinedNoAssessment)
    assertEquals(empty.rSquared, RegressionMetric.UndefinedNoAssessment)
    val pooled = RegressionAssessment.pooled(Vector((Vector(1.0, 2.0, 3.0), Vector(1.0, 2.0, 4.0)), (Vector(2.0, 2.0, 2.0), Vector(2.0, 2.0, 2.0))))
    assertEquals(pooled.rSquared, RegressionMetric.UndefinedConstantReference)
    pooled.meanSquaredError match
      case RegressionMetric.Defined(value) => assertEqualsDouble(value, 1.0 / 6.0, tolerance)
      case other => fail(s"expected defined pooled MSE, got $other")
    val overflow = RegressionAssessment.output("y", Vector(1e200, -1e200), Vector(-1e200, 1e200))
    assertEquals(overflow.meanSquaredError, RegressionMetric.UndefinedNonFinite)
    assertEquals(overflow.rSquared, RegressionMetric.UndefinedNonFinite)
    assertEquals(RegressionAssessment.output("y", Vector(1.0, Double.NaN), Vector(1.0, 2.0)).meanSquaredError, RegressionMetric.UndefinedNonFinite)
    intercept[IllegalArgumentException](RegressionAssessment.pooled(Vector((Vector(1.0, 2.0), Vector(1.0, 2.0)), (Vector(1.0), Vector(1.0)))))
    intercept[IllegalArgumentException](RegressionAssessment.output("y", Vector(1.0), Vector.empty))
  }

  test("a target constant over outer training is refused by default and recorded when declared") {
    val f = new Fixture(x.map(Vector(_)), Vector(4.0, 4.0, 4.0, 4.0, 1.0, 2.0).map(Vector(_)), Vector("y"))
    val grid = right(RidgePenaltyGrid(Vector(0.5)))
    AlderRidgeRegression.crossValidate(f.rows, f.outer, f.inner, f.response, Vector("x"), grid, budget()) match
      case Left(AlderRidgeRegressionError.ConstantTrainingTarget("y", _)) => ()
      case other => fail(s"expected ConstantTrainingTarget, got $other")
    val recorded = right(AlderRidgeRegression.crossValidate(f.rows, f.outer, f.inner, f.response, Vector("x"), grid, budget(), constantTargets = ConstantTargetPolicy.Record))
    assertEquals(foldFor(recorded, f.runKeys(2)).model.outputs.map(_.constantTrainingTarget), Vector(true))
    assertEquals(foldFor(recorded, f.runKeys(0)).model.outputs.map(_.constantTrainingTarget), Vector(false))
  }

  test("a target constant only inside an inner analysis set follows the same policy as outer fits") {
    // Run 0 is (4, 4): no outer training set is constant, but every inner
    // unit analysing only run 0 is.
    val f = new Fixture(x.map(Vector(_)), Vector(4.0, 4.0, 1.0, 2.0, 3.0, 5.0).map(Vector(_)), Vector("y"))
    val grid = right(RidgePenaltyGrid(Vector(0.5)))
    AlderRidgeRegression.crossValidate(f.rows, f.outer, f.inner, f.response, Vector("x"), grid, budget()) match
      case Left(AlderRidgeRegressionError.ConstantTrainingTarget("y", scope)) => assert(scope.contains("inner unit"), scope)
      case other => fail(s"expected an inner ConstantTrainingTarget, got $other")
    val recorded = right(AlderRidgeRegression.crossValidate(f.rows, f.outer, f.inner, f.response, Vector("x"), grid, budget(), constantTargets = ConstantTargetPolicy.Record))
    val units = recorded.folds.flatMap(_.selection.units)
    units.foreach: unit =>
      val onlyRunZero = unit.analysisKeys.toSet == f.runKeys(0).toSet
      assertEquals(unit.constantTrainingTargets, Vector(onlyRunZero))
    assert(units.exists(_.constantTrainingTargets == Vector(true)))
    recorded.folds.foreach(fold => assertEquals(fold.model.outputs.map(_.constantTrainingTarget), Vector(false)))
  }

  test("refit is a separate declared use with its own receipt and matches the all-row oracle") {
    val f = multi
    val grid = right(RidgePenaltyGrid(Vector(0.1, 2.0, 10.0)))
    AlderRidgeRegression.refit(f.rows, f.inner, f.response, Vector("x1", "x2"), grid, budget(), RidgeRefitAuthorization.Declared(" ")) match
      case Left(AlderRidgeRegressionError.InvalidRefitAuthorization) => ()
      case other => fail(s"expected InvalidRefitAuthorization, got $other")
    val refit = right(AlderRidgeRegression.refit(f.rows, f.inner, f.response, Vector("x1", "x2"), grid, budget(), RidgeRefitAuthorization.Declared("final model for held-out session")))
    assertEquals(refit.receipt.reason, "final model for held-out session")
    assertEquals(refit.receipt.trainingKeys, f.keys)
    assertEquals(refit.receipt.selection.innerDesign, f.inner.receipt)
    assertEqualsDouble(refit.receipt.selection.selectedPenalty, 2.0, 0.0)
    assertEqualsDouble(refit.model.penalty, 2.0, 0.0)
    assertEquals(refit.receipt.selection.trainingKeys, f.keys)
    refit.receipt.selection.pooledLossByPenalty.zip(Vector(1.1444468850235412, 0.8215209210702876, 1.793524960507871))
      .foreach((actual, expected) => assertEqualsDouble(actual, expected, tolerance))
    val prediction = right(refit.predict(Array(2.5, 1.0)))
    assertEquals(prediction.targets, Vector("y-a", "y-b"))
    assertEqualsDouble(prediction.values(0), 3.42352655963797, tolerance)
    assertEqualsDouble(prediction.values(1), 1.4971985777394676, tolerance)
    assertEquals(refit.responseAxis, f.response.descriptor)
    assertEquals(refit.receipt.materialization, f.rows.receipt)
    assertEquals(refit.receipt.crossFit, None)
  }

  test("refit with preparation cross-fits on all rows and serves through the fitted encoder") {
    val f = scalar()
    val encoder = new MeanTargetEncoder
    val refit = right(AlderRidgeRegression.refit(f.rows, f.inner, f.response, Vector("x", "fold-target-mean"), right(RidgePenaltyGrid(Vector(0.5, 4.0))),
      budget(), RidgeRefitAuthorization.Declared("deployment model"), Some(new RidgeCrossFitPreparation(f.crossFit, encoder))))
    val receipt = refit.receipt.crossFit.getOrElse(fail("missing refit cross-fit receipt"))
    assertEquals(receipt.assignment.map(_._1), f.keys)
    assertEquals(receipt.retainedUnits.length, 2)
    assertEquals(encoder.fitted.last, nativeIds.toSet)
    refit.receipt.selection.units.foreach(unit => assert(unit.preparation.nonEmpty))
    assertEquals(right(refit.predict(Array(3.0))).targets, Vector("y"))
  }

  test("planned provider solves are budgeted before the first solve") {
    val f = scalar()
    val grid = right(RidgePenaltyGrid(Vector(0.5, 4.0)))
    // A solve on n rows and p features counts (2n + p) p cells. Per outer
    // fold: 2 usable inner units x 2 penalties x 1 output, each on 2 rows
    // ((4 + 1) x 1 = 5 cells), plus the final solve on 4 rows ((8 + 1) x 1 = 9
    // cells): 29 cells; 3 folds give 87 cells over 3 x (2 x 2 + 1) = 15 solves.
    AlderRidgeRegression.crossValidate(f.rows, f.outer, f.inner, f.response, Vector("x"), grid, budget(86L)) match
      case Left(AlderRidgeRegressionError.SolveOverBudget(87L, 86L)) => ()
      case other => fail(s"expected SolveOverBudget(87, 86), got $other")
    val admitted = right(AlderRidgeRegression.crossValidate(f.rows, f.outer, f.inner, f.response, Vector("x"), grid, budget(87L)))
    assertEquals(admitted.solve, RidgeSolveReceipt(RidgeExecution.BoundedMaterialized, 15L, 87L, 87L))
    val maximum = Int.MaxValue
    assertEquals(AlderRidgeRegression.planCounts(Vector((maximum, Vector(maximum))), maximum, maximum, maximum, budget(Long.MaxValue)),
      Left(AlderRidgeRegressionError.SolvePlanOverflow))
  }

  test("matrix-native execution, a shared outer/inner design, and malformed inputs are refused with specific errors") {
    val f = scalar()
    val grid = right(RidgePenaltyGrid(Vector(0.5)))
    AlderRidgeRegression.crossValidate(f.rows, f.outer, f.inner, f.response, Vector("x"), grid, budget(), execution = RidgeExecution.MatrixNative) match
      case Left(AlderRidgeRegressionError.Admission(AlderPredictiveAdmissionError.MatrixNativeRidgeUnavailable)) => ()
      case other => fail(s"expected MatrixNativeRidgeUnavailable, got $other")
    AlderRidgeRegression.crossValidate(f.rows, f.outer, f.outer, f.response, Vector("x"), grid, budget()) match
      case Left(AlderRidgeRegressionError.SharedOuterInnerDesign) => ()
      case other => fail(s"expected SharedOuterInnerDesign, got $other")
    AlderRidgeRegression.crossValidate(f.rows, f.outer, f.inner, f.response, Vector("x", "extra"), grid, budget()) match
      case Left(AlderRidgeRegressionError.FeatureCountMismatch(1, 2)) => ()
      case other => fail(s"expected FeatureCountMismatch(1, 2), got $other")
    val twoResponses = right(AxisRef.fromStableKeys("responses", SpaceRole.Observed, Vector("a", "b"), "response", "none", "value"))
    AlderRidgeRegression.crossValidate(f.rows, f.outer, f.inner, twoResponses, Vector("x"), grid, budget()) match
      case Left(AlderRidgeRegressionError.ResponseCountMismatch(1, 2)) => ()
      case other => fail(s"expected ResponseCountMismatch(1, 2), got $other")
    assertEquals(RidgePenaltyGrid(Vector.empty).left.map(_.message), Left("invalid penalty grid: grid is empty"))
    assert(RidgePenaltyGrid(Vector(-1.0)).left.exists { case AlderRidgeRegressionError.InvalidPenaltyGrid(_) => true; case _ => false })
    assert(RidgePenaltyGrid(Vector(1.0, 1.0)).left.exists { case AlderRidgeRegressionError.InvalidPenaltyGrid(_) => true; case _ => false })
    assertEquals(RidgeSolveBudget(0L).left.toOption, Some(AlderRidgeRegressionError.InvalidBudget))
  }
