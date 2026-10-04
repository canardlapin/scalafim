package scalafim.fmri.mvpa.pattern

import gale.linalg.DMat
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import resample4s.core.{IndexSpace, Injection}
import scalafim.fmri.mvpa.{AxisRef, Column, ReindexingLeg}

class LocalPatternPredictionSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, size: Int) = right(AxisRef.fromStableKeys(name, SpaceRole.Observed, Vector.tabulate(size)(i => s"$name-$i"), "fixture", "one", "raw"))
  private val prepared = LocalPredictionPreparation.AlreadyPreparedLocalCoordinates("local fixture coordinates")
  private final class Fixture(rank: Int = 1, noiseRank: Int = 1, intercept: Boolean = false,
      declaredTraining: Option[scalafim.fmri.mvpa.AxisDescriptor] = None):
    val neural = axis("neural", 2)
    val target = axis("target", rank)
    val component = axis("component", rank)
    val samples = axis("samples", 4)
    val a = if rank == 1 then DMat.dense(2, 1, Vector(1.0, 2.0)) else DMat.eye(2)
    val c = DMat.eye(rank)
    val factors = right(PatternFactors(neural, target, component, a, c, GaugeEvidence.PendingNumericalCheck))
    val unit = right(AxisValues(target, Vector.fill(rank)(1.0)))
    val geometry = right(TargetGeometry.continuous(target, unit, unit, Vector("all" -> unit)))
    val covariance = right(ResidualCovariance.fromFactors(neural, Vector(2.0, 1.0),
      if noiseRank == 1 then DMat.dense(2, 1, Vector(1.0, 1.0)) else DMat.dense(2, 2, Vector(1.0, 0.0, 1.0, 1.0))))
    val centering = if intercept then CenteringPolicy.ExplicitIntercept(right(AxisValues(neural, Vector(10.0, -4.0))), "centered targets") else CenteringPolicy.CenteredBeforeFit("x", "y")
    val artifact = right(PatternArtifact(factors, geometry, centering, DegenerateTargetPolicy.Refuse,
      ResidualCovarianceCapability.DiagonalPlusLowRank(neural.descriptor, noiseRank), right(TrainingBinding(declaredTraining.getOrElse(samples.descriptor), "fixture", "fitted")), Vector("fixture"), right(PatternFitDiagnostics(Vector(0.0), "fixture", Vector.empty))))
    val prior = right(TargetPriorCovariance(target, DMat.eye(rank), ValueIdentity.source(ValueId.unsafe("prior")), "target covariance"))
    val parent = right(PatternPrediction.fromArtifact(neural, target, component, artifact, covariance, Some(prior)))
    def select(ordinals: Int*) = right(ReindexingLeg.bind(neural, right(Injection.from(IArray.from(ordinals), right(IndexSpace.of(2))))))

  test("hard ROI uses inverse marginal covariance rather than either cropped full-brain filter"):
    val f = new Fixture
    val local = right(LocalPatternPrediction.derive(f.parent, f.select(0), prepared))
    val x = right(AxisValues(local.localAxis, Vector(1.0)))
    assertEqualsDouble(local.rawFilters.neuralByComponent(0, 0), 1.0 / 3.0, 1e-12)
    assertEqualsDouble(local.gram(0, 0), 1.0 / 3.0, 1e-12)
    assertEqualsDouble(right(local.rawScores(x)).values.values.head, 1.0 / 3.0, 1e-12)
    assertEqualsDouble(right(local.calibratedScores(x)).values.values.head, 1.0, 1e-12)
    val decoded = right(local.decode(x)).values.values.head
    assertEqualsDouble(decoded, .25, 1e-12)
    assert(math.abs(decoded - 2.0 / 7.0) > .01)
    assertEqualsDouble(f.parent.rawFilters.neuralByComponent(0, 0), 0.0, 1e-12)
    assertEquals(local.receipt.parentPrediction, f.parent.numericalIdentity)
    // The local head has no complementary-coordinate input or retained reader.
    val full1 = right(f.parent.decode(right(AxisValues(f.neural, Vector(1.0, 0.0))))).values.values.head
    val full2 = right(f.parent.decode(right(AxisValues(f.neural, Vector(1.0, 100.0))))).values.values.head
    assert(math.abs(full1 - full2) > 1.0)
    assertEqualsDouble(right(local.decode(x)).values.values.head, decoded, 1e-12)

  test("local observation dimension below inherited task rank retains Gaussian posterior but refuses calibration"):
    val f = new Fixture(rank = 2)
    val local = right(LocalPatternPrediction.derive(f.parent, f.select(0), prepared))
    val x = right(AxisValues(local.localAxis, Vector(1.0)))
    val result = right(local.decode(x)).values.values
    assertEqualsDouble(result(0), .25, 1e-12)
    assertEqualsDouble(result(1), 0.0, 1e-12)
    assert(local.calibratedScores(x).isLeft)
    assertEquals(local.receipt.inheritedTaskRank, 2)
    assertEquals(local.rawFilters.neuralByComponent.cols, 2)
    assert(local.limitation.contains("omitted"))

  test("hard restriction retains all covariance noise factors when ROI size is smaller than h"):
    val f = new Fixture(noiseRank = 2)
    val by = f.select(0)
    val covariance = right(f.covariance.restrict(by))
    assertEquals(covariance.rank, 2)
    val local = right(LocalPatternPrediction.derive(f.parent, by, prepared))
    assertEqualsDouble(right(local.decode(right(AxisValues(local.localAxis, Vector(1.0))))).values.values.head, .25, 1e-12)

  test("consistent ROI reordering and selected intercept preserve predictions and local forward means"):
    val f = new Fixture(intercept = true)
    val first = right(LocalPatternPrediction.derive(f.parent, f.select(0, 1), prepared))
    val reversed = right(LocalPatternPrediction.derive(f.parent, f.select(1, 0), prepared))
    val a = right(first.decode(right(AxisValues(first.localAxis, Vector(11.0, -4.5))))).values.values.head
    val b = right(reversed.decode(right(AxisValues(reversed.localAxis, Vector(-4.5, 11.0))))).values.values.head
    assertEqualsDouble(a, b, 1e-12)
    val local = right(LocalPatternPrediction.derive(f.parent, f.select(0), prepared))
    assertEqualsDouble(right(local.decode(right(AxisValues(local.localAxis, Vector(11.0))))).values.values.head, .25, 1e-12)
    assertEquals(right(local.encode(right(AxisValues(f.target, Vector(3.0))))).values, Vector(13.0))

  test("foreign parent, foreign local coordinates, outside preprocessing and zero budgets refuse"):
    val f = new Fixture
    val foreign = axis("foreign", 2)
    val by = right(ReindexingLeg.bind(foreign, right(Injection.from(IArray(0), right(IndexSpace.of(2))))))
    assertEquals(LocalPatternPrediction.derive(f.parent, by, prepared).left.toOption, Some(LocalPredictionError.ForeignParent))
    val local = right(LocalPatternPrediction.derive(f.parent, f.select(0), prepared))
    assert(local.decode(right(AxisValues(by.child, Vector(1.0)))).isLeft)
    assert(LocalPatternPrediction.derive(f.parent, f.select(0), LocalPredictionPreparation.RequiresOutsideMeasurements("whole-brain test-row normalization")).isLeft)
    assert(LocalPatternPrediction.derive(f.parent, f.select(0), prepared, PatternPredictionPolicy(1e-12, 0L)).isLeft)

  test("actual held-out local Gaussian predictions have an explicit squared-loss calculation"):
    val f = new Fixture
    val local = right(LocalPatternPrediction.derive(f.parent, f.select(0), prepared))
    val heldout = Vector((-2.0, -1.0), (-1.0, -.5), (1.0, .5), (2.0, 1.0))
    val losses = heldout.map: (x, target) =>
      val estimate = right(local.decode(right(AxisValues(local.localAxis, Vector(x))))).values.values.head
      val difference = estimate - target
      difference * difference
    assertEqualsDouble(losses.sum / losses.size, 5.0 / 32.0, 1e-12)
    assertEquals(local.receipt.inheritedTaskRank, 1)

  test("local categorical likelihood preserves ordered priors under the marginal observation model"):
    val f = new Fixture
    val conditions = right(AxisRef.fromStableKeys("classes", SpaceRole.Observed, Vector("last", "first"), "fixture", "one", "raw"))
    val geometry = right(TargetGeometry.categorical(conditions, f.target, DMat.dense(2, 1, Vector(-1.0, 1.0)), right(AxisValues(conditions, Vector(.2, .8)))))
    val artifact = right(PatternArtifact(f.factors, geometry, f.centering, DegenerateTargetPolicy.Refuse,
      ResidualCovarianceCapability.DiagonalPlusLowRank(f.neural.descriptor, 1), f.artifact.trainingBinding, Vector("fixture"), f.artifact.diagnostics))
    val parent = right(PatternPrediction.fromArtifact(f.neural, f.target, f.component, artifact, f.covariance))
    val local = right(LocalPatternPrediction.derive(parent, f.select(0), prepared))
    val result = right(local.classify(right(AxisValues(local.localAxis, Vector(1.0)))))
    assertEquals(result.keys, Vector("last", "first"))
    assertEqualsDouble(result.logScores(0), math.log(.2) - .5, 1e-12)
    assertEqualsDouble(result.logScores(1), math.log(.8) + 1.0 / 6.0, 1e-12)
    assertEqualsDouble(result.probabilities.sum, 1.0, 1e-12)

  test("identified held-out assessment returns target reduction and equal-unit loss and rejects selection reuse"):
    val all = axis("all-samples", 6)
    def split(indices: Int*) = right(ReindexingLeg.bind(all, right(Injection.from(IArray.from(indices), right(IndexSpace.of(6))))))
    val train = split(0, 1)
    val held = split(2, 3, 4, 5)
    val f = new Fixture(declaredTraining = Some(train.child.descriptor))
    val local = right(LocalPatternPrediction.derive(f.parent, f.select(0), prepared))
    val x = right(Column.fromValues(held.child, Vector(-2.0, -1.0, 1.0, 2.0).map(value => right(AxisValues(local.localAxis, Vector(value)))), ValueIdentity.source(ValueId.unsafe("held-x"))))
    val y = right(Column.fromValues(held.child, Vector(-1.0, -.5, .5, 1.0).map(value => right(AxisValues(f.target, Vector(value)))), ValueIdentity.source(ValueId.unsafe("held-y"))))
    val units = right(Column.fromValues(held.child, Vector("subject-a", "subject-b", "subject-b", "subject-b"), ValueIdentity.source(ValueId.unsafe("units"))))
    val reduction = right(AxisValues(f.target, Vector(1.0)))
    val result = right(local.assess(train, held, Vector(train), Vector(train))(x, y, units, reduction, 1000L))
    // Unit A loss 1/4; unit B losses (1/16,1/16,1/4), mean 1/8.
    assertEqualsDouble(result.meanSquaredLoss, 3.0 / 16.0, 1e-12)
    assertEquals(result.units.map(_.rows), Vector(1, 3))
    assertEquals(result.targetReduction, Vector(1.0))
    assertEquals(result.prediction, local.receipt)
    val otherSource = right(Column.fromValues(held.child, y.values, ValueIdentity.source(ValueId.unsafe("different-held-target-source"))))
    assert(right(local.assess(train, held, Vector(train), Vector(train))(x, otherSource, units, reduction, 1000L)).identity != result.identity)
    val otherX = right(Column.fromValues(held.child, x.values, ValueIdentity.source(ValueId.unsafe("different-held-neural-source"))))
    val otherUnits = right(Column.fromValues(held.child, units.values, ValueIdentity.source(ValueId.unsafe("different-evaluation-unit-source"))))
    assert(right(local.assess(train, held, Vector(train), Vector(train))(otherX, y, units, reduction, 1000L)).identity != result.identity)
    assert(right(local.assess(train, held, Vector(train), Vector(train))(x, y, otherUnits, reduction, 1000L)).identity != result.identity)
    val overlapping = split(1, 2, 3, 4)
    val overlapX = right(Column.fromValues(overlapping.child, x.values, x.valueIdentity))
    val overlapY = right(Column.fromValues(overlapping.child, y.values, y.valueIdentity))
    val overlapUnits = right(Column.fromValues(overlapping.child, units.values, units.valueIdentity))
    assertEquals(local.assess(train, overlapping, Vector.empty, Vector.empty)(overlapX, overlapY, overlapUnits, reduction, 1000L), Left(LocalAssessmentError.AssessmentReuse("training", Vector(1))))
    // Simulates untrusted restored metadata that falsely asserts a nominal
    // witness. Exact parent descriptors still protect the boundary.
    val foreignParent = axis("foreign-sample-parent", 6)
    val foreignScope = right(ReindexingLeg.bind(foreignParent, right(Injection.from(IArray(0), right(IndexSpace.of(6))))))
      .asInstanceOf[ReindexingLeg[all.Id, String, Injection]]
    assertEquals(local.assess(train, held, Vector(foreignScope), Vector.empty)(x, y, units, reduction, 1000L), Left(LocalAssessmentError.ScopeMismatch("scope parent")))
    assertEquals(local.assess(train, held, Vector(split(0, 2)), Vector.empty)(x, y, units, reduction, 1000L), Left(LocalAssessmentError.AssessmentReuse("selection", Vector(2))))
    assertEquals(local.assess(train, held, Vector.empty, Vector(split(1, 5)))(x, y, units, reduction, 1000L), Left(LocalAssessmentError.AssessmentReuse("tuning", Vector(5))))
    assert(local.assess(split(0), held, Vector.empty, Vector.empty)(x, y, units, reduction, 1000L).isLeft)
    assert(local.assess(train, held, Vector.empty, Vector.empty)(x, y, units, reduction, 0L).isLeft)
    assert(local.assess(train, held, Vector.empty, Vector.empty)(x, y, units, right(AxisValues(f.target, Vector(0.0))), 1000L).isLeft)

  test("assessment refuses a positive target reduction whose normalization would underflow"):
    val all = axis("two-target-samples", 2)
    val train = right(ReindexingLeg.bind(all, right(Injection.from(IArray(0), right(IndexSpace.of(2))))))
    val held = right(ReindexingLeg.bind(all, right(Injection.from(IArray(1), right(IndexSpace.of(2))))))
    val f = new Fixture(rank = 2, declaredTraining = Some(train.child.descriptor))
    val local = right(LocalPatternPrediction.derive(f.parent, f.select(0), prepared))
    val x = right(Column.fromValues(held.child, Vector(right(AxisValues(local.localAxis, Vector(0.0)))), ValueIdentity.source(ValueId.unsafe("x"))))
    val y = right(Column.fromValues(held.child, Vector(right(AxisValues(f.target, Vector(1e300, 0.0)))), ValueIdentity.source(ValueId.unsafe("y"))))
    val units = right(Column.fromValues(held.child, Vector("unit"), ValueIdentity.source(ValueId.unsafe("unit"))))
    assertEquals(local.assess(train, held, Vector.empty, Vector.empty)(x, y, units, right(AxisValues(f.target, Vector(1e-300, 1e300))), 1000L), Left(LocalAssessmentError.InvalidReduction))
