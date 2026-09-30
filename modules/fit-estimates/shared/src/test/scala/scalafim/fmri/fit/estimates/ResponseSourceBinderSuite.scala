package scalafim.fmri.fit.estimates

import gale.linalg.Matrix
import scalafim.estimates.*
import scalafim.dataset.{DataSelection, DatasetError, DatasetEvents, DatasetSeriesReader, FmriDataset, FmriSeries, InMemoryDatasetBackend}
import scalafim.fmri.design.{EmptyCellPolicy, FactorLevelRegistry}
import scalafim.fmri.hrf.s
import scalafim.fmri.ar.NoisePooling
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.fit.{DenseFmriFitResult, FitPlanExecutor, FixedEffectsFmriFitResult, FmriFitResult}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ArOptions, ArStructure, FitConfig, FitEngine, FitPlan, FitStrategy, FmriModel, FmriModelBuilder, ModelBuildSpec}
import scalafim.image.SampleSpaces

/** A small real first-level fit: two task columns (A, B) plus an intercept. */
object BinderFixture:
  def checked[E, A](value: Either[E, A]): A = value.fold(e => throw new IllegalArgumentException(e.toString), identity)
  val voxels = 3
  private def task(t: Int): (Double, Double) = (if t % 4 == 0 then 1.0 else 0.0, if t % 4 == 2 then 1.0 else 0.0)

  /** Deterministic AR(1) noise, phi = 0.6, from a fixed linear congruential stream. */
  def arNoise(rows: Int, phi: Double): Vector[Vector[Double]] =
    var state = 12345L
    def uniform(): Double =
      state = (state * 6364136223846793005L + 1442695040888963407L)
      ((state >>> 11).toDouble / (1L << 53).toDouble) - 0.5
    Vector.tabulate(voxels): _ =>
      var previous = 0.0
      Vector.tabulate(rows): _ =>
        previous = phi * previous + uniform()
        previous

  final case class Fixture(model: FmriModel, dataset: FmriDataset, runs: Int):
    def reader: DatasetSeriesReader = new DatasetSeriesReader:
      val dataset = Fixture.this.dataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] = dataset.seriesEither(selection)
    def fit(plan: FitPlan): FmriFitResult = checked(FitPlanExecutor.fit(reader, plan))
    def ols: DenseFmriFitResult = checked(FitPlanExecutor.fitDense(reader, FitPlan(model, FitEngine.OrdinaryLeastSquares)))

  def fixture(runs: Int = 1, rowsPerRun: Int = 16, noise: Option[Double] = None): Fixture =
    val rows = runs * rowsPerRun
    val frame = SamplingFrame(blockLens = Seq.fill(runs)(rowsPerRun), tr = Seq.fill(runs)(1.0))
    val errors = noise.map(arNoise(rows, _))
    val data = Matrix.tabulate(rows, voxels): (t, v) =>
      val (a, b) = task(t)
      1.0 + (v + 1) * a - 0.5 * b + errors.fold(0.01 * math.sin(t * 1.7 + v))(_(v)(t))
    val dataset = FmriDataset.unsafe(InMemoryDatasetBackend(scalafim.dataset.DatasetId(s"binder-$runs-${noise.isDefined}"),
      data, SampleSpaces(Vector(voxels, 1, 1))), frame)
    val event = EventModel(Vector.empty, frame, Mat.fromRows(Vector.tabulate(rows)(t => Vector(task(t)._1, task(t)._2))),
      Vector("A", "B"), Vector(0 -> 2), Map("task" -> Vector(0, 1)))
    Fixture(FmriModel(event, BaselineModel.build(frame, basis = BaselineBasis.Constant, intercept = Intercept.Global), dataset),
      dataset, runs)

  /** Two runs compiled from events, so that every run carries a structural axis. */
  def compiledTwoRuns(): Fixture =
    val rows = 32
    val frame = SamplingFrame(blockLens = Seq(16, 16), tr = Seq(2.0, 2.0), startTime = Seq(0.0, 0.0), precision = 0.1)
    val data = Matrix.tabulate(rows, voxels)((t, v) => 1.0 + 0.1 * v + math.sin(0.9 * t + v) + 0.05 * (t % 5))
    val dataset = FmriDataset.unsafe(InMemoryDatasetBackend(scalafim.dataset.DatasetId("binder-two-runs"), data,
      SampleSpaces(Vector(voxels, 1, 1))), frame,
      DatasetEvents(Vector("run-1", "run-2").flatMap(run => Vector(
        Map("onset" -> "2.0", "cond" -> "A", "run" -> run), Map("onset" -> "12.0", "cond" -> "B", "run" -> run),
        Map("onset" -> "20.0", "cond" -> "A", "run" -> run)))))
    val model = FmriModelBuilder.buildModel(dataset, ModelBuildSpec("onset ~ hrf(cond)", blockColumn = Some("run"),
      baselineIntercept = Intercept.Global, factorLevels = checked(FactorLevelRegistry.of("cond" -> Seq("A", "B"))),
      emptyCellPolicy = EmptyCellPolicy.RetainZero, precision = 0.1.s))
    Fixture(model, dataset, 2)

  val observation: ObservationId = ObservationId("sub-01")
  val a: EstimandId = EstimandId("A")
  val b: EstimandId = EstimandId("B")

  def columnsOf(fit: FmriFitResult): Vector[ColumnId] = fit.coefficientAxis.get.columnIds.map(id => ColumnId(id.value))

  def unit(
      fit: FmriFitResult,
      acquisitions: Int = 1,
      support: Option[Vector[Int]] = None,
      extra: Vector[(EstimandId, EstimandBinding)] = Vector.empty,
      bindingColumns: Option[Vector[ColumnId]] = None
  ): EstimateUnit =
    val dataset = DatasetId("00000000-0000-4000-8000-000000000301")
    val columns = columnsOf(fit)
    def oneHot(i: Int) = Vector.tabulate(columns.size)(c => if c == i then 1.0 else 0.0)
    val boundColumns = bindingColumns.getOrElse(columns)
    val base = Vector(EstimandBinding(a, boundColumns, 1, oneHot(boundColumns.indexOf(columns(0)))),
      EstimandBinding(b, boundColumns, 1, oneHot(boundColumns.indexOf(columns(1)))))
    val ids = Vector(a, b) ++ extra.map(_._1)
    val effect = ProductDescriptor(ProductId("effect"), ProductKind.Effect, NumericPrecision.Float64, Vector(observation),
      ProductTargets.Scalar(ids.filterNot(_.value.startsWith("unrealized"))), PoolingScope.Run, "signal")
    val unknown = ScientificFact.Unknown("fixture")
    EstimateUnit(dataset, UnitId("00000000-0000-4000-8000-000000000302"), UnitRevisionId("00000000-0000-4000-8000-000000000303"),
      EstimandCatalog(ModelRevisionId("00000000-0000-4000-8000-000000000304"),
        ids.map(id => EstimandDefinition(id, id.value, EstimandKind.Coefficient, "signal", "unit", id.value))),
      checked(EstimateDomain.make(SampleSpaces(Vector(voxels, 1, 1)), support.getOrElse(fit.voxelIndices), "scanner")),
      Vector(Observation(observation, ParticipantId(dataset, "01"), Vector.tabulate(acquisitions)(r => AcquisitionId(s"run-$r")))),
      base ++ extra.map(_._2), Vector(effect), Map(effect.id -> ProductOutcome.Available(effect.id)),
      EstimabilityEvidence.Unknown("fixture"),
      EstimateProvenance("fixture", "1", "exec", unknown, unknown, unknown, unknown, Vector.empty, Vector.empty))

  val readout: Vector[ReadoutRow] =
    Vector(ReadoutRow(ReadoutRowId(ConditionLevelId("A"), 0), a), ReadoutRow(ReadoutRowId(ConditionLevelId("B"), 0), b))

  def materialized(fit: FmriFitResult, u: EstimateUnit): MaterializedUnitBinding =
    MaterializedUnitBinding(u, observation, readout, fit.voxelIndices)

class ResponseSourceBinderSuite extends munit.FunSuite:
  import BinderFixture.*

  private lazy val single = fixture()
  private lazy val ols = single.ols
  private lazy val olsUnit = unit(ols)
  private def bind(fit: FmriFitResult, m: MaterializedUnitBinding) = ResponseSourceBinder.bind(fit, m)
  private def bound(fit: FmriFitResult, u: EstimateUnit): ResponseSourceBinding = checked(bind(fit, materialized(fit, u)))

  test("an OLS fit of one run binds its native axis, selection, ordered features and white single-run record"):
    val binding = bound(ols, olsUnit)
    assertEquals(binding.unit, olsUnit.revision)
    assertEquals(binding.observation, observation)
    assertEquals(binding.columns, columnsOf(ols))
    assertEquals(binding.columns.size, 3)
    assertEquals(binding.selected, columnsOf(ols).take(2))
    assertEquals(binding.readoutAxis.rows, readout.map(_.row))
    assertEquals(binding.features, ResponseDigests.features(olsUnit.domain))
    assertEquals(binding.realizedNoise, RealizedNoise.White)
    assertEquals(binding.realizedCombination, RealizedCombination.SingleRun)
    assertEquals(bound(ols, olsUnit), binding, "binding must be deterministic")

  test("digests are canonical and identical on JVM and Scala.js"):
    val binding = bound(ols, olsUnit)
    val golden = Vector(binding.design, binding.preparation, binding.noise, binding.runCombination, binding.readout, binding.features)
      .map(d => s"${d.schema}=${d.digest.value}")
    assertEquals(golden, BinderGolden.digests)

  test("the binder refuses an axis-less dense fit result"):
    assertEquals(bind(ols.copy(coefficientAxis = None), materialized(ols, olsUnit)), Left(BindError.MissingCoefficientAxis))

  test("explicit materialized inputs are verified against native provenance"):
    val m = materialized(ols, olsUnit)
    val columns = columnsOf(ols)
    assertEquals(bind(ols, m.copy(observation = ObservationId("sub-99"))), Left(BindError.UnknownObservation(ObservationId("sub-99"))))
    assertEquals(bind(ols, m.copy(readout = Vector.empty)), Left(BindError.EmptyReadout))
    val c = EstimandId("C")
    assertEquals(bind(ols, m.copy(readout = m.readout :+ ReadoutRow(ReadoutRowId(ConditionLevelId("C"), 0), c))),
      Left(BindError.UnknownReadoutEstimand(c)))
    val hidden = EstimandId("unrealized")
    val withHidden = unit(ols, extra = Vector(hidden -> EstimandBinding(hidden, columns, 1, Vector(1.0, 1.0, 0.0))))
    assertEquals(bind(ols, MaterializedUnitBinding(withHidden, observation,
      readout :+ ReadoutRow(ReadoutRowId(ConditionLevelId("C"), 0), hidden), ols.voxelIndices)), Left(BindError.ReadoutNotRealized(hidden)))
    val wide = EstimandId("AB")
    val withWide = unit(ols, extra = Vector(wide -> EstimandBinding(wide, columns, 2, Vector(1.0, 0.0, 0.0, 0.0, 1.0, 0.0))))
    assertEquals(bind(ols, MaterializedUnitBinding(withWide, observation,
      Vector(ReadoutRow(ReadoutRowId(ConditionLevelId("AB"), 0), wide)), ols.voxelIndices)), Left(BindError.ReadoutNotScalar(wide)))
    // Same design fingerprint, reordered bound columns.
    val reordered = Vector(columns(1), columns(0), columns(2))
    assertEquals(bind(ols, materialized(ols, unit(ols, bindingColumns = Some(reordered)))),
      Left(BindError.ColumnAxisDisagrees(columns, reordered)))
    assertEquals(bind(ols, m.copy(readout = Vector(ReadoutRow(ReadoutRowId(ConditionLevelId("A"), 1), a),
      ReadoutRow(ReadoutRowId(ConditionLevelId("B"), 0), b)))),
      Left(BindError.InvalidReadoutAxis(ReadoutDefect.BinsNotSequential(ConditionLevelId("A")))))
    val zero = EstimandId("zero")
    val withZero = unit(ols, extra = Vector(zero -> EstimandBinding(zero, columns, 1, Vector(0.0, 0.0, 0.0))))
    assertEquals(bind(ols, MaterializedUnitBinding(withZero, observation,
      Vector(ReadoutRow(ReadoutRowId(ConditionLevelId("Z"), 0), zero)), ols.voxelIndices)), Left(BindError.NoSelectedColumns))

  test("ordered sample identity: count, same-set permutation against the domain and against the fit"):
    val m = materialized(ols, olsUnit)
    assertEquals(bind(ols, m.copy(orderedSampleIds = ols.voxelIndices.take(2))),
      Left(BindError.SampleCountDisagrees(SampleAxisSource.Domain, voxels, 2)))
    assertEquals(bind(ols, m.copy(orderedSampleIds = ols.voxelIndices.reverse)), Left(BindError.SampleOrderDisagrees(SampleAxisSource.Domain)))
    val permuted = unit(ols, support = Some(ols.voxelIndices.reverse))
    assertEquals(bind(ols, MaterializedUnitBinding(permuted, observation, readout, ols.voxelIndices.reverse)),
      Left(BindError.SampleOrderDisagrees(SampleAxisSource.FitResult)))
    assertNotEquals(ResponseDigests.features(permuted.domain), ResponseDigests.features(olsUnit.domain))

  test("missing native preparation provenance is refused, never defaulted"):
    assert(ols.preparationProvenance.nonEmpty, "the native OLS executor records preparation provenance")
    assertEquals(bind(ols.copy(preparationProvenance = None), materialized(ols, olsUnit)), Left(BindError.PreparationUnrecorded))

  test("the binder records estimated and fixed AR whitening without certifying exactness"):
    val estimated = single.fit(FitPlan(single.model, FitEngine.GeneralizedLeastSquares,
      FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1)))))
    val estimatedBinding = bound(estimated, unit(estimated))
    assert(estimatedBinding.realizedNoise match
      case RealizedNoise.EstimatedAr(1, _) => true
      case _ => false, estimatedBinding.realizedNoise.toString)
    val fixed = single.fit(FitPlan(single.model, FitEngine.GeneralizedLeastSquares,
      FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), rho = Some(0.3)))))
    val fixedBinding = bound(fixed, unit(fixed))
    assert(fixedBinding.realizedNoise match
      case RealizedNoise.FixedAr(1, _, None) => true
      case _ => false, fixedBinding.realizedNoise.toString)
    assertNotEquals(fixedBinding.noise, estimatedBinding.noise)
    assertNotEquals(fixedBinding.noise, bound(ols, olsUnit).noise)

  test("fixed-effects run combination is recorded as estimated weights; a joint multi-run fit is unrecorded"):
    val runs = compiledTwoRuns()
    val fixedEffects = runs.fit(FitPlan(runs.model, FitStrategy.SeparateRunsThenFixedEffects()))
    assert(fixedEffects.isInstanceOf[FixedEffectsFmriFitResult], fixedEffects.getClass.getName)
    val binding = bound(fixedEffects, unit(fixedEffects, acquisitions = 2))
    assertEquals(binding.realizedCombination, RealizedCombination.EstimatedWeights)
    val joint = runs.fit(FitPlan(runs.model, FitEngine.OrdinaryLeastSquares))
    assertEquals(bound(joint, unit(joint, acquisitions = 2)).realizedCombination, RealizedCombination.Unrecorded)
    assertEquals(bound(joint, unit(joint, acquisitions = 1)).realizedCombination, RealizedCombination.SingleRun,
      "control: the acquisition record, not the engine alone, decides single-run")

  test("AR truth fitted by OLS is recorded as white: the truth is undetectable and v1 still refuses"):
    val arTruth = fixture(noise = Some(0.6))
    val fit = arTruth.ols
    val u = unit(fit)
    val binding = bound(fit, u)
    assertEquals(binding.realizedNoise, RealizedNoise.White, "the binder records processing, not the generating law")
    val clean = bound(ols, olsUnit)
    assertEquals(binding.design, clean.design, "same compiled design, different response data")
    assertEquals(binding.noise, clean.noise, "the noise record is processing identity, not a noise estimate")
    val twoRuns = compiledTwoRuns()
    val other = twoRuns.fit(FitPlan(twoRuns.model, FitEngine.OrdinaryLeastSquares))
    assertNotEquals(bound(other, unit(other)).design, clean.design)
    val model = DeclaredResponseModel(DeclaredTemporal.White, DeclaredCombination.SingleRun, SpatialClaim.KroneckerSeparable,
      DistributionClaim.Gaussian, ModelOrigin.Declared("white Gaussian (false here)"))
    val swap = ConditionAction.Permute(Map(ConditionLevelId("A") -> ConditionLevelId("B"), ConditionLevelId("B") -> ConditionLevelId("A")))
    val request = ResponseActionRequest(ResponseActionEvidence.Version, binding, Some(model), swap,
      NullConstraint.Linear(ResponseDigests.provider("fixture/v1", "A = B")))
    val summary = StatusSummary(binding.unit, new InferenceStatusScope.Fit(observation), binding.features,
      Map(InferenceStatusCode.Estimable -> voxels.toLong), ScientificFact.Known("full rank"))
    assertEquals(ResponseActionEvidence.evaluate(request, binding, Some(StatusEvidence.Scanned(summary))),
      ResponseActionRefusal.Unavailable(UnavailableReason.NoPositiveContractInVersion(ResponseActionEvidence.Version)))

  test("canonical provenance is platform-stable, ordered for maps and fails closed"):
    assertEquals(CanonicalProvenance.render(1), CanonicalProvenance.render(1.0))
    assertNotEquals(CanonicalProvenance.render(1L), CanonicalProvenance.render(1))
    assertNotEquals(CanonicalProvenance.render(0.0), CanonicalProvenance.render(-0.0))
    assertEquals(CanonicalProvenance.render(Map("b" -> 1, "a" -> 2)), CanonicalProvenance.render(Map("a" -> 2, "b" -> 1)))
    assertEquals(CanonicalProvenance.render(Set(3, 1, 2)), CanonicalProvenance.render(Set(2, 3, 1)))
    assertNotEquals(CanonicalProvenance.render(Vector(1, 2)), CanonicalProvenance.render(Vector(2, 1)))
    assertNotEquals(CanonicalProvenance.render(("ab", "c")), CanonicalProvenance.render(("a", "bc")))
    assertEquals(CanonicalProvenance.render((Some(Vector(1.5, -0.0)), NoisePooling.Global, None, 'x', true)),
      Right("p6:Tuple5a1:5p4:Somea1:1a1:2n16:3ff8000000000000n16:8000000000000000p6:Globala1:0p4:Nonea1:0c3:120b4:true"))
    assert(CanonicalProvenance.render(new Object).isLeft)
    assert(CanonicalProvenance.render(Vector(1, null)).isLeft)

object BinderGolden:
  /** Recorded on the JVM; Scala.js must reproduce them exactly. */
  val digests: Vector[String] = Vector(
    "scalafim.response-design/1=51776bdd57c4b2a35c14707b6fbe061f83741878072ef5d44d372117dd5e5dca",
    "scalafim.response-preparation/1=60a62deaf5bf9f1b1a8bbe7aae3b46f314c9905a2f267633a4d27e8b2e864e39",
    "scalafim.realized-noise/1=411aa3cd429cda2f900c5ca93c23d8b2bc17edc7ba602a936c76097a6a36ccc0",
    "scalafim.run-combination/1=6f2a151c279a232281033352cfc9208f0204682c3fad933c09f71ac47cdef250",
    "scalafim.selected-readout/1=ea56575341fcac0f658b4074128a66d2db8e5a00921b5c6802c4aaa0ac0da0b4",
    "scalafim.response-features/1=5a3ea51c1fee30a27c113054cf9ef04bc9148ca0931dc89b8e2518b19dc402d3")
