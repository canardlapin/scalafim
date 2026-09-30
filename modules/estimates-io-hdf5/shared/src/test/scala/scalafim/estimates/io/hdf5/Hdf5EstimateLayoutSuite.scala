package scalafim.estimates.io.hdf5

import scalafim.estimates.*
import scalafim.image.SampleSpaces

private[hdf5] object PhysicalFixture:
  val ids = Vector(EstimandId("z"), EstimandId("a/β"))
  val dataset = DatasetId("00000000-0000-4000-8000-000000000201")
  val observations = Vector("row-z", "row-a").map(id => Observation(ObservationId(id), ParticipantId(dataset, id), Vector(AcquisitionId("run"))))
  val catalog = EstimandCatalog(ModelRevisionId("00000000-0000-4000-8000-000000000202"), ids.map(id =>
    EstimandDefinition(id, "same label", EstimandKind.Coefficient, "signal", "unit", id.value)))
  val effect = ProductDescriptor(ProductId("effect"), ProductKind.Effect, NumericPrecision.Float32,
    observations.map(_.id), ProductTargets.Scalar(ids), PoolingScope.Run, "signal")
  val scale = effect.copy(id = ProductId("scale"), kind = ProductKind.ResidualVariance, precision = NumericPrecision.Float64)
  val absolute = effect.copy(id = ProductId("absolute"), kind = ProductKind.Covariance, precision = NumericPrecision.Float64, targets = ProductTargets.UpperTriangle(ids))
  val shared = absolute.copy(id = ProductId("U"))
  val pairs = Vector(EstimandPair(ids(0), ids(0)), EstimandPair(ids(0), ids(1)), EstimandPair(ids(1), ids(1)))
  val products = Vector(effect, scale, absolute, shared)
  val unknown = ScientificFact.Unknown("literal storage fixture")
  def unit: EstimateUnit = EstimateUnit(dataset, UnitId("00000000-0000-4000-8000-000000000203"),
    UnitRevisionId("00000000-0000-4000-8000-000000000204"), catalog,
    EstimateDomain.make(SampleSpaces(Vector(5, 1, 1)), Vector(0, 2, 3, 4), "scanner").toOption.get,
    observations, Vector.empty, products, products.map(p => p.id -> ProductOutcome.Available(p.id)).toMap,
    EstimabilityEvidence.Unknown("storage fixture"),
    EstimateProvenance("literal fixture", "1", "storage", unknown, unknown, unknown, unknown, Vector.empty, Vector.empty),
    covariance = Vector(CovarianceDescriptor(absolute.id, effect.id, CovarianceEquation.Absolute, false, false),
      CovarianceDescriptor(shared.id, effect.id, CovarianceEquation.Normalized(scale.id), false, true)))
  def scalarUnit: EstimateUnit = unit.copy(products = Vector(effect), outcomes = Map(effect.id -> ProductOutcome.Available(effect.id)), covariance = Vector.empty)
  def value(product: ProductId, observation: Int, target: Int, sample: Int): Double =
    if product == shared.id then Vector(Vector(4.0, -0.5, 9.0), Vector(16.0, 0.25, 25.0))(observation)(target)
    else if product == absolute.id then 20.0 + observation * 100 + target * 10 + sample * 0.25
    else if product == scale.id then 2.0 + observation * 10 + target + sample * 0.25
    else -8.0 + observation * 100 + target * 10 + sample * 0.25
  def code(observation: Int, target: Int, sample: Int): Byte =
    if sample == 1 then Validity.OutsideSupport.code
    else if sample == 3 && observation == 1 && target == 0 then Validity.MissingInput.code
    else if sample == 4 && observation == 0 && target == 1 then Validity.NumericalFailure.code
    else Validity.Valid.code

class Hdf5EstimateLayoutSuite extends munit.FunSuite:
  private val f = PhysicalFixture
  private val limits = Hdf5EstimateLimits()
  test("physical axes derive from nonlexical descriptor order and exact precision"):
    val scalar = Hdf5EstimateLayout.derive(f.unit, f.effect.id, false, limits).toOption.get
    assertEquals(scalar.values.extent.dimensions, Vector(2L, 2L, 5L))
    assertEquals(scalar.values.dtype, scalafim.archive.hdf5.Hdf5DType.Float32)
    assertEquals(scalar.validity.dtype, scalafim.archive.hdf5.Hdf5DType.UInt8)
    assertEquals(scalar.coverageBytes, 3)
    val absolute = Hdf5EstimateLayout.derive(f.unit, f.absolute.id, false, limits).toOption.get
    assertEquals(absolute.values.extent.dimensions, Vector(2L, 3L, 5L))
    val shared = Hdf5EstimateLayout.derive(f.unit, f.shared.id, true, limits).toOption.get
    assertEquals(shared.values.extent.dimensions, Vector(2L, 3L))
    assertEquals(shared.values.dtype, scalafim.archive.hdf5.Hdf5DType.Float64)
    assertEquals(scalar.index(1, 1, 4), 19L)

  test("compact U requires existing normalized invariant Float64 covariance"):
    assert(Hdf5EstimateLayout.derive(f.unit, f.absolute.id, true, limits).isLeft)
    assert(Hdf5EstimateLayout.derive(f.unit, f.effect.id, true, limits).isLeft)
    assert(Hdf5EstimateLayout.derive(f.unit.copy(covariance = f.unit.covariance.map(_.copy(invariantSamples = false))), f.shared.id, true, limits).isLeft)
    val changed = f.unit.copy(products = f.products.map(p => if p.id == f.shared.id then p.copy(precision = NumericPrecision.Float32) else p))
    assert(Hdf5EstimateLayout.derive(changed, f.shared.id, true, limits).isLeft)

  test("Long products, dtype bytes, coverage and aggregate caps refuse before allocation"):
    assert(Hdf5EstimateLayout.shape(Vector(Long.MaxValue, 2), NumericPrecision.Float64, limits).isLeft)
    assert(Hdf5EstimateLayout.shape(Vector(Long.MaxValue), NumericPrecision.Float64, limits).isLeft)
    assert(Hdf5EstimateLayout.shape(Vector(0L), NumericPrecision.Float64, limits).isLeft)
    assert(Hdf5EstimateLayout.shape(Vector(9L), NumericPrecision.Float64, limits.copy(maximumCoverageBytes = 1)).isLeft)
    assert(Hdf5EstimateLayout.plans(f.unit, Set(f.shared.id), limits.copy(maximumCoverageBytes = 7)).isLeft)
    assert(Hdf5EstimateLayout.plans(f.unit, Set(ProductId("unknown")), limits).isLeft)
    assert(Hdf5EstimateLayout.plans(f.unit, Set.empty, limits.copy(maximumProducts = 1)).isLeft)
    val slab = scalafim.archive.hdf5.Hdf5Slab(Vector(Long.MaxValue), Vector(1L))
    assert(slab.isLeft)
    assert(scalafim.archive.hdf5.Hdf5Slab(Vector(0L, 0L), Vector(Long.MaxValue, 2L)).isLeft)

  test("resource profile keeps coverage independent and two datasets distinct from native IDs"):
    assertEquals(limits.archive.maxFiles, 1)
    assertEquals(limits.archive.maxDatasetsPerFile, 2)
    assertEquals(limits.archive.maxNativeIds, 24)
    assertEquals(limits.archive.rawCacheBytes, 1048576L)
    assertEquals(limits.archive.metadataCacheBytes, 4194304L)
    intercept[IllegalArgumentException](Hdf5EstimateLimits(maximumBlockCells = 65537))
    intercept[IllegalArgumentException](Hdf5EstimateLimits(maximumCoverageBytes = 0))

  test("evidence-bearing profile is refused before layout or staging"):
    val evidence = InferenceEvidence(Vector.empty, Vector(InferenceStatusScope.Fit(f.observations.head.id)))
    val unit = f.unit.copy(inferenceEvidence = Some(evidence))
    assert(Hdf5EstimateLayout.plans(unit, Set(f.shared.id), limits).left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
    assert(Hdf5EstimateLayout.derive(unit, f.effect.id, false, limits).isLeft)
