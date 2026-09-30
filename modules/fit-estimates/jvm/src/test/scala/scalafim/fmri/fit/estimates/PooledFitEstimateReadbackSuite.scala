package scalafim.fmri.fit.estimates

import java.nio.file.Files
import scalafim.estimates.*
import scalafim.estimates.io.{CovarianceLayout, LocalEstimateStore}
import scalafim.fmri.fit.EstimateUncertaintyRequest
import scalafim.fmri.group.*

class PooledFitEstimateReadbackSuite extends munit.FunSuite:
  import PooledProducerFixture.*
  private def right[A](value: Either[EstimateError, A]): A = value.fold(e => fail(e.message), identity)

  test("native pooled PairNifti roundtrip preserves every scalar/pair cell, validity and metadata at blocks 1/2/3") {
    for size <- Vector(1, 2, 3); mode <- Vector(EstimateUncertaintyRequest.None, EstimateUncertaintyRequest.Marginal, EstimateUncertaintyRequest.Joint) do
      val f = new NativePooledFixture()
      val p = f.producer(mode, size)
      val store = right(LocalEstimateStore.open(Files.createTempDirectory("pooled-readback-")))
      val reader = f.reader
      val pinned = right(p.write(reader, right(store.newSink(p.unit, size, CovarianceLayout.PairNifti))))
      assert(!reader.closed)
      val source = right(store.open(pinned, ReadLimits(1)))
      try
        assertEquals(source.unit.bindings, p.unit.bindings)
        assertEquals(source.unit.products, p.unit.products)
        assertEquals(source.unit.provenance, p.unit.provenance)
        assertEquals(source.unit.estimability, p.unit.estimability)
        assertEquals(source.unit.covariance, p.unit.covariance)
        assertEquals(source.unit.degreesOfFreedom, p.unit.degreesOfFreedom)
        assertEquals(source.unit.marginalUncertainty, p.unit.marginalUncertainty)
        assertEquals(source.unit.observations, p.unit.observations)
        assertEquals(source.unit.catalog, p.unit.catalog)
        assertEquals(source.unit.domain.support, sampleOrder)
        assertEquals(source.unit.outcomes, p.unit.outcomes)
        val value = new Array[Double](1)
        val code = new Array[Byte](1)
        for product <- source.unit.products if product.kind != ProductKind.Covariance; id <- ids.reverse; sample <- sampleOrder.reverse do
          right(source.read(product.id, EstimateSelection(product.observations, Vector(id), Vector(sample)), value, code))
          val row = ids.indexOf(id)
          val expected = if sample >= 3 then 0.0 else if product.kind == ProductKind.Effect then oracle(sample)._1(row) else math.sqrt(oracle(sample)._2(row)(row))
          assertEqualsDouble(value(0), expected, 1e-11)
          assertEquals(code(0), if sample >= 3 then Validity.NonEstimable.code else Validity.Valid.code)
        for covariance <- source.unit.covariance do
          assertEquals(covariance.equation, CovarianceEquation.Absolute)
          assert(!covariance.invariantSamples)
          for i <- ids.indices; j <- i until ids.size; sample <- sampleOrder.reverse do
            right(source.readCovariance(covariance.product, CovarianceSelection(Vector(publication.observation),
              Vector(EstimandPair(ids(i), ids(j))), Vector(sample)), value, code))
            assertEqualsDouble(value(0), if sample >= 3 then 0.0 else oracle(sample)._2(i)(j), 1e-11)
            assertEquals(code(0), if sample >= 3 then Validity.NonEstimable.code else Validity.Valid.code)
          for sample <- 0 until 3 do
            val matrix = right(CovarianceAccess.matrix(source, covariance.product, publication.observation, sample,
              ids.reverse, CovariancePolicy(3, 1e-12, 1e-12)))
            assertEqualsDouble(matrix.varianceScale, 1.0, 0.0)
            for i <- ids.indices; j <- ids.indices do assertEqualsDouble(matrix.values(i,j), oracle(sample)._2(2-i)(2-j), 1e-11)
          assert(CovarianceAccess.matrix(source, covariance.product, publication.observation, 3, ids,
            CovariancePolicy(3, 1e-12, 1e-12)).isLeft)
        assert(!source.unit.products.exists(_.kind == ProductKind.ResidualVariance))
      finally right(source.close())
  }

  test("compact physical sink refuses absolute pooled covariance before any response read") {
    val f = new NativePooledFixture()
    val p = f.producer()
    val reader = f.reader
    val store = right(LocalEstimateStore.open(Files.createTempDirectory("pooled-compact-refusal-")))
    assert(store.newSink(p.unit, 2, CovarianceLayout.SharedNormalizedTable()).isLeft)
    assertEquals(reader.reads, 0)
  }

  test("all-excluded physical publication retains declared coverage and invalid values") {
    val f = new NativePooledFixture(allExcluded = true)
    val p = f.producer()
    val store = right(LocalEstimateStore.open(Files.createTempDirectory("pooled-excluded-")))
    val ref = right(p.write(f.reader, right(store.newSink(p.unit, 2))))
    val source = right(store.open(ref, ReadLimits(1)))
    try
      val value = new Array[Double](1)
      val code = new Array[Byte](1)
      for sample <- sampleOrder; product <- source.unit.products do
        if product.kind == ProductKind.Covariance then
          for (a,b) <- product.targets.pairs do
            right(source.readCovariance(product.id, CovarianceSelection(product.observations, Vector(EstimandPair(a,b)), Vector(sample)), value, code))
            assertEqualsDouble(value(0), 0.0, 0.0)
            assertEquals(code(0), Validity.NonEstimable.code)
        else
          for id <- ids do
            right(source.read(product.id, EstimateSelection(product.observations, Vector(id), Vector(sample)), value, code))
            assertEqualsDouble(value(0), 0.0, 0.0)
            assertEquals(code(0), Validity.NonEstimable.code)
    finally right(source.close())
  }

  test("pinned independent participants preserve Unknown origin in group readback; pooled plus constituent is refused") {
    val store = right(LocalEstimateStore.open(Files.createTempDirectory("pooled-group-")))
    val identities = Vector(publication, publication.copy(unit = UnitId("00000000-0000-4000-8000-000000000112"),
      revision = UnitRevisionId("00000000-0000-4000-8000-000000000113"), participantLabel = "02", observation = ObservationId("pooled-02")))
    val inputs = identities.zipWithIndex.map: (identity, i) =>
      val f = new NativePooledFixture(i * 0.75)
      val p = f.producer(identity = identity)
      val pinned = right(p.write(f.reader, right(store.newSink(p.unit, 2))))
      GroupEstimateInput(pinned, identity.observation, p.unit.products.head.id,
        Some(GroupMarginalUncertainty.StandardError(p.unit.products.find(_.kind == ProductKind.StandardError).get.id)))
    val admission = new GroupEstimateAdmission:
      def verify(units: Vector[EstimateUnit]) =
        Right(GroupGeometryEvidence.Verified("scanner", "explicit identical synthetic scanner grid and native pooled method admission", Vector.empty))
    val budgeted = right(EstimateGroup.prepare(store, inputs, ids.reverse, admission, 32))
    assertEquals(budgeted.readBlock(Vector(2,0,1)).left.toOption.get.message, "group block exceeds the total effect/variance cell budget")
    val group = right(EstimateGroup.prepare(store, inputs, ids.reverse, admission, 36))
    val data = right(group.readBlock(Vector(2,0,1))).data
    for subject <- identities.indices; sample <- Vector(2,0,1).indices; row <- ids.indices do
      val response = data.response(ids(row).value).get
      val expected = oracle(Vector(2,0,1)(sample), subject * 0.75)
      assertEqualsDouble(response.effects(subject,sample), expected._1(row), 1e-11)
      assertEqualsDouble(response.variances.get(subject,sample), expected._2(row)(row), 1e-11)
    assert(data.uncertainty.get.sources.forall(_.origin == GroupVarianceOrigin.Unknown(PooledFitEstimateProducer.UnknownPooledDfReason)))
    val f = new NativePooledFixture()
    val pooledPlan = f.prepared()
    val runPlan = checked(scalafim.fmri.fit.FirstLevelEstimates.prepare(scalafim.fmri.model.FitPlan(f.model),
      f.request(EstimateUncertaintyRequest.Marginal), scalafim.fmri.fit.ChunkSize.unsafe(2),
      scalafim.dataset.DataSelection(time = scalafim.dataset.IndexSelection.Indices(Vector(4,5,6,7)),
        voxels = scalafim.dataset.IndexSelection.Indices(Vector(0,1,2)))))
    val runIdentity = publication.copy(unit = UnitId("00000000-0000-4000-8000-000000000122"),
      revision = UnitRevisionId("00000000-0000-4000-8000-000000000123"), observation = ObservationId("constituent-01"))
    val runProducer = right(FitEstimateProducer.shared(runPlan, runIdentity, f.catalog(pooledPlan), ids, "scanner"))
    val runRef = right(runProducer.write(f.reader, right(store.newSink(runProducer.unit, 2))))
    assert(runProducer.unit.products.forall(_.pooling == PoolingScope.Run))
    val constituent = GroupEstimateInput(runRef, runIdentity.observation, runProducer.unit.products.head.id,
      Some(GroupMarginalUncertainty.StandardError(runProducer.unit.products.find(_.kind == ProductKind.StandardError).get.id)))
    val refused = EstimateGroup.prepare(store, Vector(inputs.head, constituent), ids, admission, 32)
    assertEquals(refused.left.toOption.get.message, "group units require one dataset, shared immutable catalog, effect units and pooling scope")
    val duplicate = EstimateGroup.prepare(store, Vector(inputs.head, inputs.head), ids, admission, 32)
    assertEquals(duplicate.left.toOption.get.message, "runs, trials and pooled rows from one participant cannot become independent group subjects")
  }
