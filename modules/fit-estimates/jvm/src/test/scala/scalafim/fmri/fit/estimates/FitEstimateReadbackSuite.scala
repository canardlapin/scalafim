package scalafim.fmri.fit.estimates

import java.nio.file.Files
import scalafim.estimates.*
import scalafim.estimates.io.{CovarianceLayout, EstimateMetadata, LocalEstimateStore}

class FitEstimateReadbackSuite extends munit.FunSuite:
  private def right[A](value: Either[EstimateError, A]): A = value.fold(e => fail(e.message), identity)

  test("fit producer saves and reopens effects and marginal uncertainty through the independent reader") {
    val producer = ProducerFixture.producer
    val root = Files.createTempDirectory("scalafim-fit-estimates-")
    val store = right(LocalEstimateStore.open(root))
    val sink = right(store.newSink(producer.unit, 1))
    val reference = right(producer.write(ProducerFixture.reader, sink))
    val reader = right(LocalEstimateStore.open(root).flatMap(_.open(reference, ReadLimits(4))))
    try
      val effect = reader.unit.products.find(_.kind == ProductKind.Effect).get
      val values = new Array[Double](4)
      val validity = new Array[Byte](4)
      right(reader.read(effect.id, EstimateSelection(effect.observations, ProducerFixture.ids.reverse, Vector(1, 0)), values, validity))
      Vector(5.0, 1.0, -3.0, 2.0).zip(values).foreach((expected, actual) => assertEqualsDouble(actual, expected, 1e-12))
      assertEquals(validity.toVector, Vector[Byte](0, 0, 0, 0))
      assertEquals(reader.unit.degreesOfFreedom, producer.unit.degreesOfFreedom)
      assertEquals(reader.unit.marginalUncertainty, producer.unit.marginalUncertainty)
      assertEquals(reader.unit.bindings, producer.unit.bindings)
    finally right(reader.close())
  }

  test("literal analytic FIR publication writes physical Core-NIfTI maps and a fresh store preserves reversed bins and samples") {
    import FirEstimateProducerFixture.*
    val producer = FirEstimateProducerFixture.producer
    val root = Files.createTempDirectory("analytic-fir-core-nifti-")
    val initialStore = right(LocalEstimateStore.open(root))
    val reference = right(producer.write(reader, right(initialStore.newSink(producer.unit, 1))))
    val manifest = Files.readString(root.resolve(reference.manifest.path))
    val representations = right(EstimateMetadata.representations(manifest))
    assertEquals(representations.length, 3)
    representations.foreach: representation =>
      assert(Files.isRegularFile(root.resolve(representation.values.path)))
      assert(Files.isRegularFile(root.resolve(representation.validity.path)))
      assert(Files.size(root.resolve(representation.values.path)) > 352L)
      assert(Files.size(root.resolve(representation.validity.path)) > 352L)
      assertEquals(representation.storedDatatype, Some(scalafim.estimates.io.NiftiStoredDatatype.Float64))
    val freshStore = right(LocalEstimateStore.open(root))
    val source = right(freshStore.open(reference, ReadLimits(4)))
    try
      val effect = source.unit.products.find(_.kind == ProductKind.Effect).get
      val values = new Array[Double](4)
      val validity = new Array[Byte](4)
      right(source.read(effect.id, EstimateSelection(effect.observations, ids.reverse, Vector(1, 0)), values, validity))
      Vector(0.0, 5.0, -3.0, 2.0).zip(values).foreach: (expected, actual) =>
        assertEqualsDouble(actual, expected, 1e-10)
      assertEquals(validity.toVector, Vector.fill(4)(Validity.Valid.code))
      val standardError = source.unit.products.find(_.kind == ProductKind.StandardError).get
      right(source.read(standardError.id, EstimateSelection(standardError.observations, ids.reverse, Vector(1, 0)), values, validity))
      Vector(14.0 / 3.0, 7.0 / 6.0, 8.0 / 3.0, 2.0 / 3.0).zip(values).foreach: (variance, actual) =>
        assertEqualsDouble(actual, math.sqrt(variance), 1e-10)
      assertEquals(validity.toVector, Vector.fill(4)(Validity.Valid.code))
      val variance = source.unit.products.find(_.kind == ProductKind.ResidualVariance).get
      right(source.read(variance.id, EstimateSelection(variance.observations, ids.reverse, Vector(1, 0)), values, validity))
      Vector(4.0, 1.0, 4.0, 1.0).zip(values).foreach: (expected, actual) =>
        assertEqualsDouble(actual, expected, 1e-10)
      assertEquals(validity.toVector, Vector.fill(4)(Validity.Valid.code))
      assertEquals(source.unit.degreesOfFreedom, Vector(DegreesOfFreedom(DfRole.Residual, DfValue.Scalar(6.0), "OLS n - numerical rank", false)))
      assertEquals(source.unit.catalog.entries.map(_.response), Vector(
        ResponseCoordinate.FirInterval("event onset", 0.0, 2.0), ResponseCoordinate.FirInterval("event onset", 2.0, 4.0)))
      assertEquals(source.unit.bindings.map(_.weights), Vector(Vector(1.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0)))
      assertEquals(source.unit.products.map(_.pooling), Vector.fill(3)(PoolingScope.Run))
      assertEquals(source.unit.provenance.scans, Vector(AcquisitionScans(AcquisitionId("run-1"), Vector.range(0, 9))))
      assertEquals(source.unit.marginalUncertainty, Vector(MarginalUncertaintyDescriptor(standardError.id, effect.id,
        MarginalVarianceOrigin.Estimated(DegreesOfFreedom(DfRole.Residual, DfValue.Scalar(6.0), "OLS n - numerical rank", false)))))
    finally right(source.close())
  }

  test("joint OLS persists normalized cross-covariance and applies residual variance exactly once") {
    val fixture = ProducerFixture
    val producer = right(FitEstimateProducer.shared(fixture.prepared(scalafim.fmri.fit.EstimateUncertaintyRequest.Joint),
      fixture.identity, fixture.catalog, fixture.ids, "scanner"))
    val store = right(LocalEstimateStore.open(Files.createTempDirectory("scalafim-joint-estimates-")))
    val reference = right(producer.write(fixture.reader, right(store.newSink(producer.unit, 1))))
    val source = right(store.open(reference, ReadLimits(1)))
    try
      val covariance = source.unit.covariance.head
      assert(covariance.equation.isInstanceOf[CovarianceEquation.Normalized])
      val matrix = right(CovarianceAccess.matrix(source, covariance.product, fixture.identity.observation, 1,
        fixture.ids.reverse, CovariancePolicy(2, 1e-12, 1e-12)))
      // Independent closed form: X = [t,1], t=0..3; inverse(X'X)
      // = [[.2,-.3],[-.3,.7]]. Residual [2,-2,-2,2] has SSE/df=8.
      assertEqualsDouble(matrix.varianceScale, 8.0, 1e-12)
      assertEqualsDouble(matrix.values(0, 0), 5.6, 1e-12)
      assertEqualsDouble(matrix.values(0, 1), -2.4, 1e-12)
      assertEqualsDouble(matrix.values(1, 0), -2.4, 1e-12)
      assertEqualsDouble(matrix.values(1, 1), 1.6, 1e-12)
      assertEquals(matrix.estimands, fixture.ids.reverse)
      val values = new Array[Double](1)
      val validity = new Array[Byte](1)
      right(source.readCovariance(covariance.product, CovarianceSelection(Vector(fixture.identity.observation),
        Vector(EstimandPair(fixture.ids.head, fixture.ids.last)), Vector(0)), values, validity))
      assertEqualsDouble(values(0), -0.3, 1e-12)
      assertEquals(validity(0), Validity.Valid.code)
    finally right(source.close())
  }

  test("compact actual OLS reopens normalized U and scaled covariance across voxel block sizes") {
    val fixture = ProducerFixture
    for size <- Vector(1, 2) do
      val producer = right(FitEstimateProducer.shared(fixture.prepared(scalafim.fmri.fit.EstimateUncertaintyRequest.Joint, size),
        fixture.identity, fixture.catalog, fixture.ids, "scanner"))
      val store = right(LocalEstimateStore.open(Files.createTempDirectory("compact-fit-readback-")))
      val reference = right(producer.write(fixture.reader, right(store.newSink(producer.unit, size, CovarianceLayout.SharedNormalizedTable()))))
      val source = right(store.open(reference, ReadLimits(1)))
      try
        assertEquals(source.unit.bindings, producer.unit.bindings)
        assertEquals(source.unit.degreesOfFreedom, producer.unit.degreesOfFreedom)
        assertEquals(source.unit.outcomes, producer.unit.outcomes)
        val covariance = source.unit.covariance.head
        val matrix = right(CovarianceAccess.matrix(source, covariance.product, fixture.identity.observation, 1,
          fixture.ids.reverse, CovariancePolicy(2, 1e-12, 1e-12)))
        assertEqualsDouble(matrix.varianceScale, 8.0, 1e-12)
        assertEqualsDouble(matrix.values(0, 0), 5.6, 1e-12)
        assertEqualsDouble(matrix.values(0, 1), -2.4, 1e-12)
        assertEqualsDouble(matrix.values(1, 1), 1.6, 1e-12)
      finally right(source.close())
  }

  test("reopened pinned units feed bounded group blocks after explicit admission") {
    val fixture = ProducerFixture
    val store = right(LocalEstimateStore.open(Files.createTempDirectory("scalafim-pinned-group-")))
    val identities = Vector(fixture.identity, fixture.identity.copy(
      unit = UnitId("00000000-0000-4000-8000-000000000012"),
      revision = UnitRevisionId("00000000-0000-4000-8000-000000000013"), participantLabel = "02"))
    val inputs = identities.map: identity =>
      val producer = right(FitEstimateProducer.shared(fixture.prepared(scalafim.fmri.fit.EstimateUncertaintyRequest.Marginal),
        identity, fixture.catalog, fixture.ids, "scanner"))
      val ref = right(producer.write(fixture.reader, right(store.newSink(producer.unit, 1))))
      scalafim.fmri.group.GroupEstimateInput(ref, identity.observation,
        producer.unit.products.find(_.kind == ProductKind.Effect).get.id,
        Some(scalafim.fmri.group.GroupMarginalUncertainty.StandardError(producer.unit.products.find(_.kind == ProductKind.StandardError).get.id)))
    val admission = new scalafim.fmri.group.GroupEstimateAdmission:
      def verify(units: Vector[EstimateUnit]) =
        // These two fixtures were generated on this exact shared synthetic grid.
        if units.forall(u => u.domain.dimensions == Vector(2, 1, 1) && u.domain.worldFrame == "scanner") then
          Right(scalafim.fmri.group.GroupGeometryEvidence.Verified("scanner", "owned identical synthetic scanner grid", Vector.empty))
        else Left(EstimateError.Invalid("unexpected fixture geometry"))
    val group = right(scalafim.fmri.group.EstimateGroup.prepare(store, inputs, fixture.ids.reverse, admission, 16))
    val block = right(group.readBlock(Vector(1, 0)))
    assertEquals(block.inputs, inputs)
    val data = block.data
    assertEquals(data.nSubjects, 2)
    assertEqualsDouble(data.response("intercept").get.effects(1, 0), 5.0, 1e-12)
    assertEqualsDouble(data.response("task").get.effects(0, 1), 2.0, 1e-12)
    assertEqualsDouble(data.response("task").get.variances.get(0, 0), 1.6, 1e-12)
    val uncertainty = data.uncertainty.get
    assertEquals(uncertainty.geometry,
      scalafim.fmri.group.GroupGeometryEvidence.Verified("scanner", "owned identical synthetic scanner grid", Vector.empty))
    assertEquals(uncertainty.sources.map(_.samples).distinct, Vector(Vector(1, 0)))
    assert(uncertainty.sources.forall(_.origin ==
      scalafim.fmri.group.GroupVarianceOrigin.Estimated(
        scalafim.fmri.group.GroupDegreesOfFreedom(DfRole.Residual,
          scalafim.fmri.group.GroupDfValues.Scalar(2.0), "OLS n - numerical rank", false))))
    assert(uncertainty.sources.forall(_.fit.estimator ==
      ScientificFact.Known("shared ordinary least squares; compiled rank-revealing QR selected readout")))
    assert(group.readBlock(Vector(0, 0)).isLeft)
  }
