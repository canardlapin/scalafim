package scalafim.fmri.fit.estimates

import java.nio.file.Files
import scalafim.estimates.*
import scalafim.estimates.io.LocalEstimateStore

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
      assertEquals(reader.unit.bindings, producer.unit.bindings)
    finally right(reader.close())
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
        if units.forall(u => u.domain.dimensions == Vector(2, 1, 1) && u.domain.worldFrame == "scanner") then Right(())
        else Left(EstimateError.Invalid("unexpected fixture geometry"))
    val group = right(scalafim.fmri.group.EstimateGroup.prepare(store, inputs, fixture.ids.reverse, admission, 16))
    val block = right(group.readBlock(Vector(1, 0)))
    assertEquals(block.inputs, inputs)
    val data = block.data
    assertEquals(data.nSubjects, 2)
    assertEqualsDouble(data.response("intercept").get.effects(1, 0), 5.0, 1e-12)
    assertEqualsDouble(data.response("task").get.effects(0, 1), 2.0, 1e-12)
    assertEqualsDouble(data.response("task").get.variances.get(0, 0), 1.6, 1e-12)
    assert(group.readBlock(Vector(0, 0)).isLeft)
  }
