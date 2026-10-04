package scalafim.fmri.mvpa.analysis

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import munit.FunSuite
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import scalafim.fmri.mvpa.{AxisRef, EvidenceSource, Observations}
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class ObservationProductSuite extends FunSuite:
  private def right[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)

  private val samples = right(AxisRef.fromStableKeys("product-samples", SpaceRole.Samples, Vector("s0", "s1", "s2"), "native", "trial", "raw", Vector("fixture")))
  private val neural = right(AxisRef.fromStableKeys("product-neural", SpaceRole.Observed, Vector("n0", "n1"), "native", "psc", "raw", Vector("fixture")))
  private val values = DMat.dense(3, 2, Vector(1.0, 2.0, 3.0, 5.0, 7.0, 11.0))
  private val valueId = ValueIdentity.source(ValueId.unsafe("observation-product-values-v1"))
  private val source =
    val id = SourceId.unsafe("observation-product-source")
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe("observation-product-root"), id)))

  private final class Counted(value: Option[DMat]) extends DoubleLinearOperator:
    val rows = 3
    val cols = 2
    var calls = 0
    def applyTo(input: DVec, output: MutableDVec): Unit =
      calls += 1
      value.fold(throw IllegalStateException("poison source"))(_.applyTo(input, output))
    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
      calls += 1
      value.fold(throw IllegalStateException("poison source"))(_.transposeApplyTo(input, output))

  private final class Resource extends ObservationProductResource:
    var acquired = 0
    var closed = 0
    def acquire() =
      acquired += 1
      Right(())
    def close() =
      closed += 1
      Right(())

  private val replay = ObservationReplay.Scoped("fixture-provider", "rev-1")
  private val costs = ObservationProviderCosts(ResourceBound.Unknown("external matrix-free source"), ResourceBound.Unknown("provider scratch"))
  private val ownedBudget = ResourceBudget(ResourceLimit.OwnedNumeric(100000L), MaterializationPolicy.AllowSourceCopy(100000L))

  private def observations(operator: DoubleLinearOperator) =
    right(Observations.fromOperator(samples, neural, operator, valueId, source))

  private def matrix(table: Observations[?, ?]): DMat = right(table.patterns(DMat.eye(2)))

  test("direct and owned dense products preserve evidence identity and return the literal matrix"):
    val directOperator = new Counted(Some(values))
    val denseOperator = new Counted(Some(values))
    val directResource = new Resource
    val denseResource = new Resource
    val direct = ObservationProduct.withPrepared(samples, neural, observations(directOperator), replay, directResource, costs, ObservationProductRoute.Direct, ownedBudget, "same values; direct execution"): product =>
      assertEquals(product.observations.identity, observations(directOperator).identity)
      Right(matrix(product.observations) -> product.work)
    val dense = ObservationProduct.withPrepared(samples, neural, observations(denseOperator), replay, denseResource, costs, ObservationProductRoute.OwnedDenseCopy, ownedBudget, "same values; owned dense execution"): product =>
      Right(matrix(product.observations) -> product.work)
    val (directValues, directWork) = right(direct)
    val (denseValues, denseWork) = right(dense)
    for row <- 0 until 3; column <- 0 until 2 do
      assertEqualsDouble(directValues(row, column), values(row, column), 1e-12)
      assertEqualsDouble(denseValues(row, column), values(row, column), 1e-12)
    assert(directWork.forwardCalls > 0L)
    assertEquals(denseWork.materializationForwardColumns, 2L)
    assertEquals(denseWork.copiedCells, 6L)
    assertEquals(directResource.closed, 1)
    assertEquals(denseResource.closed, 1)

  test("strict unknown and one-shot replay refuse before resource acquisition or poison reads"):
    val poison = new Counted(None)
    val resource = new Resource
    val strict = ResourceBudget(ResourceLimit.WholeNumeric(100000L), MaterializationPolicy.AllowSourceCopy(100000L))
    val unknown = ObservationProduct.withPrepared(samples, neural, observations(poison), replay, resource, costs, ObservationProductRoute.Direct, strict, "strict direct")(_ => Right(()))
    assert(unknown.isLeft)
    assertEquals(resource.acquired, 0)
    assertEquals(poison.calls, 0)
    val oneShot = ObservationProduct.withPrepared(samples, neural, observations(poison), ObservationReplay.OneShot, resource, costs, ObservationProductRoute.Direct, ownedBudget, "one shot")(_ => Right(()))
    assertEquals(oneShot, Left(ObservationProductError.ReplayRequired))
    assertEquals(resource.acquired, 0)
    assertEquals(poison.calls, 0)

  test("operator attempts and successful cells are recorded and escaped products expire"):
    val operator = new Counted(Some(values))
    val resource = new Resource
    var escaped: Option[() => Either[?, DMat]] = None
    val result = ObservationProduct.withPrepared(samples, neural, observations(operator), replay, resource, costs, ObservationProductRoute.Direct, ownedBudget, "counter route"): product =>
      right(product.observations.patterns(DMat.eye(2)))
      right(product.observations.patterns.star(DMat.eye(3)))
      escaped = Some(() => product.observations.patterns(DMat.eye(2)))
      val work = product.work
      assertEquals(work.forwardColumns, 2L)
      assertEquals(work.adjointColumns, 3L)
      assertEquals(work.returnedCells, 12L)
      Right(())
    assertEquals(result, Right(()))
    intercept[IllegalStateException](escaped.get.apply())
    assertEquals(resource.closed, 1)

  test("bounded whole-population probe records both success and callback failure"):
    val plan = PlanId.derived(EstimandId("product-probe"), Vector.empty, scalafim.fmri.mvpa.AxisSignature.unsafe("0" * 64), scalafim.fmri.mvpa.AxisSignature.unsafe("1" * 64), "e", "d", "f", "q", Vector.empty, Vector.empty, "r", Vector.empty, Set.empty)
    val request = ExposureRequest(ExposurePurpose.PayloadRead, ExposureActorRole.Analyst, ExposureScope.WholePopulation, ExposurePayload.Payload, ExposureAssurance.Instrumented, 100L)
    val operator = new Counted(Some(values))
    val resource = new Resource
    val result = ObservationProduct.withPrepared(samples, neural, observations(operator), replay, resource, costs, ObservationProductRoute.Direct, ownedBudget, "probe route"): product =>
      val exposure = EvidenceExposure.external(product.exposureReference(plan, ResultIdentity("result")))
      val successful = ObservationProduct.probe(product, exposure, request)(p => p.observations.patterns(DMat.eye(2)).left.map(_.toString).map(_ => "ok"))
      assertEquals(successful.work.forwardColumns, 2L)
      assertEquals(successful.work.returnedCells, 6L)
      assert(successful.elapsedNanos >= 0L)
      successful.attempt match
        case ExposureAttempt.Completed(value, next) =>
          assertEquals(value, "ok")
          assertEquals(next.events.size, 1)
        case other => fail(s"expected probe success, got $other")
      ObservationProduct.probe(product, exposure, request)(_ => Left("provider failure")).attempt match
        case ExposureAttempt.Failed(_, next) => assertEquals(next.events.size, 1)
        case other => fail(s"expected probe failure, got $other")
      val beforeCalls = operator.calls
      ObservationProduct.probe(product, exposure, request.copy(maximumCells = 0))(p => p.observations.patterns(DMat.eye(2)).left.map(_.toString)).attempt match
        case ExposureAttempt.Failed(_, next) => assertEquals(next.events.size, 1)
        case other => fail(s"expected pre-read budget failure, got $other")
      assertEquals(operator.calls, beforeCalls)
      val unrelated = EvidenceExposure.internal(ExposureReference(plan, "different evidence", "different provenance", ResultIdentity("foreign")))
      assertEquals(ObservationProduct.probe(product, unrelated, request.copy(scope = ExposureScope.Training))(_ => Right(())).attempt,
        ExposureAttempt.Refused(ExposureError.ReferenceMismatch, unrelated))
      assertEquals(ObservationProduct.probe(product, exposure, request.copy(assurance = ExposureAssurance.Constrained))(_ => Right(())).attempt,
        ExposureAttempt.Refused(ExposureError.NativeAssuranceUnsupported, exposure))
      assertEquals(operator.calls, beforeCalls)

      ObservationProduct.probe(product, exposure, request.copy(scope = ExposureScope.Training))(_ => Right(())).attempt match
        case ExposureAttempt.Refused(ExposureError.UnknownFootprintForTrainingProbe, next) => assertEquals(next.events, Vector.empty)
        case other => fail(s"expected unknown training refusal, got $other")
      Right(())
    assertEquals(result, Right(()))

  test("failed provider reads retain partial work and marked fit limits stop before the next fit"):
    val plan = PlanId.derived(EstimandId("failed-probe"), Vector.empty, scalafim.fmri.mvpa.AxisSignature.unsafe("0" * 64), scalafim.fmri.mvpa.AxisSignature.unsafe("1" * 64), "e", "d", "f", "q", Vector.empty, Vector.empty, "r", Vector.empty, Set.empty)
    val request = ExposureRequest(ExposurePurpose.PayloadRead, ExposureActorRole.Analyst, ExposureScope.WholePopulation, ExposurePayload.Payload, ExposureAssurance.Instrumented, 100)
    val poison = new Counted(None)
    val reader = new Resource
    right(ObservationProduct.withPrepared(samples, neural, observations(poison), replay, reader, costs, ObservationProductRoute.Direct, ownedBudget, "failed read"): product =>
      val exposure = EvidenceExposure.external(product.exposureReference(plan, ResultIdentity("failed-result")))
      val failed = ObservationProduct.probe(product, exposure, request)(p => p.observations.patterns(DMat.eye(2)).left.map(_.toString))
      failed.attempt match
        case ExposureAttempt.Failed(_, next) => assertEquals(next.events.size, 1)
        case other => fail(other.toString)
      assertEquals(failed.work.forwardCalls, 1L)
      assertEquals(failed.work.attemptedCells, 3L)
      assertEquals(failed.work.returnedCells, 0L)
      assertEquals(poison.calls, 1)
      var fits = 0
      val limit = ObservationProduct.probe(product, exposure, request, maximumFits = 1): p =>
        p.fit:
          fits += 1
        p.fit:
          fits += 1
        Right(())
      limit.attempt match
        case ExposureAttempt.Failed(_, _) => ()
        case other => fail(other.toString)
      assertEquals(limit.work.fitCalls, 1L)
      assertEquals(limit.maximumFits, 1L)
      assertEquals(fits, 1)
      Right(()))
    assertEquals(reader.closed, 1)

  test("strict product costs require enclosing matrix output admission and one worker"):
    val operator = new Counted(Some(values))
    val strict = ResourceBudget(ResourceLimit.WholeNumeric(100000), MaterializationPolicy.ForbidSourceCopy)
    val known = ResourceBound.Known(48, "resident 3-by-2 matrix")
    val provider = ObservationProviderCosts(known, ResourceBound.Known(0, "no extra provider scratch"))
    val missingOutputs = ObservationProduct.preflight(observations(operator), replay, provider, ObservationProductRoute.Direct, strict, "output boundary")
    assert(missingOutputs.isLeft)
    val boundedOutputs = provider.copy(applicationBuffers = ResourceBound.Known(80, "enclosing width-two inputs/outputs"))
    val admitted = right(ObservationProduct.preflight(observations(operator), replay, boundedOutputs, ObservationProductRoute.Direct, strict, "output boundary"))
    assertEquals(admitted.ownedNumericBytes, 160L)
    assertEquals(admitted.wholeNumericBytes, Some(288L))
    assertEquals(ObservationProduct.preflight(observations(operator), replay, boundedOutputs.copy(workers = 2), ObservationProductRoute.Direct, strict, "workers"),
      Left(ObservationProductError.ConcurrentWorkersUnsupported(2)))
    assertEquals(operator.calls, 0)
