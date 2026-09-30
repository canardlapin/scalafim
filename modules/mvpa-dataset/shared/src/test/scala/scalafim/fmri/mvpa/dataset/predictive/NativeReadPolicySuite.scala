package scalafim.fmri.mvpa.dataset.predictive

import alder.kernel.DataFingerprint
import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import munit.FunSuite
import scalafim.fmri.mvpa.{AxisRef, EvidenceSource, MultiResponse, Observations}
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class NativeReadPolicySuite extends FunSuite:
  private def right[A](value: Either[?, A]): A =
    value.fold(error => fail(error.toString), identity)

  private final class OneShotColumns(values: DMat, fail: Boolean = false, failAtColumn: Option[Int] = None) extends DoubleLinearOperator:
    val rows = values.rows
    val cols = values.cols
    var calls = 0
    private val read = Array.fill(cols)(false)

    def applyTo(input: DVec, output: MutableDVec): Unit =
      calls += 1
      var selected = -1
      var index = 0
      while index < input.length do
        if input(index) != 0.0 then
          if input(index) != 1.0 || selected >= 0 then throw IllegalArgumentException("expected one basis column")
          selected = index
        index += 1
      if selected < 0 || read(selected) then throw IllegalStateException("one-shot column reread")
      if fail || failAtColumn.contains(selected) then throw IllegalStateException("native read failed")
      read(selected) = true
      var row = 0
      while row < rows do
        output(row) = values(row, selected)
        row += 1

  private def source(name: String, provenanceRoot: Option[String] = None): EvidenceSource =
    val sourceId = SourceId.unsafe(name)
    right(EvidenceSource(sourceId, Provenance.source(ProvenanceId.unsafe(s"${provenanceRoot.getOrElse(name)}-root"), sourceId)))

  private final class Fixture[SK, NK, FK](
      val samples: AxisRef[SK],
      val inputs: AxisRef[NK],
      val responses: AxisRef[FK],
      val observations: Observations[samples.Id, inputs.Id],
      val targets: MultiResponse[samples.Id, responses.Id],
      val mapping: NativeAxisMapping,
      val inputOperator: OneShotColumns,
      val targetOperator: OneShotColumns
  )

  private def tables(
      inputValues: DMat = DMat.dense(3, 3, Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)),
      targetValues: DMat = DMat.dense(3, 2, Vector(10.0, 11.0, 12.0, 13.0, 14.0, 15.0)),
      inputSource: String = "input-source",
      targetSource: String = "target-source",
      inputValue: String = "input-values",
      targetValue: String = "target-values",
      failInput: Boolean = false,
      failInputAt: Option[Int] = None,
      failTarget: Boolean = false,
      inputProvenance: Option[String] = None,
      targetProvenance: Option[String] = None,
      sampleKeys: Vector[String] = Vector("a", "b", "c")
  ) =
    val samples = right(AxisRef.fromStableKeys("samples", SpaceRole.Samples, sampleKeys, "trial", "none", "one"))
    val inputs = right(AxisRef.fromStableKeys("inputs", SpaceRole.Observed, Vector("i1", "i2", "i3"), "native", "none", "one"))
    val responses = right(AxisRef.fromStableKeys("responses", SpaceRole.Observed, Vector("r1", "r2"), "native", "none", "one"))
    val inputOperator = new OneShotColumns(inputValues, failInput, failInputAt)
    val targetOperator = new OneShotColumns(targetValues, failTarget)
    val observations = right(Observations.fromOperator(samples, inputs, inputOperator, ValueIdentity.source(ValueId.unsafe(inputValue)), source(inputSource, inputProvenance)))
    val targets = right(MultiResponse.fromOperator(samples, responses, targetOperator, ValueIdentity.source(ValueId.unsafe(targetValue)), source(targetSource, targetProvenance)))
    val mapping = right(NativeAxisMapping.fromAxis(samples, Vector(30L, 10L, 20L), DataFingerprint.external("declared-native-source")))
    new Fixture(samples, inputs, responses, observations, targets, mapping, inputOperator, targetOperator)

  private def allRows(rows: AlderMaterializedRows[String]) =
    right(rows.root.training(rows.mapping.nativeIds)).data.foldRows(Vector.empty[(Long, Vector[Double], Vector[Double], String)]): (out, id, example) =>
      out :+ (id.value, example.input.toVector, example.target.toVector, example.meta)

  test("rectangular batches retain all values and read every native column once"):
    val fixture = tables()
    val policy = right(NativeReadPolicy(2, right(MaterializationBudget(80L)), right(NativeReadAccess.OwnedReplay("suite-owned-fixture"))))
    val admitted = right(AlderPredictiveAdmission.nativeTables(fixture.observations, fixture.targets, Vector("a", "b", "c"), DataFingerprint.external("metadata-v1"), fixture.mapping, policy))

    assertEquals(allRows(admitted), Vector(
      (30L, Vector(1.0, 2.0, 3.0), Vector(10.0, 11.0), "a"),
      (10L, Vector(4.0, 5.0, 6.0), Vector(12.0, 13.0), "b"),
      (20L, Vector(7.0, 8.0, 9.0), Vector(14.0, 15.0), "c")
    ))
    assertEquals(fixture.inputOperator.calls, 3)
    assertEquals(fixture.targetOperator.calls, 2)
    val receipt = admitted.nativeReadReceipt.getOrElse(fail("missing native read receipt"))
    assertEquals(receipt.inputBlockCalls, 2)
    assertEquals(receipt.targetBlockCalls, 1)
    assertEquals(receipt.inputReturnedCells, 9L)
    assertEquals(receipt.targetReturnedCells, 6L)
    assertEquals(receipt.inputCopiedCells, 9L)
    assertEquals(receipt.targetCopiedCells, 6L)

  test("metadata, axis, provenance, and value identities bind the training fingerprint"):
    val policy = right(NativeReadPolicy(2, right(MaterializationBudget(80L)), right(NativeReadAccess.OwnedReplay("suite-identity-fixture"))))
    def fingerprint(inputSource: String, targetSource: String, inputValue: String, targetValue: String, metadata: String = "metadata-v1", inputProvenance: Option[String] = None, sampleKeys: Vector[String] = Vector("a", "b", "c")): String =
      val fixture = tables(inputSource = inputSource, targetSource = targetSource, inputValue = inputValue, targetValue = targetValue, inputProvenance = inputProvenance, sampleKeys = sampleKeys)
      right(AlderPredictiveAdmission.nativeTables(fixture.observations, fixture.targets, Vector("a", "b", "c"), DataFingerprint.external(metadata), fixture.mapping, policy)).root.fingerprint.digest
    val baseline = fingerprint("input-source", "target-source", "input-values", "target-values")
    assertNotEquals(fingerprint("other-input-source", "target-source", "input-values", "target-values"), baseline)
    assertNotEquals(fingerprint("input-source", "other-target-source", "input-values", "target-values"), baseline)
    assertNotEquals(fingerprint("input-source", "target-source", "other-input-values", "target-values"), baseline)
    assertNotEquals(fingerprint("input-source", "target-source", "input-values", "other-target-values"), baseline)
    assertNotEquals(fingerprint("input-source", "target-source", "input-values", "target-values", metadata = "metadata-v2"), baseline)
    assertNotEquals(fingerprint("input-source", "target-source", "input-values", "target-values", inputProvenance = Some("other-input-provenance")), baseline)
    assertNotEquals(fingerprint("input-source", "target-source", "input-values", "target-values", sampleKeys = Vector("a", "b", "changed")), baseline)

  test("preflight refusal performs zero reads and large dimensions overflow safely"):
    val fixture = tables()
    val refused = right(NativeReadPolicy(2, right(MaterializationBudget(20L)), right(NativeReadAccess.OwnedReplay("suite-budget-fixture"))))
    assert(AlderPredictiveAdmission.nativeTables(fixture.observations, fixture.targets, Vector("a", "b", "c"), DataFingerprint.external("metadata-v1"), fixture.mapping, refused).isLeft)
    assertEquals(fixture.inputOperator.calls, 0)
    assertEquals(fixture.targetOperator.calls, 0)
    assert(MaterializationBudget.authorizeNative(right(MaterializationBudget(Long.MaxValue)), Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue).isLeft)
    assert(MaterializationBudget.authorizeNative(right(MaterializationBudget(Long.MaxValue)), Int.MaxValue, Int.MaxValue, Int.MaxValue, 1).isLeft)

  test("poison single-application sources reject replay before callbacks, but one full-width application is admitted"):
    val blocked = tables(failInput = true)
    val requiresReplay = right(NativeReadPolicy(2, right(MaterializationBudget(80L))))
    assertEquals(
      AlderPredictiveAdmission.nativeTables(blocked.observations, blocked.targets, Vector("a", "b", "c"), DataFingerprint.external("metadata-v1"), blocked.mapping, requiresReplay),
      Left(AlderPredictiveAdmissionError.ReplayRequired(2, 1))
    )
    assertEquals(blocked.inputOperator.calls, 0)
    assertEquals(blocked.targetOperator.calls, 0)

    val fixture = tables()
    val fullWidth = right(NativeReadPolicy(3, right(MaterializationBudget(80L))))
    val admitted = right(AlderPredictiveAdmission.nativeTables(fixture.observations, fixture.targets, Vector("a", "b", "c"), DataFingerprint.external("metadata-v1"), fixture.mapping, fullWidth))
    assertEquals(admitted.nativeReadReceipt.getOrElse(fail("missing receipt")).inputBlockCalls, 1)
    assertEquals(admitted.nativeReadReceipt.getOrElse(fail("missing receipt")).targetBlockCalls, 1)
    assertEquals(fixture.inputOperator.calls, 3)
    assertEquals(fixture.targetOperator.calls, 2)

  test("failed blocks preserve prior returned and copied cell totals"):
    val fixture = tables(failInputAt = Some(2))
    val policy = right(NativeReadPolicy(2, right(MaterializationBudget(80L)), right(NativeReadAccess.OwnedReplay("suite-failure-fixture"))))
    AlderPredictiveAdmission.nativeTables(fixture.observations, fixture.targets, Vector("a", "b", "c"), DataFingerprint.external("metadata-v1"), fixture.mapping, policy) match
      case Left(AlderPredictiveAdmissionError.NativeReadFailure("inputs", _, receipt)) =>
        assertEquals(receipt.inputBlockCalls, 2)
        assertEquals(receipt.targetBlockCalls, 0)
        assertEquals(receipt.inputReturnedCells, 6L)
        assertEquals(receipt.inputCopiedCells, 6L)
      case other => fail(s"expected failed native input receipt, got $other")
    assertEquals(fixture.inputOperator.calls, 3)
    assertEquals(fixture.targetOperator.calls, 0)

    val targetFixture = tables(failTarget = true)
    AlderPredictiveAdmission.nativeTables(targetFixture.observations, targetFixture.targets, Vector("a", "b", "c"), DataFingerprint.external("metadata-v1"), targetFixture.mapping, policy) match
      case Left(AlderPredictiveAdmissionError.NativeReadFailure("targets", _, receipt)) =>
        assertEquals(receipt.inputBlockCalls, 2)
        assertEquals(receipt.inputReturnedCells, 9L)
        assertEquals(receipt.inputCopiedCells, 9L)
        assertEquals(receipt.targetBlockCalls, 1)
        assertEquals(receipt.targetReturnedCells, 0L)
        assertEquals(receipt.targetCopiedCells, 0L)
      case other => fail(s"expected failed native target receipt, got $other")
    assertEquals(targetFixture.inputOperator.calls, 3)
    assertEquals(targetFixture.targetOperator.calls, 1)
