package scalafim.fmri.design

import scalafim.fmri.design.event.*
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

class ConvolvedBasisOrthogonalizationSuite extends munit.FunSuite:
  test("per-event HRF assignments cannot claim a shared transformed response"):
    val term = EventTerm(Vector(Event.factor(Vector("task", "task"), "condition")), Vector(0.s, 4.s))
    val frame = SamplingFrame(blockLens = Seq(16), tr = Seq(1.0))
    val assigned = term.convolvePerEvent(Vector(Hrfs.SPMG2, Hrfs.fir(2, 8.s)), frame)
    assertEquals(assigned.eventHrfs.length, 2)
    assert(ConvolvedBasisOrthogonalization(assigned).left.toOption.exists(_.message.contains("per-event HRFs")))

  test("receipts distinguish original scaling policies with equal divisors"):
    val term = EventTerm(Vector(Event.factor(Vector("task"), "condition")), Vector(0.s))
    val frame = SamplingFrame(blockLens = Seq(12), tr = Seq(1.0))
    val raw = term.convolve(Hrfs.SPMG2, frame)
    val declared = raw.copy(columnScales = Vector.fill(2)(HrfColumnScale.applied(HrfColumnScaling.UnitMaximumAbsolute, 1.0)))
    val rawReceipt = ConvolvedBasisOrthogonalization(raw).toOption.get._2.head
    val declaredReceipt = ConvolvedBasisOrthogonalization(declared).toOption.get._2.head
    assertEquals(rawReceipt.groups.head.originalDivisors, declaredReceipt.groups.head.originalDivisors)
    assertNotEquals(rawReceipt.canonical, declaredReceipt.canonical)

  test("global basis transform records scaled serial projection and effective HRFs"):
    val term = EventTerm(Vector(Event.factor(Vector("task", "task", "task", "task"), "condition")), Vector(0.s, 4.s, 0.s, 6.s), blockIds = Vector(0, 0, 1, 1), termTag = Some("task"))
    val frame = SamplingFrame(blockLens = Seq(12, 9), tr = Seq(1.0, 1.0))
    val raw = term.convolve(Hrfs.SPMG2, frame)
    val divided = scalafim.fmri.hrf.linalg.Mat.unsafe(raw.data.rows, 2, raw.data.data.zipWithIndex.map((value, index) => value / (if index % 2 == 0 then 2.0 else 3.0)))
    val scaled = raw.copy(data = divided, columnScales = Vector(HrfColumnScale.applied(HrfColumnScaling.UnitMaximumAbsolute, 2.0), HrfColumnScale.applied(HrfColumnScaling.UnitMaximumAbsolute, 3.0)))
    val result = ConvolvedBasisOrthogonalization(scaled).fold(error => fail(error.message), identity)
    val (orthogonal, receipts) = result
    assertEquals(receipts.length, 1)
    val group = receipts.head.groups.head
    assertEquals(group.scope, "all-selected-scans")
    assertEquals(group.originalDivisors, Vector(2.0, 3.0))
    assertEquals(group.sourceRows, (0 until scaled.data.rows).toVector)
    assert(group.transform(1) != 0.0)
    assertEquals(orthogonal.columnScales, Vector(HrfColumnScale.identity, HrfColumnScale.identity))
    assert(orthogonal.columnHrfs.forall(_.descriptor != Hrfs.SPMG2.descriptor))
    for row <- 0 until raw.data.rows; column <- 0 until 2 do
      val expected = (0 until 2).map(k => scaled.data(row, k) * group.transform(k * 2 + column)).sum
      assertEqualsDouble(orthogonal.data(row, column), expected, 1e-12)
    val native = Hrfs.SPMG2(Lag(5.0)).data
    val effective = orthogonal.hrfForColumn(0)(Lag(5.0)).data
    for column <- 0 until 2 do
      val expected = (0 until 2).map(k => native(k) / group.originalDivisors(k) * group.transform(k * 2 + column)).sum
      assertEqualsDouble(effective(column), expected, 1e-12)
    val dot = (0 until orthogonal.data.rows).map(row => orthogonal.data(row, 0) * orthogonal.data(row, 1)).sum
    assertEqualsDouble(dot, 0.0, 1e-12)
