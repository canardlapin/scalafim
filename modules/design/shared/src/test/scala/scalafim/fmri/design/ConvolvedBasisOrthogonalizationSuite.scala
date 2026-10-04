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

  private def dotColumns(data: scalafim.fmri.hrf.linalg.Mat, a: Int, b: Int): Double =
    (0 until data.rows).map(row => data(row, a) * data(row, b)).sum

  test("a collinear basis column is a typed error, not a silently zeroed column"):
    val term = EventTerm(Vector(Event.factor(Vector("task", "task"), "condition")), Vector(0.s, 7.s), termTag = Some("task"))
    val frame = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0))
    val raw = term.convolve(Hrfs.SPMG2, frame)
    val collinear = raw.copy(data = scalafim.fmri.hrf.linalg.Mat.unsafe(raw.data.rows, 2,
      Array.tabulate(raw.data.rows * 2)(i => if i % 2 == 0 then raw.data.data(i) else 3.0 * raw.data.data(i - 1))))
    val error = ConvolvedBasisOrthogonalization(collinear).left.toOption.getOrElse(fail("collinear columns must fail"))
    assert(error.message.contains("vanishes"), error.message)
    val tiny = raw.copy(data = scalafim.fmri.hrf.linalg.Mat.unsafe(raw.data.rows, 2,
      Array.tabulate(raw.data.rows * 2)(i => if i % 2 == 0 then raw.data.data(i) else raw.data.data(i - 1) * (1.0 + 1e-13 * (i % 3)))))
    assert(ConvolvedBasisOrthogonalization(tiny).isLeft, "a residual below the relative tolerance is degenerate")

  test("a cell whose events all fall outside the scan window is kept with rank 0 and an identity transform"):
    val term = EventTerm(Vector(Event.factor(Vector("a", "a", "b"), "condition")), Vector(0.s, 8.s, 200.s), termTag = Some("task"))
    val frame = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0))
    val raw = term.convolve(Hrfs.SPMG2, frame)
    val (orthogonal, receipts) = ConvolvedBasisOrthogonalization(raw).fold(error => fail(error.message), identity)
    val groups = receipts.head.groups
    assertEquals(groups.length, 2)
    val outside = groups.find(_.cell.exists(_.canonical.contains("b"))).getOrElse(fail("cell b receipt"))
    val inside = groups.find(_.cell.exists(_.canonical.contains("a"))).getOrElse(fail("cell a receipt"))
    assertEquals(outside.referenceRank, 0)
    assertEquals(outside.transform, Vector(1.0, 0.0, 0.0, 1.0))
    assertEquals(inside.referenceRank, 2)
    outside.columns.foreach(column => assert((0 until orthogonal.data.rows).forall(row => orthogonal.data(row, column) == 0.0)))

  test("FIR and B-spline groups with more than two columns are serially orthogonalized with full rank"):
    val term = EventTerm(Vector(Event.factor(Vector.fill(4)("task"), "condition")), Vector(0.s, 1.s, 3.s, 10.s), termTag = Some("task"))
    val frame = SamplingFrame(blockLens = Seq(40), tr = Seq(1.0))
    Vector(Hrfs.fir(4, 8.s), Hrfs.bspline(nBasis = 5, span = 12.s)).foreach: kernel =>
      val raw = term.convolve(kernel, frame)
      val p = kernel.nbasis
      assert((1 until p).exists(j => math.abs(dotColumns(raw.data, 0, j)) > 1e-6), "the source columns overlap")
      val (orthogonal, receipts) = ConvolvedBasisOrthogonalization(raw).fold(error => fail(error.message), identity)
      val group = receipts.head.groups.head
      assertEquals(group.columns.length, p)
      assertEquals(group.referenceRank, p)
      for a <- 0 until p; b <- a + 1 until p do
        val scale = math.sqrt(dotColumns(orthogonal.data, a, a) * dotColumns(orthogonal.data, b, b))
        assertEqualsDouble(dotColumns(orthogonal.data, a, b) / scale, 0.0, 1e-10, s"${kernel.name} columns $a,$b")
      // Serial: the first column is unchanged.
      (0 until raw.data.rows).foreach(row => assertEqualsDouble(orthogonal.data(row, 0), raw.data(row, 0), 0.0))

  test("a broadcast per-event HRF is a shared kernel and can be orthogonalized"):
    val term = EventTerm(Vector(Event.factor(Vector("task", "task"), "condition")), Vector(0.s, 6.s), termTag = Some("task"))
    val frame = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0))
    val broadcast = term.convolvePerEvent(Vector(Hrfs.SPMG2), frame)
    assert(broadcast.eventHrfs.isEmpty)
    assertEquals(broadcast.hrf.descriptor, Hrfs.SPMG2.descriptor)
    assert(ConvolvedBasisOrthogonalization(broadcast).isRight)

  test("group-specific effective HRFs are per column; the term HRF stays the source kernel"):
    val term = EventTerm(Vector(Event.factor(Vector("a", "b", "a", "b"), "condition")), Vector(0.s, 3.s, 11.s, 13.s), termTag = Some("task"))
    val frame = SamplingFrame(blockLens = Seq(40), tr = Seq(1.0))
    val raw = term.convolve(Hrfs.SPMG2, frame)
    val (orthogonal, receipts) = ConvolvedBasisOrthogonalization(raw).fold(error => fail(error.message), identity)
    assertEquals(receipts.head.groups.length, 2)
    assert(orthogonal.hrf eq raw.hrf)
    val byGroup = receipts.head.groups.map(group => orthogonal.hrfForColumn(group.columns.head))
    assert(byGroup.forall(_.descriptor != Hrfs.SPMG2.descriptor))
    val transforms = receipts.head.groups.map(_.transform)
    assertNotEquals(transforms(0), transforms(1))

  // Boundary-scale reproducers from the 2026-10-04 model-authoring seam review
  // (R2 norm underflow/overflow, R3 reference-SVD rank loss).
  private def convolveImpulse(kernel: Hrf): ConvolvedTerm =
    val term = EventTerm(Vector(Event.factor(Vector("task"), "condition")), Vector(Seconds(0.0)), termTag = Some("task"))
    term.convolve(kernel, SamplingFrame(blockLens = Seq(4), tr = Seq(1.0)))

  /** Pairwise cosine computed on columns pre-scaled by their maximum magnitude,
    * so the check itself neither underflows nor overflows. */
  private def scaledCosine(data: scalafim.fmri.hrf.linalg.Mat, a: Int, b: Int): Double =
    def column(c: Int) =
      val raw = (0 until data.rows).map(row => data(row, c))
      val scale = raw.map(math.abs).max
      raw.map(_ / scale)
    val left = column(a)
    val right = column(b)
    left.zip(right).map(_ * _).sum / math.sqrt(left.map(x => x * x).sum * right.map(x => x * x).sum)

  test("tiny nonzero collinear columns are refused rather than recorded as an exact-zero group"):
    val kernel = Hrf.multi("tiny-collinear", 2, span = Seconds(3.0))(_ => Array(1e-200, 3e-200))
    val raw = convolveImpulse(kernel)
    assert(raw.data.data.exists(_ != 0.0), "the design has finite nonzero entries")
    val error = ConvolvedBasisOrthogonalization(raw).left.toOption.getOrElse(fail("tiny collinear columns must be refused"))
    assert(error.message.contains("collinear"), error.message)

  test("tiny and huge non-collinear columns orthogonalize at full rank"):
    for amplitude <- Seq(1e-200, 1e-160, 1e160, 1e200) do
      val kernel = Hrf.multi(s"scaled-$amplitude", 2, span = Seconds(3.0)) { lag =>
        if lag.value < 1.0 then Array(amplitude, amplitude) else Array(0.0, 2.0 * amplitude)
      }
      val raw = convolveImpulse(kernel)
      val (orthogonal, receipts) = ConvolvedBasisOrthogonalization(raw).fold(error => fail(s"amplitude $amplitude: ${error.message}"), identity)
      assertEquals(receipts.head.groups.head.referenceRank, 2, s"amplitude $amplitude")
      assertEqualsDouble(scaledCosine(orthogonal.data, 0, 1), 0.0, 1e-12, s"amplitude $amplitude")

  test("huge collinear columns are refused rather than accepted through an overflowed norm"):
    val kernel = Hrf.multi("huge-collinear", 2, span = Seconds(3.0))(_ => Array(1e200, 3e200))
    val error = ConvolvedBasisOrthogonalization(convolveImpulse(kernel)).left.toOption.getOrElse(fail("huge collinear columns must be refused"))
    assert(error.message.contains("collinear"), error.message)

  test("a genuinely zero basis group keeps rank 0 and the identity transform"):
    val kernel = Hrf.multi("zero", 2, span = Seconds(3.0))(_ => Array(0.0, 0.0))
    val (changed, receipts) = ConvolvedBasisOrthogonalization(convolveImpulse(kernel)).fold(error => fail(error.message), identity)
    assertEquals(receipts.head.groups.head.referenceRank, 0)
    assertEquals(receipts.head.groups.head.transform, Vector(1.0, 0.0, 0.0, 1.0))
    assert(changed.data.data.forall(_ == 0.0))

  test("a tiny earlier reference is not truncated away: the result is orthogonal at full rank"):
    val kernel = Hrf.multi("unequal-scale", 3, span = Seconds(3.0)) { lag =>
      if lag.value < 1.0 then Array(1e-18, 0.0, 1.0)
      else if lag.value < 2.0 then Array(0.0, 1.0, 1.0)
      else Array(0.0, 0.0, 1.0)
    }
    val raw = convolveImpulse(kernel)
    val (orthogonal, receipts) = ConvolvedBasisOrthogonalization(raw).fold(error => fail(error.message), identity)
    assertEquals(receipts.head.groups.head.referenceRank, 3)
    for a <- 0 until 3; b <- a + 1 until 3 do
      assertEqualsDouble(scaledCosine(orthogonal.data, a, b), 0.0, 1e-12, s"columns $a,$b")
