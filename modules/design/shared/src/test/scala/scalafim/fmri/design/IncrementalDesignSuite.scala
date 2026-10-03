package scalafim.fmri.design

import scalafim.fmri.design.data.*
import scalafim.fmri.design.formula.*
import scalafim.fmri.design.formula.EventModelBuilder.*
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

class IncrementalDesignSuite extends munit.FunSuite:
  private def request =
    val table = DataTable.fromColumns("onset" -> Column.Doubles(Vector(2.0, 8.0, 16.0)),
      "condition" -> Column.Strings(Vector("a", "b", "a")))
    EventDesignRequest.fromText("onset ~ hrf(condition, id = first) + hrf(condition, id = second)",
      table, SamplingFrame(blockLens = Seq(50), tr = Seq(2.0))).toOption.get

  test("one-term edit recompiles exactly one term and matches full compilation including identity"):
    val req = request
    val initial = prepareIncremental(req).toOption.get
    assertEquals(initial.recompiledTerms, 2)
    val edited = req.formula.terms(1).asInstanceOf[HrfCall].copy(lag = Some(2.0))
    val updated = initial.replaceTerm(1, edited).toOption.get
    assertEquals(updated.recompiledTerms, 1)
    assertEquals(updated.reusedTerms, 1)
    val full = buildEither(req.copy(formula = updated.formula)).toOption.get
    assertEquals(updated.model.designMatrix.data.toVector, full.designMatrix.data.toVector)
    assertEquals(updated.model.designSchema.fingerprint, full.designSchema.fingerprint)
    assertEquals(updated.model.designSchema.audit, full.designSchema.audit)

  test("exposed legacy matrix buffers cannot corrupt a later cache hit"):
    val initial = prepareIncremental(request).toOption.get
    val expected = initial.model.designMatrix.data.toVector
    initial.model.designMatrix.data(0) = 999.0
    initial.model.terms.head._2.data.data(0) = 999.0
    val rebuilt = initial.updateFormula(initial.formula).toOption.get
    assertEquals(rebuilt.recompiledTerms, 0)
    assertEquals(rebuilt.model.designMatrix.data.toVector, expected)
    assert(initial.replaceTerm(-1, initial.formula.terms.head).isLeft)

  test("unchanged context avoids repeated convolution and retains bounded cache"):
    var evaluations = 0
    val hrf = Hrf.of("counting", 1, 24.s): lag =>
      evaluations += 1
      Hrfs.SPMG1(lag)
    val req = request.copy(options = BuildOptions(defaultHrf = hrf))
    val initial = prepareIncremental(req).toOption.get
    val before = evaluations
    val same = initial.updateFormula(initial.formula).toOption.get
    assertEquals(evaluations, before)
    assertEquals(same.reusedTerms, 2)

  test("incremental and full build performance compare equivalent twelve-term designs"):
    val req0 = request
    val baseTerm = req0.formula.terms.head.asInstanceOf[HrfCall]
    val req = req0.copy(formula = req0.formula.copy(terms = Vector.tabulate(12)(i => baseTerm.copy(id = Some(TermId.unsafe(s"term$i"))))))
    val prepared = prepareIncremental(req).toOption.get
    val changed = baseTerm.copy(id = Some(TermId.unsafe("term0")), lag = Some(1.0))
    val formula = req.formula.copy(terms = req.formula.terms.updated(0, changed))
    (0 until 3).foreach: _ =>
      assert(prepared.replaceTerm(0, changed).isRight)
      assert(buildEither(req.copy(formula = formula)).isRight)
    val samples = Vector.fill(9):
      val begin = System.nanoTime()
      val partial = prepared.replaceTerm(0, changed).toOption.get
      val middle = System.nanoTime()
      val full = buildEither(req.copy(formula = formula)).toOption.get
      val end = System.nanoTime()
      assertEquals(partial.model.designMatrix.data.toVector, full.designMatrix.data.toVector)
      assertEquals(partial.recompiledTerms, 1)
      ((middle - begin).toDouble / 1e6, (end - middle).toDouble / 1e6)
    println(s"12-term build median_ms incremental=${samples.map(_._1).sorted.apply(4)} full=${samples.map(_._2).sorted.apply(4)}")

  private val richTable = DataTable.fromColumns(
    "onset" -> Column.Doubles(Vector(2.0, 8.0, 16.0, 24.0)),
    "late_onset" -> Column.Doubles(Vector(3.0, 9.0, 17.0, 25.0)),
    "condition" -> Column.Strings(Vector("a", "b", "a", "b")),
    "keep" -> Column.Bools(Vector(true, false, true, true)),
    "rt" -> Column.Doubles(Vector(0.4, 0.7, 0.5, 0.9)),
    "trial" -> Column.Doubles(Vector(1.0, 2.0, 3.0, 4.0))
  )
  private def richRequest(formula: String): EventDesignRequest =
    val motion = DataTable.fromColumns("x" -> Column.Doubles(Vector.tabulate(50)(i => math.sin(i / 7.0))))
    EventDesignRequest.fromText(formula, richTable, SamplingFrame(blockLens = Seq(50), tr = Seq(2.0)), tables = Map("motion" -> motion))
      .fold(error => fail(error.message), identity)

  private def assertMatchesFull(incremental: IncrementalDesign, request: EventDesignRequest): Unit =
    val full = buildEither(request.copy(formula = incremental.formula)).fold(error => fail(error.message), identity)
    assertEquals(incremental.model.designMatrix.data.toVector, full.designMatrix.data.toVector)
    assertEquals(incremental.model.columnNames, full.columnNames)
    assertEquals(incremental.model.termKeys, full.termKeys)
    assertEquals(incremental.model.designSchema.fingerprint, full.designSchema.fingerprint)
    assertEquals(incremental.model.designSchema.audit, full.designSchema.audit)

  test("trialwise, covariate and subset terms recompile incrementally exactly as a full build"):
    val req = richRequest("onset ~ trialwise(id = trial, subset = keep, label = probe) + covariate(x, data = motion, id = motion) + hrf(condition, subset = keep, id = kept)")
    val initial = prepareIncremental(req).fold(error => fail(error.message), identity)
    assertEquals(initial.recompiledTerms, 3)
    assertMatchesFull(initial, req)
    val edited = req.formula.terms(2).asInstanceOf[HrfCall].copy(lag = Some(1.0))
    val updated = initial.replaceTerm(2, edited).fold(error => fail(error.message), identity)
    assertEquals(updated.recompiledTerms, 1)
    assertEquals(updated.reusedTerms, 2)
    assertMatchesFull(updated, req)
    val trialwise = req.formula.terms(0).asInstanceOf[TrialwiseCall].copy(label = Some(TermId.unsafe("sample")))
    val relabelled = updated.replaceTerm(0, trialwise).fold(error => fail(error.message), identity)
    assertEquals(relabelled.recompiledTerms, 1)
    assertMatchesFull(relabelled, req)

  test("a TermCall repeated within one formula matches full compilation"):
    val req = richRequest("onset ~ hrf(condition) + hrf(condition)")
    val initial = prepareIncremental(req).fold(error => fail(error.message), identity)
    assertMatchesFull(initial, req)
    assertEquals(initial.model.termKeys.distinct.length, 2)
    val same = initial.updateFormula(initial.formula).fold(error => fail(error.message), identity)
    assertEquals(same.recompiledTerms, 0)
    assertMatchesFull(same, req)

  test("changing the onset column rebuilds every term and equals full recompilation"):
    val req = richRequest("onset ~ hrf(condition) + trialwise(id = trial)")
    val initial = prepareIncremental(req).fold(error => fail(error.message), identity)
    val moved = initial.formula.copy(onset = ColumnId.unsafe("late_onset"))
    val updated = initial.updateFormula(moved).fold(error => fail(error.message), identity)
    assertEquals(updated.recompiledTerms, 2)
    assertEquals(updated.reusedTerms, 0)
    assertMatchesFull(updated, req)
    assertNotEquals(updated.model.designMatrix.data.toVector, initial.model.designMatrix.data.toVector)
