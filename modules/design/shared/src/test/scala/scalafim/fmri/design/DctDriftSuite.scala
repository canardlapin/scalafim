package scalafim.fmri.design

import scalafim.fmri.design.baseline.*
import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

class DctDriftSuite extends munit.FunSuite:
  private def cutoff(seconds: Double): DctCutoffPeriod = DctCutoffPeriod.unsafeSeconds(seconds)
  private def drift(model: BaselineModel): BaselineTerm = model.terms.find(_._1 == "drift").get._2
  private def count(samples: Int, tr: Double, period: Double): Int =
    DctDrift.componentCount(samples, Seconds(tr), cutoff(period)).fold(e => fail(e.message), identity)

  test("cutoff counts include the endpoint and exclude the constant") {
    assertEquals(count(100, 2.0, 128.0), 3)
    assertEquals(count(10, 1.0, 10.0), 2)
    assertEquals(count(10, 1.0, 10.000001), 1)
    assertEquals(count(10, 1.0, 9.999999), 2)
    assertEquals(count(1, 1.0, 100.0), 0)
    assertEquals(count(10, 1.0, 100.0), 0)
    assertEquals(count(10, 1e300, 1e301), 2)
  }

  test("invalid periods and excessive finite-grid requests fail explicitly") {
    for value <- Vector(0.0, -1.0, Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity) do
      assert(DctCutoffPeriod.fromSeconds(value).isLeft)
    assert(DctDrift.componentCount(4, Seconds(1.0), cutoff(2.0)).isLeft)
    assert(DctDrift.componentCount(4, Seconds(1.0), cutoff(0.01)).isLeft)
    assert(DctDrift.componentCount(0, Seconds(1.0), cutoff(10.0)).isLeft)
    assert(DctDrift.componentCount(4, Seconds(-1.0), cutoff(10.0)).isLeft)
    val result = BaselineModel.buildEither(SamplingFrame(Seq(4), Seq(1.0)), basis = BaselineBasis.Dct(cutoff(2.0)))
    assert(result.left.toOption.exists(_.message.contains("twice TR")))
  }

  test("four-scan drift matches independent radical entries and a separate intercept") {
    val frame = SamplingFrame(Seq(4), Seq(1.0))
    val model = BaselineModel.build(frame, basis = BaselineBasis.Dct(cutoff(4.0)))
    val a = math.sqrt(2.0 + math.sqrt(2.0)) / math.sqrt(8.0)
    val b = math.sqrt(2.0 - math.sqrt(2.0)) / math.sqrt(8.0)
    val expected = Vector(Vector(a, 0.5, 1.0), Vector(b, -0.5, 1.0),
      Vector(-b, -0.5, 1.0), Vector(-a, 0.5, 1.0))
    assertEquals(model.designMatrix.cols, 3)
    for row <- 0 until 4; col <- 0 until 3 do
      assertEqualsDouble(model.designMatrix(row, col), expected(row)(col), 1e-15)
    assert(model.designSchemaValidation.isRight)
  }

  test("mixed scan counts and TRs have variable run-local column spans and stable identities") {
    val frame = SamplingFrame(Seq(4, 6, 9), Seq(1.0, 2.0, 0.5))
    val model = BaselineModel.build(frame, basis = BaselineBasis.Dct(cutoff(6.0)))
    val term = drift(model)
    assertEquals(term.colInd, Vector(Vector(0), Vector(1, 2, 3, 4), Vector(5)))
    assertEquals(term.rowInd.map(_.length), Vector(4, 6, 9))
    assertEquals(model.designMatrix.cols, 9)
    for block <- 0 until 3 do
      val local = term.designMatrix(Some(block))
      assertEquals(local.cols, Vector(1, 4, 1)(block))
      for col <- term.colInd(block); row <- 0 until 19 if !term.rowInd(block).contains(row) do
        assertEquals(term.data(row, col), 0.0)
    val schema = model.designSchema
    val receipts = schema.audit.policyReceipts.filter(_.name == "dct-drift")
    assertEquals(receipts.size, 3)
    assert(receipts(1).detail.contains("count=4"))
    val cutoffText = receipts(1).detail.split(";").find(_.startsWith("cutoff_seconds=")).get.stripPrefix("cutoff_seconds=")
    assertEquals(cutoffText.toDouble, 6.0)
    assertEquals(BaselineBasis.Dct(cutoff(6.0)).id, "dct_ii_period_bits_4618441417868443648")
    assertNotEquals(BaselineBasis.Dct(cutoff(6.0)).id, BaselineBasis.Dct(cutoff(6.000001)).id)
    assert(schema.columns.take(6).forall(_.origin match
      case StructuralColumnOrigin.Drift(_, Some(component), RunScope.Run(_)) => component.basisId == "dct_ii_period_bits_4618441417868443648"
      case _ => false
    ))
  }

  test("empty drift bases preserve rows and explicit runwise, global or absent intercepts") {
    val frame = SamplingFrame(Seq(1, 4), Seq(1.0))
    for (policy, expected) <- Vector(Intercept.Runwise -> 2, Intercept.Global -> 1, Intercept.None -> 0) do
      val model = BaselineModel.build(frame, basis = BaselineBasis.Dct(cutoff(100.0)), intercept = policy)
      assertEquals(model.designMatrix.rows, 5)
      assertEquals(model.designMatrix.cols, expected)
      assertEquals(drift(model).colInd, Vector(Vector.empty, Vector.empty))
      assertEquals(model.designMatrixFor(Some(1)).rows, 4)
  }

  test("censoring selects original scan positions and acquisition-time identity") {
    val frame = SamplingFrame(Seq(6, 6), Seq(1.0, 2.0), startTime = Seq(0.25, 0.75))
    val model = BaselineModel.build(frame, basis = BaselineBasis.Dct(cutoff(8.0)))
    val selected = Vector(1, 3, 5, 7, 10, 12).map(ScanIndex.unsafeOneBased)
    val sliced = model.designSchema.runwiseSlice(RunIndex.unsafeOneBased(2), selected).fold(e => fail(e.message), identity)
    assertEquals(sliced.sourceRowIndices, Vector(6, 9, 11))
    for row <- 0 until 3; col <- 0 until 3 do
      assertEqualsDouble(sliced.matrix(row, col), drift(model).designMatrix(Some(1))(Vector(0, 3, 5)(row), col), 1e-15)
    assertEquals(model.designSchema.rows.acquisitionTimes(6).value, 6.75)
    val shifted = BaselineModel.build(SamplingFrame(Seq(6, 6), Seq(1.0, 2.0), startTime = Seq(0.0)),
      basis = BaselineBasis.Dct(cutoff(8.0)))
    assertEquals(shifted.designMatrix.data.toVector, model.designMatrix.data.toVector)
    assertNotEquals(shifted.designSchema.rows.canonical, model.designSchema.rows.canonical)
  }

  test("supplied cosine confounds use the existing explicit alias policy") {
    val frame = SamplingFrame(Seq(8), Seq(1.0))
    val basis = BaselineBasis.Dct(cutoff(8.0))
    val native = BaselineModel.build(frame, basis = basis)
    val duplicate = Mat.fromRows((0 until 8).map(row => Vector(native.designMatrix(row, 0))))
    val refused = BaselineModel.buildEither(frame, basis = basis, nuisanceList = Some(Vector(duplicate)),
      nuisanceNames = Some(Vector(Vector("cosine00"))), nuisanceCheck = NuisanceCheck.Error)
    assert(refused.left.toOption.exists {
      case BaselineError.NuisanceProblems(report) => report.byBlock.head.aliasedColumns.contains("cosine00")
      case _ => false
    })
    val dropped = BaselineModel.build(frame, basis = basis, nuisanceList = Some(Vector(duplicate)),
      nuisanceNames = Some(Vector(Vector("cosine00"))), nuisanceCheck = NuisanceCheck.Drop)
    assertEquals(dropped.nuisanceReport.get.droppedByBlock, Vector(Vector("cosine00")))
    assertEquals(dropped.designMatrix.cols, native.designMatrix.cols)
  }

  test("the documented typed plan builds and legacy names remain unchanged") {
    val period = DctCutoffPeriod.fromSeconds(120.0).fold(error => fail(error.message), identity)
    val frame = SamplingFrame(blockLens = Seq(100, 80), tr = Seq(2.0, 1.5))
    val model = BaselineModel.buildEither(BaselinePlan(frame, basis = BaselineBasis.Dct(period), intercept = Intercept.Runwise))
      .fold(error => fail(error.message), identity)
    assertEquals(drift(model).colInd.map(_.size), Vector(3, 2))
    assertEquals(model.designMatrix.cols, 7)
    assertEquals(BaselineSpec(basis = BaselineBasis.Poly, degree = 2).name, "baseline_poly_2")
    assertEquals(BaselineBasis.parse("poly"), BaselineBasis.Poly)
  }
