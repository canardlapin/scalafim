package scalafim.fmri.design

import scalafim.fmri.design.baseline.*
import scalafim.fmri.design.basis.ParametricBasis
import scalafim.fmri.design.linalg.QrDecomposition
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

class BaselineModelSuite extends munit.FunSuite:

  private def nuisanceTerm(model: BaselineModel): BaselineTerm =
    model.terms.collectFirst { case ("nuisance", term) => term }.get

  private def rank(mat: Mat): Int =
    QrDecomposition.decompose(
      mat.data,
      rows = mat.rows,
      cols = mat.cols,
      pivoting = true,
      tol = BaselineModel.DefaultNuisanceTol
    ).rank

  test("baseline_model: 1 block, bs degree=5") {
    val sf = SamplingFrame(blockLens = Seq(100), tr = Seq(2.0))
    val bm = BaselineModel.build(samplingFrame = sf, basis = BaselineBasis.Bs, degree = 5)

    assertEquals(bm.designMatrix.rows, 100)
    assertEquals(bm.designMatrix.cols, 6) // 5 drift + 1 intercept
    assertEquals(bm.termKeys, Vector("drift", "block"))
  }

  test("baseline_model: 1 block, ns df=5") {
    val sf = SamplingFrame(blockLens = Seq(100), tr = Seq(2.0))
    val bm = BaselineModel.build(samplingFrame = sf, basis = BaselineBasis.Ns, degree = 5)

    assertEquals(bm.designMatrix.rows, 100)
    assertEquals(bm.designMatrix.cols, 6) // 5 drift + 1 intercept
    assertEquals(bm.termKeys, Vector("drift", "block"))
  }

  test("baseline_model: 1 block, bs degree=5 with nuisance list") {
    val sf = SamplingFrame(blockLens = Seq(100), tr = Seq(2.0))
    val nuisX = sf.samples(global = false).map(_.value)
    val nuis = ParametricBasis.Poly.fit(nuisX, degree = 3, argName = "n").y

    val bm = BaselineModel.build(
      samplingFrame = sf,
      basis = BaselineBasis.Bs,
      degree = 5,
      nuisanceList = Some(Seq(nuis))
    )

    assertEquals(bm.designMatrix.rows, 100)
    assertEquals(bm.designMatrix.cols, 9) // 5 drift + 1 intercept + 3 nuisance
    assertEquals(bm.termKeys, Vector("drift", "block", "nuisance"))
  }

  test("baseline_model: 2 blocks, bs degree=5") {
    val sf = SamplingFrame(blockLens = Seq(100, 100), tr = Seq(2.0))
    val bm = BaselineModel.build(samplingFrame = sf, basis = BaselineBasis.Bs, degree = 5)

    assertEquals(bm.designMatrix.rows, 200)
    assertEquals(bm.designMatrix.cols, 12) // (5*2 drift) + (2 runwise intercept)
    assertEquals(bm.termKeys, Vector("drift", "block"))
  }

  test("baseline_model: 2 blocks, global intercept") {
    val sf = SamplingFrame(blockLens = Seq(100, 100), tr = Seq(2.0))
    val bm = BaselineModel.build(
      samplingFrame = sf,
      basis = BaselineBasis.Poly,
      degree = 3,
      intercept = Intercept.Global
    )

    assertEquals(bm.designMatrix.cols, 3 * 2 + 1)
    assertEquals(bm.termKeys, Vector("drift", "block"))
  }

  test("baseline_model: 2 blocks with nuisance list") {
    val sf = SamplingFrame(blockLens = Seq(100, 100), tr = Seq(2.0))
    val n1 = Mat.unsafe(100, 2, Array.fill(100 * 2)(0.0))
    val n2 = Mat.unsafe(100, 3, Array.fill(100 * 3)(0.0))

    val bm = BaselineModel.build(
      samplingFrame = sf,
      basis = BaselineBasis.Bs,
      degree = 5,
      nuisanceList = Some(Seq(n1, n2))
    )

    assertEquals(bm.designMatrix.cols, 17) // (5*2 drift) + (2 runwise intercept) + (2+3 nuisance)
    assertEquals(bm.termKeys.toSet, Set("drift", "block", "nuisance"))
  }

  test("baseline_model: basis='constant'") {
    val sf = SamplingFrame(blockLens = Seq(100, 100), tr = Seq(2.0))

    val bmRunwise = BaselineModel.build(samplingFrame = sf, basis = BaselineBasis.Constant, intercept = Intercept.Runwise)
    assertEquals(bmRunwise.designMatrix.cols, 2)
    assertEquals(bmRunwise.termKeys, Vector("drift"))

    val bmGlobal = BaselineModel.build(samplingFrame = sf, basis = BaselineBasis.Constant, intercept = Intercept.Global)
    assertEquals(bmGlobal.designMatrix.cols, 1)
    assertEquals(bmGlobal.termKeys, Vector("drift"))

    val bmNone = BaselineModel.build(samplingFrame = sf, basis = BaselineBasis.Constant, intercept = Intercept.None)
    assertEquals(bmNone.designMatrix.cols, 2)
    assertEquals(bmNone.termKeys, Vector("drift"))

    // blockId subsetting should return per-block rows.
    val dmBlock1 = bmGlobal.designMatrixFor(blockId = Some(1), allRows = false)
    assertEquals(dmBlock1.rows, 100)
  }

  test("checkNuisance reports zero-variance, duplicates, and baseline aliasing") {
    val sf = SamplingFrame(blockLens = Seq(6, 6), tr = Seq(1.0))
    val n1 = Mat.unsafe(
      6,
      3,
      Array(
        1.0, 10.0, 0.0,
        2.0, 20.0, 0.0,
        3.0, 30.0, 0.0,
        4.0, 40.0, 0.0,
        5.0, 50.0, 0.0,
        6.0, 60.0, 0.0
      )
    )
    val n2 = Mat.unsafe(
      6,
      2,
      Array(
        -2.0, 1.0,
        -1.0, -1.0,
        0.0, 1.0,
        1.0, -1.0,
        2.0, 1.0,
        3.0, -1.0
      )
    )

    val report = BaselineModel.checkNuisance(
      Seq(n1, n2),
      sf,
      basis = BaselineBasis.Constant,
      nuisanceNames = Some(Seq(Seq("dvars", "std_dvars", "zero_col"), Seq("motion_x", "motion_y")))
    )

    assert(!report.ok)
    assert(report.problems.exists(_.issue == NuisanceIssue.ZeroVariance))
    assert(report.problems.exists(_.issue == NuisanceIssue.Duplicate))
    assert(report.problems.exists(_.issue == NuisanceIssue.RankDeficientWithBaseline))
    assertEquals(report.byBlock(0).zeroVariance, Vector("zero_col"))
    assertEquals(report.byBlock(0).aliasedColumns, Vector("std_dvars"))
  }

  test("baseline_model default warning policy records report and keeps nuisance columns") {
    val sf = SamplingFrame(blockLens = Seq(6), tr = Seq(1.0))
    val nuis = Mat.unsafe(
      6,
      3,
      Array(
        1.0, 10.0, 0.0,
        2.0, 20.0, 0.0,
        3.0, 30.0, 0.0,
        4.0, 40.0, 0.0,
        5.0, 50.0, 0.0,
        6.0, 60.0, 0.0
      )
    )

    val model = BaselineModel.build(
      samplingFrame = sf,
      basis = BaselineBasis.Constant,
      nuisanceList = Some(Seq(nuis)),
      nuisanceNames = Some(Seq(Seq("dvars", "std_dvars", "zero_col")))
    )

    assertEquals(model.nuisanceReport.exists(!_.ok), true)
    assertEquals(nuisanceTerm(model).data.cols, 3)
    assertEquals(model.designMatrix.cols, 4)
  }

  test("baseline_model can error on nuisance rank problems") {
    val sf = SamplingFrame(blockLens = Seq(6), tr = Seq(1.0))
    val nuis = Mat.unsafe(
      6,
      2,
      Array(
        1.0, 10.0,
        2.0, 20.0,
        3.0, 30.0,
        4.0, 40.0,
        5.0, 50.0,
        6.0, 60.0
      )
    )

    val err = intercept[IllegalArgumentException] {
      BaselineModel.build(
        samplingFrame = sf,
        basis = BaselineBasis.Constant,
        nuisanceList = Some(Seq(nuis)),
        nuisanceCheck = NuisanceCheck.Error,
        nuisanceNames = Some(Seq(Seq("dvars", "std_dvars")))
      )
    }

    assert(err.getMessage.contains("Duplicate or near-duplicate columns"))
  }

  test("baseline_model can drop nuisance columns that do not increase rank") {
    val sf = SamplingFrame(blockLens = Seq(6, 6), tr = Seq(1.0))
    val n1 = Mat.unsafe(
      6,
      3,
      Array(
        1.0, 10.0, 0.0,
        2.0, 20.0, 0.0,
        3.0, 30.0, 0.0,
        4.0, 40.0, 0.0,
        5.0, 50.0, 0.0,
        6.0, 60.0, 0.0
      )
    )
    val n2 = Mat.unsafe(
      6,
      2,
      Array(
        -2.0, 1.0,
        -1.0, -1.0,
        0.0, 1.0,
        1.0, -1.0,
        2.0, 1.0,
        3.0, -1.0
      )
    )

    val model = BaselineModel.build(
      samplingFrame = sf,
      basis = BaselineBasis.Constant,
      nuisanceList = Some(Seq(n1, n2)),
      nuisanceCheck = NuisanceCheck.Drop,
      nuisanceNames = Some(Seq(Seq("dvars", "std_dvars", "zero_col"), Seq("motion_x", "motion_y")))
    )

    assertEquals(nuisanceTerm(model).data.cols, 3)
    assertEquals(model.nuisanceReport.map(_.droppedByBlock).get, Vector(Vector("std_dvars", "zero_col"), Vector.empty))
    assertEquals(rank(model.designMatrix), model.designMatrix.cols)
  }

  test("cleanNuisance returns cleaned matrices and an audit report") {
    val sf = SamplingFrame(blockLens = Seq(6), tr = Seq(1.0))
    val nuis = Mat.unsafe(
      6,
      3,
      Array(
        1.0, 10.0, 0.0,
        2.0, 20.0, 0.0,
        3.0, 30.0, 0.0,
        4.0, 40.0, 0.0,
        5.0, 50.0, 0.0,
        6.0, 60.0, 0.0
      )
    )

    val cleaned = BaselineModel.cleanNuisance(
      Seq(nuis),
      sf,
      basis = BaselineBasis.Constant,
      nuisanceNames = Some(Seq(Seq("dvars", "std_dvars", "zero_col")))
    )

    assertEquals(cleaned.nuisanceList.head.cols, 1)
    assertEquals(cleaned.report.retainedByBlock, Vector(Vector("dvars")))
  }

  test("naAction zero retains a leading missing confound that default drop removes") {
    val sf = SamplingFrame(blockLens = Seq(6), tr = Seq(1.0))
    val nuis = Mat.unsafe(
      6,
      2,
      Array(
        Double.NaN, -2.0,
        2.0, -1.0,
        3.0, 0.0,
        4.0, 1.0,
        5.0, 2.0,
        6.0, 3.0
      )
    )

    val dropped = BaselineModel.build(
      samplingFrame = sf,
      basis = BaselineBasis.Constant,
      nuisanceList = Some(Seq(nuis)),
      nuisanceCheck = NuisanceCheck.Drop,
      nuisanceNames = Some(Seq(Seq("dvars", "motion")))
    )
    assertEquals(nuisanceTerm(dropped).data.cols, 1)

    val repaired = BaselineModel.build(
      samplingFrame = sf,
      basis = BaselineBasis.Constant,
      nuisanceList = Some(Seq(nuis)),
      nuisanceCheck = NuisanceCheck.Drop,
      naAction = NaAction.Zero,
      nuisanceNames = Some(Seq(Seq("dvars", "motion")))
    )
    val nz = nuisanceTerm(repaired).data

    assertEquals(nz.cols, 2)
    assertEquals(nz.data(0), 0.0)
    assert(!repaired.designMatrix.data.exists(_.isNaN))
  }

  test("naAction median imputes the column median") {
    val sf = SamplingFrame(blockLens = Seq(6), tr = Seq(1.0))
    val nuis = Mat.unsafe(
      6,
      2,
      Array(
        Double.NaN, -2.0,
        2.0, -1.0,
        3.0, 0.0,
        4.0, 1.0,
        5.0, 2.0,
        6.0, 3.0
      )
    )

    val model = BaselineModel.build(
      samplingFrame = sf,
      basis = BaselineBasis.Constant,
      nuisanceList = Some(Seq(nuis)),
      nuisanceCheck = NuisanceCheck.Drop,
      naAction = NaAction.Median,
      nuisanceNames = Some(Seq(Seq("dvars", "motion")))
    )

    assertEquals(nuisanceTerm(model).data.cols, 2)
    assertEquals(nuisanceTerm(model).data.data(0), 4.0)
  }

  test("naAction repair leaves infinite corruption subject to dropping") {
    val sf = SamplingFrame(blockLens = Seq(6), tr = Seq(1.0))
    val nuis = Mat.unsafe(
      6,
      2,
      Array(
        Double.PositiveInfinity, -2.0,
        2.0, -1.0,
        3.0, 0.0,
        4.0, 1.0,
        5.0, 2.0,
        6.0, 3.0
      )
    )

    val model = BaselineModel.build(
      samplingFrame = sf,
      basis = BaselineBasis.Constant,
      nuisanceList = Some(Seq(nuis)),
      nuisanceCheck = NuisanceCheck.Drop,
      naAction = NaAction.Zero,
      nuisanceNames = Some(Seq(Seq("bad", "motion")))
    )

    assertEquals(nuisanceTerm(model).data.cols, 1)
    assert(!model.designMatrix.data.exists(v => v.isNaN || v.isInfinity))
  }

  test("all-missing nuisance columns become zero-variance under naAction zero") {
    val sf = SamplingFrame(blockLens = Seq(6), tr = Seq(1.0))
    val nuis = Mat.unsafe(
      6,
      2,
      Array(
        Double.NaN, -2.0,
        Double.NaN, -1.0,
        Double.NaN, 0.0,
        Double.NaN, 1.0,
        Double.NaN, 2.0,
        Double.NaN, 3.0
      )
    )

    val model = BaselineModel.build(
      samplingFrame = sf,
      basis = BaselineBasis.Constant,
      nuisanceList = Some(Seq(nuis)),
      nuisanceCheck = NuisanceCheck.Drop,
      naAction = NaAction.Zero,
      nuisanceNames = Some(Seq(Seq("empty", "motion")))
    )

    assertEquals(nuisanceTerm(model).data.cols, 1)
    assertEquals(model.nuisanceReport.map(_.byBlock.head.zeroVariance).get, Vector("empty"))
  }

  test("naAction repairs missing values even when nuisance checks are disabled") {
    val sf = SamplingFrame(blockLens = Seq(6), tr = Seq(1.0))
    val nuis = Mat.unsafe(6, 1, Array(Double.NaN, 2.0, 3.0, 4.0, 5.0, 6.0))

    val leaked = BaselineModel.build(
      samplingFrame = sf,
      basis = BaselineBasis.Constant,
      nuisanceList = Some(Seq(nuis)),
      nuisanceCheck = NuisanceCheck.None
    )
    assert(leaked.designMatrix.data.exists(_.isNaN))
    assertEquals(leaked.nuisanceReport, None)

    val repaired = BaselineModel.build(
      samplingFrame = sf,
      basis = BaselineBasis.Constant,
      nuisanceList = Some(Seq(nuis)),
      nuisanceCheck = NuisanceCheck.None,
      naAction = NaAction.Zero
    )
    assert(!repaired.designMatrix.data.exists(_.isNaN))
    assertEquals(repaired.nuisanceReport, None)
  }
