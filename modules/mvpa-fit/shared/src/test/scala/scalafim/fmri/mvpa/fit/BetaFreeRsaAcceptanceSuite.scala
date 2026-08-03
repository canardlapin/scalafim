package scalafim.fmri.mvpa.fit

import scalafim.dataset.RunId
import scalafim.fmri.fit.{LeastSquaresSeparate, LssTrialDesign, ResponseBlock}
import scalafim.fmri.mvpa.*
import gale.linalg.{DMat, Matrix}

class BetaFreeRsaAcceptanceSuite extends munit.FunSuite:

  test("trial readouts feed exact beta-free crossnobis and RSA through the ordinary MVPA engine"):
    val dataset = OneShotDataset.make(runBlocks()).toOption.get
    val response = Response
      .categorical(Vector.fill(3)(Vector("a", "b", "c")).flatten)
      .toOption
      .get
    val featureSet = FeatureSet.unsafe(RoiId(41), Vector(0, 1, 2, 3))
    val plan = FeatureSetPlan.regional("beta-free-rsa", Vector(featureSet)).toOption.get
    val folds = dataset.leaveOneRunOut.toOption.get

    val betaFree = OneShotMvpaEngine
      .run(dataset, plan, response, OperatorCrossnobisAnalysis(storeRdm = true))
      .toOption
      .get
    val explicit = MvpaEngine
      .run(
        dataset.explicitPatterns.toOption.get,
        plan,
        response,
        CrossnobisAnalysis(storeRdm = true),
        Some(folds)
      )
      .toOption
      .get

    val observed = rdmFrom(betaFree.successes.head)
    val reference = rdmFrom(explicit.successes.head)
    assertEquals(observed.labels, Vector("a", "b", "c"))
    observed.rdm.values.zip(reference.rdm.values).foreach: (actual, expected) =>
      assertEqualsDouble(actual, expected, 1e-12)
    assertEquals(betaFree.successes.head.metrics("TrialPatternMaterializations"), Some(0.0))

    val model = RdmModel.unsafe(
      "hypothesis",
      Vector("c", "b", "a"),
      RdmVector.unsafe(3, Vector(1.0, 3.0, 2.0))
    )
    val rsa = OneShotMvpaEngine
      .run(
        dataset,
        plan,
        response,
        OperatorCrossnobisRsaAnalysis(Vector(model), storeObservedRdm = true)
      )
      .toOption
      .get
    rsa.successes.head.payload match
      case Some(RoiPayload.Rsa(Some(rdm), scores)) =>
        val aligned = model.alignTo(rdm.items).toOption.get
        val expected = RdmScorer.Pearson.score(rdm, aligned).toOption.get
        assertEqualsDouble(scores.head.value, expected, 1e-12)
        assertEqualsDouble(rsa.successes.head.metrics("hypothesis.Pearson").get, expected, 1e-12)
      case other => fail(s"unexpected beta-free RSA payload: $other")

  private def runBlocks(): Vector[RunTrialReadout] =
    val trialDesign = fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0),
        Vector(1.0, 0.0, 0.0),
        Vector(0.0, 1.0, 0.0),
        Vector(0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 1.0),
        Vector(0.0, 0.0, 1.0)
      )
    )
    val responses = Vector(
      Vector(
        Vector(0.0, 0.2, 0.0, 0.1), Vector(0.2, 0.0, 0.1, -0.1),
        Vector(1.0, 0.0, 0.5, 0.2), Vector(1.2, 0.2, 0.3, 0.0),
        Vector(0.0, 2.0, 0.2, -0.2), Vector(-0.2, 1.8, 0.4, 0.0)
      ),
      Vector(
        Vector(0.1, 0.0, 0.0, 0.2), Vector(-0.1, 0.2, 0.2, 0.0),
        Vector(1.1, 0.1, 0.4, 0.1), Vector(1.3, -0.1, 0.6, -0.1),
        Vector(0.2, 2.1, 0.1, 0.0), Vector(0.0, 1.9, 0.3, -0.2)
      ),
      Vector(
        Vector(-0.1, 0.1, 0.1, 0.0), Vector(0.1, -0.1, -0.1, 0.2),
        Vector(0.9, 0.2, 0.5, 0.0), Vector(1.1, 0.0, 0.5, 0.2),
        Vector(-0.1, 1.9, 0.3, -0.1), Vector(0.1, 2.1, 0.1, 0.1)
      )
    )

    responses.zipWithIndex.map: (rows, index) =>
      val suffix = index + 1
      val readout = LeastSquaresSeparate
        .unsafePrepare(
          LssTrialDesign.unsafe(trialDesign, Vector(s"a_$suffix", s"b_$suffix", s"c_$suffix"))
        )
        .trialReadout
        .toOption
        .get
      RunTrialReadout
        .make(RunId(s"run-$suffix"), ResponseBlock.unsafe(fromRows(rows)), readout)
        .toOption
        .get

  private def rdmFrom(outcome: RoiOutcome.Success): LabeledRdm =
    outcome.payload match
      case Some(RoiPayload.Rdm(rdm)) => rdm
      case other => fail(s"unexpected RDM payload: $other")

  private def fromRows(rows: Seq[Seq[Double]]): DMat =
    val matrix = Matrix.newBuilder(rows.length, rows.head.length)
    var row = 0
    while row < rows.length do
      var col = 0
      while col < rows.head.length do
        matrix(row, col) = rows(row)(col)
        col += 1
      row += 1
    matrix.result()
