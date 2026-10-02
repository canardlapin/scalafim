package scalafim.fmri.mvpa

/** Pure labeled/scoring kernels; native relation workflow tests live in relation suites. */
class RsaSuite extends munit.FunSuite:

  test("Spearman RDM scorer uses average ranks for ties") {
    val observed = RdmVector.unsafe(4, Vector(1.0, 1.0, 2.0, 3.0, 3.0, 4.0))
    val model = RdmVector.unsafe(4, Vector(1.0, 2.0, 2.0, 3.0, 4.0, 4.0))
    val score = RdmScorer.Spearman.score(observed, model).toOption.get

    assertEqualsDouble(score, 10.0 / 11.0, 1e-12)
  }

  test("Spearman RDM scorer reports typed finite and zero-variance errors") {
    val nonFinite = RdmScorer.Spearman.score(
      RdmVector.unsafe(3, Vector(1.0, Double.NaN, 2.0)),
      RdmVector.unsafe(3, Vector(1.0, 2.0, 3.0))
    )
    val zeroVariance = RdmScorer.Spearman.score(
      RdmVector.unsafe(3, Vector(1.0, 1.0, 1.0)),
      RdmVector.unsafe(3, Vector(1.0, 2.0, 3.0))
    )

    assert(nonFinite.swap.toOption.get.message.contains("finite distances"))
    assert(zeroVariance.swap.toOption.get.message.contains("zero-variance"))
  }

  test("RDM vectors expose symmetric row-distance access") {
    val rdm = RdmVector.unsafe(4, Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0))

    assertEqualsDouble(rdm.distance(0, 0).toOption.get, 0.0, 1e-12)
    assertEqualsDouble(rdm.distance(1, 0).toOption.get, 1.0, 1e-12)
    assertEqualsDouble(rdm.distance(0, 1).toOption.get, 1.0, 1e-12)
    assertEqualsDouble(rdm.distance(3, 1).toOption.get, 5.0, 1e-12)
    assertEqualsDouble(rdm.distance(2, 3).toOption.get, 6.0, 1e-12)
    assert(rdm.distance(4, 0).swap.toOption.get.message.contains("out of bounds"))
  }

  test("partial Pearson RDM scorer aligns labeled controls and residualizes them") {
    val items = Vector("a", "b", "c", "d")
    val observed = RdmVector.unsafe(4, Vector(3.0, 3.0, 6.0, 8.0, 9.0, 13.0))
    val model = RdmVector.unsafe(4, Vector(1.0, -10.0, -9.0, -12.0, -19.0, -14.0))
    val reversedControl = RdmModel.unsafe(
      "trend",
      Vector("d", "c", "b", "a"),
      RdmVector.unsafe(4, Vector(6.0, 5.0, 3.0, 4.0, 2.0, 1.0))
    )
    val scorer = RdmScorer.PartialPearson.unsafe(Vector(reversedControl))
    val score = scorer.score(items, observed, model).toOption.get

    assertEqualsDouble(score, 1.0, 1e-12)
  }

  test("partial Pearson RDM scorer reports typed zero-residual errors") {
    val items = Vector("a", "b", "c", "d")
    val observed = RdmVector.unsafe(4, Vector(3.0, 3.0, 6.0, 8.0, 9.0, 13.0))
    val modelExplainedByControl = RdmVector.unsafe(4, Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0))
    val control = RdmModel.unsafe("trend", items, modelExplainedByControl)
    val scorer = RdmScorer.PartialPearson.unsafe(Vector(control))
    val result = scorer.score(items, observed, modelExplainedByControl)

    assert(result.swap.toOption.get.message.contains("zero-variance residual"))
  }
