package scalafim.fmri.fit.profile

import scalafim.fmri.hrf.family.NormalizationRule

class OutputRequestSuite extends munit.FunSuite:

  private def query(label: String, w: Double*): SignedQuery =
    SignedQuery.make(label, w.toVector, 1e-6).fold(e => fail(e.message), identity)

  test("queries validate labels, weights and tolerances"):
    assert(SignedQuery.make("", Vector(1.0), 1e-6).isLeft)
    assert(SignedQuery.make("q", Vector(1.0, Double.NaN), 1e-6).isLeft)
    assert(SignedQuery.make("q", Vector(1.0), 0.0).isLeft)
    assert(SignedQuery.make("q", Vector(1.0, -1.0), 1e-6).isRight)

  test("requests validate against the condition axis and refuse trial outputs on the condition backend"):
    val ok = OutputRequest.ConditionQueries(Vector(query("a-b", 1.0, -1.0, 0.0), query("mean", 1.0 / 3, 1.0 / 3, 1.0 / 3)), NormalizationRule.Unnormalised)
    assert(ok.validateFor(3).isRight)
    assert(ok.validateFor(2).left.exists(_.isInstanceOf[OutputError.WeightLength]))
    val dup = OutputRequest.ConditionQueries(Vector(query("x", 1.0), query("x", -1.0)), NormalizationRule.Unnormalised)
    assert(dup.validateFor(1).left.exists(_.isInstanceOf[OutputError.DuplicateLabel]))
    assert(OutputRequest.TrialAmplitudes(NormalizationRule.Density).validateFor(3).left.exists(_.isInstanceOf[OutputError.TrialOutputsNeedTrialBackend]))
    assert(OutputRequest.ConditionAmplitudes(NormalizationRule.Density).isConditionNative)
    assert(!OutputRequest.TrialQueries(Vector.empty, NormalizationRule.Density).isConditionNative)

  test("query evaluation keeps signs and audits float32 conversion per query tolerance"):
    val values = QueryEvaluation.evaluate(Vector(2.0, -0.5, 1.25), Vector(query("a-b", 1.0, -1.0, 0.0), query("c", 0.0, 0.0, 1.0)))
    assertEqualsDouble(values(0).value, 2.5, 0.0)
    assertEqualsDouble(values(1).value, 1.25, 0.0)
    assert(values.forall(_.withinTolerance))
    val fine = QueryEvaluation.audit("pi", math.Pi, 1e-9)
    assert(!fine.withinTolerance, "float32 cannot carry pi to 1e-9")
    assert(fine.conversionError > 0.0 && fine.conversionError < 1e-6)
    val coarse = QueryEvaluation.audit("pi", math.Pi, 1e-6)
    assert(coarse.withinTolerance)
    val bad = QueryEvaluation.audit("nan", Double.NaN, 1.0)
    assert(!bad.withinTolerance && bad.conversionError.isPosInfinity)
    // a near-zero contrast needs an absolute scale: a relative error is meaningless here
    val tiny = QueryEvaluation.evaluate(Vector(1.0, 1.0 + 1e-9), Vector(query("a-b", 1.0, -1.0)))
    assert(tiny.head.withinTolerance)
    assert(math.abs(tiny.head.value) < 1e-8)
