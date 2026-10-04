package scalafim.fmri.fit.profile

class ConditionProfileProvenanceSuite extends munit.FunSuite:

  test("decode budget canonical encodes every DecodeBudget field"):
    val budget = DecodeBudget()
    val encoded = ConditionProfileProvenance.budgetCanonical(budget)
    val missing = budget.productElementNames.filterNot(name => encoded.contains(s"|$name=")).toVector
    assertEquals(missing, Vector.empty, s"decode-budget canonical omits fields; bump its version when adding them: $encoded")

  test("condition profile provenance has a platform-independent IEEE and string-framed golden"):
    val provenance = ConditionProfileProvenance(
      basis = "basis|=;[]",
      structure = Vector(Vector("A|B", "C;D"), Vector("x=y")),
      preparation = "prep|=;[]",
      nodesPerAxis = Vector(2, 21),
      budget = DecodeBudget(
        coarseStride = 3,
        maxNewtonSteps = 4,
        maxJets = 5,
        maxExactEvaluations = 6,
        weakSdLimit = Vector(-0.0, 1.0),
        ambiguityEnergy = -0.0,
        maxCandidateAttempts = 7,
        stationarityStepTolerance = 1e-9
      ),
      output = "output|=;[]",
      noiseVariance = -0.0
    )
    assertEquals(
      provenance.canonical,
      "condition-profile/v2|basis=10:basis|=;[]|structure=conditions(22:condition(3:A|B,3:C;D),16:condition(3:x=y))|preparation=9:prep|=;[]|nodes=nodes(1:2,2:21)|budget=decode-budget/v1|coarseStride=3|maxNewtonSteps=4|maxJets=5|maxExactEvaluations=6|weakSdLimit=values(25:bits:-9223372036854775808,24:bits:4607182418800017408)|ambiguityEnergy=bits:-9223372036854775808|maxCandidateAttempts=7|stationarityStepTolerance=bits:4472406533629990549|output=11:output|=;[]|sigma2=bits:-9223372036854775808"
    )
