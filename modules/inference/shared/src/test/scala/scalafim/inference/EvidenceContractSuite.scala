package scalafim.inference

class EvidenceContractSuite extends munit.FunSuite:

  import InferenceRReferenceFixtures as R

  private def extreme(alternative: String, observed: Double, value: Double): Boolean =
    alternative match
      case "greater"   => value >= observed
      case "less"      => value <= observed
      case "two_sided" => Math.abs(value) >= Math.abs(observed)
      case other       => fail(s"unknown fixture alternative $other")

  private def assertMatrixClose(actual: R.MatrixData, expected: R.MatrixData, tol: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var i = 0
    while i < actual.values.length do
      assertEqualsDouble(actual.values(i), expected.values(i), tol)
      i += 1

  test("fixture provenance pins the external reference implementation") {
    assertEquals(R.schema, "scalafim-inference-r-v1")
    assertEquals(R.multiferVersion, "1.0.0")
    assertEquals(R.multiferCommit.length, 40)
    assert(!R.multiferSourcesDirty, "the reference source files used by fixtures must be clean")
    assert(R.rVersion.startsWith("R version "))
    assertEquals(
      R.sourceSha256.keySet,
      Set(
        "R/mc_pvalue.R",
        "R/mc_sequential.R",
        "R/unit_formation.R",
        "R/alignment.R",
        "R/infer_plan.R",
        "R/engine_oneblock.R",
        "R/engine_cross.R"
      )
    )
    assert(R.sourceSha256.values.forall(_.length == 64))
  }

  test("fixed Monte Carlo fixtures preserve counts and Phipson-Smyth values") {
    assertEquals(R.fixedMonteCarlo.length, 3)
    R.fixedMonteCarlo.foreach { fixture =>
      val exceedances = fixture.nullValues.count(extreme(fixture.alternative, fixture.observed, _))
      val expectedP = (1.0 + exceedances) / (fixture.nullValues.length + 1.0)
      val expectedSe = Math.sqrt(expectedP * (1.0 - expectedP) / fixture.nullValues.length)
      assertEquals(exceedances, fixture.exceedances)
      assertEqualsDouble(fixture.pValue, expectedP, 1e-15)
      assertEqualsDouble(fixture.mcSe, expectedSe, 1e-15)
    }
  }

  test("sequential fixtures preserve complete batch and stopping receipts") {
    assertEquals(R.sequentialMonteCarlo.map(_.stopReason), Vector("non_reject_early", "exhausted"))
    R.sequentialMonteCarlo.foreach { fixture =>
      assertEquals(fixture.batchSchedule.sum, fixture.drawn)
      assertEquals(fixture.nullValues.length, fixture.drawn)
      assertEquals(
        fixture.nullValues.count(extreme(fixture.alternative, fixture.observed, _)),
        fixture.exceedances
      )
      val denominator =
        if fixture.stopReason == "non_reject_early" then fixture.drawn + 1.0
        else fixture.maxDraws + 1.0
      assertEqualsDouble(fixture.pValue, (1.0 + fixture.exceedances) / denominator, 1e-15)
    }
  }

  test("unit fixtures make near-tie policy and one-based membership explicit") {
    val singleton = R.unitFormation.find(_.name == "single_axes_default").get
    val grouped = R.unitFormation.find(_.name == "near_tie_partial_selection").get
    val chain = R.unitFormation.find(_.name == "consecutive_tie_chain").get

    assertEquals(singleton.units.map(_.membersOneBased), Vector(Vector(1), Vector(2), Vector(3)))
    assertEquals(grouped.units.head.kind, "subspace")
    assertEquals(grouped.units.head.membersOneBased, Vector(1, 2))
    assert(!grouped.units.head.identifiable)
    assert(!grouped.units.head.selected)
    assertEquals(chain.units.head.membersOneBased, Vector(1, 2, 3))
  }

  test("alignment fixtures pin matching, sign correction, and ambiguity") {
    val unique = R.alignment.find(_.name == "permuted_sign_flips").get
    val ambiguous = R.alignment.find(_.name == "ambiguous_equal_scores").get

    assertEquals(unique.permutationOneBased, Vector(3, 1, 2))
    assertMatrixClose(unique.aligned, unique.reference, 1e-15)
    assert(!unique.ambiguous)
    assert(ambiguous.ambiguous)
    assertEqualsDouble(ambiguous.matchMargin, 0.0, 1e-15)
  }

  test("principal-angle fixtures include analytic axis and subspace cases") {
    val thirty = R.principalAngles.find(_.name == "axis_30_degrees").get
    val orthogonal = R.principalAngles.find(_.name == "orthogonal_axes").get
    val planes = R.principalAngles.find(_.name == "planes_one_tilted_axis").get

    assertEqualsDouble(thirty.anglesRadians.head, Math.PI / 6.0, 1e-12)
    assertEqualsDouble(orthogonal.anglesRadians.head, Math.PI / 2.0, 1e-12)
    assertEquals(planes.anglesRadians.length, 2)
    assertEqualsDouble(planes.anglesRadians.head, 0.0, 1e-12)
    assertEqualsDouble(planes.anglesRadians(1), Math.PI / 6.0, 1e-12)
  }

  test("PCA and PLSC fixtures preserve full ordered-ladder receipts") {
    assertEquals(R.ladders.map(_.family), Vector("pca", "plsc"))
    R.ladders.foreach { fixture =>
      assertEquals(fixture.x.rows, 8)
      assertEquals(fixture.x.cols, 4)
      assertEquals(fixture.rejectedThrough, 2)
      assertEquals(fixture.lastStepTested, 3)
      assertEquals(fixture.steps.map(_.selected), Vector(true, true, false))
      assertEquals(fixture.steps.map(_.stopReason), Vector("exhausted", "exhausted", "non_reject_early"))
      assert(fixture.roots(1) > fixture.roots(2))
      fixture.steps.foreach { step =>
        assertEquals(step.batchSchedule.sum, step.drawn)
        assertEquals(step.nullValues.length, step.drawn)
        val denominator =
          if step.stopReason == "non_reject_early" then step.drawn + 1.0
          else step.allocated + 1.0
        assertEqualsDouble(step.pValue, (1.0 + step.exceedances) / denominator, 1e-15)
      }
    }
  }
