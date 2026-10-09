package scalafim.fmri.laws.profile

class TrialDomainGateSuite extends munit.FunSuite:
  override val munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(10, "min")

  test("declared domain routes without response information and refuses excluded slow shapes"):
    val p = TrialDomainProposal
    assertEquals(p.references.count, 8)
    (0 until 8).foreach: node =>
      assertEquals(p.route(p.references.point(node).coordinates), Right(node))
    TrialDomainGate.boundaryUnits.foreach(u => assert(p.route(p.coordinates(u)).isRight))
    assert(p.route(Vector(math.log(0.25), math.log(0.1 / 0.9), 0.8)).isLeft)
    assert(p.tailMassUpper > 0.0 && p.tailMassUpper < 4e-4, p.tailMassUpper.toString)
    assertEqualsDouble(scalafim.fmri.hrf.family.Cascade34Family.Default.horizon.value, 48.0, 0.0)
    assertEqualsDouble(scalafim.fmri.hrf.family.Cascade34Family.Default.chart.lower(2), 0.0, 0.0)

  test("frozen samples retain all boundaries and paired noise sensitivities"):
    val samples = TrialDomainGate.samples(30, 64, 32, true)
    assertEquals(samples.length, 155)
    assertEquals(samples.count(_.cohort == "boundary"), 27)
    assertEquals(samples.map(_.id).distinct.length, samples.length)
    samples
      .filter(_.cohort == "sensitivity")
      .foreach: sample =>
        val original = samples.find(s => s.cohort == "fresh" && s.responseSeed == sample.responseSeed).get
        assertEquals(original.unit, sample.unit)

  test("public fixed-shape gate uses one reference and separates approximation from representation"):
    val result = TrialDomainGate.run(30, 4, 2, boundaries = false)
    assertEquals(result.records.length, 8)
    result.records.foreach: r =>
      assertEquals(r.evaluatedReferences, 1)
      assertEquals(r.work.residualCorrections, 1)
      assertEquals(r.work.referenceInverseAttempts, 3L)
      assertEquals(r.work.exactReadoutFactorAttempts, 0L)
      assertEquals(TrialDomainProposal.route(r.coordinates), Right(r.reference))
      assert(r.originalRelativeError.isFinite && r.exactOriginalRelativeError.isFinite)
    assert(!result.productionScenario.ciPass)
    assertEquals(result.coverage.find(_.cohort == "fresh").get.count, 4)
