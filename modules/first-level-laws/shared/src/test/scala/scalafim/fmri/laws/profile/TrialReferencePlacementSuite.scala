package scalafim.fmri.laws.profile

class TrialReferencePlacementSuite extends munit.FunSuite:
  test("development banks have eight distinct points and fresh confirmation seeds"):
    TrialReferencePlacement.candidates.foreach: (name, _) =>
      val points = TrialReferencePlacement.points(name)
      assertEquals(points.count, 8)
      points.coordinates.zipWithIndex.foreach((p, i) => assertEquals(points.nearest(p), Right(i)))
    val development = TrialDomainGate.samples(300, 256, 64, true).filter(_.cohort == "fresh")
    val confirmation = TrialReferencePlacement.confirmation(300, 256, 64).filter(_.cohort == "fresh")
    assert(confirmation.forall(c => !development.exists(d => c.unit == d.unit || c.responseSeed == d.responseSeed)))

  test("explicit bank uses public corrected readout with the unchanged work budget"):
    val result = TrialReferencePlacement.run("4x2x1", 30, 2, 0, confirm = false)
    result.records.foreach: record =>
      assertEquals(record.work.referenceInverseAttempts, 3L)
      assertEquals(record.work.residualCorrections, 1)
      assertEquals(record.work.exactReadoutFactorAttempts, 0L)
      assertEquals(TrialReferencePlacement.points("4x2x1").nearest(record.coordinates), Right(record.reference))
    assert(!result.productionScenario.ciPass)

  test("three-dimensional compact bank preserves all six Hessian components"):
    import scalafim.fmri.fit.profile.*
    val f = TrialDomainProposal.fixture(30)
    val prep = f.prepare.flatMap(_.trialOutputs).toOption.get.axis.preparation
    val points = TrialReferencePlacement.points("4x2x1")
    val banks = Vector(prep.referenceBank(points).toOption.get,
      prep.referenceBank(points, TrialReferenceStorage.ReconstructSecondBands).toOption.get)
    val encoded = prep.encodeWhitened(Array.tabulate(f.rows)(i => math.sin(0.17 * i))).toOption.get
    banks.foreach(_.pointAt(encoded))
    Vector(0, 3, 7).foreach: node =>
      val jets = banks.map: bank =>
        val out = new ProfileJetBuffer(3, 3)
        assert(bank.jetAtNode(node, out))
        out
      jets(0).hessian.zip(jets(1).hessian).foreach((a, b) => assertEqualsDouble(a, b, 1e-10))
      assertEqualsDouble(jets(0).energy, jets(1).energy, 1e-10)
    assertEquals(banks(1).work.snapshot.attempted.reconstructedBands, 18L)
    assertEquals(banks(1).work.snapshot.attempted.solveAttempts, banks(0).work.snapshot.attempted.solveAttempts)
