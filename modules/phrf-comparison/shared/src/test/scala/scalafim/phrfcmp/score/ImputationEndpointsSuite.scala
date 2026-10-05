package scalafim.phrfcmp.score

class ImputationEndpointsSuite extends munit.FunSuite:
  private def o(xs: Option[Double]*): Vector[Option[Double]] = xs.toVector

  test("worst-in-family: errors take the largest observed value at the voxel"):
    // arms: PHRF (refused at voxel 1), CAN, INF3 (refused at voxel 2), FIR; voxel 3 all refused
    val arms = Vector(
      o(Some(1.0), None, Some(5.0), None),
      o(Some(2.0), Some(4.0), Some(1.0), None),
      o(Some(3.0), Some(9.0), None, None),
      o(Some(0.5), Some(2.0), Some(2.0), None)
    )
    val imp = Imputation.worstInFamily(arms, Worse.Higher)
    assertEquals(imp.excluded, 1)
    assertEquals(imp.values(0), Vector(1.0, 9.0, 5.0)) // PHRF imputed with 9 at voxel 1
    assertEquals(imp.values(1), Vector(2.0, 4.0, 1.0))
    assertEquals(imp.values(2), Vector(3.0, 9.0, 5.0)) // INF3 imputed with 5 at voxel 2
    assertEquals(imp.values(3), Vector(0.5, 2.0, 2.0))
    assertEquals(imp.imputedPerArm, Vector(1, 0, 1, 0))
    assertEquals(imp.completeCase, Vector(true, false, false))

  test("worst-in-family: scores take the lowest observed value"):
    val arms = Vector(o(None, Some(0.2)), o(Some(0.9), Some(0.4)), o(Some(0.3), None))
    val imp = Imputation.worstInFamily(arms, Worse.Lower)
    assertEquals(imp.values(0), Vector(0.3, 0.2))
    assertEquals(imp.values(2), Vector(0.3, 0.2))
    assertEquals(imp.excluded, 0)

  test("only the arms passed in influence imputation (secondary arms cannot move a score)"):
    val gating = Vector(o(None), o(Some(1.0)))
    val withExtra = Vector(o(None), o(Some(1.0)), o(Some(100.0)))
    assertEquals(Imputation.worstInFamily(gating, Worse.Higher).values(0), Vector(1.0))
    assertEquals(Imputation.worstInFamily(withExtra, Worse.Higher).values(0), Vector(100.0))

  test("T-G pool adds GLMs-D; other families do not"):
    assertEquals(GatingPair.TTXFastGlmsD.family, Family.TG)
    assertEquals(GatingPair.TTXFastLsa.family, Family.TTx)
    assertEquals(GatingPair.TTSFastLss.family, Family.TTs)
    assertEquals(GatingPair.CTS5Fir.family, Family.CTs)
    assertEquals(GatingPair.values.length, 23)
    assertEquals(GatingPair.values.count(_.isCondition), 12)

  test("zero-MISE floor is applied, counted, and equals 1e-9 sigma2 H"):
    assertEquals(Endpoints.miseFloor(2.0, 32.0), 1e-9 * 2.0 * 32.0)
    val fl = Endpoints.miseFloor(1.0, 32.0)
    val (lam, n) = Endpoints.lambda(0.0, 1.0, fl)
    assertEquals(n, 1)
    assertEqualsDouble(lam, math.log(fl), 1e-15)
    assertEquals(Endpoints.lambda(0.0, 0.0, fl), (0.0, 2))
    assertEquals(Endpoints.lambda(2.0, 1.0, fl)._2, 0)

  test("conditionPair counts floor uses, imputed and excluded voxels (sealed quantities)"):
    val arms = Method.all.take(4).map { m =>
      m -> Vector[VoxelOutcome[Double]](
        if m == Method.Phrf then VoxelOutcome.Estimated(0.0) else VoxelOutcome.Estimated(1.0),
        if m == Method.Can then VoxelOutcome.Failed else if m == Method.Phrf then VoxelOutcome.Estimated(0.0) else VoxelOutcome.Estimated(2.0),
        VoxelOutcome.Refused
      )
    }.toMap
    val ds = ScoreSynth.ok(ConditionDataset.of(0, 1.0, 32.0, arms))
    val r = Endpoints.conditionPair(ds, GatingPair.CTX5Can)
    assertEquals(r.floorUses, 1)
    assertEquals(r.excludedVoxels, 1)
    assertEquals(r.imputedVoxels, 1) // CAN imputed at voxel 1
    val fl = Endpoints.miseFloor(1.0, 32.0)
    assertEqualsDouble(r.endpoint.get, math.log(fl / 1.5), 1e-12)
    // complete-case uses voxel 0 only: PHRF 0 -> floor
    assertEqualsDouble(r.completeCaseEndpoint.get, math.log(fl / 1.0), 1e-12)

  test("Fisher z: perfect correlation is clamped, constant estimate gives z = 0, constant truth is refused"):
    val cond = Vector.tabulate(9)(_ % 3)
    val truth = Vector(1.0, 2.0, 3.0, 2.0, 4.0, 1.0, 5.0, 3.0, 0.0)
    val (z, zv) = FisherZ.voxelZ(truth.map(_ * 2 + 1), truth, cond, 3).toOption.get
    assertEquals(zv, 0)
    assertEqualsDouble(z, 0.5 * math.log((1 + FisherZ.RClamp) / (1 - FisherZ.RClamp)), 1e-9)
    val (z0, zv0) = FisherZ.voxelZ(Vector.fill(9)(3.0), truth, cond, 3).toOption.get
    assertEquals((z0, zv0), (0.0, 3))
    assert(FisherZ.voxelZ(truth, Vector.fill(9)(1.0), cond, 3).isLeft)
    // anti-correlated: negative z
    assert(FisherZ.voxelZ(truth.map(-_), truth, cond, 3).toOption.get._1 < 0.0)

  test("Fisher z equals atanh of a hand-computed Pearson r"):
    val cond = Vector(0, 0, 0, 0)
    val est = Vector(1.0, 2.0, 3.0, 5.0)
    val tru = Vector(1.0, 3.0, 2.0, 4.0)
    // r = 0.8 / sqrt(...) computed by hand: sxy = 5.5, sxx = 8.75, syy = 5.0 -> r = 5.5 / sqrt(43.75)
    val r = 5.5 / math.sqrt(8.75 * 5.0)
    assertEqualsDouble(FisherZ.voxelZ(est, tru, cond, 1).toOption.get._1, 0.5 * math.log((1 + r) / (1 - r)), 1e-12)
