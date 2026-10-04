package scalafim.fmri.fit.profile

class CriterionJetSuite extends munit.FunSuite:
  private def get[A](value: Either[CriterionJet.Error, A]): A = value.fold(e => fail(e.message), identity)
  private val owner = get(CriterionOwner.checked(1.7))
  private val epoch = get(CriterionEpoch.checked(owner, 1L))
  private def reference(d: Int): CriterionReference =
    get(CriterionReference.checked(owner, Vector.fill(d)(0.2), CriterionDerivativeOrder.Full))
  private def profile(d: Int): ProfileJet = ProfileJet(4.0, Vector.tabulate(d)(i => i + 1.0),
    Vector.tabulate(d * d)(i => if i / d == i % d then 2.0 else 0.5), Vector(7.0, -3.0), CurvatureStatus.PositiveDefinite)

  test("d=1/2/3 directly assemble J at sigma2=2.5 and retain raw amplitudes and HE"):
    for d <- 1 to 3 do
      val ref = reference(d)
      val raw = get(RawEnergyJet.checked(ref, epoch, profile(d), 2))
      val hd = Vector.tabulate(d * d)(i => if i / d == i % d then 3.0 else -0.2)
      val determinant = get(DeterminantJet.checked(ref, 2.0, Vector.fill(d)(0.4), hd))
      val pair = get(CoherentCriterionJet.checked(raw, determinant))
      val ml = get(CriterionJet.assemble(2.5, CriterionJet.Input.TrialMl(pair)))
      assertEqualsDouble(ml.minimizationJet.energy, 9.0, 1e-15)
      assertEqualsDouble(ml.score, -1.8, 1e-15)
      for i <- 0 until d do assertEqualsDouble(ml.minimizationJet.gradient(i), i + 2.0, 1e-15)
      for i <- 0 until d * d do assertEqualsDouble(ml.minimizationJet.hessian(i), profile(d).hessian(i) + 2.5 * hd(i), 1e-15)
      assertEquals(ml.raw.jet, profile(d))
      assertEquals(ml.minimizationJet.amplitudes, Vector(7.0, -3.0))
      assertEquals(ml.minimizationJet.curvature, CurvatureStatus.PositiveDefinite)
      val penalized = get(CriterionJet.assemble(2.5, CriterionJet.Input.Penalized(raw)))
      assertEqualsDouble(penalized.score, -0.8, 1e-15)
      assertEquals(penalized.minimizationJet, profile(d))
      assertEquals(penalized.determinant, None)

  test("dimensions, derivative order and lengths refuse before indexing"):
    for d <- Vector(0, 4) do assertEquals(CriterionReference.checked(owner, Vector.fill(d)(0.0), CriterionDerivativeOrder.Full), Left(CriterionJet.Error.Dimension(d)))
    for d <- 1 to 3 do
      val ref = reference(d)
      assert(DeterminantJet.checked(ref, 0.0, Vector.empty, Vector.empty).isLeft)
      assert(DeterminantJet.checked(ref, 0.0, Vector.fill(d)(0.0), Vector.fill(d * d - 1)(0.0)).isLeft)
      assert(RawEnergyJet.checked(ref, epoch, profile(d).copy(gradient = Vector.empty), 2).isLeft)
      assert(RawEnergyJet.checked(ref, epoch, profile(d).copy(hessian = Vector.empty), 2).isLeft)
      assert(RawEnergyJet.checked(ref, epoch, profile(d), 1).isLeft)
      assert(RawEnergyJet.checked(ref, epoch, profile(d), 0).isLeft)
    val value = get(CriterionReference.checked(owner, Vector(0.0), CriterionDerivativeOrder.Value))
    assertEquals(DeterminantJet.checked(value, 0.0, Vector(0.0), Vector(0.0)), Left(CriterionJet.Error.FullJetRequired))

  test("nonfinite coordinates, values, every derivative and amplitudes refuse"):
    for bad <- Vector(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity) do
      assert(CriterionReference.checked(owner, Vector(bad), CriterionDerivativeOrder.Full).isLeft)
      for d <- 1 to 3 do
        val ref = reference(d)
        assert(DeterminantJet.checked(ref, bad, Vector.fill(d)(0.0), Vector.fill(d * d)(0.0)).isLeft)
        for i <- 0 until d do assert(DeterminantJet.checked(ref, 0.0, Vector.fill(d)(0.0).updated(i, bad), Vector.fill(d * d)(0.0)).isLeft)
        for i <- 0 until d * d do assert(DeterminantJet.checked(ref, 0.0, Vector.fill(d)(0.0), Vector.fill(d * d)(0.0).updated(i, bad)).isLeft)
        assert(RawEnergyJet.checked(ref, epoch, profile(d).copy(energy = bad), 2).isLeft)
        assert(RawEnergyJet.checked(ref, epoch, profile(d).copy(amplitudes = Vector(bad, 1.0)), 2).isLeft)
      assert(RawEnergyValue.checked(reference(1), epoch, bad, Vector(1.0), 1).isLeft)
      assert(DeterminantValue.checked(reference(1), bad).isLeft)

  test("full Hessian symmetry is exact with no silent symmetrization"):
    for d <- 2 to 3 do
      val ref = reference(d)
      val asymmetric = profile(d).hessian.updated(1, 0.50000000000001)
      assertEquals(DeterminantJet.checked(ref, 0.0, Vector.fill(d)(0.0), asymmetric), Left(CriterionJet.Error.Asymmetric(0, 1)))
      assertEquals(RawEnergyJet.checked(ref, epoch, profile(d).copy(hessian = asymmetric), 2), Left(CriterionJet.Error.Asymmetric(0, 1)))

  test("owner, coordinate and reference identity cannot be replaced by equal values"):
    val ref = reference(1)
    val raw = get(RawEnergyJet.checked(ref, epoch, profile(1), 2))
    def det(r: CriterionReference): DeterminantJet = get(DeterminantJet.checked(r, 0.0, Vector(0.0), Vector(0.0)))
    assertEquals(CoherentCriterionJet.checked(raw, det(reference(1))), Left(CriterionJet.Error.WrongReference))
    val differentCoords = get(CriterionReference.checked(owner, Vector(0.3), CriterionDerivativeOrder.Full))
    assertEquals(CoherentCriterionJet.checked(raw, det(differentCoords)), Left(CriterionJet.Error.WrongCoordinates))
    val other = get(CriterionOwner.checked(1.7))
    val otherRef = get(CriterionReference.checked(other, Vector(0.2), CriterionDerivativeOrder.Full))
    assertEquals(CoherentCriterionJet.checked(raw, det(otherRef)), Left(CriterionJet.Error.WrongOwner))
    val wrongEpoch = get(CriterionEpoch.checked(other, 1L))
    assertEquals(RawEnergyJet.checked(ref, wrongEpoch, profile(1), 2), Left(CriterionJet.Error.WrongOwner))
    val rawValue = get(RawEnergyValue.checked(ref, epoch, 4.0, Vector(1.0), 1))
    assertEquals(CoherentCriterionValue.checked(rawValue, get(DeterminantValue.checked(reference(1), 0.0))), Left(CriterionJet.Error.WrongReference))
    assert(CriterionEpoch.checked(owner, 0L).isLeft)
    assert(CriterionOwner.checked(0.0).isLeft)

  test("invalid sigma2 and assembled energy, gradient, Hessian and score overflow are typed"):
    val ref = reference(1)
    val raw = get(RawEnergyJet.checked(ref, epoch, profile(1), 2))
    for bad <- Vector(0.0, -1.0, Double.NaN, Double.PositiveInfinity) do
      assert(CriterionJet.assemble(bad, CriterionJet.Input.Penalized(raw)).isLeft)
    for (value, gradient, hessian, field) <- Vector(
      (Double.MaxValue, 0.0, 0.0, "J"), (0.0, Double.MaxValue, 0.0, "gJ"), (0.0, 0.0, Double.MaxValue, "HJ")) do
      val det = get(DeterminantJet.checked(ref, value, Vector(gradient), Vector(hessian)))
      val pair = get(CoherentCriterionJet.checked(raw, det))
      assertEquals(CriterionJet.assemble(2.5, CriterionJet.Input.TrialMl(pair)), Left(CriterionJet.Error.AssembledNonFinite(field)))
    assertEquals(CriterionJet.assemble(java.lang.Double.MIN_VALUE, CriterionJet.Input.Penalized(raw)), Left(CriterionJet.Error.AssembledNonFinite("score")))

  test("finite indefinite HE and HJ remain valid, Gram refusal remains distinct"):
    val ref = reference(1)
    val raw = get(RawEnergyJet.checked(ref, epoch, profile(1).copy(hessian = Vector(-2.0), curvature = CurvatureStatus.Indefinite), 2))
    val det = get(DeterminantJet.checked(ref, 0.0, Vector(0.0), Vector(0.2)))
    val result = get(CriterionJet.assemble(2.5, CriterionJet.Input.TrialMl(get(CoherentCriterionJet.checked(raw, det)))))
    assertEqualsDouble(result.minimizationJet.hessian.head, -1.5, 1e-15)
    assertEquals(result.minimizationJet.curvature, CurvatureStatus.Indefinite)
    assertEquals(RawEnergyJet.checked(ref, epoch, profile(1).copy(curvature = CurvatureStatus.GramNotPositiveDefinite), 2), Left(CriterionJet.Error.ProfilingRefused))

  test("a finite subnormal score is not refused by an overflowing intermediate scale"):
    val ref = reference(1)
    val raw = get(RawEnergyJet.checked(ref, epoch, profile(1).copy(energy = java.lang.Double.MIN_VALUE), 2))
    val result = get(CriterionJet.assemble(java.lang.Double.MIN_VALUE, CriterionJet.Input.Penalized(raw)))
    assertEqualsDouble(result.score, -0.5, 1e-15)
