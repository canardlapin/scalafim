package scalafim.fmri.hrf

/** Literal goldens computed with Python struct, independently of Scala. */
class HrfIdentitySuite extends munit.FunSuite:
  test("double encodings preserve finite bits and define non-finite values") {
    val vectors = Vector(
      0.0 -> "bits:0",
      -0.0 -> "bits:-9223372036854775808",
      1.0 -> "bits:4607182418800017408",
      5.0 -> "bits:4617315517961601024",
      15.0 -> "bits:4624633867356078080",
      0.008333333333333333 -> "bits:4575957461383581969",
      1e-10 -> "bits:4457293557087583675",
      5e-324 -> "bits:1",
      1.7976931348623157e+308 -> "bits:9218868437227405311"
    )
    vectors.foreach { case (value, expected) => assertEquals(HrfIdentity.number(value), expected) }
    assertEquals(HrfIdentity.number(Double.NaN), "bits:9221120237041090560")
    assertEquals(HrfIdentity.number(Double.PositiveInfinity), "bits:9218868437227405312")
    assertEquals(HrfIdentity.number(Double.NegativeInfinity), "bits:-4503599627370496")
  }

  test("SPMG descriptor and basis element share one platform-independent golden") {
    val expected = "hrf-descriptor/v2|family=known(5:spmg1)|basis=1|span=bits:4627448617123184640|params=spmg(24:bits:4617315517961601024,24:bits:4624633867356078080,24:bits:4575957461383581969)|derivative=spmg(89:spmg(24:bits:4617315517961601024,24:bits:4624633867356078080,24:bits:4575957461383581969),1:1)|penalty=identity|integration=spmg1(89:spmg(24:bits:4617315517961601024,24:bits:4624633867356078080,24:bits:4575957461383581969))|components=sequence()"
    assertEquals(Hrfs.SPMG1.descriptor.canonicalId, expected)
    assertEquals(Hrfs.SPMG1.basisElementsValidated.toOption.get.head.id.value, s"$expected|canonical|1")
  }

  test("role indices and FIR intervals use stable formatting") {
    assertEquals(BasisRole.FirBin(1, Seconds(0.0), Seconds(8.0)).stableLabel, "fir-bin-01-bits:0..bits:4620693217682128896")
    val roles = Vector(BasisRole.Spline(1), BasisRole.Tent(1), BasisRole.Fourier(1),
      BasisRole.Sine(1), BasisRole.Daguerre(1), BasisRole.Custom(1, "x"), BasisRole.Generic(1))
    assertEquals(roles.map(_.stableLabel), Vector("spline-01", "tent-01", "fourier-01", "sine-01", "daguerre-01", "custom-01-x", "basis-01"))
    assertEquals(BasisRole.Generic(12).stableLabel, "basis-12")
    assertEquals(BasisRole.Generic(100).stableLabel, "basis-100")
  }

  test("names, nested components, sample order and signed zero remain part of identity") {
    val base = HrfDescriptor.custom("a,b|c", 1, Seconds(24.0))
    val variants = Vector(base, base.copy(family = HrfFamily.Derived("a,b|c")),
      base.copy(params = HrfParams.Coefficients("a", Vector(0.0))),
      base.copy(params = HrfParams.Coefficients("a", Vector(-0.0))),
      base.copy(params = HrfParams.Coefficients("a", Vector(1.0, 2.0))),
      base.copy(params = HrfParams.Coefficients("a", Vector(2.0, 1.0))),
      base.copy(components = Vector(base)),
      base.copy(components = Vector(base, base.copy(family = HrfFamily.Custom("c")))))
    assertEquals(variants.map(_.canonicalId).distinct.size, variants.size)
    assert(base.canonicalId.contains("family=custom(5:a,b|c)"))
  }

  test("all parameter and integration alternatives have independent literal goldens") {
    val profile = WeightedProfile.fromExplicit(Vector(1.0, 2.0), Vector(Seconds(0.0), Seconds(2.0))).toOption.get
    val curve = SampledCurve.fromUnsorted(Vector(Seconds(0.0), Seconds(2.0)), Vector(1.0, 2.0)).toOption.get
    val base = HrfDescriptor.custom("probe", 1, Seconds(24.0))
    val parameters = Vector(
      HrfParams.Empty -> "empty",
      HrfParams.Gamma(1.0, 2.0) -> "gamma(24:bits:4607182418800017408,24:bits:4611686018427387904)",
      HrfParams.Gaussian(1.0, 2.0) -> "gaussian(24:bits:4607182418800017408,24:bits:4611686018427387904)",
      HrfParams.Mexhat(1.0, 2.0) -> "mexhat(24:bits:4607182418800017408,24:bits:4611686018427387904)",
      HrfParams.Spmg(SpmgParams(1.0, 2.0, 0.25)) -> "spmg(24:bits:4607182418800017408,24:bits:4611686018427387904,24:bits:4598175219545276416)",
      HrfParams.InvLogit(1.0, 2.0, 3.0, 4.0, Seconds(0.25)) -> "inv-logit(24:bits:4607182418800017408,24:bits:4611686018427387904,24:bits:4613937818241073152,24:bits:4616189618054758400,24:bits:4598175219545276416)",
      HrfParams.HalfCosine(Seconds(1.0), Seconds(2.0), Seconds(3.0), Seconds(4.0), 1.0, 2.0) -> "half-cosine(24:bits:4607182418800017408,24:bits:4611686018427387904,24:bits:4613937818241073152,24:bits:4616189618054758400,24:bits:4607182418800017408,24:bits:4611686018427387904)",
      HrfParams.Lwu(LwuParams(1.0, 2.0, 0.25), HrfFunctions.LwuNormalize.None) -> "lwu(24:bits:4607182418800017408,24:bits:4611686018427387904,24:bits:4598175219545276416,4:none)",
      HrfParams.Lwu(LwuParams(1.0, 2.0, 0.25), HrfFunctions.LwuNormalize.Height) -> "lwu(24:bits:4607182418800017408,24:bits:4611686018427387904,24:bits:4598175219545276416,6:height)",
      HrfParams.Lwu(LwuParams(1.0, 2.0, 0.25), HrfFunctions.LwuNormalize.Area) -> "lwu(24:bits:4607182418800017408,24:bits:4611686018427387904,24:bits:4598175219545276416,4:area)",
      HrfParams.Cascade34(Cascade34Params(1.0, 2.0, 0.25)) -> "cascade34(24:bits:4607182418800017408,24:bits:4611686018427387904,24:bits:4598175219545276416)",
      HrfParams.Boxcar(Seconds(2.0), 1.0, true) -> "boxcar(24:bits:4611686018427387904,24:bits:4607182418800017408,4:true)",
      HrfParams.Weighted(profile, Hrfs.WeightedMethod.Linear, false) -> "weighted(46:sequence(6:bits:0,24:bits:4611686018427387904),65:sequence(24:bits:4607182418800017408,24:bits:4611686018427387904),6:linear,5:false)",
      HrfParams.Weighted(profile, Hrfs.WeightedMethod.Constant, false) -> "weighted(46:sequence(6:bits:0,24:bits:4611686018427387904),65:sequence(24:bits:4607182418800017408,24:bits:4611686018427387904),8:constant,5:false)",
      HrfParams.Empirical(curve) -> "empirical(46:sequence(6:bits:0,24:bits:4611686018427387904),65:sequence(24:bits:4607182418800017408,24:bits:4611686018427387904))",
      HrfParams.Sine(BasisCount(2)) -> "sine(1:2)",
      HrfParams.Fourier(BasisCount(2)) -> "fourier(1:2)",
      HrfParams.Fir(BasisCount(2)) -> "fir(1:2)",
      HrfParams.Tent(BasisCount(2)) -> "tent(1:2)",
      HrfParams.Daguerre(BasisCount(2), 0.25) -> "daguerre(1:2,24:bits:4598175219545276416)",
      HrfParams.Bspline(BasisCount(2), 3) -> "bspline(1:2,1:3)",
      HrfParams.Coefficients("a,b|c", Vector(1.0, 2.0)) -> "coefficients(5:a,b|c,65:sequence(24:bits:4607182418800017408,24:bits:4611686018427387904))"
    )
    parameters.foreach { case (value, expected) =>
      assert(base.copy(params = value).canonicalId.contains(s"|params=$expected|derivative="))
    }
    val integrations = Vector(
      IntegrationPolicy.Quadrature -> "quadrature",
      IntegrationPolicy.Gamma(1.0, 2.0) -> "gamma(24:bits:4607182418800017408,24:bits:4611686018427387904)",
      IntegrationPolicy.Gaussian(1.0, 2.0) -> "gaussian(24:bits:4607182418800017408,24:bits:4611686018427387904)",
      IntegrationPolicy.Cascade34(Cascade34Params(1.0, 2.0, 0.25)) -> "cascade34(24:bits:4607182418800017408,24:bits:4611686018427387904,24:bits:4598175219545276416)",
      IntegrationPolicy.Spmg1(SpmgParams(1.0, 2.0, 0.25)) -> "spmg1(89:spmg(24:bits:4607182418800017408,24:bits:4611686018427387904,24:bits:4598175219545276416))",
      IntegrationPolicy.SpmgTemporalDeriv(SpmgParams(1.0, 2.0, 0.25)) -> "spmg-temporal-derivative(89:spmg(24:bits:4607182418800017408,24:bits:4611686018427387904,24:bits:4598175219545276416))",
      IntegrationPolicy.SpmgDispersionDeriv(SpmgParams(1.0, 2.0, 0.25)) -> "spmg-dispersion-derivative(89:spmg(24:bits:4607182418800017408,24:bits:4611686018427387904,24:bits:4598175219545276416))",
      IntegrationPolicy.Boxcar(Seconds(2.0), 1.0) -> "boxcar(24:bits:4611686018427387904,24:bits:4607182418800017408)",
      IntegrationPolicy.PiecewisePolynomial(Vector(Seconds(0.0), Seconds(2.0)), 3) -> "piecewise-polynomial(46:sequence(6:bits:0,24:bits:4611686018427387904),1:3)",
      IntegrationPolicy.Stacked -> "stacked"
    )
    integrations.foreach { case (value, expected) =>
      assert(base.copy(integration = value).canonicalId.contains(s"|integration=$expected|components="))
    }
  }
