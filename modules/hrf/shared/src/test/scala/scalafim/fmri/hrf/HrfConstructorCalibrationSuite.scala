package scalafim.fmri.hrf

class HrfConstructorCalibrationSuite extends munit.FunSuite:
  private def sampleLimit(result: Either[HrfConstructorError, ?]): Unit = result match
    case Left(HrfConstructorError.InvalidCalibration(
        HrfNormalizationError.ReferenceSampleLimitExceeded(_, _, maximum))) =>
      assertEquals(maximum, NormalizationReferenceGrid.MaxSamples)
    case other => fail(s"expected constructor sample refusal, got $other")

  private def workLimit(result: Either[HrfConstructorError, ?]): Unit = result match
    case Left(HrfConstructorError.InvalidCalibration(
        HrfNormalizationError.ReferenceWorkLimitExceeded(_, _, maximum))) =>
      assertEquals(maximum, NormalizationReferenceGrid.MaxScalarEvaluations)
    case other => fail(s"expected constructor work refusal, got $other")

  private def parameterError(result: Either[HrfConstructorError, ?], name: String, value: Double): Unit =
    result match
      case Left(HrfConstructorError.InvalidParameter(actualName, actual)) =>
        assertEquals(actualName, name)
        if value.isNaN then assert(actual.isNaN)
        else assertEqualsDouble(actual, value, 0.0)
      case other => fail(s"expected invalid $name, got $other")

  test("LWU-calibration-sample-boundary"):
    val limit = NormalizationReferenceGrid.MaxSamples
    val step = HrfConstructorCalibration.LwuStep.value
    Vector(HrfFunctions.LwuNormalize.Height, HrfFunctions.LwuNormalize.Area).foreach: mode =>
      assertEquals(HrfConstructorCalibration.lwu(6.0, 2.5, 0.35, mode,
        Seconds((limit - 1).toDouble * step)), Right(limit))
      sampleLimit(HrfConstructorCalibration.lwu(6.0, 2.5, 0.35, mode,
        Seconds(limit.toDouble * step)))

  test("huge finite and overflowing constructor grids fail before calibration"):
    Vector(1e12, Double.MaxValue).foreach: span =>
      Vector(HrfFunctions.LwuNormalize.Height, HrfFunctions.LwuNormalize.Area).foreach: mode =>
        sampleLimit(Hrfs.lwuValidated(normalize = mode, span = Seconds(span)))
      sampleLimit(Hrfs.daguerreValidated(span = Seconds(span)))
    assert(intercept[IllegalArgumentException](Hrfs.lwu(
      normalize = HrfFunctions.LwuNormalize.Height, span = 5000.0.s)).getMessage.contains("reference samples"))
    assert(intercept[IllegalArgumentException](Hrfs.daguerre(span = 100000.0.s)).getMessage.contains("reference samples"))

  test("unnormalized LWU has no calibration grid constraint"):
    val span = Seconds(Double.MaxValue)
    assertEquals(HrfConstructorCalibration.lwu(6.0, 2.5, 0.35, HrfFunctions.LwuNormalize.None, span), Right(0))
    val hrf = Hrfs.lwuValidated(span = span).toOption.get
    assertEqualsDouble(hrf(Lag(6.0)).data(0), Hrfs.lwu()(Lag(6.0)).data(0), 1e-15)
    assert(HrfSpec(HrfKind.Lwu, span = span).toOption.get.toHrf.isRight)

  test("zero-span constructors retain their one-point calibration behavior"):
    assertEquals(HrfConstructorCalibration.lwu(6.0, 2.5, 0.35, HrfFunctions.LwuNormalize.Height, 0.0.s), Right(1))
    assertEquals(HrfConstructorCalibration.daguerre(3, 4.0, 0.0.s), Right(1))
    assert(Hrfs.lwuValidated(normalize = HrfFunctions.LwuNormalize.Height, span = 0.0.s).isRight)
    assert(Hrfs.daguerreValidated(span = 0.0.s).isRight)
    assert(Hrfs.lwuValidated(span = -1.0.s).isLeft)
    assert(Hrfs.daguerreValidated(span = -1.0.s).isLeft)

  test("Daguerre-calibration-recurrence-work-boundary"):
    val count = NormalizationReferenceGrid.MaxSamples
    val span = Seconds((count - 1).toDouble * HrfConstructorCalibration.DaguerreStep.value)
    // Four columns charge 4 initializations + 2 recurrences + 4 peak scans.
    assertEquals(HrfConstructorCalibration.daguerre(4, 4.0, span), Right(count))
    workLimit(HrfConstructorCalibration.daguerre(5, 4.0, span))
    sampleLimit(HrfConstructorCalibration.daguerre(1, 4.0, 100000.0.s))

  test("Daguerre refuses very wide bases before allocating basis or scale arrays"):
    workLimit(Hrfs.daguerreValidated(nBasis = Int.MaxValue, span = 0.1.s))
    val error = intercept[IllegalArgumentException](Hrfs.daguerre(nBasis = Int.MaxValue, span = 0.1.s))
    assert(error.getMessage.contains("scalar evaluations"))

  test("constructor parameter failures stay typed before calibration"):
    Vector(0.0, -1.0, Double.NaN, Double.PositiveInfinity).foreach: value =>
      parameterError(Hrfs.lwuValidated(sigma = value), "sigma", value)
      parameterError(Hrfs.daguerreValidated(scale = value), "scale", value)
    assertEquals(Hrfs.daguerreValidated(nBasis = 0).left.toOption,
      Some(HrfConstructorError.InvalidBasisCount(0)))
    assert(Hrfs.lwuValidated(tau = Double.PositiveInfinity).isLeft)
    assert(Hrfs.lwuValidated(rho = Double.NaN).isLeft)
    assert(Hrfs.lwuValidated(sigma = Double.MaxValue).isLeft)

  test("Daguerre refuses a nonfinite derived lag after a finite ceil endpoint"):
    Hrfs.daguerreValidated(scale = Double.MinPositiveValue, span = 0.01.s) match
      case Left(HrfConstructorError.InvalidParameter("Daguerre final scaled lag", value)) =>
        assert(!value.isFinite)
      case other => fail(s"expected scaled lag refusal, got $other")

  test("finite-LWU-inputs-refuse-overflowing-area-calibration"):
    // Only 101 samples; all raw values are finite, but their sum overflows.
    Hrfs.lwuValidated(rho = Double.MaxValue, normalize = HrfFunctions.LwuNormalize.Area, span = 0.5.s) match
      case Left(HrfConstructorError.NonFiniteCalibrationScale("lwu", 0, value)) =>
        assert(!value.isFinite)
      case other => fail(s"expected nonfinite calibration scale refusal, got $other")
    assert(intercept[IllegalArgumentException](Hrfs.lwu(rho = Double.MaxValue,
      normalize = HrfFunctions.LwuNormalize.Area, span = 0.5.s)).getMessage.contains("calibration scale"))

  test("calibration-refuses-nonfinite-raw-values-and-preserves-finite-zero-fallback"):
    Hrfs.lwuValidated(tau = 0.0, sigma = Double.MinPositiveValue,
      normalize = HrfFunctions.LwuNormalize.Height, span = 0.0.s) match
      case Left(HrfConstructorError.NonFiniteCalibrationValue("lwu", 0, 0, value)) =>
        assert(value.isNaN)
      case other => fail(s"expected nonfinite calibration value refusal, got $other")
    Vector(HrfFunctions.LwuNormalize.Height, HrfFunctions.LwuNormalize.Area).foreach: mode =>
      val hrf = Hrfs.lwuValidated(tau = 1e300, normalize = mode, span = 0.01.s).toOption.get
      assertEqualsDouble(hrf(Lag(0.0)).data(0), 0.0, 0.0)

  test("HrfSpec-constructor-refusal-is-typed"):
    Vector(100000.0, Double.MaxValue).foreach: span =>
      val spec = HrfSpec(HrfKind.Daguerre, span = Seconds(span)).toOption.get
      Vector(spec.toHrf, spec.toLegacyHrf).foreach:
        case Left(HrfSpecError.InvalidConstructor(error)) => sampleLimit(Left(error))
        case other => fail(s"expected typed constructor refusal, got $other")
    val wide = HrfSpec(HrfKind.Daguerre, nbasis = Int.MaxValue).toOption.get
    wide.toHrf match
      case Left(HrfSpecError.InvalidConstructor(error)) => workLimit(Left(error))
      case other => fail(s"expected typed wide-basis refusal, got $other")

  test("LWU calibration preserves nonmultiple ceil grids for height and area"):
    val span = 0.013.s
    val times = Vector(0.0, 0.005, 0.01, 0.015)
    val raw = times.map(t => HrfFunctions.lwu(Lag(t)))
    val height = raw.map(math.abs).max
    val area = (raw.head * 0.5 + raw(1) + raw(2) + raw.last * 0.5) * 0.005
    Vector((HrfFunctions.LwuNormalize.Height, height), (HrfFunctions.LwuNormalize.Area, area)).foreach: (mode, scale) =>
      val hrf = Hrfs.lwuValidated(normalize = mode, span = span).toOption.get
      assertEqualsDouble(hrf(Lag(0.007)).data(0), HrfFunctions.lwu(Lag(0.007)) / scale, 1e-12)
      assertEquals(hrf.descriptor.params, HrfParams.Lwu(LwuParams(6.0, 2.5, 0.35), mode))

  test("Daguerre calibration preserves nonmultiple ceil grid and descriptors"):
    val span = 0.13.s
    val raw = Vector(0.0, 0.1, 0.2).map(t => HrfFunctions.daguerreBasis(Lag(t), 3, 0.05))
    val scales = Vector.tabulate(3)(j => raw.map(row => math.abs(row(j))).max)
    val hrf = Hrfs.daguerreValidated(nBasis = 3, scale = 0.05, span = span).toOption.get
    val probe = HrfFunctions.daguerreBasis(Lag(0.07), 3, 0.05)
    hrf(Lag(0.07)).data.zipWithIndex.foreach: (actual, j) =>
      assertEqualsDouble(actual, probe(j) / scales(j), 1e-12)
    assertEquals(hrf.descriptor.params, HrfParams.Daguerre(BasisCount(3), 0.05))
