package scalafim.fmri.model

/** Portable build specs carry formula text, so every HRF kind with a formula
  * form is portable without a caller-supplied `hrfFuns` registry.
  */
class ModelBuildSpecHrfKindSuite extends munit.FunSuite:
  private val formulas = Vector(
    "onset ~ hrf(cond, basis = lwu(tau = 5, sigma = 2, rho = 0.3, normalize = height))",
    "onset ~ hrf(cond, basis = cascade34(kappaP = 0.6, kappaU = 0.25, rho = 0.4), span = 30)",
    "onset ~ hrf(cond, basis = mexhat) + hrf(cond, basis = inv_logit(lag = 0.5), id = late)",
    "onset ~ hrf(cond, basis = half_cosine(h2 = 4)) + trialwise(basis = daguerre(scale = 3), nbasis = 2)",
    "onset ~ hrf(cond, basis = sine, nbasis = 4) + hrf(cond, basis = boxcar(width = 4, normalize = TRUE), id = box)",
    "onset ~ hrf(cond, basis = weighted(weights = c(0, 0.5, 1), times = c(0, 2, 5), method = linear))",
    "onset ~ hrf(cond, basis = gam(shape = 5)) + trialwise(basis = bs)"
  )

  formulas.zipWithIndex.foreach: (formula, index) =>
    test(s"formula-only HRF kinds round trip through portable build JSON $index"):
      val spec = ModelBuildSpec(formula)
      val json = ModelBuildSpecJsonCodec.encode(spec).fold(error => fail(error.toString), identity)
      assertEquals(ModelBuildSpecJsonCodec.decode(json), Right(spec))

  test("an unknown HRF kind is refused at the formula path"):
    assert(ModelBuildSpecJsonCodec.encode(ModelBuildSpec("onset ~ hrf(cond, basis = nope)")).isLeft)
