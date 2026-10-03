package scalafim.fmri.mvpa.pattern

import gale.linalg.DMat
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import munit.FunSuite
import scalafim.fmri.mvpa.AxisRef

/** Independent dense leave-one-voxel-out determinant checks for the
  * factorized conditional-information map. */
class ConditionalInformationSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), value => value)
  private def axis(name: String, n: Int) = right(AxisRef.fromStableKeys(name, SpaceRole.Observed, Vector.tabulate(n)(i => s"$name-$i"), "fixture", "one", "raw"))
  private def valueIdentity(name: String) = ValueIdentity.source(ValueId.unsafe(name))
  private def close(actual: Double, expected: Double, tolerance: Double = 1e-10) = assertEqualsDouble(actual, expected, tolerance)
  private def det2(a: Double, b: Double, c: Double, d: Double) = a * d - b * c
  private def det3(matrix: DMat) =
    matrix(0, 0) * det2(matrix(1, 1), matrix(1, 2), matrix(2, 1), matrix(2, 2)) -
      matrix(0, 1) * det2(matrix(1, 0), matrix(1, 2), matrix(2, 0), matrix(2, 2)) +
      matrix(0, 2) * det2(matrix(1, 0), matrix(1, 1), matrix(2, 0), matrix(2, 1))
  private def denseLovo3(psi: DMat, signal: Vector[Double], omitted: Int) =
    val joint = DMat.tabulate(3, 3)((i, j) => psi(i, j) + signal(i) * signal(j))
    val keep = (0 until 3).filter(_ != omitted).toVector
    val psiKeep = det2(psi(keep(0), keep(0)), psi(keep(0), keep(1)), psi(keep(1), keep(0)), psi(keep(1), keep(1)))
    val jointKeep = det2(joint(keep(0), keep(0)), joint(keep(0), keep(1)), joint(keep(1), keep(0)), joint(keep(1), keep(1)))
    .5 * math.log(det3(joint) / det3(psi)) - .5 * math.log(jointKeep / psiKeep)

  private def fixture(a: DMat, c: DMat, psiDiagonal: Vector[Double], loadings: DMat, categorical: Boolean = false) =
    val neural = axis("neural", a.rows); val target = axis("target", c.rows); val components = axis("component", a.cols); val samples = axis("samples", 2)
    val unit = right(AxisValues(target, Vector.fill(target.size)(1.0)))
    val geometry = if categorical then
      val conditions = axis("class", 2)
      right(TargetGeometry.categorical(conditions, target, DMat.dense(2, target.size, Vector(-1.0, 1.0)), right(AxisValues(conditions, Vector(.5, .5)))))
    else right(TargetGeometry.continuous(target, unit, unit, Vector("all" -> unit)))
    val factors = right(PatternFactors(neural, target, components, a, c, GaugeEvidence.PendingNumericalCheck))
    val covariance = right(ResidualCovariance.fromFactors(neural, psiDiagonal, loadings))
    val artifact = right(PatternArtifact(factors, geometry, CenteringPolicy.CenteredBeforeFit("x", "y"), DegenerateTargetPolicy.Refuse,
      ResidualCovarianceCapability.DiagonalPlusLowRank(neural.descriptor, covariance.rank), right(TrainingBinding(samples.descriptor, "fixture", "digest")), Vector("fixture"), right(PatternFitDiagnostics(Vector(0.0), "fixture", Vector.empty))))
    (neural, target, components, artifact, covariance)

  test("matches the independent dense leave-one-voxel-out Gaussian determinant oracle"):
    val (n, q, r, artifact, covariance) = fixture(DMat.dense(2, 1, Vector(1.0, 0.0)), DMat.eye(1), Vector(.5, 1.0 / 3.0), DMat.dense(2, 1, Vector(math.sqrt(1.5), math.sqrt(2.0 / 3.0))))
    val prior = right(TargetPriorCovariance(q, DMat.eye(1), valueIdentity("prior"), "scalar"))
    val support = right(SupportedGaussianTarget.fullSupport(q, prior, "dense-2x2"))
    val actual = right(ConditionalInformation.fromArtifact(n, q, r, artifact, covariance, support)).values.values
    // Psi=[[2,1],[1,1]], f=A C^T=(1,0).  Direct dense determinants give
    // I(y;X)-I(y;X_-0)=.5 log(2) and I(y;X)-I(y;X_-1)=.5 log(4/3).
    close(actual(0), .34657359027997265)
    close(actual(1), .14384103622589045)

  test("rank-one declared target support in a two-coordinate target matches its scalar model"):
    val (n, q, r, artifact, covariance) = fixture(DMat.dense(2, 1, Vector(1.0, 0.0)), DMat.dense(2, 1, Vector(1.0, 0.0)), Vector(.5, 1.0 / 3.0), DMat.dense(2, 1, Vector(math.sqrt(1.5), math.sqrt(2.0 / 3.0))))
    val supported = axis("supported", 1)
    val prior = right(TargetPriorCovariance(supported, DMat.eye(1), valueIdentity("support-prior"), "scalar"))
    val support = right(SupportedGaussianTarget(q, supported, DMat.dense(2, 1, Vector(1.0, 0.0)), prior, "q2-rank1"))
    val actual = right(ConditionalInformation.fromArtifact(n, q, r, artifact, covariance, support)).values.values
    close(actual(0), .34657359027997265); close(actual(1), .14384103622589045)

  test("factorized three-voxel map matches dense marginal-covariance cofactors and preserves forward-mean gauges"):
    val a = DMat.dense(3, 1, Vector(1.0, .5, -.2)); val c = DMat.eye(1)
    val (n, q, r, artifact, covariance) = fixture(a, c, Vector(2.0, 3.0, 4.0), DMat.zeros(3, 1))
    val prior = right(TargetPriorCovariance(q, DMat.eye(1), valueIdentity("three-prior"), "scalar"))
    val support = right(SupportedGaussianTarget.fullSupport(q, prior, "three-dense"))
    val actual = right(ConditionalInformation.fromArtifact(n, q, r, artifact, covariance, support)).values.values
    val psi = DMat.dense(3, 3, Vector(2.0, 0.0, 0.0, 0.0, 3.0, 0.0, 0.0, 0.0, 4.0))
    actual.zipWithIndex.foreach((value, index) => close(value, denseLovo3(psi, Vector(1.0, .5, -.2), index), 1e-11))
    def mapFor(scaleA: Double, scaleC: Double) =
      val f = fixture(DMat.dense(3, 1, Vector(scaleA, .5 * scaleA, -.2 * scaleA)), DMat.dense(1, 1, Vector(scaleC)), Vector(2.0, 3.0, 4.0), DMat.zeros(3, 1))
      right(ConditionalInformation.fromArtifact(f._1, f._2, f._3, f._4, f._5,
        right(SupportedGaussianTarget.fullSupport(f._2, right(TargetPriorCovariance(f._2, DMat.eye(1), valueIdentity(s"scale-$scaleA"), "scalar")), "gauge")))).values.values
    mapFor(5.0, .2).zip(actual).foreach((x, y) => close(x, y))
    mapFor(-1.0, -1.0).zip(actual).foreach((x, y) => close(x, y))
    val zero = fixture(DMat.zeros(3, 1), c, Vector(2.0, 3.0, 4.0), DMat.zeros(3, 1))
    right(ConditionalInformation.fromArtifact(zero._1, zero._2, zero._3, zero._4, zero._5,
      right(SupportedGaussianTarget.fullSupport(zero._2, right(TargetPriorCovariance(zero._2, DMat.eye(1), valueIdentity("zero"), "scalar")), "zero")))).values.values.foreach(value => close(value, 0.0))

  test("nonorthogonal support and correlated prior match the independent dense LOVO oracle"):
    val a = DMat.dense(3, 2, Vector(1.0, .3, .5, -.7, -.2, .8))
    val c = DMat.dense(3, 2, Vector(.6, .2, -.4, .9, .3, -.5))
    val f = fixture(a, c, Vector(.7, 1.1, 1.4), DMat.dense(3, 1, Vector(.3, .8, -.2)))
    val supported = axis("nonorthogonal-supported", 2)
    val prior = right(TargetPriorCovariance(supported, DMat.dense(2, 2, Vector(1.7, .4, .4, .9)), valueIdentity("correlated-prior"), "dense-oracle"))
    val basis = DMat.dense(3, 2, Vector(1.0, .2, .3, 1.0, -.2, .7))
    val support = right(SupportedGaussianTarget(f._2, supported, basis, prior, "nonorthogonal"))
    val actual = right(ConditionalInformation.fromArtifact(f._1, f._2, f._3, f._4, f._5, support)).values.values
    // Independently computed dense V=Psi+A C^T B S B^T C A^T, then
    // .5*(logdet(V)-logdet(Psi)-logdet(V_-v)+logdet(Psi_-v)).
    val expected = Vector(.28075327930913685, .06521806666890578, .09498140772710029)
    actual.zip(expected).foreach((x, y) => close(x, y, 1e-11))

  test("a permutation of full supported coordinates preserves the map"):
    val base = fixture(DMat.dense(2, 1, Vector(1.0, -.5)), DMat.dense(2, 1, Vector(.6, .8)), Vector(2.0, 3.0), DMat.zeros(2, 1))
    val scalar = axis("support-coordinate", 2)
    val prior = right(TargetPriorCovariance(scalar, DMat.eye(2), valueIdentity("coordinate-prior"), "permuted"))
    val ordinary = right(SupportedGaussianTarget.fullSupport(base._2, right(TargetPriorCovariance(base._2, DMat.eye(2), valueIdentity("ordinary-prior"), "ordinary")), "ordinary"))
    val permuted = right(SupportedGaussianTarget(base._2, scalar, DMat.dense(2, 2, Vector(0.0, 1.0, 1.0, 0.0)), prior, "permuted"))
    val first = right(ConditionalInformation.fromArtifact(base._1, base._2, base._3, base._4, base._5, ordinary)).values.values
    val second = right(ConditionalInformation.fromArtifact(base._1, base._2, base._3, base._4, base._5, permuted)).values.values
    first.zip(second).foreach((x, y) => close(x, y))

  test("numerical identity binds numeric inputs, supported prior, and every numerical policy"):
    val first = fixture(DMat.dense(2, 1, Vector(1.0, 0.0)), DMat.eye(1), Vector(1.0, 1.0), DMat.zeros(2, 1))
    val second = fixture(DMat.dense(2, 1, Vector(2.0, 0.0)), DMat.eye(1), Vector(1.0, 1.0), DMat.zeros(2, 1))
    val support1 = right(SupportedGaussianTarget.fullSupport(first._2, right(TargetPriorCovariance(first._2, DMat.eye(1), valueIdentity("identity-prior-1"), "one")), "identity-support"))
    val support2 = right(SupportedGaussianTarget.fullSupport(first._2, right(TargetPriorCovariance(first._2, DMat.dense(1, 1, Vector(2.0)), valueIdentity("identity-prior-2"), "two")), "identity-support"))
    val one = right(ConditionalInformation.fromArtifact(first._1, first._2, first._3, first._4, first._5, support1))
    val changedA = right(ConditionalInformation.fromArtifact(second._1, second._2, second._3, second._4, second._5, support1))
    val changedPrior = right(ConditionalInformation.fromArtifact(first._1, first._2, first._3, first._4, first._5, support2))
    val changedPolicy = right(ConditionalInformation.fromArtifact(first._1, first._2, first._3, first._4, first._5, support1,
      ConditionalInformationPolicy(roundingTolerance = 1e-9)))
    assertNotEquals(one.numericalIdentity, changedA.numericalIdentity)
    assertNotEquals(one.numericalIdentity, changedPrior.numericalIdentity)
    assertNotEquals(one.numericalIdentity, changedPolicy.numericalIdentity)

  test("cancellation between voxels returns no partial map and work records one factorized pass"):
    val f = fixture(DMat.dense(2, 1, Vector(1.0, 0.0)), DMat.eye(1), Vector(1.0, 1.0), DMat.zeros(2, 1))
    val support = right(SupportedGaussianTarget.fullSupport(f._2, right(TargetPriorCovariance(f._2, DMat.eye(1), valueIdentity("cancel-prior"), "one")), "cancel"))
    var calls = 0
    val cancelled = ConditionalInformation.fromArtifact(f._1, f._2, f._3, f._4, f._5, support, cancelled = () =>
      calls += 1
      calls >= 5
    )
    assertEquals(cancelled.left.toOption, Some(ConditionalInformationError.Cancelled(1, 2)))
    val complete = right(ConditionalInformation.fromArtifact(f._1, f._2, f._3, f._4, f._5, support))
    assertEquals(complete.work.precisionApplications, 1)
    assertEquals(complete.work.precisionDiagonalCalls, 1)
    assertEquals(complete.work.posteriorSolves, 1)
    assert(complete.work.excludesKernelPrivateScratch && complete.work.excludesRss)

  test("refuses categorical, deficient support, budget, cancellation, and near-one requests"):
    val (n, q, r, artifact, covariance) = fixture(DMat.dense(2, 1, Vector(1.0, 0.0)), DMat.eye(1), Vector(1.0, 1.0), DMat.zeros(2, 1), categorical = true)
    val prior = right(TargetPriorCovariance(q, DMat.eye(1), valueIdentity("prior-refusal"), "scalar"))
    val support = right(SupportedGaussianTarget.fullSupport(q, prior, "refusal"))
    assert(ConditionalInformation.fromArtifact(n, q, r, artifact, covariance, support).left.toOption.exists(_.isInstanceOf[ConditionalInformationError.UnsupportedTarget]))
    val two = axis("two", 2)
    val prior2 = right(TargetPriorCovariance(two, DMat.eye(2), valueIdentity("bad-support"), "two"))
    assert(SupportedGaussianTarget(q, two, DMat.dense(1, 2, Vector(1.0, 1.0)), prior2, "bad").isLeft)
    val good = fixture(DMat.dense(2, 1, Vector(1.0, 0.0)), DMat.eye(1), Vector(1.0, 1.0), DMat.zeros(2, 1))
    assert(ConditionalInformation.fromArtifact(good._1, good._2, good._3, good._4, good._5, support, ConditionalInformationPolicy(maximumWorkspaceCells = 0L)).isLeft)
    val foreign = axis("foreign-target", 1)
    val foreignSupport = right(SupportedGaussianTarget.fullSupport(foreign, right(TargetPriorCovariance(foreign, DMat.eye(1), valueIdentity("foreign-prior"), "foreign")), "foreign"))
    assert(ConditionalInformation.fromArtifact(good._1, good._2, good._3, good._4, good._5, foreignSupport).isLeft)
    val illTarget = axis("ill-target", 2); val illSupport = axis("ill-support", 2)
    val illPrior = right(TargetPriorCovariance(illSupport, DMat.eye(2), valueIdentity("ill-prior"), "ill"))
    assert(SupportedGaussianTarget(illTarget, illSupport, DMat.dense(2, 2, Vector(1.0, 1.0, 1.0, 1.0 + 1e-14)), illPrior, "ill").isLeft)
    assertEquals(SupportedGaussianTarget(illTarget, illSupport, DMat.eye(2), illPrior, "apply-budget", policy = ConditionalInformationPolicy(maximumWorkspaceCells = 11L)).left.toOption,
      Some(ConditionalInformationError.Budget(BigInt(12), 11L)))
    assert(SupportedGaussianTarget(illTarget, illSupport, DMat.eye(2), illPrior, "apply-exact", policy = ConditionalInformationPolicy(maximumWorkspaceCells = 12L)).isRight)
    val fullPrior = right(TargetPriorCovariance(illTarget, DMat.eye(2), valueIdentity("full-budget-prior"), "full"))
    assertEquals(SupportedGaussianTarget.fullSupport(illTarget, fullPrior, "full-budget", policy = ConditionalInformationPolicy(maximumWorkspaceCells = 8L)).left.toOption,
      Some(ConditionalInformationError.Budget(BigInt(16), 8L)))
    assertEquals(SupportedGaussianTarget.fullSupport(illTarget, fullPrior, "full-short", policy = ConditionalInformationPolicy(maximumWorkspaceCells = 15L)).left.toOption,
      Some(ConditionalInformationError.Budget(BigInt(16), 15L)))
    assert(SupportedGaussianTarget.fullSupport(illTarget, fullPrior, "full-exact", policy = ConditionalInformationPolicy(maximumWorkspaceCells = 16L)).isRight)
    assertEquals(ConditionalInformation.fromArtifact(good._1, good._2, good._3, good._4, good._5, support, cancelled = () => true).left.toOption, Some(ConditionalInformationError.Cancelled(0, 2)))
    val huge = fixture(DMat.dense(2, 1, Vector(1e8, 0.0)), DMat.eye(1), Vector(1.0, 1.0), DMat.zeros(2, 1))
    assert(ConditionalInformation.fromArtifact(huge._1, huge._2, huge._3, huge._4, huge._5, support).isLeft)
