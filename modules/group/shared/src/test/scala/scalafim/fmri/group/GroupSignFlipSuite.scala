package scalafim.fmri.group

import gale.linalg.Matrix
import scalafim.dataset.SubjectId

class GroupSignFlipSuite extends munit.FunSuite:
  private val assumption = GroupSymmetryAssumption.IndependentSymmetricErrorsConditionalOnSelectionAndPrecisions
  private def get[A](value: Either[GroupError, A]): A = value.fold(e => fail(e.message), identity)
  private def ids(n: Int): Vector[SubjectId] = Vector.tabulate(n)(i => SubjectId(s"s$i"))
  private def data(y: Vector[Vector[Double]], v: Option[Vector[Vector[Double]]] = None,
      subjects: Option[Vector[SubjectId]] = None): GroupData[VarianceCapability] =
    get(GroupData.single(subjects.getOrElse(ids(y.length)), GroupSpace.SampleAxis(y.head.length), "a-b",
      Matrix.tabulate(y.length, y.head.length)((i,j) => y(i)(j)),
      v.map(x => Matrix.tabulate(x.length, x.head.length)((i,j) => x(i)(j)))))
  private def testData(d: GroupData[VarianceCapability], plan: GroupSignFlipPlan,
      weighting: GroupSymmetryWeighting = GroupSymmetryWeighting.EqualSubjects, nullCenter: Double = 0.0) =
    get(GroupSignFlip.test(d, GroupDesign.intercept(d.nSubjects), "a-b", plan, assumption, weighting, nullCenter))

  test("exact distribution agrees with an independent decimal enumeration including ties") {
    val y = Vector(2.0, -7.0, 11.0, 4.0, -3.0, 8.0, 1.0, 6.0)
    val v = Vector(1.0, 2.0, 4.0, 8.0, 1.0, 2.0, 4.0, 8.0)
    val d = data(y.map(Vector(_)), Some(v.map(Vector(_))))
    val plan = get(GroupSignFlipPlan.compile(d.subjects, GroupSignSampling.Exact))
    for weighting <- GroupSymmetryWeighting.values do
      val a = y.indices.map(i => BigDecimal(y(i)) /
        (if weighting == GroupSymmetryWeighting.EqualSubjects then BigDecimal(1) else BigDecimal(v(i)))).toVector
      val sums = a.foldLeft(Vector(BigDecimal(0)))((acc, x) => acc.flatMap(s => Vector(s+x, s-x)))
      val count = sums.count(_.abs >= a.sum.abs)
      val result = testData(d, plan, weighting)
      assertEqualsDouble(result.pValues(0), count / 256.0, 1e-15)
      assertEquals(result.exceedances, Vector(count))
      // tools/group-symmetry/reference.py uses Fraction and 60-digit Decimal.
      val expected = if weighting == GroupSymmetryWeighting.EqualSubjects then
        (2.75, 1.27017059221717668, 0.2421875) else (1.0, 0.53199517659893149, 0.6328125)
      assertEqualsDouble(result.estimate(0), expected._1, 1e-14)
      assertEqualsDouble(result.score(0), expected._2, 1e-14)
      assertEqualsDouble(result.pValues(0), expected._3, 1e-15)
      assertEquals(result.failures, Vector.empty)
  }

  test("every point of a heterogeneous null orbit controls rejection at each attainable level") {
    val n = 8
    val magnitudes = Vector(1.0, 2.0, 3.0, 5.0, 8.0, 13.0, 21.0, 34.0)
    val y = Vector.tabulate(n)(i => Vector.tabulate(256)(s =>
      magnitudes(i) * (if (s & (1 << i)) == 0 then 1 else -1)))
    val d = data(y)
    val plan = get(GroupSignFlipPlan.compile(d.subjects, GroupSignSampling.Exact))
    val r = testData(d,plan)
    for alpha <- Vector(.01,.05,.1,.5) do
      assert((0 until 256).count(i => r.pValues(i) <= alpha) <= math.floor(256*alpha).toInt)
    assertEqualsDouble(r.minimumP, 2.0/256, 1e-15)
    assertEqualsDouble(r.pValues(0), 2.0/256, 1e-15)
  }

  test("Monte Carlo counts include ties, replacement draws and the added identity") {
    val d = data(Vector(1,2,3,4,5,6).map(i => Vector(i.toDouble)))
    val plan = get(GroupSignFlipPlan.compile(d.subjects, GroupSignSampling.MonteCarlo(199,7182L)))
    val r = testData(d,plan)
    val count = (0 until plan.draws).count { b =>
      math.abs((0 until 6).map(i => plan.sign(b,i)*(i+1)).sum) >= 21
    }
    assertEquals(r.exceedances, Vector(count))
    assertEqualsDouble(r.pValues(0), (count+1)/200.0, 1e-15)
    assert(r.pValues(0) > 0)
    assertEqualsDouble(r.minimumP, 1.0/200, 1e-15)
    val repeated = get(GroupSignFlipPlan.compile(d.subjects, plan.sampling))
    assert((0 until 199).forall(b => (0 until 6).forall(i => plan.sign(b,i) == repeated.sign(b,i))))
  }

  test("units, reflection, shifted null, subject reordering and block reuse preserve inference") {
    val y = Vector.tabulate(8)(i => Vector(i-.75, math.cos(i), i%3-1.0))
    val v = Vector.tabulate(8)(i => Vector.fill(3)(.1+i))
    val d = data(y,Some(v))
    val plan = get(GroupSignFlipPlan.compile(d.subjects,GroupSignSampling.MonteCarlo(199,932L)))
    val original = testData(d,plan,GroupSymmetryWeighting.FixedInverseVariance)
    val transformed = data(y.map(_.map(x => -1e120*x)),Some(v.map(_.map(_*1e240))))
    val reflected = testData(transformed,plan,GroupSymmetryWeighting.FixedInverseVariance)
    val shifted = testData(data(y.map(_.map(_+16)),Some(v)),plan,GroupSymmetryWeighting.FixedInverseVariance,16)
    val permutation = Vector(3,1,6,0,7,2,4,5)
    val reorderedData = data(permutation.map(y),Some(permutation.map(v)),Some(permutation.map(d.subjects)))
    val reorderedPlan = get(GroupSignFlipPlan.compile(reorderedData.subjects,plan.sampling))
    val reordered = testData(reorderedData,reorderedPlan,GroupSymmetryWeighting.FixedInverseVariance)
    for j <- 0 until 3 do
      assertEqualsDouble(reflected.pValues(j),original.pValues(j),1e-15)
      assertEqualsDouble(shifted.pValues(j),original.pValues(j),1e-15)
      assertEqualsDouble(reordered.pValues(j),original.pValues(j),1e-15)
      assertEqualsDouble(reflected.score(j),-original.score(j),1e-12)
      val block = testData(data(y.map(x=>Vector(x(j))),Some(v.map(x=>Vector(x(j))))),plan,GroupSymmetryWeighting.FixedInverseVariance)
      assertEqualsDouble(block.pValues(0),original.pValues(j),1e-15)
      assertEqualsDouble(block.estimate(0),original.estimate(j),1e-15)
    assertEquals(reordered.subjects,reorderedData.subjects)
  }

  test("all-zero maps yield p=1 and extreme finite effects do not overflow the score") {
    val d = data(Vector.fill(8)(Vector(0.0,Double.MaxValue)))
    val plan = get(GroupSignFlipPlan.compile(d.subjects,GroupSignSampling.Exact))
    val r = testData(d,plan)
    assertEqualsDouble(r.pValues(0),1.0,1e-15)
    assertEqualsDouble(r.score(0),0.0,1e-15)
    assertEqualsDouble(r.pValues(1),2.0/256,1e-15)
    assert(r.estimate(1).isFinite)
    val shifted = testData(d,plan,nullCenter = -Double.MaxValue)
    assert(shifted.score(1).isFinite)
  }

  test("unsupported designs, missing variances, identities and invalid nulls fail explicitly") {
    val d = data(Vector.fill(8)(Vector(1.0)))
    val plan = get(GroupSignFlipPlan.compile(d.subjects,GroupSignSampling.Exact))
    val covariates = get(GroupDesign.fromMatrix(Matrix.tabulate(8,2)((i,j)=>if j==0 then 1.0 else i.toDouble),Vector("Intercept","x")))
    assert(GroupSignFlip.test(d,covariates,"a-b",plan,assumption).left.exists(_.isInstanceOf[GroupError.UnsupportedInference]))
    val slope = get(GroupDesign.fromMatrix(Matrix.tabulate(8,1)((i,_)=>i.toDouble),Vector("x")))
    assert(GroupSignFlip.test(d,slope,"a-b",plan,assumption).isLeft)
    assert(GroupSignFlip.test(d,GroupDesign.intercept(8),"a-b",plan,assumption,GroupSymmetryWeighting.FixedInverseVariance).left.exists(_.isInstanceOf[GroupError.MissingVariances]))
    assert(GroupSignFlip.test(d,GroupDesign.intercept(8),"a-b",plan,assumption,nullCenter=Double.NaN).isLeft)
    assert(GroupSignFlip.test(d,GroupDesign.intercept(8),"other",plan,assumption).isLeft)
    val foreign = get(GroupSignFlipPlan.compile(ids(8).updated(0,SubjectId("foreign")),GroupSignSampling.Exact))
    assert(GroupSignFlip.test(d,GroupDesign.intercept(8),"a-b",foreign,assumption).isLeft)
  }

  test("plan validates duplicate subjects, exact capacity, draw count and byte budget before allocation") {
    assert(GroupSignFlipPlan.compile(ids(1),GroupSignSampling.Exact).isLeft)
    assert(GroupSignFlipPlan.compile(Vector.fill(8)(ids(1).head),GroupSignSampling.Exact).isLeft)
    assert(GroupSignFlipPlan.compile(ids(17),GroupSignSampling.Exact).isLeft)
    assert(GroupSignFlipPlan.compile(ids(8),GroupSignSampling.MonteCarlo(0,1)).isLeft)
    assert(GroupSignFlipPlan.compile(ids(8),GroupSignSampling.MonteCarlo(Int.MaxValue,1)).isLeft)
    assert(GroupSignFlipPlan.compile(ids(8),GroupSignSampling.Exact,2047).isLeft)
    assertEquals(get(GroupSignFlipPlan.compile(ids(8),GroupSignSampling.Exact,2048)).signBytes,2048L)
  }

  test("unrepresentable precision ratios retain identified partial failures and refuse all-failed maps") {
    val d = data(Vector.fill(8)(Vector(1.0,2.0)),Some(Vector.tabulate(8)(i=>Vector(1.0,if i==0 then 1e-300 else 1e300))))
    val plan = get(GroupSignFlipPlan.compile(d.subjects,GroupSignSampling.Exact))
    val r = testData(d,plan,GroupSymmetryWeighting.FixedInverseVariance)
    assertEquals(r.failures.map(_.sample),Vector(1))
    assert(r.pValues(0).isFinite)
    assert(r.pValues(1).isNaN)
    val failed = data(Vector.fill(8)(Vector(2.0)),Some(Vector.tabulate(8)(i=>Vector(if i==0 then 1e-300 else 1e300))))
    assert(GroupSignFlip.test(failed,GroupDesign.intercept(8),"a-b",plan,assumption,GroupSymmetryWeighting.FixedInverseVariance).left.exists(_.isInstanceOf[GroupError.AllSamplesFailed]))
  }
