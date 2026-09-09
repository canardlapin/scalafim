package scalafim.fmri.group

import gale.linalg.{DMat, DVec, Matrix}
import scalafim.dataset.SubjectId
import scalafim.fmri.fit.{ResidualDegreesOfFreedom, TContrastResult}
import scalafim.image.NeuroSpace

class GroupRepairSuite extends munit.FunSuite:
  private def value[A](x: Either[GroupError,A]): A = x.fold(e => fail(e.message), identity)
  private def matrix(rows: Vector[Vector[Double]]): DMat =
    val b = Matrix.newBuilder(rows.length, rows.head.length)
    rows.indices.foreach(i => rows(i).indices.foreach(j => b(i,j) = rows(i)(j)))
    b.result()
  private val names = Vector("(Intercept)","group","age","covariate")
  private val subjects = Vector.tabulate(24)(i => SubjectId(s"s$i"))
  private val modes = Vector("OLS" -> GroupWeighting.Unweighted, "FE" -> GroupWeighting.InverseVariance, "DL" -> GroupWeighting.RandomEffects())
  private def run(x: Vector[Vector[Double]], y: Vector[Vector[Double]], v: Vector[Vector[Double]], mode: GroupWeighting): Either[GroupError,GroupFit] =
    for
      design <- GroupDesign.fromMatrix(matrix(x), names.take(x.head.length))
      data <- GroupData.withVariances(subjects.take(x.length), GroupSpace.SampleAxis(y.head.length), "effect", matrix(y), matrix(v))
      model <- GroupModel.build(data,design,mode)
      fit <- GroupEngine.fit(model)
    yield fit.fit("effect").get
  private def close(a: Double, b: Double, tol: Double = 2e-9): Unit =
    assert(a.isFinite && b.isFinite && math.abs(a-b) <= tol*(1+math.abs(b)), s"actual=$a expected=$b")
  private val contrast = GroupContrast.unsafe("adjusted difference",Map("group"->1.0,"age"-> -0.5,"covariate"->0.2))

  modes.foreach { (label, mode) =>
    test(s"independent R reference: $label coefficients, SE, contrast p and heterogeneity") {
      val fit = value(run(GroupRepairFixture.x,GroupRepairFixture.y,GroupRepairFixture.v,mode))
      val result = value(contrast.evaluate(fit))
      val refs = GroupRepairReference.rows.filter(_.head == label)
      refs.foreach { row =>
        val s = row(1).toInt
        for j <- 0 until 4 do
          close(fit.coefficients(j,s),row(8+j).toDouble)
          close(fit.standardErrors(j,s),row(12+j).toDouble)
        close(result.estimates(s),row(2).toDouble)
        close(result.standardErrors(s),row(3).toDouble)
        close(result.statistics(s),row(4).toDouble)
        close(result.pValues(s),row(5).toDouble,2e-7)
        if label != "OLS" then
          close(fit.heterogeneity.get.tau2(s),row(6).toDouble)
          close(fit.heterogeneity.get.q(s),row(7).toDouble)
      }
    }
    test(s"participant permutation and independent sample blocks: $label") {
      val fit = value(run(GroupRepairFixture.x,GroupRepairFixture.y,GroupRepairFixture.v,mode))
      val reversed = value(run(GroupRepairFixture.x.reverse,GroupRepairFixture.y.reverse,GroupRepairFixture.v.reverse,mode))
      val block = value(run(GroupRepairFixture.x,GroupRepairFixture.y.map(_.slice(2,5)),GroupRepairFixture.v.map(_.slice(2,5)),mode))
      for j <- 0 until 4; s <- 0 until 7 do
        close(reversed.coefficients(j,s),fit.coefficients(j,s))
        close(reversed.standardErrors(j,s),fit.standardErrors(j,s))
      for j <- 0 until 4; s <- 0 until 3 do close(block.coefficients(j,s),fit.coefficients(j,s+2))
    }
    test(s"covariate unit change preserves the same estimand: $label") {
      val expected = value(contrast.evaluate(value(run(GroupRepairFixture.x,GroupRepairFixture.y,GroupRepairFixture.v,mode))))
      val scale = 1e-6
      val x = GroupRepairFixture.x.map(row => row.updated(2,row(2)*scale))
      val changed = value(run(x,GroupRepairFixture.y,GroupRepairFixture.v,mode))
      val c = GroupContrast.unsafe("same effect",Map("group"->1.0,"age"->(-0.5*scale),"covariate"->0.2))
      val actual = value(c.evaluate(changed))
      for s <- 0 until 7 do
        close(actual.estimates(s),expected.estimates(s))
        close(actual.standardErrors(s),expected.standardErrors(s))
    }
  }

  test("weighted rank deficiency is a typed failure rather than a successful all-NaN map") {
    val x = GroupRepairFixture.x.map(row => row.updated(3,row(2)))
    for mode <- Vector(GroupWeighting.InverseVariance,GroupWeighting.RandomEffects()) do
      val result = run(x,GroupRepairFixture.y,GroupRepairFixture.v,mode)
      assert(result.isLeft,s"rank deficient weighted fit was Right; coefficients=${result.toOption.map(_.coefficients(0,0))}")
  }
  test("subtracting a term from itself cannot become a negative term estimate") {
    val fit = value(run(GroupRepairFixture.x,GroupRepairFixture.y,GroupRepairFixture.v,GroupWeighting.Unweighted))
    val result = GroupContrast.difference("self difference","group","group").evaluate(fit)
    assert(result.isLeft || result.toOption.get.estimates.toSeq.forall(_ == 0.0),s"actual=${result.toOption.map(_.estimates.toSeq)}")
  }
  test("validating contrast constructor returns typed errors for non-finite and empty weights") {
    assert(GroupContrast.fromStrings("bad",Map("age"->Double.NaN)).isLeft)
    assert(GroupContrast.fromStrings("empty",Map.empty).isLeft)
  }
  test("two-sample labels cannot silently create duplicate intercept terms") {
    val d = GroupDesign.twoSample(Vector("control","(Intercept)","control","(Intercept)"))
    assert(d.isLeft || d.toOption.get.termNames.distinct.size == d.toOption.get.termNames.size,s"actual=$d")
  }

  private val people = Vector("a","b","c").map(SubjectId(_))
  private val space = GroupSpace.VoxelAxis(NeuroSpace(Vector(2,1,1)),Vector(0,1))
  private def t(ys: Vector[Double], indices: Vector[Int], name: String = "effect", se: Double = 1.0): TContrastResult =
    TContrastResult(name,DVec.fromSeq(ys),DVec.fromSeq(ys.map(_=>se)),DVec.fromSeq(ys.map(_/se)),ResidualDegreesOfFreedom.unsafe(80),indices)
  test("first-level bridge aligns or rejects reordered voxel identities") {
    val results = Map((people(0),"effect")->t(Vector(1,10),Vector(0,1)),(people(1),"effect")->t(Vector(20,2),Vector(1,0)),(people(2),"effect")->t(Vector(3,30),Vector(0,1)))
    val data = FirstLevel.groupData(space,people,Vector("effect"),results)
    val d = value(data)
    locally {
      val fit = value(GroupEngine.fit(value(GroupModel.build(d,GroupDesign.intercept(3))))).fit("effect").get
      close(fit.coefficients(0,0),2.0)
      close(fit.coefficients(0,1),20.0)
    }
  }
  test("first-level bridge rejects substituted contrast identity") {
    val results = people.map(p => (p,"effect")->t(Vector(1,2),Vector(0,1),name="different effect")).toMap
    assert(FirstLevel.groupData(space,people,Vector("effect"),results).isLeft)
  }
  test("first-level bridge does not turn a negative standard error into valid variance") {
    val results = people.map(p => (p,"effect")->t(Vector(1,2),Vector(0,1),se= -1.0)).toMap
    assert(FirstLevel.groupData(space,people,Vector("effect"),results).isLeft)
  }

  test("contrast validation is total for each invalid input and normalized collision") {
    for weights <- Vector(Map.empty[String, Double], Map("age" -> 0.0), Map("age" -> Double.NaN), Map("age" -> Double.PositiveInfinity), Map(" " -> 1.0), Map("age" -> 1.0, " age " -> -1.0)) do
      assert(GroupContrast.fromStrings("contrast", weights).isLeft)
    assert(GroupContrast.fromStrings(" ", Map("age" -> 1.0)).isLeft)
    val fit = value(run(GroupRepairFixture.x, GroupRepairFixture.y, GroupRepairFixture.v, GroupWeighting.Unweighted))
    assert(GroupContrast.difference("self", "group", " group ").evaluate(fit).isLeft)
  }

  test("two-sample labels validate blanks and normalized levels before construction") {
    assert(GroupDesign.twoSample(Vector("control", " ", "control", " ")).isLeft)
    assert(GroupDesign.twoSample(Vector("control", " control ", "control", " control ")).isLeft)
    assertEquals(value(GroupDesign.twoSample(Vector(" control ", "case", "control", " case "))).termNames, Vector("(Intercept)", "case"))
  }

  test("bridge rejects duplicate, substituted, and ambiguous spatial axes") {
    val good = people.map(p => (p,"effect") -> t(Vector(1,2),Vector(0,1))).toMap
    for indices <- Vector(Vector(0,0), Vector(0,2), Vector(1,2)) do
      val changed = good.updated((people(1),"effect"), t(Vector(1,2),indices))
      assert(FirstLevel.groupData(space,people,Vector("effect"),changed).left.toOption.exists(_.isInstanceOf[GroupError.SpatialIdentityMismatch]))
    val duplicateSpace = GroupSpace.VoxelAxis(NeuroSpace(Vector(2,1,1)),Vector(0,0))
    assert(FirstLevel.groupData(duplicateSpace,people,Vector("effect"),good).isLeft)
    val nonCanonical = people.map(p => (p,"effect") -> t(Vector(1,2),Vector(3,4))).toMap
    assert(FirstLevel.groupData(GroupSpace.SampleAxis(2),people,Vector("effect"),nonCanonical).isLeft)
    val parcel = GroupSpace.ParcelAxis(Vector(SampleLabel.unsafe("a"),SampleLabel.unsafe("b")))
    assert(FirstLevel.groupData(parcel,people,Vector("effect"),good).isLeft)
  }

  test("bridge preserves reordered SEs as well as estimates") {
    val permuted = TContrastResult("effect",DVec.fromSeq(Vector(20.0,2.0)),DVec.fromSeq(Vector(4.0,3.0)),DVec.fromSeq(Vector(5.0,2.0/3)),ResidualDegreesOfFreedom.unsafe(80),Vector(1,0))
    val results = people.map(p => (p,"effect") -> permuted).toMap
    val data = value(FirstLevel.groupData(space,people,Vector("effect"),results))
    val fit = value(GroupEngine.fit(value(GroupModel.build(data,GroupDesign.intercept(3),GroupWeighting.InverseVariance)))).fit("effect").get
    close(fit.coefficients(0,0),2.0)
    close(fit.coefficients(0,1),20.0)
    close(fit.standardErrors(0,0),3/math.sqrt(3))
    close(fit.standardErrors(0,1),4/math.sqrt(3))
  }

  test("bridge refuses zero, nonfinite, overflowing and underflowing uncertainty") {
    for se <- Vector(0.0,Double.NaN,Double.PositiveInfinity,1e200,1e-200) do
      val results = people.map(p => (p,"effect") -> t(Vector(1,2),Vector(0,1),se=se)).toMap
      assert(FirstLevel.groupData(space,people,Vector("effect"),results).left.toOption.exists(_.isInstanceOf[GroupError.InvalidStandardError]))
  }

  private val expandedModes = modes.map(_._2) ++ Vector(
    GroupWeighting.RandomEffects(TauEstimator.DerSimonianLaird,MetaInference.ModifiedKnappHartung),
    GroupWeighting.RandomEffects(TauEstimator.PauleMandel,MetaInference.Normal),
    GroupWeighting.RandomEffects(TauEstimator.PauleMandel,MetaInference.ModifiedKnappHartung))

  expandedModes.foreach { mode =>
    test(s"units, offsets and column ordering preserve contrast meaning: ${mode.label}") {
      val expected = value(contrast.evaluate(value(run(GroupRepairFixture.x,GroupRepairFixture.y,GroupRepairFixture.v,mode))))
      for scale <- Vector(1e-12,1e12,-1e-6) do
        val changedX = GroupRepairFixture.x.map(row => row.updated(2,row(2)*scale))
        val c = GroupContrast.unsafe("same",Map("group"->1.0,"age"->(-0.5*scale),"covariate"->0.2))
        val actual = value(c.evaluate(value(run(changedX,GroupRepairFixture.y,GroupRepairFixture.v,mode))))
        for s <- 0 until 7 do
          close(actual.estimates(s),expected.estimates(s),2e-8)
          close(actual.standardErrors(s),expected.standardErrors(s),2e-8)
      val shiftedX = GroupRepairFixture.x.map(row => row.updated(2,row(2)+1000.0))
      val shifted = value(contrast.evaluate(value(run(shiftedX,GroupRepairFixture.y,GroupRepairFixture.v,mode))))
      for s <- 0 until 7 do
        close(shifted.estimates(s),expected.estimates(s),2e-8)
        close(shifted.standardErrors(s),expected.standardErrors(s),2e-8)
      val order = Vector(3,1,0,2)
      val reorderedX = GroupRepairFixture.x.map(row => order.map(row))
      val reordered = GroupContrast.unsafe("same",Map("(Intercept)"->0.2,"group"->1.0,"covariate"-> -0.5))
      val actual = value(reordered.evaluate(value(run(reorderedX,GroupRepairFixture.y,GroupRepairFixture.v,mode))))
      for s <- 0 until 7 do
        close(actual.estimates(s),expected.estimates(s),2e-8)
        close(actual.standardErrors(s),expected.standardErrors(s),2e-8)
    }
  }

  test("identified partial failures preserve usable samples and propagate to contrasts") {
    val x = Vector.fill(4)(Vector(1.0))
    val y = Vector.tabulate(4)(i => Vector(i.toDouble,1e308))
    val v = Vector.fill(4)(Vector(1.0,1.0))
    for mode <- expandedModes.filter(_ != GroupWeighting.Unweighted) do
      val fit = value(run(x,y,v,mode))
      assertEquals(fit.failures.map(_.sample),Vector(1))
      close(fit.coefficients(0,0),1.5)
      val term = fit.term("(Intercept)").get
      val c = value(GroupContrast.term("(Intercept)").evaluate(fit))
      assertEquals(term.failures,fit.failures)
      assertEquals(c.failures,fit.failures)
      assert(term.pValues(0).isFinite && term.pValues(1).isNaN)
      assert(term.adjustedP()(0).isFinite && term.adjustedP()(1).isNaN)
      val allBad = run(x,y.map(row => Vector(row(1))),v.map(_.take(1)),mode)
      assert(allBad.left.toOption.exists(_.isInstanceOf[GroupError.AllSamplesFailed]))
  }

  test("direct WLS validates shapes and weights before numerical access") {
    val x=matrix(Vector.fill(4)(Vector(1.0))); val y=matrix(Vector.fill(4)(Vector(2.0)))
    assert(GroupGlm.wls(x,y,matrix(Vector.fill(3)(Vector(1.0)))).isLeft)
    assert(GroupGlm.wls(x,y,matrix(Vector.fill(4)(Vector(1.0,1.0)))).isLeft)
    for w <- Vector(0.0,-1.0,Double.NaN,Double.PositiveInfinity) do
      assert(GroupGlm.wls(x,y,matrix(Vector.fill(4)(Vector(w)))).isLeft)
    assertEquals(GroupGlm.ols(x,matrix(Vector.fill(3)(Vector(1.0)))).left.toOption,Some(GroupError.subjectMismatch(3,4)))
  }

  test("finite-sample policies match independent metafor coefficient and contrast covariance") {
    for row <- GroupRepairMetaReference.rows do
      val mode = GroupWeighting.RandomEffects(
        if row(0)=="DL" then TauEstimator.DerSimonianLaird else TauEstimator.PauleMandel,
        if row(1)=="z" then MetaInference.Normal else MetaInference.ModifiedKnappHartung)
      val fit = value(run(GroupRepairFixture.x,GroupRepairFixture.y,GroupRepairFixture.v,mode))
      val c = value(contrast.evaluate(fit))
      val s = row(2).toInt
      close(c.estimates(s),row(3).toDouble,2e-8)
      close(c.standardErrors(s),row(4).toDouble,2e-8)
      close(c.statistics(s),row(5).toDouble,2e-8)
      close(c.pValues(s),row(6).toDouble,2e-7)
      close(fit.heterogeneity.get.tau2(s),row(7).toDouble,2e-8)
      close(fit.heterogeneity.get.q(s),row(8).toDouble,2e-8)
      for j <- 0 until 4 do
        close(fit.coefficients(j,s),row(9+j).toDouble,2e-8)
        close(fit.standardErrors(j,s),row(13+j).toDouble,2e-8)
      if row(1)=="adhoc" then assertEquals(c.statistic,GroupStatistic.unsafeStudentT(20))
  }

  test("a severely imbalanced but identifiable weighted design uses stable covariance") {
    val x=Vector(Vector(1.0,0.0),Vector(1.0,0.0),Vector(0.0,1.0),Vector(0.0,1.0))
    val y=Vector(1.0,3.0,7.0,9.0).map(v=>Vector(v))
    val variances=Vector(1e-16,1e-16,1.0,1.0).map(v=>Vector(v))
    val fit=value(run(x,y,variances,GroupWeighting.InverseVariance))
    assertEquals(fit.failures,Vector.empty)
    close(fit.coefficients(0,0),2.0)
    close(fit.coefficients(1,0),8.0)
    assertEqualsDouble(fit.standardErrors(0,0),math.sqrt(1e-16/2),1e-18)
    assertEqualsDouble(fit.standardErrors(1,0),math.sqrt(0.5),1e-12)
  }

  test("covariate origin changes preserve contrasts involving the intercept") {
    val weights=Map("(Intercept)"->1.0,"group"->0.5,"age"->0.2)
    val baseContrast=GroupContrast.unsafe("mean at reference covariates",weights)
    for mode <- expandedModes do
      val base=value(baseContrast.evaluate(value(run(GroupRepairFixture.x,GroupRepairFixture.y,GroupRepairFixture.v,mode))))
      val shiftedX=GroupRepairFixture.x.map(row=>row.updated(2,row(2)+100.0))
      val shiftedContrast=GroupContrast.unsafe("same physical estimand",weights.updated("age",100.2))
      val shifted=value(shiftedContrast.evaluate(value(run(shiftedX,GroupRepairFixture.y,GroupRepairFixture.v,mode))))
      for sample <- 0 until 7 do
        close(shifted.estimates(sample),base.estimates(sample),2e-8)
        close(shifted.standardErrors(sample),base.standardErrors(sample),2e-8)
  }

object GroupRepairFixture:
  val x = Vector(Vector(1,-1,-0.47916666666666669,0),Vector(1,1,-0.4375,0.64421768723769102),Vector(1,-1,-0.39583333333333331,0.98544972998846014),Vector(1,1,-0.35416666666666669,0.86320936664887393),Vector(1,-1,-0.3125,0.33498815015590511),Vector(1,1,-0.27083333333333331,-0.35078322768961984),Vector(1,-1,-0.22916666666666666,-0.87157577241358775),Vector(1,1,-0.1875,-0.98245261262433259),Vector(1,-1,-0.14583333333333334,-0.63126663787232162),Vector(1,1,-0.10416666666666667,0.016813900484349713),Vector(1,-1,-0.0625,0.65698659871878906),Vector(1,1,-0.020833333333333332,0.98816823387700026),Vector(1,-1,0.020833333333333332,0.85459890808828143),Vector(1,1,0.0625,0.31909836234935213),Vector(1,-1,0.10416666666666667,-0.36647912925192677),Vector(1,1,0.14583333333333334,-0.87969575997167015),Vector(1,-1,0.1875,-0.9791777291513174),Vector(1,1,0.22916666666666666,-0.61813711223703471),Vector(1,-1,0.27083333333333331,0.033623047221136695),Vector(1,1,0.3125,0.66956976219660103),Vector(1,-1,0.35416666666666669,0.99060735569487035),Vector(1,1,0.39583333333333331,0.84574683114293425),Vector(1,-1,0.4375,0.30311835674570398),Vector(1,1,0.47916666666666669,-0.38207141718400583))
  val y = Vector(Vector(0.21160818031700368,1.5473958800600323,-1.1232960337618856,0.92327780254038971,-1.5711418168463436,-0.32093611852258586,-1.7347625170938716),Vector(0.20619376212390955,0.54904525804873305,-0.17434585042720507,0.90372235569805737,0.00016467388723462317,1.057228194503032,0.5905965931788939),Vector(-0.97656509617736875,0.76667020425813204,-1.3943931491605865,2.4030019637650502,-0.78635173058086894,1.2575494769742792,-1.0325073984234932),Vector(-0.61365439042364656,0.87172393083229627,1.3769826475029459,2.0613344187137237,-0.52829858640767302,0.43857235264398597,0.58058080630797315),Vector(-0.95797765539950752,1.85343241283549,-0.8116621379404878,0.54042956633527461,-1.071605253585395,1.4535903671035046,-1.2225390962702334),Vector(0.28409682971067068,1.3118931766708464,-0.038360569749902651,0.91909056210982376,1.3962597140997062,-0.80478509005461507,-0.36725002740642393),Vector(0.40575253252892995,0.81014271735140941,-1.6207451406751132,1.2004968062892694,-1.2904539550294587,0.3898348275610653,-1.6820090682332298),Vector(1.4568065033611457,-0.40217494813296906,1.2605718529853154,-0.12374881520080949,1.5490295488528474,-0.7158590282178765,-1.0139967142413775),Vector(0.82429312865449211,0.13025092115338677,-0.5457985664117706,0.13134973476910267,-0.23086017234375689,-0.17736032078756034,-1.2425474650692103),Vector(0.94330841529597564,0.73178104835296409,0.18688468030289007,1.7946396453376496,-0.053522711367380849,0.93722077571024709,-0.69447444113039303),Vector(-0.31960499347140303,2.037529996206902,-1.7851401150511372,1.5348760020380527,-0.56073553657153152,0.31287202859709473,-0.180946229828709),Vector(-0.2026608831919694,1.5869687271390296,1.0661365758740144,0.91535200258869431,0.344211734049016,1.935402150726818,-0.36637116441677686),Vector(-0.83489014555957619,1.1025735617387336,-0.3560412251910654,1.9670963942813948,-1.8814747109816512,0.78902550871798582,0.18307358636025506),Vector(0.2237685201883497,-0.086102496296511677,0.47635306188158955,1.7804718423006469,1.4332350491172083,0.79157207210114877,-0.96287754163363792),Vector(0.36085131400960951,0.34122806291229041,-1.8617824277918726,0.019688516347317941,-0.80080277187092241,0.68504131174501293,-0.37825223698052568),Vector(1.6198443383129828,0.56009160521867973,0.8233915199998888,0.14631685326549485,0.8701591085761915,-0.89142650035391857,-1.7910383151855007),Vector(1.2832612429193055,1.3331617300247687,-0.26279719784152489,1.0578612341407472,0.60142022118831284,0.70949776452247804,-0.66994520804049784),Vector(1.6361621893689695,0.60208499715860186,0.79662918432084939,0.25201168493719006,0.73728215359216631,-0.68049520136285824,-1.6415150544816894),Vector(0.42578102720816541,0.40854800664235813,-1.8363246508416382,0.27863981524386461,-0.83786237769076666,1.1565614908348079,-0.090551530693945392),Vector(0.38630406119690319,-0.039787856246565556,0.56835388524566377,1.9872800750938955,1.3839336928056631,1.1209224711176886,-0.77277635799652977),Vector(-0.53269664570856001,1.0551459317882452,-0.27388243637478249,1.9133164307996302,-1.7833725227539992,1.1258963739109846,0.35591675140259899),Vector(0.25404385205360935,1.3852231619117035,1.1102784776066841,0.6811788275346411,0.73046898296595519,2.001241790114145,-0.47888098962096143),Vector(0.27217101727607584,1.6839473561632041,-1.7077393387373825,1.1913783046371873,-0.12855590370748493,0.26702274383577795,-0.26598464063259536),Vector(1.625884192644149,0.30630061744724707,0.33864456192368975,1.3200185818751595,0.48498625478586788,0.96006187090513873,-0.95877225188608839))
  val v = Vector(Vector(0.070000000000000007,0.074499999999999997,0.079000000000000001,0.083499999999999991,0.087999999999999995,0.092499999999999999,0.097000000000000003),Vector(0.10000000000000001,0.10899999999999999,0.11799999999999999,0.127,0.13600000000000001,0.14499999999999999,0.154),Vector(0.13,0.14349999999999999,0.157,0.17050000000000001,0.184,0.19750000000000001,0.21099999999999999),Vector(0.16,0.17799999999999999,0.19600000000000001,0.214,0.23200000000000001,0.25,0.26799999999999996),Vector(0.19,0.21249999999999999,0.23500000000000001,0.25750000000000001,0.27999999999999997,0.30249999999999999,0.32499999999999996),Vector(0.070000000000000007,0.074499999999999997,0.079000000000000001,0.083499999999999991,0.087999999999999995,0.092499999999999999,0.097000000000000003),Vector(0.10000000000000001,0.10899999999999999,0.11799999999999999,0.127,0.13600000000000001,0.14499999999999999,0.154),Vector(0.13,0.14349999999999999,0.157,0.17050000000000001,0.184,0.19750000000000001,0.21099999999999999),Vector(0.16,0.17799999999999999,0.19600000000000001,0.214,0.23200000000000001,0.25,0.26799999999999996),Vector(0.19,0.21249999999999999,0.23500000000000001,0.25750000000000001,0.27999999999999997,0.30249999999999999,0.32499999999999996),Vector(0.070000000000000007,0.074499999999999997,0.079000000000000001,0.083499999999999991,0.087999999999999995,0.092499999999999999,0.097000000000000003),Vector(0.10000000000000001,0.10899999999999999,0.11799999999999999,0.127,0.13600000000000001,0.14499999999999999,0.154),Vector(0.13,0.14349999999999999,0.157,0.17050000000000001,0.184,0.19750000000000001,0.21099999999999999),Vector(0.16,0.17799999999999999,0.19600000000000001,0.214,0.23200000000000001,0.25,0.26799999999999996),Vector(0.19,0.21249999999999999,0.23500000000000001,0.25750000000000001,0.27999999999999997,0.30249999999999999,0.32499999999999996),Vector(0.070000000000000007,0.074499999999999997,0.079000000000000001,0.083499999999999991,0.087999999999999995,0.092499999999999999,0.097000000000000003),Vector(0.10000000000000001,0.10899999999999999,0.11799999999999999,0.127,0.13600000000000001,0.14499999999999999,0.154),Vector(0.13,0.14349999999999999,0.157,0.17050000000000001,0.184,0.19750000000000001,0.21099999999999999),Vector(0.16,0.17799999999999999,0.19600000000000001,0.214,0.23200000000000001,0.25,0.26799999999999996),Vector(0.19,0.21249999999999999,0.23500000000000001,0.25750000000000001,0.27999999999999997,0.30249999999999999,0.32499999999999996),Vector(0.070000000000000007,0.074499999999999997,0.079000000000000001,0.083499999999999991,0.087999999999999995,0.092499999999999999,0.097000000000000003),Vector(0.10000000000000001,0.10899999999999999,0.11799999999999999,0.127,0.13600000000000001,0.14499999999999999,0.154),Vector(0.13,0.14349999999999999,0.157,0.17050000000000001,0.184,0.19750000000000001,0.21099999999999999),Vector(0.16,0.17799999999999999,0.19600000000000001,0.214,0.23200000000000001,0.25,0.26799999999999996))

object GroupRepairReference:
  val rows = Vector(Vector("OLS","0","-0.313342969289351","0.119801030353568","-2.61552816669927","0.0165607400949199","NA","NA","0.425485363368285","0.294623019939957","0.85111045505591","-0.912053808506763","0.0581812790201233","0.0576920277766838","0.200151433359288","0.0846141678845394"),Vector("OLS","1","-0.0928105724546537","0.254243379877521","-0.365046171504502","0.718908788374652","NA","NA","0.818828204796812","-0.231728417319317","-0.147718515419701","0.325292935774064","0.123473103528552","0.122434807869706","0.424764100562515","0.179569340639219"),Vector("OLS","2","0.819069069326848","0.258572217639374","3.16766076728781","0.00484027290986804","NA","NA","-0.246032289362869","0.887564104471148","0.156084336864364","0.0477356664394104","0.125575400286033","0.12451942623785","0.431996284461582","0.182626751782057"),Vector("OLS","3","0.158047335585124","0.259937402527157","0.608020754414563","0.550016639546797","NA","NA","1.00650890669012","-0.015241097910506","-0.077756476653611","0.672050975844124","0.126238401285572","0.125176852006509","0.43427709716648","0.183590967055894"),Vector("OLS","4","0.362454546137049","0.262399586764522","1.38130761029863","0.182423360375306","NA","NA","-0.0367482864640316","0.762501461453305","0.618150339073928","-0.454858728896459","0.127434159182564","0.126362554675293","0.438390665329031","0.185329981067778"),Vector("OLS","5","-0.237532072809739","0.26430370614354","-0.898708823555953","0.379500633200708","NA","NA","0.493625395742263","-0.0701871794335649","0.653969214574056","0.79819856955427","0.12835889330673","0.127279512632835","0.441571875222409","0.186674839925276"),Vector("OLS","6","0.0158047549733892","0.251680126153355","0.0627969924163142","0.950551528557824","NA","NA","-0.723487307087367","0.00343383583197733","0.224933653087926","0.624188728426874","0.122228261312379","0.121200433635888","0.42048167573327","0.177758942345422"),Vector("FE","0","-0.299704362011276","0.14219615243282","-2.10768264037854","0.0350584498809226","0","14.8114302346161","0.429213650708524","0.260892992754199","0.768728405244748","-0.881165760715507","0.0694126562412934","0.0690491195149677","0.235211833907951","0.0977336644448698"),Vector("FE","1","-0.0624901409573286","0.148520662331081","-0.420750486676568","0.673937290605708","0","55.1068422070783","0.928695361941059","-0.215934057155734","-0.18891564792482","0.294930461179978","0.0724641280482775","0.0721174923804419","0.245430919446356","0.101994726103706"),Vector("FE","2","0.593740909160902","0.154556218665431","3.8415853744855","0.000122242202909891","0","53.8121792453027","-0.233778675419105","0.855935770897948","0.541302642829568","0.0422822983886894","0.0753722229162775","0.0750437700665769","0.255178473500606","0.106068760396095"),Vector("FE","3","0.0274256617912094","0.16034158418938","0.17104522154912","0.864188207849823","0","41.6670781392728","1.00371049666884","-0.128954534950502","-0.0746280552623904","0.59533084555258","0.0781568900853353","0.0778474439325216","0.264519015352543","0.109979838802247"),Vector("FE","4","0.445244898176262","0.165907293806709","2.68369694882129","0.0072813074550085","0","49.498747744571","-0.0408370334784642","0.80107045650192","0.519133297957105","-0.481294546735527","0.0808337109880315","0.080543766542611","0.273503125176746","0.113747097889474"),Vector("FE","5","-0.260211627448578","0.171277944556891","-1.51923604712659","0.128703090861228","0","51.0559384891778","0.383781908361865","-0.0495783868125007","0.751396038432972","0.825323892902046","0.0834151433897469","0.0831449583064507","0.282171373253458","0.117386076115873"),Vector("FE","6","0.018449374923809","0.176473721019001","0.104544601979705","0.916737174913045","0","42.2961245380484","-0.729568358675281","0.0333559071589252","0.306034328461574","0.690553159978353","0.0859113431186811","0.0856610032769279","0.290556926308704","0.120909613472114"),Vector("DL","0","-0.299704362011276","0.14219615243282","-2.10768264037854","0.0350584498809226","0","14.8114302346161","0.429213650708524","0.260892992754199","0.768728405244748","-0.881165760715507","0.0694126562412934","0.0690491195149677","0.235211833907951","0.0977336644448698"),Vector("DL","1","-0.0944953274233589","0.252716471677018","-0.373918355207681","0.708465059916481","0.223141350917879","55.1068422070783","0.85724863443882","-0.225852547781185","-0.136663129063565","0.315128279130217","0.123345681009624","0.122356199318552","0.421376390252658","0.176281543460277"),Vector("DL","2","0.735192261701475","0.260228267283857","2.82518217323227","0.00472537414237515","0.233024591553296","53.8121792453027","-0.239506686654278","0.87707255684986","0.301745229767233","0.0449615986761569","0.127037682519981","0.126024567141678","0.4338179770665","0.1813901724194"),Vector("DL","3","0.0907191524464104","0.237557700672224","0.381882600268061","0.702548446938257","0.160880076254432","41.6670781392728","1.00593007444251","-0.0789864555776079","-0.0875228119153178","0.629721010331797","0.116066674797442","0.115177698014444","0.395553134294509","0.164972901524607"),Vector("DL","4","0.389733112881061","0.268470721747509","1.45167826995897","0.146591084157089","0.234709276686693","49.498747744571","-0.0361140825331113","0.775593523072551","0.583908917243595","-0.469529757848464","0.131119199545656","0.130090965362059","0.447325852360386","0.186805259170588"),Vector("DL","5","-0.243974422695628","0.281786821629887","-0.865812039343972","0.386593245840358","0.263557642197856","51.0559384891778","0.456876165712114","-0.0607085174425999","0.690085057786095","0.808883118200097","0.137619809732581","0.136539552894229","0.469526710558114","0.196088756544302"),Vector("DL","6","0.0200259750407097","0.264235561912274","0.0757883416440304","0.93958749146824","0.201004841320238","42.2961245380484","-0.724782961638486","0.0158785605856877","0.252495678716005","0.651976269065122","0.129117528874522","0.128140182419053","0.439835943359681","0.183350044013495"))

// R metafor 5.0.1: tools/group-repair/pm-reference.R; audit fixture seeds unchanged.
object GroupRepairMetaReference:
  val rows = Vector(
    Vector("DL","z","0","-0.299704362011276","0.14219615243282","-2.10768264037854","0.0350584498809226","0","14.8114302346161","0.429213650708524","0.260892992754199","0.768728405244748","-0.881165760715507","0.0694126562412934","0.0690491195149677","0.235211833907951","0.0977336644448698"),
    Vector("DL","z","1","-0.0944953274233589","0.252716471677018","-0.373918355207681","0.708465059916481","0.223141350917879","55.1068422070783","0.85724863443882","-0.225852547781185","-0.136663129063565","0.315128279130217","0.123345681009624","0.122356199318552","0.421376390252658","0.176281543460277"),
    Vector("DL","z","2","0.735192261701475","0.260228267283857","2.82518217323227","0.00472537414237515","0.233024591553296","53.8121792453027","-0.239506686654278","0.87707255684986","0.301745229767233","0.0449615986761569","0.127037682519981","0.126024567141678","0.4338179770665","0.1813901724194"),
    Vector("DL","z","3","0.0907191524464104","0.237557700672224","0.381882600268061","0.702548446938257","0.160880076254432","41.6670781392728","1.00593007444251","-0.0789864555776079","-0.0875228119153178","0.629721010331797","0.116066674797442","0.115177698014444","0.395553134294509","0.164972901524607"),
    Vector("DL","z","4","0.389733112881061","0.268470721747509","1.45167826995897","0.146591084157089","0.234709276686693","49.498747744571","-0.0361140825331113","0.775593523072551","0.583908917243595","-0.469529757848464","0.131119199545656","0.130090965362059","0.447325852360386","0.186805259170588"),
    Vector("DL","z","5","-0.243974422695628","0.281786821629887","-0.865812039343972","0.386593245840358","0.263557642197856","51.0559384891778","0.456876165712114","-0.0607085174425999","0.690085057786095","0.808883118200097","0.137619809732581","0.136539552894229","0.469526710558114","0.196088756544302"),
    Vector("DL","z","6","0.0200259750407097","0.264235561912274","0.0757883416440304","0.93958749146824","0.201004841320238","42.2961245380484","-0.724782961638486","0.0158785605856877","0.252495678716005","0.651976269065122","0.129117528874522","0.128140182419053","0.439835943359681","0.183350044013495"),
    Vector("DL","adhoc","0","-0.299704362011276","0.14219615243282","-2.10768264037854","0.0478756543857175","0","14.8114302346161","0.429213650708524","0.260892992754199","0.768728405244748","-0.881165760715507","0.0694126562412934","0.0690491195149677","0.235211833907951","0.0977336644448698"),
    Vector("DL","adhoc","1","-0.0944953274233589","0.252716471677018","-0.373918355207681","0.712399670713705","0.223141350917879","55.1068422070783","0.85724863443882","-0.225852547781185","-0.136663129063565","0.315128279130217","0.123345681009624","0.122356199318552","0.421376390252658","0.176281543460277"),
    Vector("DL","adhoc","2","0.735192261701475","0.260228267283857","2.82518217323227","0.0104574445170029","0.233024591553296","53.8121792453027","-0.239506686654278","0.87707255684986","0.301745229767233","0.0449615986761569","0.127037682519981","0.126024567141678","0.4338179770665","0.1813901724194"),
    Vector("DL","adhoc","3","0.0907191524464104","0.247535549544085","0.366489389558382","0.7178484529089","0.160880076254432","41.6670781392728","1.00593007444251","-0.0789864555776079","-0.0875228119153178","0.629721010331797","0.120941683003496","0.120015367603538","0.412167074333555","0.171902058839681"),
    Vector("DL","adhoc","4","0.389733112881061","0.268470721747509","1.45167826995897","0.162097982473722","0.234709276686693","49.498747744571","-0.0361140825331113","0.775593523072551","0.583908917243595","-0.469529757848464","0.131119199545656","0.130090965362059","0.447325852360386","0.186805259170588"),
    Vector("DL","adhoc","5","-0.243974422695628","0.281786821629887","-0.865812039343972","0.396860878132421","0.263557642197856","51.0559384891778","0.456876165712114","-0.0607085174425999","0.690085057786095","0.808883118200097","0.137619809732581","0.136539552894229","0.469526710558114","0.196088756544302"),
    Vector("DL","adhoc","6","0.0200259750407097","0.264235561912274","0.0757883416440304","0.940340486192085","0.201004841320238","42.2961245380484","-0.724782961638486","0.0158785605856877","0.252495678716005","0.651976269065122","0.129117528874522","0.128140182419053","0.439835943359681","0.183350044013495"),
    Vector("PM","z","0","-0.299704362011276","0.14219615243282","-2.10768264037854","0.0350584498809226","0","14.8114302346161","0.429213650708524","0.260892992754199","0.768728405244748","-0.881165760715507","0.0694126562412934","0.0690491195149677","0.235211833907951","0.0977336644448698"),
    Vector("PM","z","1","-0.0944384334821539","0.251458722875817","-0.375562368257124","0.707242271772223","0.219677322043583","55.1068422070783","0.857637166697045","-0.225796021393294","-0.136706595768433","0.315021450134616","0.122735681647727","0.121751946148915","0.419267208303319","0.175384824881111"),
    Vector("PM","z","2","0.733291153273765","0.25727981811108","2.85016974381243","0.00436959025487728","0.224705851932296","53.8121792453027","-0.239390146725242","0.876817818352962","0.305020848310672","0.0449187953806961","0.125607246001074","0.12460783681351","0.428871174047012","0.179287367957707"),
    Vector("PM","z","3","0.0961223347752627","0.248695093810711","0.386506759350968","0.699121397576954","0.190143815476293","41.6670781392728","1.00605576303182","-0.0742194896969777","-0.0874908803559304","0.632981921471376","0.121479622248429","0.120534453040601","0.414280017115562","0.17292248192555"),
    Vector("PM","z","4","0.39151642230744","0.260645291625922","1.50210433445828","0.133070165654868","0.21226536696409","49.498747744571","-0.036171517859932","0.776471790921582","0.581820784734264","-0.470224881235055","0.127318503654181","0.12632860435369","0.434178000437302","0.181220714174235"),
    Vector("PM","z","5","-0.244785262750114","0.268480257216917","-0.911743996700439","0.361903497544966","0.223886929267651","51.0559384891778","0.452980167619615","-0.0598690572292228","0.693796564695761","0.809910384134946","0.131156325914055","0.13014155803933","0.447167053279365","0.186592349880536"),
    Vector("PM","z","6","0.0200937805977361","0.253228705644982","0.0793503269961302","0.93675397730912","0.170470491710179","42.2961245380484","-0.725023447118074","0.0170243865707051","0.255669621103266","0.65452102289332","0.123757859915413","0.122840511637345","0.421291960951515","0.175492487180969"),
    Vector("PM","adhoc","0","-0.299704362011276","0.14219615243282","-2.10768264037854","0.0478756543857175","0","14.8114302346161","0.429213650708524","0.260892992754199","0.768728405244748","-0.881165760715507","0.0694126562412934","0.0690491195149677","0.235211833907951","0.0977336644448698"),
    Vector("PM","adhoc","1","-0.0944384334821539","0.251458722875817","-0.375562368257124","0.711195988130819","0.219677322043583","55.1068422070783","0.857637166697045","-0.225796021393294","-0.136706595768433","0.315021450134616","0.122735681647727","0.121751946148915","0.419267208303319","0.175384824881111"),
    Vector("PM","adhoc","2","0.733291153273765","0.25727981811108","2.85016974381243","0.00989326933556025","0.224705851932296","53.8121792453027","-0.239390146725242","0.876817818352962","0.305020848310672","0.0449187953806961","0.125607246001074","0.12460783681351","0.428871174047012","0.179287367957707"),
    Vector("PM","adhoc","3","0.0961223347752627","0.248695093810711","0.386506759350968","0.703202808929233","0.190143815476293","41.6670781392728","1.00605576303182","-0.0742194896969777","-0.0874908803559304","0.632981921471376","0.121479622248429","0.120534453040601","0.414280017115562","0.17292248192555"),
    Vector("PM","adhoc","4","0.39151642230744","0.260645291625922","1.50210433445828","0.148695226987589","0.21226536696409","49.498747744571","-0.036171517859932","0.776471790921582","0.581820784734264","-0.470224881235055","0.127318503654181","0.12632860435369","0.434178000437302","0.181220714174235"),
    Vector("PM","adhoc","5","-0.244785262750114","0.268480257216917","-0.911743996700439","0.372762784279327","0.223886929267651","51.0559384891778","0.452980167619615","-0.0598690572292228","0.693796564695761","0.809910384134946","0.131156325914055","0.13014155803933","0.447167053279365","0.186592349880536"),
    Vector("PM","adhoc","6","0.0200937805977361","0.253228705644982","0.0793503269961301","0.937542575629629","0.170470491710179","42.2961245380484","-0.725023447118074","0.0170243865707051","0.255669621103266","0.65452102289332","0.123757859915413","0.122840511637345","0.421291960951515","0.175492487180969")
  )
