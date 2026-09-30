package scalafim.fmri.fit

import gale.linalg.Matrix
import scalafim.fmri.ar.{ArmaCoefficients, InitialConditionPolicy, NoiseEstimationLayout, TimeSegment, WhiteningPlan, WhiteningTransform}

class VoxelwiseReducedRankBootstrapSuite extends munit.FunSuite:
  test("boundary seeds remain inside the Park-Miller state space") {
    Vector(0, 1, Int.MaxValue - 1, Int.MaxValue).foreach { seed =>
      val rng = new VoxelwiseReducedRankBootstrap.BootstrapRng(seed)
      for _ <- 0 until 50 do
        val value = rng.nextInt(7)
        assert(value >= 0 && value < 7)
    }
  }

  test("inverse whitening round trips ARMA recurrence, resets and initial scales") {
    val segments = Vector(TimeSegment(0, 4, 0), TimeSegment(4, 5, 0), TimeSegment(5, 11, 1))
    val configurations = Vector(
      (ArmaCoefficients.ar(0.7), InitialConditionPolicy.ExactAr1),
      (ArmaCoefficients(Vector(0.35, -0.1), Vector(0.2)), InitialConditionPolicy.Identity),
      (ArmaCoefficients(Vector(-0.25), Vector(0.3)), InitialConditionPolicy.PrecomputedScale(0.8))
    )
    val innovations = Matrix.tabulate(11, 3)((r, c) => math.sin((r + 1) * (c + 0.3)))
    configurations.foreach { case (coefficients, initial) =>
      val w = WhiteningPlan.globalWithInitialCondition(coefficients, segments, initial).toOption.get
      val raw = VoxelwiseReducedRankBootstrap.inverseWhiten(w, innovations).fold(e => fail(e.message), identity)
      val actual = WhiteningTransform.matrix(w, raw).toOption.get
      for r <- 0 until 11; c <- 0 until 3 do assertEqualsDouble(actual(r, c), innovations(r, c), 1e-12)
    }
    val zero = WhiteningPlan.globalWithInitialCondition(ArmaCoefficients.ar(0.3), segments, InitialConditionPolicy.PrecomputedScale(0.0)).toOption.get
    assert(VoxelwiseReducedRankBootstrap.inverseWhiten(zero, innovations).isLeft)
  }

  test("joint block donors respect runs, excluded rows and whitening resets") {
    val segments = Vector(TimeSegment(0, 5, 0), TimeSegment(5, 6, 0), TimeSegment(6, 12, 0), TimeSegment(12, 20, 1))
    val layout = NoiseEstimationLayout.excludingRows(segments, 20, Set(5, 15)).toOption.get
    val donors = VoxelwiseReducedRankBootstrap.donorStarts(layout, 3).toOption.get
    assertEquals(donors, Vector(0 -> Vector(0, 1, 2, 6, 7, 8, 9), 1 -> Vector(12, 16, 17)))
    val rng = new VoxelwiseReducedRankBootstrap.BootstrapRng(11)
    for _ <- 0 until 20 do
      val indices = VoxelwiseReducedRankBootstrap.sampleIndices(layout, donors, 3, rng)
      assertEquals(indices.length, 20)
      assert(!indices.exists(Set(5, 15)))
      segments.foreach { s =>
        for target <- s.start until s.endExclusive by 3 do
          val source = indices(target)
          val count = math.min(3, s.endExclusive - target)
          assert(layout.estimationSegments.exists(d => d.runIndex == s.runIndex && source >= d.start && source + count <= d.endExclusive))
          assertEquals(indices.slice(target, target + count), (source until source + count).toVector)
      }
    val repeated = VoxelwiseReducedRankBootstrap.sampleIndices(layout, donors, 3, new VoxelwiseReducedRankBootstrap.BootstrapRng(11))
    val again = VoxelwiseReducedRankBootstrap.sampleIndices(layout, donors, 3, new VoxelwiseReducedRankBootstrap.BootstrapRng(11))
    assertEquals(repeated, again)
    assert(VoxelwiseReducedRankBootstrap.donorStarts(layout, 9).isLeft)
  }

  test("bootstrap refuses unit leverage and preserves the identity of a failed replicate") {
    import scalafim.fmri.model.*
    def checked[A](x: Either[FitError, A]): A = x.fold(e => fail(e.message), identity)
    val x = Matrix.tabulate(6, 3)((r, c) => if c == 0 then (if r == 0 then 1.0 else 0.0) else if c == 1 then r.toDouble else 1.0)
    val y = Matrix.tabulate(6, 2)((r, v) => math.sin((r + 1) * (v + 0.5)))
    val plans = Vector.fill(2)(WhiteningPlan.global(ArmaCoefficients.ar(0.0), Vector(TimeSegment(0, 6, 0))))
    val partition = checked(ReducedRankDesignPartition.fromColumns(3, Vector(0, 1), Vector(2)))
    val gs = checked(VoxelwiseReducedRankGls.geometry(x, y, plans, partition))
    val config = ReducedRankGlsConfig.unsafe(components = ReducedRankComponentSpec.unsafeFixed(1), inference = ReducedRankInferencePolicy.EstimatesOnly)
    val fitted = checked(VoxelwiseReducedRankGls.solve(gs, partition, 1, config.solver))
    val bootstrap = VoxelwiseReducedRankBootstrapConfig.unsafe(resampling = ReducedRankBootstrapConfig.unsafe(replicates = 2, seed = 19))
    val rows = (0 until 6).toVector
    val highLeverage = VoxelwiseReducedRankBootstrap.run(DesignMatrix.unsafe(x), y, Vector(RunPartition(0, rows, rows)), Vector(0, 1), plans, gs, partition, 1, fitted, config, bootstrap)
    assert(highLeverage.left.toOption.exists(_.message.contains("residual leverage")))

    val nuisanceContrast = VoxelwiseReducedRankBootstrapConfig.unsafe(
      resampling = ReducedRankBootstrapConfig.unsafe(replicates = 2, seed = 19),
      contrasts = Vector(VoxelwiseBootstrapContrast.unsafe("nuisance", Vector(2 -> 1.0)))
    )
    val rejected = VoxelwiseReducedRankBootstrap.run(DesignMatrix.unsafe(x), y, Vector(RunPartition(0, rows, rows)), Vector(0, 1), plans, gs, partition, 1, fitted, config, nuisanceContrast)
    assert(rejected.left.toOption.exists(_.message.contains("target columns only")))

    import VoxelwiseReducedRankFixtures.{design as oracleX, response as oracleY, plans as oraclePlans, partition as oraclePartition}
    val oracleGs = checked(VoxelwiseReducedRankGls.geometry(oracleX, oracleY, oraclePlans, oraclePartition))
    val oracleFit = checked(VoxelwiseReducedRankGls.solve(oracleGs, oraclePartition, 1, config.solver))
    val exhausted = ReducedRankGlsConfig.unsafe(components = ReducedRankComponentSpec.unsafeFixed(1), solver = ReducedRankSolverConfig.unsafe(maxIterations = 1))
    val oracleRows = (0 until 12).toVector
    val times = (0 until 6).toVector ++ (7 until 13).toVector
    val failure = VoxelwiseReducedRankBootstrap.run(DesignMatrix.unsafe(oracleX), oracleY, Vector(RunPartition(0, oracleRows, times)), Vector(0, 1, 2, 3), oraclePlans, oracleGs, oraclePartition, 1, oracleFit, exhausted, bootstrap)
    assert(failure.left.toOption.exists {
      case FitError.ReducedRankBootstrapFailed(1, error) => error.message.contains("no start converged")
      case _ => false
    })
  }

  test("frozen bootstrap matches independent base-R circle-search covariance and percentile oracle") {
    import VoxelwiseReducedRankFixtures.*
    import scalafim.fmri.model.*
    def checked[A](x: Either[FitError, A]): A = x.fold(e => fail(e.message), identity)
    val gs = checked(VoxelwiseReducedRankGls.geometry(design, response, plans, partition))
    val config = ReducedRankGlsConfig.unsafe(components = ReducedRankComponentSpec.unsafeFixed(1), inference = ReducedRankInferencePolicy.EstimatesOnly)
    val fit = checked(VoxelwiseReducedRankGls.solve(gs, partition, 1, config.solver))
    val rows = (0 until 12).toVector
    val times = (0 until 6).toVector ++ (7 until 13).toVector
    val runs = Vector(RunPartition(0, rows, times))
    val difference = VoxelwiseBootstrapContrast.unsafe("difference", Vector(0 -> 1.0, 1 -> -1.0))
    val unit = VoxelwiseBootstrapContrast.unsafe("task_a", Vector(0 -> 1.0))
    val negated = VoxelwiseBootstrapContrast.unsafe("negative_difference", Vector(0 -> -1.0, 1 -> 1.0))
    val reordered = VoxelwiseBootstrapContrast.unsafe("reordered_difference", Vector(1 -> -1.0, 0 -> 1.0))
    val bootstrap = VoxelwiseReducedRankBootstrapConfig.unsafe(
      resampling = ReducedRankBootstrapConfig.unsafe(replicates = 32, blockSize = 2, seed = 19),
      contrasts = Vector(difference, unit, negated, reordered)
    )
    val actual = checked(VoxelwiseReducedRankBootstrap.run(DesignMatrix.unsafe(design), response, runs, Vector(0, 1, 2, 3), plans, gs, partition, 1, fit, config, bootstrap)).bootstrap.get
    // tools/r-parity/qualify_voxelwise_ar_rrg_bootstrap.R, R 4.5.1; independent
    // 128/256 grid refinement error < 2.5e-8. Compare original coefficient axes.
    val covariance = Vector(
      Matrix.dense(2, 2, Seq(0.0004333776338790191, -0.0004917154008722767, -0.0004917154008722767, 0.0010760262929619953)),
      Matrix.dense(2, 2, Seq(0.0014939830320472324, 9.487140095991573e-05, 9.487140095991573e-05, 0.0003420199188410644)),
      Matrix.dense(2, 2, Seq(0.0009808775826806043, -0.0009611803528124384, -0.0009611803528124384, 0.002266705615434213)),
      Matrix.dense(2, 2, Seq(0.00044934527926402356, -0.000648194631024913, -0.000648194631024913, 0.0009387851114155585))
    )
    val lower = Matrix.dense(2, 4, Seq(0.3199813275150321, -0.9853768855106516, 0.5233572114892051, -0.0529044837149697, -0.5770532503616257, 1.3093926582971667, -0.9387845849915387, -0.03110890185577432))
    val upper = Matrix.dense(2, 4, Seq(0.39074656336713054, -0.8448241942844806, 0.6328924369878387, 0.02037468067839446, -0.4609012691968693, 1.3685933093605707, -0.7777903302183365, 0.07509024443404891))
    val objectives = Vector(0.1748026753006648, 0.20133517976511528, 0.14279948389142233, 0.1271648597593039, 0.15813289713125925, 0.14602790755568743, 0.1802896135730286, 0.18051160948000322, 0.21497409933638376, 0.28661299024874914, 0.1874873144892565, 0.17892190848642928, 0.09169967562564704, 0.11604397984588412, 0.17140127027563215, 0.2006181111646813, 0.18950753560383513, 0.212219617257587, 0.1791433942268513, 0.2269793457534473, 0.2118047146209619, 0.10993452101153732, 0.08194104330935156, 0.1827809930187038, 0.17646298244940703, 0.19383195381195928, 0.24022593413906132, 0.2225408184672097, 0.14662380469604408, 0.1347627230463115, 0.19691684969114465, 0.17076997841708413)
    for v <- 0 until 4; a <- 0 until 2; b <- 0 until 2 do
      assertEqualsDouble(checked(actual.covariance.matrixForVoxelPosition(v))(a, b), covariance(v)(a, b), 1e-8)
    for r <- 0 until 2; v <- 0 until 4 do
      assertEqualsDouble(actual.lower(r, v), lower(r, v), 2e-6)
      assertEqualsDouble(actual.upper(r, v), upper(r, v), 2e-6)
    actual.diagnostics.replicateObjectives.zip(objectives).foreach { case (a, e) => assertEqualsDouble(a, e, 1e-8) }
    assertEquals(actual.diagnostics.donorBlocksByRun, Vector(0 -> 10))
    assertEquals(actual.diagnostics.refittedWhiteningReplicates, 0)
    val contrast = actual.contrasts.head
    val unitContrast = actual.contrasts(1)
    val negatedContrast = actual.contrasts(2)
    val reorderedContrast = actual.contrasts(3)
    assertEquals(contrast.definition, difference)
    val expectedContrastLower = Vector(0.7847589652809042, -2.315952335516221, 1.3106680744944792, -0.12799472814901863)
    val expectedContrastUpper = Vector(0.9584850307126647, -2.1715032662542857, 1.5488670946865852, 0.05148358253416878)
    for v <- 0 until 4 do
      assertEqualsDouble(contrast.estimate(v), fit.targetCoefficients(0, v) - fit.targetCoefficients(1, v), 1e-12)
      val c = Vector(1.0, -1.0)
      val expectedVariance = (for a <- 0 until 2; b <- 0 until 2 yield c(a) * covariance(v)(a, b) * c(b)).sum
      assertEqualsDouble(contrast.standardErrors(v) * contrast.standardErrors(v), expectedVariance, 1e-8)
      assertEqualsDouble(contrast.lower(v), expectedContrastLower(v), 2e-6)
      assertEqualsDouble(contrast.upper(v), expectedContrastUpper(v), 2e-6)
      assertEqualsDouble(unitContrast.estimate(v), fit.targetCoefficients(0, v), 1e-12)
      assertEqualsDouble(unitContrast.lower(v), actual.lower(0, v), 1e-12)
      assertEqualsDouble(unitContrast.upper(v), actual.upper(0, v), 1e-12)
      assertEqualsDouble(negatedContrast.estimate(v), -contrast.estimate(v), 1e-12)
      assertEqualsDouble(negatedContrast.standardErrors(v), contrast.standardErrors(v), 1e-12)
      assertEqualsDouble(negatedContrast.lower(v), -contrast.upper(v), 1e-12)
      assertEqualsDouble(negatedContrast.upper(v), -contrast.lower(v), 1e-12)
      assertEqualsDouble(reorderedContrast.estimate(v), contrast.estimate(v), 1e-12)
      assertEqualsDouble(reorderedContrast.standardErrors(v), contrast.standardErrors(v), 1e-12)
      assertEqualsDouble(reorderedContrast.lower(v), contrast.lower(v), 1e-12)
      assertEqualsDouble(reorderedContrast.upper(v), contrast.upper(v), 1e-12)
      assert(contrast.lower(v) <= contrast.upper(v))
    assert((0 until 4).exists(v => math.abs(contrast.lower(v) - (actual.lower(0, v) - actual.upper(1, v))) > 1e-4))
  }
