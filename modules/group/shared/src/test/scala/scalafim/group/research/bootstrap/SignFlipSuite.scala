package scalafim.group.research.bootstrap

import ResearchTestSupport.*

/** Declaration §4 and §7.3: the sign-flip reference, level <= alpha exactly. */
class SignFlipSuite extends munit.FunSuite:
  val Alpha = 0.05
  val Bound: Int = math.floor(Alpha * 256).toInt

  private def interceptDesign(n: Int): ResearchDesign =
    value(ResearchDesign.of(n, 1, Array.fill(n)(1.0), Vector("intercept"), Array(1.0)))

  /** Fails (None) on exactly the transformed datasets of the listed patterns. */
  private def withInjectedFailures(y: Array[Double], b0: Double, patterns: Vector[Int], base: SignFlip.Statistic): SignFlip.Statistic =
    val failing = patterns.map { g =>
      val out = new Array[Double](y.length)
      SignFlip.transform(y, b0, g.toLong, out)
      out
    }
    data => if failing.exists(_.sameElements(data)) then None else base(data)

  test("§7.3 literal n = 8 data (unequal v, a zero, tied |y - b0|): every orbit point's p is valid and matches R"):
    val y = BootstrapReferenceFixtures.SignFlipY
    val v = BootstrapReferenceFixtures.SignFlipV
    val b0 = BootstrapReferenceFixtures.SignFlipB0
    assert(y.exists(_ == b0), "the literal data contain a zero y_i - b0")
    assert(y.map(a => math.abs(a - b0)).distinct.length < y.length, "the literal data contain tied |y_i - b0|")
    assertEquals(BootstrapReferenceFixtures.SignFlipCases.length, 2)
    BootstrapReferenceFixtures.SignFlipCases.foreach { expected =>
      val base = SignFlip.pmStatistic(interceptDesign(8), v, b0)
      val statistic = withInjectedFailures(y, b0, expected.failingPatterns, base)
      val orbit = SignFlip.enumerate(y, b0, statistic)
      assertEquals(orbit.size, 256)
      (0 until 256).foreach { g =>
        assertEqualsDouble(orbit.statistics(g), expected.statistics(g), 1e-8 * math.max(1.0, expected.statistics(g)), s"${expected.id} S_$g")
      }
      // The zero subject makes each failing pattern fail twice (both of its signs give the same data).
      assertEquals(orbit.failures, 2 * expected.failingPatterns.length, expected.id)
      val rejections = orbit.rejections(Alpha)
      assertEquals(rejections, expected.rejections, expected.id)
      assert(rejections <= Bound, s"${expected.id}: $rejections of 256 orbit points rejected, bound $Bound")
      (0 until 256).foreach { g =>
        val fromR = expected.statistics.count(SignFlip.atLeast(_, expected.statistics(g))) / 256.0
        assertEqualsDouble(orbit.pValue(g), fromR, 0.0, s"${expected.id} p_$g")
      }
    }

  test("level <= alpha for every orbit, over random Model-J data with and without injected failures"):
    val d = interceptDesign(8)
    (0 until 25).foreach { r =>
      val sim = study(if r % 2 == 0 then "C-n8-DI-Vspread-T2-N8" else "C-n8-DI-Vflat-T0-N8", r)
      val base = SignFlip.pmStatistic(d, sim.data.v, 0.0)
      val clean = SignFlip.enumerate(sim.data.y, 0.0, base)
      assert(clean.rejections(Alpha) <= Bound, s"study $r")
      val failing = Vector(r, (r * 37) % 256, (r * 101 + 7) % 256)
      val broken = SignFlip.enumerate(sim.data.y, 0.0, withInjectedFailures(sim.data.y, 0.0, failing, base))
      assert(broken.failures >= 1)
      assert(broken.rejections(Alpha) <= Bound, s"study $r with failures")
    }

  test("structural ties: |T(gy)| = |T(-gy)| exactly, so rejected orbit points come in pairs"):
    val sim = study("C-n8-DI-Vspread-T2-N8", 3)
    val orbit = SignFlip.enumerate(sim.data.y, 0.0, SignFlip.pmStatistic(interceptDesign(8), sim.data.v, 0.0))
    (0 until 256).foreach(g => assertEquals(orbit.statistics(g), orbit.statistics(255 - g)))
    assertEquals(orbit.rejections(Alpha) % 2, 0)

  test("an observed fit failure gives |T| := 0 and p = 1; the failure is counted"):
    val d = interceptDesign(8)
    val y = BootstrapReferenceFixtures.SignFlipY
    val b0 = BootstrapReferenceFixtures.SignFlipB0
    val orbit = SignFlip.enumerate(y, b0, withInjectedFailures(y, b0, Vector(0), SignFlip.pmStatistic(d, BootstrapReferenceFixtures.SignFlipV, b0)))
    assertEquals(orbit.statistics(0), 0.0)
    assertEquals(orbit.pValue(0), 1.0)
    assert(orbit.failures >= 1)

  test("Monte Carlo sign flips (identity included) keep level <= alpha at n = 20"):
    val c = cell("C-n20-DI-Vspread-T2-N8")
    val d = c.researchDesign
    val studies = 300
    val rejections = (0 until studies).count { r =>
      val sim = ModelJ.draw(c, Phase.Harness, StudyPurpose.Null, r, d)
      val key = StreamKey.of(Phase.Harness, StudyPurpose.Null, c.id, r, StreamKind.SignFlip)
      val mc = SignFlip.monteCarlo(sim.data.y, 0.0, 99, key.lane(Lane.Signs), SignFlip.pmStatistic(d, sim.data.v, 0.0))
      assert(mc.pValue >= 1.0 / 100.0 && mc.pValue <= 1.0)
      mc.pValue <= Alpha
    }
    println(s"SIGNFLIP_MC,n20,rejections=$rejections/$studies")
    assert(ClopperPearson.lower(rejections, studies, 0.0005) <= Alpha, s"Monte Carlo sign-flip level exceeds alpha: $rejections/$studies")

  test("heavy: Monte Carlo sign flips at n = 80, B = 999 (opt-in)"):
    assume(heavy, howToEnableHeavy)
    val c = cell("C-n80-DI-Vspread-T2-N8")
    val d = c.researchDesign
    val studies = 1000
    val rejections = (0 until studies).count { r =>
      val sim = ModelJ.draw(c, Phase.Harness, StudyPurpose.Null, r, d)
      val key = StreamKey.of(Phase.Harness, StudyPurpose.Null, c.id, r, StreamKind.SignFlip)
      SignFlip.monteCarlo(sim.data.y, 0.0, 999, key.lane(Lane.Signs), SignFlip.pmStatistic(d, sim.data.v, 0.0)).pValue <= Alpha
    }
    println(s"SIGNFLIP_MC_HEAVY,n80,rejections=$rejections/$studies")
    assert(ClopperPearson.lower(rejections, studies, 0.0005) <= Alpha)
