package scalafim.group.research.bootstrap

import ResearchTestSupport.*

/** Declaration §7.5: the independent R reference (metafor PM + adhoc on explicit
  * pre-drawn variates, explicit uniroot restricted fit, explicit Smyth fit,
  * sandwich HC3) against the harness.
  */
class ReferenceParitySuite extends munit.FunSuite:
  /** Relative tolerance on every T* (task contract; metafor tol 1e-10, harness root 1e-12). */
  val StatisticTolerance = 1e-8

  private val fixtures = BootstrapReferenceFixtures.Cases

  private def design(c: BootstrapReferenceFixtures.Case): ResearchDesign =
    value(ResearchDesign.of(c.n, c.p, c.designRowMajor, c.terms, c.contrast))

  private def data(c: BootstrapReferenceFixtures.Case): StudyData = StudyData(design(c), c.y, c.v, c.nu, c.b0)

  private def variates(c: BootstrapReferenceFixtures.Case): MatrixDrawVariates =
    MatrixDrawVariates(c.n, c.draws, c.zu, c.ze, c.chi, c.post)

  private def scheme(code: String): Scheme = Scheme.values.find(_.code == code).getOrElse(fail(s"unknown scheme $code"))

  test("the fixture covers several studies for every candidate, with interior and boundary tau^2"):
    assert(fixtures.length >= 3, "several fixture studies")
    fixtures.foreach(c => assertEquals(c.expected.map(_.scheme).toSet, Set("B-plug", "B-fixV", "B-EB", "C-uncentred", "C-omit-u"), c.id))
    assert(fixtures.exists(_.tau2Hat > 0.0) && fixtures.exists(_.restrictedTau2 > 0.0))
    assert(fixtures.exists(_.b0 != 0.0), "one fixture tests a non-zero b0")
    assert(fixtures.exists(_.p > 1) && fixtures.exists(_.p == 1))
    println(s"REFERENCE_ORACLE,${BootstrapReferenceFixtures.Oracle},inputs=${BootstrapReferenceFixtures.InputSha256}")

  test("observed statistic and tau^2 match metafor PM + adhoc"):
    fixtures.foreach { c =>
      val f = new StudyFitter(design(c))
      assert(f.fitFull(c.y, c.v, TauPolicy.PauleMandel).ok)
      assertEqualsDouble(f.mkhStatistic(c.b0), c.observedStatistic, StatisticTolerance * math.max(1.0, math.abs(c.observedStatistic)), c.id)
      assertEqualsDouble(f.full.tau2, c.tau2Hat, 1e-9, c.id)
    }

  test("Smyth EB hyperparameters match the explicit R moment fit"):
    fixtures.foreach { c =>
      val fit = value(SmythFit.fit(c.v, c.nu))
      if c.d0.isInfinite then assert(fit.d0.isInfinite, c.id)
      else assertEqualsDouble(fit.d0, c.d0, 1e-8 * c.d0, s"${c.id} d0")
      assertEqualsDouble(fit.s0Squared, c.s0Squared, 1e-10 * c.s0Squared, s"${c.id} s0^2")
    }

  test("every T*, k, f and p of every scheme matches the R reference on the same explicit variates"):
    var compared = 0
    fixtures.foreach { c =>
      val d = data(c)
      val engine = new BootstrapEngine(d.design)
      c.expected.foreach { e =>
        val s = scheme(e.scheme)
        val sink = new Array[Double](c.draws)
        val out = value(engine.run(d, s, c.draws, BootstrapEngine.fixedVariates(variates(c)), sink = Some(sink)))
        val label = s"${c.id} ${e.scheme}"
        (0 until c.draws).foreach { b =>
          val expected = e.statistics(b)
          if expected.isNaN then assert(sink(b).isNaN, s"$label draw $b failed in R only")
          else assertEqualsDouble(sink(b), expected, StatisticTolerance * math.max(1.0, math.abs(expected)), s"$label T*_$b")
          compared += 1
        }
        assertEquals(out.failed, e.failed, s"$label f")
        assertEquals(out.nearTies, e.nearTies, s"$label near ties")
        if e.nearTies == 0 then
          assertEquals(out.exceed, e.exceed, s"$label k")
          assertEqualsDouble(out.pLower, (1.0 + e.exceed) / (c.draws + 1.0), 0.0, s"$label p")
        else assert(math.abs(out.exceed - e.exceed) <= e.nearTies, s"$label k within the flagged ties")
      }
    }
    println(s"REFERENCE_PARITY,T*=$compared")

  test("HC3 baselines match sandwich::vcovHC(type = \"HC3\")"):
    fixtures.foreach { c =>
      val d = design(c)
      def check(label: String, got: BaselineResult, want: BootstrapReferenceFixtures.BaselineExpected): Unit =
        assertEqualsDouble(got.estimate, want.estimate, 1e-10 * math.max(1.0, math.abs(want.estimate)), s"${c.id} $label estimate")
        assertEqualsDouble(got.standardError, want.standardError, 1e-10 * want.standardError, s"${c.id} $label se")
        assertEqualsDouble(got.statistic, want.statistic, 1e-9 * math.max(1.0, math.abs(want.statistic)), s"${c.id} $label t")
        assertEqualsDouble(got.pValue, want.pValue, 1e-7, s"${c.id} $label p")
      check("equal", value(Hc3.equalWeight(d, c.y, c.b0)), c.hc3Equal)
      check("inverse-v", value(Hc3.inverseVariance(d, c.y, c.v, c.b0)), c.hc3InverseVariance)
    }
