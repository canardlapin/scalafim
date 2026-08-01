package scalafim.fmri.design

import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.design.SamplingFrame

/** How much of the `DesignError` ADT survives a realistic user mistake.
  *
  * `buildEither` is total, but it used to reach totality by catching `NonFatal`
  * and stringifying into `DesignError.BuildFailed`: where a failing step called
  * a throwing accessor instead of its `*Either` twin, the case that existed for
  * exactly that mistake was never constructed.
  *
  * These began as characterization tests pinning that loss. Phase 3 of
  * docs/plans/design-hardening.md flipped them: `catchBuild` is now a backstop
  * for genuinely unexpected failures, so no row below reports `BuildFailed`.
  */
class ErrorFidelityProbeSuite extends munit.FunSuite:

  private val table = DataTable.fromColumns(
    "onset" -> Column.Doubles(Vector(1.0, 3.0)),
    "cond" -> Column.Strings(Vector("A", "B")),
    "dur" -> Column.Doubles(Vector(0.5, 0.25)),
    "rt" -> Column.Doubles(Vector(1.2, 1.4)),
    "listcol" -> Column.DoubleLists(Vector(Vector(1.0), Vector(2.0))),
    "hrfcol" -> Column.Hrfs(Vector(scalafim.fmri.hrf.Hrfs.SPMG1, scalafim.fmri.hrf.Hrfs.SPMG1))
  )

  private val sf = SamplingFrame(blockLens = Seq(10), tr = Seq(1.0))

  private def errorFor(formula: String): DesignError =
    EventModelBuilder
      .buildEither(formula = formula, data = table, samplingFrame = sf, blockIds = Seq(0, 0))
      .fold(identity, _ => fail(s"expected a failure for '$formula'"))

  test("a missing column reports the MissingColumn case that exists for it"):
    assertEquals(errorFor("onset ~ hrf(missing)"), DesignError.MissingColumn("missing"))

  test("column-shape mistakes name the column, what was expected, and what was found"):
    assertEquals(
      errorFor("onset ~ hrf(listcol)"),
      DesignError.InvalidColumnType("listcol", "usable as an event variable", "numeric-list")
    )
    assertEquals(
      errorFor("onset ~ hrf(hrfcol)"),
      DesignError.InvalidColumnType("hrfcol", "usable as an event variable", "HRF")
    )

  test("an unknown basis call reports the call and what the formula grammar knows"):
    errorFor("onset ~ hrf(BogusCall(cond))") match
      case DesignError.UnknownBasisFunction(name, known) =>
        assertEquals(name, "BogusCall")
        assert(known.contains("scale"), s"known basis calls should be listed, got $known")
      case other =>
        fail(s"expected UnknownBasisFunction, got $other")

  test("the paths that were already structured stay structured"):
    assert(errorFor("onset ~ hrf(cond, basis = nope)").isInstanceOf[DesignError.UnknownBasis])
    assert(errorFor("onset ~ hrf(cond, contrasts = nosuch)").isInstanceOf[DesignError.UnknownContrast])
    assert(errorFor("""onset ~ hrf(cond, subset = rt < "fast")""").isInstanceOf[DesignError.InvalidSubset])
    assert(errorFor("onset ~ hrf(").isInstanceOf[DesignError.FormulaParse])

  test("no realistic user mistake degrades to the stringly catch-all"):
    // The table this suite exists to hold flat: BuildFailed is a backstop for
    // the genuinely unexpected, not a routine outcome.
    val mistakes = Vector(
      "onset ~ hrf(missing)",
      "onset ~ hrf(listcol)",
      "onset ~ hrf(hrfcol)",
      "onset ~ hrf(BogusCall(cond))",
      "onset ~ hrf(cond, basis = nope)",
      "onset ~ hrf(cond, contrasts = nosuch)",
      """onset ~ hrf(cond, subset = rt < "fast")""",
      "onset ~ hrf(cond, onsets = missing)",
      "onset ~ hrf(cond, durations = listcol)",
      "onset ~ hrf(Poly(rt))",
      "onset ~ hrf(Scale(cond))",
      "onset ~ covariate(missing)",
      "onset ~ hrf(",
      "onset ~ hrf(cond",
      "onset ~ nosuchterm(cond)",
      "onset ~ hrf(cond, typo = TRUE)",
      "onset ~ trialwise(cond)"
    )

    mistakes.foreach { formula =>
      errorFor(formula) match
        case DesignError.BuildFailed(detail) =>
          fail(s"'$formula' still degrades to BuildFailed($detail)")
        case _ => ()
    }
