package scalafim.testkit

/** Real-asset suites skip when their inputs are absent, unless
  * `SCALAFIM_REQUIRE_REAL_ASSETS=1`, which turns absence into a failure.
  */
trait RealAssetGate:
  self: munit.FunSuite =>

  def requireReal(present: Boolean, what: String): Unit =
    if !present && sys.env.get("SCALAFIM_REQUIRE_REAL_ASSETS").contains("1") then
      fail(s"$what not found and SCALAFIM_REQUIRE_REAL_ASSETS=1")
    assume(present, s"$what not found; skipped (set SCALAFIM_REQUIRE_REAL_ASSETS=1 to fail instead)")
