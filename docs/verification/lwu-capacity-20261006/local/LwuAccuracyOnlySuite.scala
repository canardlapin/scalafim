package scalafim.fmri.laws.profile

/** Verification-only execution selector; inherits the complete frozen LWU
  * assertion body, fixture, seeds, oracle and timeout without altering them.
  * Supplied only by a scoped Test/unmanagedSources setting in its own process.
  */
class LwuAccuracyOnlySuite extends ConditionMilestoneSuite:
  override def munitTests(): Seq[munit.Test] =
    val expected = "LWU: accuracy on the frozen C0 cohorts at SNR 1.0 and 0.5"
    val selected = super.munitTests().filter(_.name == expected)
    require(selected.size == 1 && selected.head.name == expected,
      "LWU qualification must execute exactly the complete original accuracy test")
    selected
