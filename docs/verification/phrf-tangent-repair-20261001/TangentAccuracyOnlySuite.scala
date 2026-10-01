package scalafim.fmri.laws.profile

/** Read-only execution selector for Scala.js, whose MUnit runner does not
  * support JUnit's --tests filter. Inherits the existing assertion bodies,
  * fixtures, seeds, thresholds and timeout; excludes only the throughput test.
  * Added through an ephemeral Test/unmanagedSources setting, not library code.
  */
class TangentAccuracyOnlySuite extends ConditionMilestoneSuite:
  override def munitTests(): Seq[munit.Test] =
    val names = Set(
      "Gaussian: accuracy gates on the frozen C0 cohorts",
      "LWU: accuracy on the frozen C0 cohorts at SNR 1.0 and 0.5"
    )
    val selected = super.munitTests().filter(t => names.contains(t.name))
    require(selected.map(_.name).toSet == names && selected.size == 2, "accuracy selection must retain both complete existing tests")
    selected
