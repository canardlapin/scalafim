package scalafim.fmri.group

class GroupRuntimeBoundarySuite extends munit.FunSuite:
  test("group execution has no first-level fitter on its runtime classpath") {
    intercept[ClassNotFoundException]:
      Class.forName("scalafim.fmri.fit.SelectedEstimates$", false, getClass.getClassLoader)
  }
