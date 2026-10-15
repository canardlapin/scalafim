package scalafim.fmri.hrf.regressor

/** Runs one existing finite boundary regression; no overflow allocation or
  * unbounded event loop is evaluated against the deliberately unsafe mutant.
  */
class NeuralInputBudgetMutationSuite extends NeuralInputSuite:
  override def munitTests(): Seq[munit.Test] =
    val name = "sample budget refuses one sample beyond the cap before allocation"
    val selected = super.munitTests().filter(_.name == name)
    assert(selected.length == 1)
    selected
