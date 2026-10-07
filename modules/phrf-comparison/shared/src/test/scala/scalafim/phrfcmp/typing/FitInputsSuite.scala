package scalafim.phrfcmp.typing

import scala.compiletime.constValueTuple
import scala.deriving.Mirror

import scalafim.phrfcmp.ingest.*

/** FitInputs must not be able to reach any truth-bearing array (this suite sits outside the `ingest` package). */
class FitInputsSuite extends munit.FunSuite:

  private val truthNames: Set[String] = Set(
    "signal", "evOnsetTrue", "truthT", "truthKernel", "truthParams", "condCoef", "condPeakAmp", "signalScale",
    "nuisanceCoef", "nuisanceCoefPool", "trialBeta", "signalConditionMean"
  )

  private inline def labels[T](using m: Mirror.ProductOf[T]): List[String] =
    constValueTuple[m.MirroredElemLabels].toList.map(_.toString)

  private def fit: FitInputs =
    val (npz, m) = Synthetic.build()
    PhrfDatasetBinding.bind(npz, Synthetic.render(m), Synthetic.expectation(CellKind.Trial)).fold(r => fail(r.message), _.fit)

  test("FitInputs has no truth field (compile-time field labels)"):
    val labs = labels[FitInputs]
    assert(labs.nonEmpty)
    assertEquals(labs.toSet.intersect(truthNames), Set.empty[String])
    assertEquals(labels[ScoreTruth].toSet, truthNames)

  test("selecting any truth field on FitInputs does not compile"):
    val f = fit
    assertEquals(f.y.rows, 2)
    assert(compileErrors("f.signal").nonEmpty)
    assert(compileErrors("f.evOnsetTrue").nonEmpty)
    assert(compileErrors("f.truthT").nonEmpty)
    assert(compileErrors("f.truthKernel").nonEmpty)
    assert(compileErrors("f.truthParams").nonEmpty)
    assert(compileErrors("f.condCoef").nonEmpty)
    assert(compileErrors("f.condPeakAmp").nonEmpty)
    assert(compileErrors("f.signalScale").nonEmpty)
    assert(compileErrors("f.nuisanceCoef").nonEmpty)
    assert(compileErrors("f.nuisanceCoefPool").nonEmpty)
    assert(compileErrors("f.trialBeta").nonEmpty)
    assert(compileErrors("f.signalConditionMean").nonEmpty)
    assert(compileErrors("f.y").isEmpty)

  test("FitInputs cannot be forged outside the ingest package"):
    assert(compileErrors("FitInputs(null, None, null, null, null, null, null, null, null, null)").nonEmpty)
