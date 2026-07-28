package scalafim.linalg

import gale.linalg.{DMat, DVec}
import gale.spectral.{Eigen, EigenSelection, EigenVectors}

/** Cross-platform consumer gate for the Gale artifact boundary.
  *
  * This suite belongs to the migration gate, not to Scalafim's eventual
  * numerical API. It must execute on the JVM and survive a full Scala.js link
  * before the pinned Gale revision is advanced.
  */
class GaleConsumerProbeSuite extends munit.FunSuite:
  test("Scala 3.7.4 consumes Gale dense and spectral APIs") {
    val builder = DMat.newBuilder(2, 2)
    builder(0, 0) = 2.0
    builder(0, 1) = 1.0
    builder(1, 0) = 1.0
    builder(1, 1) = 2.0
    val matrix = builder.result()

    val vector = DVec.newBuilder(2)
    vector(0) = 1.0
    vector(1) = -1.0
    val product = matrix * vector.result()
    val spectrum = Eigen
      .eigSymmetric(matrix, EigenSelection.All, EigenVectors.ValuesOnly)
      .toOption
      .get

    assertEqualsDouble(product(0), 1.0, 1.0e-12)
    assertEqualsDouble(product(1), -1.0, 1.0e-12)
    assertEqualsDouble(spectrum.eigenvalues(0), 1.0, 1.0e-12)
    assertEqualsDouble(spectrum.eigenvalues(1), 3.0, 1.0e-12)
  }
