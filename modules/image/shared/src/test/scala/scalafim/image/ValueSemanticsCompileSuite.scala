package scalafim.image

import scala.compiletime.testing.typeCheckErrors

class ValueSemanticsCompileSuite extends munit.FunSuite:

  test("continuous values admit linear sampling"):
    val errors = typeCheckErrors(
      """
import image4s.*
summon[LinearSampling[Double, Continuous]]
"""
    )

    assert(errors.isEmpty)

  test("mask values cannot enter continuous interpolation"):
    val errors = typeCheckErrors(
      """
import image4s.*
summon[LinearSampling[Boolean, Mask]]
"""
    )

    assert(errors.nonEmpty)

  test("categorical values cannot enter continuous interpolation"):
    val errors = typeCheckErrors(
      """
import image4s.*
summon[LinearSampling[Int, Categorical]]
"""
    )

    assert(errors.nonEmpty)
