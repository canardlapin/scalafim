package scalafim.fmri.laws

import scala.compiletime.testing.typeCheckErrors

class StaticTypeContractsSuite extends munit.FunSuite:

  test("an HRF accepts a lag derived from absolute time, not an absolute clock reading"):
    val absoluteTimeErrors = typeCheckErrors("""
      import scalafim.fmri.hrf.*
      Hrfs.SPMG1(Seconds(6.0))
    """)
    assert(absoluteTimeErrors.nonEmpty, "an absolute Seconds value must not typecheck as a kernel lag")

    val lagErrors = typeCheckErrors("""
      import scalafim.fmri.hrf.*
      val onset = Seconds(4.0)
      val acquisition = Seconds(6.0)
      Hrfs.SPMG1(Lag.between(onset, acquisition))
    """)
    assertEquals(lagErrors, Nil)

  test("basis coefficients remain owned by the basis that constructed them"):
    val foreignBasisErrors = typeCheckErrors("""
      import scalafim.fmri.hrf.*
      val leftKernel = Hrfs.SPMG2
      val rightKernel = Hrfs.SPMG2
      val left = ResponseBasis.of(leftKernel)
      val right = ResponseBasis.of(rightKernel)
      val coefficients = left.coefficients(Vector(1.0, 0.0)).toOption.get
      right.reconstruct(coefficients)
    """)
    assert(foreignBasisErrors.nonEmpty, "coefficients from a distinct response basis must not typecheck")

    val sameBasisErrors = typeCheckErrors("""
      import scalafim.fmri.hrf.*
      val kernel = Hrfs.SPMG2
      val basis = ResponseBasis.of(kernel)
      val coefficients = basis.coefficients(Vector(1.0, 0.0)).toOption.get
      basis.reconstruct(coefficients)
    """)
    assertEquals(sameBasisErrors, Nil)
