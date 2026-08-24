package scalafim.image

import gale.linalg.DMat
import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.GeometryError
import munit.FunSuite
import scalafim.image.NeuroAffineSyntax.*

class ProviderAffineContractSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A =
    value.fold(error => fail(error.toString), identity)

  private def affine(values: Double*): Affine[D3] =
    right(Affine.fromRowMajor[D3](values))

  private def assertVectorClose(
      actual: Vector[Double],
      expected: Vector[Double],
      tolerance: Double
  ): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).foreach: (observed, target) =>
      assertEqualsDouble(observed, target, tolerance)

  test("provider affine and Gale matrix constructors copy borrowed input"):
    val affineInput = Array(
      2.0, 0.0, 0.0, 4.0,
      0.0, 3.0, 0.0, 5.0,
      0.0, 0.0, 4.0, 6.0,
      0.0, 0.0, 0.0, 1.0
    )
    val provider = right(Affine.fromRowMajor[D3](affineInput))
    affineInput(0) = 99.0
    affineInput(3) = -200.0
    assertEqualsDouble(provider.matrix(0, 0), 2.0, 0.0)
    assertEqualsDouble(provider.matrix(0, 3), 4.0, 0.0)

    val matrixInput = Array(1.0, 2.0, 3.0, 4.0)
    val matrix = DMat.dense(2, 2, matrixInput.toSeq)
    matrixInput(0) = 100.0
    assertEqualsDouble(matrix(0, 0), 1.0, 0.0)

  test("provider inverse and composition retain coordinate laws"):
    val first = affine(
      1.0, 0.0, 0.0, 4.0,
      0.0, 1.0, 0.0, -2.0,
      0.0, 0.0, 1.0, 3.0,
      0.0, 0.0, 0.0, 1.0
    )
    val second = affine(
      2.0, 0.0, 0.0, 0.0,
      0.0, 3.0, 0.0, 0.0,
      0.0, 0.0, 4.0, 0.0,
      0.0, 0.0, 0.0, 1.0
    )
    val third = affine(
      1.0, 0.1, 0.0, 0.0,
      0.0, 1.0, 0.2, 0.0,
      0.0, 0.0, 1.0, 0.0,
      0.0, 0.0, 0.0, 1.0
    )
    val point = Vector(1.25, -2.0, 0.5)
    val roundTrip = right(first.inverse(right(first(point))))
    assertVectorClose(roundTrip, point, 1e-12)

    val leftAssociated = right(right(first.andThen(second)).andThen(third))
    val rightAssociated = right(first.andThen(right(second.andThen(third))))
    assertVectorClose(
      right(leftAssociated(point)),
      right(rightAssociated(point)),
      1e-12
    )
    assertVectorClose(
      right(Affine.identity[D3](point)),
      point,
      0.0
    )

  test("provider rejects non-affine, singular, and excessive residual inputs"):
    val invalidBottom = Vector(
      1.0, 0.0, 0.0, 0.0,
      0.0, 1.0, 0.0, 0.0,
      0.0, 0.0, 1.0, 0.0,
      0.0, 1.0, 0.0, 1.0
    )
    assert(
      Affine.fromRowMajor[D3](invalidBottom).left.toOption.exists:
        case _: GeometryError.InvalidHomogeneousBottomRow => true
        case _ => false
    )

    val singular = Vector(
      1.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 1.0, 0.0,
      0.0, 0.0, 0.0, 1.0
    )
    assert(
      Affine.fromRowMajor[D3](singular).left.toOption.exists:
        case _: GeometryError.NonInvertibleAffine => true
        case _ => false
    )

    val stable = affine(
      2.0, 0.2, 0.1, 4.0,
      0.1, 3.0, 0.2, -2.0,
      0.0, 0.1, 1.5, 1.0,
      0.0, 0.0, 0.0, 1.0
    )
    assert(
      Affine
        .fromRowMajor[D3](
          stable.rowMajor,
          maximumInverseResidual = 0.0
        )
        .left
        .toOption
        .exists:
          case _: GeometryError.AffineInverseResidualTooLarge => true
          case _ => false
    )

  test("orientation and neuro affine extensions consume provider geometry"):
    val oblique = affine(
      0.0, -2.0, 0.2, 10.0,
      3.0, 0.0, 0.1, -4.0,
      0.0, 0.0, 4.0, 2.0,
      0.0, 0.0, 0.0, 1.0
    )
    assertVectorClose(
      oblique.neuroVoxelSizes,
      Vector(3.0, 2.0, math.sqrt(16.05)),
      1e-12
    )
    assert(oblique.neuroObliquity.exists(_ > 0.0))
    val orientation = Orientation.findAnatomy(oblique.matrix)
    assertEquals(orientation.axes.map(_.abbrev), Vector("P", "R", "I"))

    val rescaled = right(
      oblique.rescaledVoxelGeometry(
        shape = Vector(9, 7, 5),
        voxelSizes = Vector(1.5, 1.0, 2.0)
      )
    )
    assertVectorClose(
      rescaled.neuroVoxelSizes,
      Vector(1.5, 1.0, 2.0),
      1e-12
    )
