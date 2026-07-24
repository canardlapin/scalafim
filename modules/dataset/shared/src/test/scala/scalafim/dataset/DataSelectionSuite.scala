package scalafim.dataset

import narr.NArray
import scalafim.image.{DMat, Mask, NeuroSpace}

class DataSelectionSuite extends munit.FunSuite:

  test("mask domains reject equal-shaped spaces with different affines") {
    val translatedAffine =
      DMat.fromRows(
        Vector(
          Vector(1.0, 0.0, 0.0, 10.0),
          Vector(0.0, 1.0, 0.0, 0.0),
          Vector(0.0, 0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    val shape = DatasetShape.unsafe(NeuroSpace(Vector(2, 2, 1)), timepoints = 3)
    val maskSpace = NeuroSpace(Vector(2, 2, 1), trans = Some(translatedAffine))
    val mask = Mask.fromIndices(maskSpace, NArray[Int](0, 1))

    assert(VoxelDomain.fromMask(mask, shape).isLeft)
  }
