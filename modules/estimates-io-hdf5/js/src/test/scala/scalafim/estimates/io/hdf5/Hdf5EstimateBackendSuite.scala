package scalafim.estimates.io.hdf5

import scalafim.estimates.EstimateError

class Hdf5EstimateBackendSuite extends munit.FunSuite:
  test("JS refuses capability before local path, archive receipt or native work"):
    val result = Hdf5EstimateBackend.open(null, null)
    assert(result.left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
