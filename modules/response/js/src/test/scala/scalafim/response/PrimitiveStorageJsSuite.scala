package scalafim.response

import narr.NArray
import scala.scalajs.js.typedarray.{Float64Array, Int32Array}

class PrimitiveStorageJsSuite extends munit.FunSuite:
  test("axis and response storage use Scala.js typed primitive arrays"):
    val indices =
      OrderedIndices
        .fromInts(
          DomainId.unsafe[TimeAxis]("js-storage"),
          3,
          Vector(2, 0)
        )
        .toOption
        .get

    assert(indices.primitiveValues.isInstanceOf[Int32Array])
    assert(NArray[Double](1.0).isInstanceOf[Float64Array])

