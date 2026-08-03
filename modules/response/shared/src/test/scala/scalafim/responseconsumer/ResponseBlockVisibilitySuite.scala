package scalafim.responseconsumer

import scala.compiletime.testing.typeCheckErrors

class ResponseBlockVisibilitySuite extends munit.FunSuite:
  test("owned response storage is not visible outside the response package"):
    assert(typeCheckErrors("""
      import scalafim.response.ResponseBlock
      def illegal(block: ResponseBlock) = block.ownedRowMajor
    """).nonEmpty)

