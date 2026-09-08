package scalafim.fmri.workflow

import bids4s.BidsTable
import scalafim.dataset.RunId

class FirstLevelCompanionReviewSuite extends munit.FunSuite:
  private val table = BidsTable.parse("trans_x\tframewise_displacement\tbad\n1.5\tn/a\ta\n-2\t0.4\t3\n").toOption.get
  test("QC binding preserves original samples and missing values without a nuisance request") {
    val columns = FirstLevelCompanionReview.bind(RunId("01"),2,table,Map("trans_x" -> "mm")).toOption.get
    assertEquals(columns(0).values,Right(Vector(Some(1.5),Some(-2.0))))
    assertEquals(columns(0).units,Some("mm"))
    assertEquals(columns(1).values,Right(Vector(None,Some(0.4))))
    assertEquals(columns(1).units,None)
    assert(columns(2).values.isLeft)
  }
  test("QC row mismatch cannot shift scan identity") {
    assert(FirstLevelCompanionReview.bind(RunId("01"),3,table).isLeft)
  }
