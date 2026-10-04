package scalafim.fmri.design

import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.formula.*
import scalafim.fmri.hrf.design.SamplingFrame

class PortableNamingSuite extends munit.FunSuite:
  private val frame = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0))

  test("numeric factor levels name columns identically on every platform"):
    val data = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 6.0, 11.0, 16.0)),
      "dose" -> Column.Doubles(Vector(1.0e7, 2.0, 1.0e7, 2.0))
    )
    val levels = FactorLevelRegistry.of("dose" -> Seq("2", "10000000")).fold(error => fail(error.message), identity)
    val model = EventModelBuilder.buildEither("onset ~ hrf(dose)", data, frame, blockIds = Vector(0, 0, 0, 0), factorLevels = levels)
      .fold(error => fail(error.message), identity)
    assert(model.columnNames.exists(_.contains("10000000")), model.columnNames.mkString(", "))
    assert(!model.columnNames.exists(name => name.contains("E7") || name.contains("e7") || name.contains(".0")), model.columnNames.mkString(", "))
