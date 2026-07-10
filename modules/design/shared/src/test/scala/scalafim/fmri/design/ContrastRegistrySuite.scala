package scalafim.fmri.design

import scalafim.fmri.design.contrast.*
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.design.SamplingFrame

class ContrastRegistrySuite extends munit.FunSuite:

  test("EventModelBuilder attaches contrasts= and validateAttachedContrasts checks them") {
    val sf = SamplingFrame(blockLens = Seq(40), tr = Seq(1.0))

    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 10.0, 20.0, 30.0)),
      "cond" -> Column.Strings(Vector("A", "B", "A", "B"))
    )

    val cset =
      ContrastSpec.ContrastSet(
        ContrastSpec.Pair(
          name = "A_vs_B",
          A = cell => cell("cond") == "A",
          B = cell => cell("cond") == "B"
        )
      )

    val model = EventModelBuilder.build(
      formula = "onset ~ hrf(cond, id = task, contrasts = myset)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 0, 0),
      contrastSets = Map("myset" -> cset)
    )

    assert(model.contrastSetsByTerm.contains("task"))

    import ContrastRegistry.*

    val weights = model.contrastWeights
    assertEquals(weights.keySet, Set("task#A_vs_B"))
    assertEquals(weights("task#A_vs_B").weights.rows, model.designMatrix.cols)

    val compiled = model.compiledContrasts.fold(error => fail(error.message), identity)
    assertEquals(compiled.keySet, Set("task#A_vs_B"))
    assertEquals(compiled("task#A_vs_B").id.value, "task#A_vs_B")
    assertEquals(compiled("task#A_vs_B").effect.map(_.value), Some("A_vs_B"))
    assertEquals(compiled("task#A_vs_B").source, ContrastSource.Legacy)
    assertEquals(compiled("task#A_vs_B").toLegacy.condNames, model.columnNames)

    val res = model.validateAttachedContrasts()
    assertEquals(res.map(_.name), Vector("task#A_vs_B"))
    val row = res.head
    assertEquals(row.estimable, true)
    assertEquals(row.sumToZero, true)
    assertEquals(row.orthogonalToIntercept, true)
  }
