package scalafim.fmri.design

import scalafim.fmri.design.contrast.TermCells
import scalafim.fmri.design.event.Event
import scalafim.fmri.design.event.EventTerm
import scalafim.fmri.hrf.Seconds

class TermCellsSuite extends munit.FunSuite:

  test("TermCells shortNames match R cells/shortnames ordering") {
    val category = Vector("face", "scene", "face", "scene")
    val attention = Vector("attend", "attend", "ignored", "ignored")
    val onsets = Vector(1.0, 2.0, 3.0, 4.0).map(Seconds(_))

    val term = EventTerm(
      events = Vector(
        Event.factor(category, "category"),
        Event.factor(attention, "attention")
      ),
      onsets = onsets,
      termTag = Some("t")
    )

    val cells = TermCells.from(term, dropEmpty = true)
    assertEquals(cells.shortNames, Vector("face:attend", "scene:attend", "face:ignored", "scene:ignored"))
  }

  test("TermCells dropEmpty drops unobserved combinations") {
    val category = Vector("face", "scene", "face", "scene")
    val attention = Vector("attend", "attend", "attend", "ignored")
    val onsets = Vector(1.0, 2.0, 3.0, 4.0).map(Seconds(_))

    val term = EventTerm(
      events = Vector(
        Event.factor(category, "category"),
        Event.factor(attention, "attention")
      ),
      onsets = onsets,
      termTag = Some("t")
    )

    val cellsAll = TermCells.from(term, dropEmpty = false).shortNames
    assertEquals(cellsAll, Vector("face:attend", "scene:attend", "face:ignored", "scene:ignored"))

    val cellsDrop = TermCells.from(term, dropEmpty = true).shortNames
    assertEquals(cellsDrop, Vector("face:attend", "scene:attend", "scene:ignored"))
  }
