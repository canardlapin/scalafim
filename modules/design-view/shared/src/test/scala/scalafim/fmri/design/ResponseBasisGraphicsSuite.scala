package scalafim.fmri.design

import scalafim.fmri.hrf.*

class ResponseBasisGraphicsSuite extends munit.FunSuite:
  private def checked[E,A](value: Either[E,A]): A = value.fold(error => fail(error.toString),identity)

  test("selected basis semantics bind only selected curves while retaining full-axis readout receipts") {
    val review = checked(ResponseBasisReview.make(Hrfs.fir(nBasis=4,span=8.s),
      Some(ResponseFunctional.WindowMean(1.s,7.s)),samples=5))
    val selectedIds = Vector(review.basisIds(1),review.basisIds(3))
    val selected = checked(ResponseBasisGraphics.plotSelected(review,selectedIds,width=640,height=280))
    val full = checked(ResponseBasisGraphics.plot(review,width=640,height=280))
    val description = selected.scene.semantics.accessibleDescription.getOrElse(fail("selected basis semantics missing"))
    val readout = review.readout.get
    assertEquals(selected.layout.get.xScale,full.layout.get.xScale)
    assertEquals(selected.layout.get.yScale,full.layout.get.yScale)
    assert(description.contains(s"Native response basis ${review.descriptor.canonicalId}; fixed full-basis axes; no display normalization; model components, not fitted responses."))
    assert(description.contains("Showing 2/4 coordinates."))
    assert(description.contains("Sample times (seconds): " + review.times.map(_.value).mkString(",")))
    selectedIds.foreach { id =>
      val curve = review.curves.find(_.element.id == id).getOrElse(fail(s"missing selected curve $id"))
      assert(description.contains(s"id=${id.value}; role=${curve.element.role.stableLabel}; values=${curve.values.mkString(",")}; geometry=FirStep("))
    }
    review.basisIds.filterNot(selectedIds.contains).foreach(id => assert(!description.contains("id=" + id.value + ";")))
    assert(description.contains(s"Readout=${readout.functional}; units=${readout.units}; weight axis=${review.basisIds.map(_.value).mkString(",")}; exact basis weights=${readout.weights.mkString(",")}; discretization=${readout.discretization}."))
    assert(description.contains("Display samples and curve selection do not determine these weights."))
  }

  test("full basis semantics enumerate every current coordinate") {
    val review = checked(ResponseBasisReview.make(Hrfs.SPMG3,samples=7))
    val description = checked(ResponseBasisGraphics.plot(review)).scene.semantics.accessibleDescription
      .getOrElse(fail("full basis semantics missing"))
    assert(description.contains(s"Showing ${review.basisIds.size}/${review.curves.size} coordinates."))
    review.curves.foreach(curve =>
      assert(description.contains(s"id=${curve.element.id.value}; role=${curve.element.role.stableLabel}; values=${curve.values.mkString(",")}; geometry=Sampled")))
    assert(!description.contains("Readout="))
  }
