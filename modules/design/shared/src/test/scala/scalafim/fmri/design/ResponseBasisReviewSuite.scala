package scalafim.fmri.design

import scalafim.fmri.hrf.*

class ResponseBasisReviewSuite extends munit.FunSuite:
  private def checked[E,A](value: Either[E,A]): A = value.fold(e => fail(e.toString),identity)

  test("canonical and derivative curves retain native coordinates and unnormalized values") {
    for hrf <- Vector(Hrfs.SPMG1,Hrfs.SPMG2,Hrfs.SPMG3) do
      val review = checked(ResponseBasisReview.make(hrf,samples=33))
      assertEquals(review.descriptor,hrf.descriptor)
      assertEquals(review.basisIds,hrf.basisElements.map(_.id))
      assertEquals(review.times.head.value,0.0)
      assertEquals(review.times.last.value,hrf.span.value)
      val expected = hrf.evalDoubles(review.times.map(_.value))
      for row <- review.times.indices; col <- review.curves.indices do
        assertEqualsDouble(review.curves(col).values(row),expected(row,col),1e-14)
      assert(review.readout.isEmpty)
  }

  test("FIR intervals and exact window weights do not depend on the display grid") {
    val hrf = Hrfs.fir(nBasis=4,span=8.s)
    val functional = ResponseFunctional.WindowMean(1.s,7.s)
    val coarse = checked(ResponseBasisReview.make(hrf,Some(functional),samples=2))
    val fine = checked(ResponseBasisReview.make(hrf,Some(functional),samples=257))
    assertEquals(coarse.readout,fine.readout)
    assertEquals(coarse.readout.get.units,ResponseUnits.ResponseValue)
    assertEquals(coarse.readout.get.discretization.policy,FunctionalDiscretization.Exact)
    val expected = Vector(1.0/6,2.0/6,2.0/6,1.0/6)
    coarse.readout.get.weights.zip(expected).foreach((a,b) => assertEqualsDouble(a,b,1e-14))
    for index <- 0 until 4 do
      coarse.curves(index).geometry match
        case BasisPreviewGeometry.FirStep(from,until,height) =>
          assertEqualsDouble(from.value,index*2.0,1e-14)
          assertEqualsDouble(until.value,(index+1)*2.0,1e-14)
          assertEqualsDouble(height,1.0,1e-14)
        case other => fail(s"FIR preview must keep its discontinuous geometry: $other")
    val point = checked(ResponseBasisReview.make(hrf,Some(ResponseFunctional.At(2.s))))
    assertEquals(point.readout.get.weights,Vector(0.0,1.0,0.0,0.0))
    val integral = checked(ResponseBasisReview.make(hrf,Some(ResponseFunctional.WindowIntegral(1.s,7.s))))
    assertEquals(integral.readout.get.units,ResponseUnits.ResponseIntegral)
    integral.readout.get.weights.zip(Vector(1.0,2.0,2.0,1.0)).foreach((a,b) => assertEqualsDouble(a,b,1e-14))
  }

  test("derivative window functionals remain exact when display sampling changes") {
    for functional <- Vector(ResponseFunctional.At(6.s),ResponseFunctional.WindowMean(4.s,8.s),ResponseFunctional.WindowIntegral(4.s,8.s)) do
      val a = checked(ResponseBasisReview.make(Hrfs.SPMG3,Some(functional),samples=2))
      val b = checked(ResponseBasisReview.make(Hrfs.SPMG3,Some(functional),samples=129))
      assertEquals(a.readout,b.readout)
      assertEquals(a.readout.get.weights.size,3)
      assertEquals(a.readout.get.discretization.policy,FunctionalDiscretization.Exact)
  }

  test("preview boundaries return typed errors for invalid budgets and custom kernels") {
    assert(ResponseBasisReview.make(Hrfs.SPMG3,samples=1).isLeft)
    assert(ResponseBasisReview.make(Hrfs.SPMG3,samples=10,maximumValues=29).isLeft)
    assert(ResponseBasisReview.make(Hrfs.SPMG1,Some(ResponseFunctional.WindowMean(8.s,4.s))).isLeft)
    val nonfinite = Hrf.scalar("nonfinite",span=8.s)(_ => Double.NaN)
    assert(checked(ResponseBasisReview.make(nonfinite).swap).isInstanceOf[BasisReviewError.NonFiniteValue])
    val malformed = Hrf.multi("wrong-width",nbasis=2,span=8.s)(_ => Array(1.0))
    assert(checked(ResponseBasisReview.make(malformed).swap).isInstanceOf[BasisReviewError.KernelEvaluation])
    checked(ResponseBasisReview.make(malformed,Some(ResponseFunctional.At(2.s))).swap) match
      case BasisReviewError.InvalidFunctional(BasisError.DimensionMismatch(_,2,1)) => ()
      case other => fail(s"Keep the native functional dimension error: $other")
    val throwing = Hrf.scalar("throwing",span=8.s)(_ => throw IllegalArgumentException("kernel failed"))
    assert(checked(ResponseBasisReview.make(throwing,Some(ResponseFunctional.At(2.s))).swap).isInstanceOf[BasisReviewError.KernelEvaluation])
    val noPrimitive = Hrf.scalar("without-primitive",span=8.s)(lag => lag.value)
    assert(ResponseBasisReview.make(noPrimitive,Some(ResponseFunctional.WindowMean(1.s,2.s))).isLeft)
  }

  test("renderer-neutral plots preserve selected identities and full readout receipts") {
    val review = checked(ResponseBasisReview.make(Hrfs.SPMG3,Some(ResponseFunctional.WindowMean(4.s,8.s))))
    for (width,height) <- Vector((640.0,280.0),(380.0,220.0),(1280.0,560.0)) do
      val plot = checked(ResponseBasisGraphics.plot(review,width,height))
      assertEquals(plot.droppedRows,Vector.empty)
      val axes = plot.guides.map(_.spec).collect { case a: intaglio.GuideSpec.Axis => a }
      assertEquals(axes.map(_.side).toSet,Set(intaglio.AxisSide.Bottom,intaglio.AxisSide.Left))
      assert(axes.flatMap(_.title).contains("Response time (s)"))
      assert(axes.flatMap(_.title).contains("Basis value"))
      assert(plot.guides.exists(_.spec.isInstanceOf[intaglio.GuideSpec.Legend]))
      val description = plot.semantics.description.getOrElse("")
      review.basisIds.foreach(id => assert(description.contains(id.value)))
      assert(description.contains("WindowMean") && description.contains("ResponseValue"))
      assertEquals(plot.scene.semantics.plots.head.description,plot.semantics.description)
    val selected = checked(ResponseBasisGraphics.plotSelected(review,Vector(review.basisIds(2))))
    val full = checked(ResponseBasisGraphics.plot(review))
    assertEquals(selected.layout.get.xScale,full.layout.get.xScale)
    assertEquals(selected.layout.get.yScale,full.layout.get.yScale)
    assert(selected.semantics.description.get.contains("Showing 1/3"))
    assert(selected.semantics.description.get.contains("weight axis="+review.basisIds.map(_.value).mkString(",")))
    assert(ResponseBasisGraphics.plotSelected(review,Vector.empty).isLeft)
    assert(ResponseBasisGraphics.plotSelected(review,Vector(review.basisIds.head,review.basisIds.head)).isLeft)
    assert(ResponseBasisGraphics.plotSelected(review,Vector(BasisElementId.unsafe("unknown"))).isLeft)
    assert(ResponseBasisGraphics.plotSelected(review,review.basisIds,maximumCurves=2).isLeft)
    assert(ResponseBasisGraphics.plot(review,width=Double.NaN).isLeft)
    assert(ResponseBasisGraphics.plot(review,height=0).isLeft)
    val fir = checked(ResponseBasisReview.make(Hrfs.fir(nBasis=4,span=8.s),samples=2))
    val firPlot = checked(ResponseBasisGraphics.plot(fir))
    assertEquals(firPlot.droppedRows,Vector.empty)
    val steps = raw"FirStep\(([^,]+),([^,]+),([^\)]+)\)".r
      .findAllMatchIn(firPlot.semantics.description.get).map(m => Vector(m.group(1).toDouble,m.group(2).toDouble,m.group(3).toDouble)).toVector
    assertEquals(steps.head,Vector(0.0,2.0,1.0))
    val lastBin = checked(ResponseBasisGraphics.plotSelected(fir,Vector(fir.basisIds.last)))
    assertEquals(lastBin.layout.get.xScale,firPlot.layout.get.xScale)
    assertEquals(lastBin.layout.get.yScale,firPlot.layout.get.yScale)
  }

  test("canonical polynomial-exponential reference and finite-difference derivatives retain amplitude") {
    def reference(t: Double): Double = math.exp(-t)*(0.0833*math.pow(t,5)-1.274527e-13*math.pow(t,15))
    val review = checked(ResponseBasisReview.make(Hrfs.SPMG3,samples=25))
    for index <- Vector(3,5,10,18) do
      val t = review.times(index).value
      val h = 1e-4
      val f = reference(t)
      val first = (reference(t+h)-reference(t-h))/(2*h)
      val second = (reference(t+h)-2*f+reference(t-h))/(h*h)
      assertEqualsDouble(review.curves(0).values(index),f,1e-13)
      assertEqualsDouble(review.curves(1).values(index),first,1e-7)
      assertEqualsDouble(review.curves(2).values(index),second,2e-6)
  }
