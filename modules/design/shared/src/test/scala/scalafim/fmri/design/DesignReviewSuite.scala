package scalafim.fmri.design

import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

class DesignReviewSuite extends munit.FunSuite:
  private val frame = SamplingFrame(blockLens = Seq(3,3), tr = Seq(2.0,2.0), startTime = Seq(1.0,1.0))
  private val columns = Vector(
    StructuralColumn.fromOrigin(1,StructuralColumnOrigin.Intercept(RunScope.Run(RunIndex.unsafeOneBased(1))),"first intercept").toOption.get,
    StructuralColumn.fromOrigin(2,StructuralColumnOrigin.Intercept(RunScope.Run(RunIndex.unsafeOneBased(2))),"second intercept").toOption.get)
  private val schema = DesignSchema.validated(Mat.unsafe(6,2,Array(1.0,0.0,1.0,0.0,1.0,0.0,0.0,2.0,0.0,-4.0,0.0,0.0)),
    RowLayout.fromSamplingFrame(frame),columns).toOption.get
  private val retained = Vector(1,3,4,6).map(ScanIndex.unsafeOneBased)
  private def review(scope: DesignReviewScope) = DesignReview.forRun(schema,frame,RunIndex.unsafeOneBased(2),retained,scope).toOption.get

  test("review keeps original scan identity, acquisition time and excluded samples") {
    val r = review(DesignReviewScope.RunwiseFit)
    assertEquals(r.scans.map(_.source.oneBased),Vector(4,5,6))
    assertEquals(r.scans.map(_.time.value),Vector(1.0,3.0,5.0))
    assertEquals(r.scans.map(_.retained),Vector(true,false,true))
    assertEquals(r.sourceColumnIndices,Vector(1))
    assertEquals(r.trace(r.columns.head.id).toOption.get.values,Vector(Some(2.0),Some(-4.0),Some(0.0)))
    assertEquals(r.peaks,Vector(4.0))
    assertEquals(Vector.tabulate(3)(r.scaled(_,0)),Vector(0.5,-1.0,0.0))
    assertEquals(r.fingerprint,schema.fingerprint)
    assertEquals(r.trace(r.columns.head.id).toOption.get.designColumn,Some(schema.fingerprint -> r.columns.head.id))
  }
  test("source review includes structurally omitted and zero columns") {
    val r = review(DesignReviewScope.SourceColumns)
    assertEquals(r.columns,columns)
    assertEquals(r.peaks,Vector(0.0,4.0))
    assertEquals(r.scaled(0,0),0.0)
    assert(r.trace(ColumnId.unsafe("absent")).isLeft)
  }
  test("review rejects incompatible sampling and repeated row identities") {
    assert(DesignReview.forRun(schema,frame,RunIndex.unsafeOneBased(2),retained :+ retained.head).isLeft)
    assert(DesignReview.forRun(schema,SamplingFrame(blockLens=Seq(6),tr=Seq(2.0)),RunIndex.unsafeOneBased(1),retained).isLeft)
  }
  test("renderer-neutral recipes enforce size and missing sample contracts") {
    val r = review(DesignReviewScope.SourceColumns)
    val matrix = DesignReviewGraphics.matrixPlot(r,width=600,height=300).toOption.get
    val trace = DesignReviewGraphics.tracePlot(r.scans,Vector(r.trace(r.columns.head.id).toOption.get),width=600,height=140,alignWith=matrix.layout).toOption.get
    assertEquals(trace.layout.get.xScale,matrix.layout.get.xScale)
    assertEquals(trace.layout.get.frame.origin.x,matrix.layout.get.frame.origin.x)
    assertEquals(trace.layout.get.frame.size.width,matrix.layout.get.frame.size.width)
    assert(DesignReviewGraphics.matrixScene(r,width=620,height=140).isRight)
    assert(DesignReviewGraphics.traceScene(r.scans,Vector(r.trace(r.columns.head.id).toOption.get),width=620,height=90).isRight)
    assert(DesignReviewGraphics.matrixScene(r,maximumCells=5).isLeft)
    assert(DesignReviewGraphics.matrixScene(r,firstColumn=2).isLeft)
    assert(DesignReviewGraphics.traceScene(r.scans,Vector(ReviewSeries("FD","source units",Vector(Some(0.2),None,Some(0.3))))).isRight)
    assert(DesignReviewGraphics.traceScene(r.scans,Vector(ReviewSeries("FD","source units",Vector.fill(3)(None)))).isLeft)
  }

  test("unequal runs and repeated labels retain distinct columns in a rank-deficient view") {
    val sampling = SamplingFrame(blockLens=Seq(2,4),tr=Seq(1.0,2.5),startTime=Seq(0.25,1.25))
    val first = RunScope.Run(RunIndex.unsafeOneBased(1))
    val second = RunScope.Run(RunIndex.unsafeOneBased(2))
    val origins = Vector(StructuralColumnOrigin.Intercept(first),StructuralColumnOrigin.Intercept(second),
      StructuralColumnOrigin.Sampled(ModulatorId.unsafe("motion-a"),ColumnRole.Nuisance,second),
      StructuralColumnOrigin.Sampled(ModulatorId.unsafe("motion-b"),ColumnRole.Nuisance,second))
    val columns = origins.zipWithIndex.map((origin,i) => StructuralColumn.fromOrigin(i+1,origin,"The same long source label that exceeds the plotted tick width").toOption.get)
    val raw = Mat.unsafe(6,4,Array(1.0,0.0,0.0,0.0, 1.0,0.0,0.0,0.0,
      0.0,1.0,1.0,2.0, 0.0,1.0,2.0,4.0, 0.0,1.0,3.0,6.0, 0.0,1.0,4.0,8.0))
    val compiled = DesignSchema.validated(raw,RowLayout.fromSamplingFrame(sampling),columns).toOption.get
    val kept = Vector(1,2,3,5,6).map(ScanIndex.unsafeOneBased)
    val review = DesignReview.forRun(compiled,sampling,RunIndex.unsafeOneBased(2),kept).toOption.get
    assertEquals(review.sourceColumnIndices,Vector(1,2,3))
    assertEquals(review.scans.map(_.source.oneBased),Vector(3,4,5,6))
    assertEquals(review.scans.map(_.time.value),Vector(1.25,3.75,6.25,8.75))
    assertEquals(review.scans.map(_.retained),Vector(true,false,true,true))
    assertEquals(review.repetitionTime.value,2.5)
    assertEquals(review.trace(columns(2).id).toOption.get.values,Vector(1.0,2.0,3.0,4.0).map(Some(_)))
    assertEquals(review.trace(columns(3).id).toOption.get.values,Vector(2.0,4.0,6.0,8.0).map(Some(_)))
    assertNotEquals(columns(2).id,columns(3).id)
    val page = DesignReviewGraphics.matrixScene(review,firstColumn=2,columnCount=1).toOption.get
    assert(page.semantics.plots.exists(_.accessibleDescription.contains(columns(3).id.value)))
    assert(page.semantics.plots.exists(_.accessibleDescription.contains(columns(3).label)))
  }
