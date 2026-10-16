package scalafim.fmri.design

import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

class DesignReviewGraphicsSuite extends munit.FunSuite:
  private val first = RunScope.Run(RunIndex.unsafeOneBased(1))
  private val second = RunScope.Run(RunIndex.unsafeOneBased(2))
  private val frame = SamplingFrame(blockLens=Seq(4,4),tr=Seq(1.0,1.5),startTime=Seq(0.0,0.25))
  private val columns = Vector(
    StructuralColumn.fromOrigin(1,StructuralColumnOrigin.Intercept(first),"off-page first").toOption.get,
    StructuralColumn.fromOrigin(2,StructuralColumnOrigin.Intercept(second),"same long truncated prefix <&> page two full source label").toOption.get,
    StructuralColumn.fromOrigin(3,StructuralColumnOrigin.Sampled(ModulatorId.unsafe("motion"),ColumnRole.Nuisance,second),"same long truncated prefix <&> page three full source label").toOption.get,
    StructuralColumn.fromOrigin(4,StructuralColumnOrigin.Sampled(ModulatorId.unsafe("off-page"),ColumnRole.Nuisance,second),"off-page last").toOption.get)
  private val event = EventRowProvenance(TrialId.unsafe("trial<&>"),None,7,1,1.s,0.5.s)
  private val schema = DesignSchema.validated(
    Mat.unsafe(8,4,Array(
      1.0,0.0,0.0,0.0,
      1.0,0.0,0.0,0.0,
      1.0,0.0,0.0,0.0,
      1.0,0.0,0.0,0.0,
      0.0,2.0,10.0,100.0,
      0.0,-4.0,20.0,200.0,
      0.0,1.0,-5.0,300.0,
      0.0,0.0,40.0,400.0)),
    RowLayout.fromSamplingFrame(frame),columns,
    DesignAudit(eventProvenance=Vector(event))).toOption.get
  private val review = DesignReview.forRun(schema,frame,RunIndex.unsafeOneBased(2),
    Vector(1,2,3,4,5,7,8).map(ScanIndex.unsafeOneBased),DesignReviewScope.SourceColumns).toOption.get

  test("matrix semantics retain complete identities and values only for the rendered page") {
    assertEquals(columns(1).label.take(28),columns(2).label.take(28))
    val description = DesignReviewGraphics.matrixPlot(review,firstColumn=1,columnCount=2,width=640,height=280).toOption.get
      .scene.semantics.accessibleDescription.getOrElse(fail("matrix semantics missing"))
    assert(description.contains("Source=" + review.fingerprint.value))
    assert(description.contains("Rows=5:0.25:true,6:1.75:false,7:3.25:true,8:4.75:true"))
    assert(description.contains(s"column=2; label=${columns(1).label}; id=${columns(1).id.value}; source-index=1; peak=${4.0}; raw=${Vector(2.0,-4.0,1.0,0.0).mkString(",")}"))
    assert(description.contains(s"column=3; label=${columns(2).label}; id=${columns(2).id.value}; source-index=2; peak=${40.0}; raw=${Vector(10.0,20.0,-5.0,40.0).mkString(",")}"))
    assert(!description.contains(columns.head.label))
    assert(!description.contains(columns.last.label))
  }

  test("trace semantics retain missing samples and original source rows") {
    val series = ReviewSeries("motion <&>","mm<&>",Vector(Some(1.0),None,Some(-2.5),Some(4.0)),
      Some(review.fingerprint -> columns(2).id))
    val description = DesignReviewGraphics.tracePlot(review.scans,Vector(series),width=640,height=160).toOption.get
      .scene.semantics.accessibleDescription.getOrElse(fail("trace semantics missing"))
    assert(description.contains(s"motion <&>; design-column=${series.designColumn}; units=mm<&>; values=${1.0},missing,${-2.5},${4.0}"))
    assert(description.contains("Original source scans=2:5:0.25:true,2:6:1.75:false,2:7:3.25:true,2:8:4.75:true"))
  }

  test("event timeline semantics retain canonical current provenance") {
    val description = DesignReviewGraphics.eventTimelinePlot(review,width=640,height=160).toOption.get
      .scene.semantics.accessibleDescription.getOrElse(fail("event semantics missing"))
    assertEquals(description,"Effective model event timing; " + event.canonical)
  }
