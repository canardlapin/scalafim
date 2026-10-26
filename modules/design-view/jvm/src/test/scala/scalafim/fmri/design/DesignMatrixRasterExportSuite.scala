package scalafim.fmri.design

import intaglio.svg.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

class DesignMatrixRasterExportSuite extends munit.FunSuite:
  test("80x600 SVG contains one bounded raster and reports actual warmed export time"):
    val frame = SamplingFrame(blockLens = Seq(600), tr = Seq(2.0))
    val columns = Vector.tabulate(80)(i => StructuralColumn.fromOrigin(i + 1,
      StructuralColumnOrigin.Legacy(ModelSource.Event, s"column-$i", i + 1), s"column-$i").toOption.get)
    val schema = DesignSchema.validated(Mat.unsafe(600, 80, Array.tabulate(48000)(i => math.sin(i / 13.0))),
      RowLayout.fromSamplingFrame(frame), columns).toOption.get
    val review = DesignReview.forRun(schema, frame, RunIndex.unsafeOneBased(1), schema.rows.selectedRows,
      DesignReviewScope.SourceColumns).toOption.get
    def exportSvg(): String =
      val raster = DesignMatrixRaster.build(review).toOption.get
      SvgRenderer.render(raster.scene, SvgOptions.unsafe(width = 800, height = 600)).toOption.get.value
    (0 until 5).foreach(_ => assert(exportSvg().nonEmpty))
    val times = Vector.fill(15):
      val start = System.nanoTime()
      val svg = exportSvg()
      val elapsed = (System.nanoTime() - start).toDouble / 1e6
      assertEquals("<image".r.findAllIn(svg).length, 1)
      assert("<rect".r.findAllIn(svg).length <= 1)
      assert(svg.length < 270000, s"unexpected SVG expansion: ${svg.length}")
      elapsed
    println(s"80x600 raster+SVG export median_ms=${times.sorted.apply(times.size / 2)} max_ms=${times.max} bytes=${exportSvg().length}")
