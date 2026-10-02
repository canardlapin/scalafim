package scalafim.fmri.design

import intaglio.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

class DesignMatrixRasterSuite extends munit.FunSuite:
  private def review(rows: Int, cols: Int): DesignReview =
    val frame = SamplingFrame(blockLens = Seq(rows), tr = Seq(2.0))
    val columns = Vector.tabulate(cols)(i => StructuralColumn.fromOrigin(i + 1,
      StructuralColumnOrigin.Legacy(ModelSource.Event, s"c$i", i + 1), s"c$i").toOption.get)
    val matrix = Mat.unsafe(rows, cols, Array.tabulate(rows * cols)(i => math.sin(i.toDouble / 13)))
    val schema = DesignSchema.validated(matrix, RowLayout.fromSamplingFrame(frame), columns).toOption.get
    DesignReview.forRun(schema, frame, RunIndex.unsafeOneBased(1), schema.rows.selectedRows, DesignReviewScope.SourceColumns).toOption.get

  test("80 by 600 matrix is one unpaged nearest-neighbour image with time down"):
    val r = review(600, 80)
    val raster = DesignMatrixRaster.build(r).toOption.get
    assertEquals(raster.image.width, 80)
    assertEquals(raster.image.height, 600)
    assertEquals(raster.scene.size, 1)
    assertEquals(raster.columns, r.columns.map(_.id))
    assertEquals(raster.scans, r.scans.map(_.source))
    assertEquals(raster.image.pixelUnsafe(0, 0), Rgba32.unsafe(248, 249, 247))
    assert(DesignMatrixRaster.build(r, maximumCells = 47999).isLeft)

  test("raster construction has a reproducible bounded benchmark specimen"):
    val r = review(600, 80)
    (0 until 5).foreach(_ => assert(DesignMatrixRaster.build(r).isRight))
    val elapsed = Vector.fill(15):
      val start = System.nanoTime()
      assertEquals(DesignMatrixRaster.build(r).toOption.get.scene.size, 1)
      (System.nanoTime() - start).toDouble / 1e6
    println(s"80x600 raster build median_ms=${elapsed.sorted.apply(elapsed.size / 2)} max_ms=${elapsed.max}")

  test("CSV sidecar contains identified raw values, never display-scaled values"):
    val r = review(4, 2)
    val lines = DesignMatrixRaster.toCsv(r).linesIterator.toVector
    assertEquals(lines.length, 5)
    assert(lines.head.contains(r.columns.head.id.value))
    val row = lines(2).split(",")
    assertEquals(Vector(row(0), row(1), row(3)), Vector("2", "1", "true"))
    assertEqualsDouble(row(2).toDouble, 3.0, 0.0)
    assertEqualsDouble(row(4).toDouble, r.raw(1, 0), 0.0)
