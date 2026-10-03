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
    assertEquals(DesignMatrixRaster.build(r, maximumCells = 47999).left.toOption,
      Some(DesignMatrixRasterError.CellBudgetExceeded(48000L, 47999)))
    assertEquals(DesignMatrixRaster.build(r, maximumCells = 0).left.toOption, Some(DesignMatrixRasterError.InvalidCellBudget(0)))

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
    val lines = DesignMatrixRaster.toCsv(r).fold(error => fail(error.message), identity).linesIterator.toVector
    assertEquals(lines.length, 5)
    assert(lines.head.contains(r.columns.head.id.value))
    val row = lines(2).split(",")
    assertEquals(Vector(row(0), row(1), row(3)), Vector("2", "1", "true"))
    assertEqualsDouble(row(2).toDouble, 3.0, 0.0)
    assertEqualsDouble(row(4).toDouble, r.raw(1, 0), 0.0)

  private val event = StructuralColumnOrigin.Legacy(ModelSource.Event, "task", 1)
  private val nuisance = StructuralColumnOrigin.Nuisance(TermId.unsafe("nuisance"), ModulatorId.unsafe("motion_x"), RunScope.PerRun)

  private def custom(
      values: Vector[Vector[Double]],
      columns: Vector[StructuralColumn],
      tr: Double = 0.5,
      retained: Option[Vector[Int]] = None
  ): DesignReview =
    val rows = values.length
    val frame = SamplingFrame(blockLens = Seq(rows), tr = Seq(tr))
    val matrix = Mat.unsafe(rows, columns.length, values.flatten.toArray)
    val schema = DesignSchema.validated(matrix, RowLayout.fromSamplingFrame(frame), columns).fold(error => fail(error.message), identity)
    val kept = retained.fold(schema.rows.selectedRows)(_.map(ScanIndex.unsafeOneBased))
    DesignReview.forRun(schema, frame, RunIndex.unsafeOneBased(1), kept, DesignReviewScope.SourceColumns).fold(error => fail(error.message), identity)

  private def column(ordinal: Int, origin: StructuralColumnOrigin): StructuralColumn =
    StructuralColumn.fromOrigin(ordinal, origin, s"c$ordinal").fold(error => fail(error.message), identity)

  test("CSV sidecar bytes are identical on every platform, including fractions, exponents and censored scans"):
    val r = custom(
      Vector(Vector(1.0, 2.0), Vector(-0.25, 0.0), Vector(1e-7, -3.5)),
      Vector(column(1, event), column(2, nuisance)),
      retained = Some(Vector(1, 3))
    )
    val first = r.columns(0).id.value
    val second = r.columns(1).id.value
    // Acquisition times are mid-TR: 0.25, 0.75 and 1.25 seconds.
    val expected =
      s""""source_scan","run","time_seconds","retained","$first","$second"\n""" +
        "1,1,0.25,true,1,2\n" +
        "2,1,0.75,false,-0.25,0\n" +
        "3,1,1.25,true,1e-7,-3.5\n"
    assertEquals(DesignMatrixRaster.toCsv(r), Right(expected))

  test("CSV header quotes column ids and rejects ids that collide with sidecar columns"):
    val quotedId = StructuralColumn(ColumnId.unsafe("a,\"b\""), DesignColumnIndex.unsafeOneBased(1), event, "a")
    val quoted = custom(Vector(Vector(1.0), Vector(2.0)), Vector(quotedId))
    val header = DesignMatrixRaster.toCsv(quoted).fold(error => fail(error.message), identity).linesIterator.next()
    assertEquals(header, "\"source_scan\",\"run\",\"time_seconds\",\"retained\",\"a,\"\"b\"\"\"")
    DesignMatrixRaster.ReservedCsvColumns.foreach { reserved =>
      val clash = StructuralColumn(ColumnId.unsafe(reserved), DesignColumnIndex.unsafeOneBased(1), event, "a")
      val review = custom(Vector(Vector(1.0), Vector(2.0)), Vector(clash))
      assertEquals(DesignMatrixRaster.toCsv(review), Left(DesignMatrixRasterError.ReservedColumnId(ColumnId.unsafe(reserved))))
    }

  test("negative task values, muted non-task columns and censored scans render explicitly"):
    val r = custom(
      Vector(Vector(-2.0, 1.0), Vector(1.0, 0.0), Vector(0.0, -1.0)),
      Vector(column(1, event), column(2, nuisance)),
      retained = Some(Vector(1, 2))
    )
    assertEquals(r.scans.map(_.retained), Vector(true, true, false))
    val raster = DesignMatrixRaster.build(r).fold(error => fail(error.message), identity)
    // Task column: full-strength negative ink at the peak, half-strength positive ink.
    assertEquals(raster.image.pixelUnsafe(0, 0), Rgba32.unsafe(63, 108, 143))
    assertEquals(raster.image.pixelUnsafe(0, 1), Rgba32.unsafe(215, 173, 151))
    // Non-task column: one muted ink regardless of sign, at 35% strength.
    assertEquals(raster.image.pixelUnsafe(1, 0), Rgba32.unsafe(184, 191, 187))
    assertEquals(raster.image.pixelUnsafe(1, 2), Rgba32.unsafe(184, 191, 187))
    assertEquals(raster.image.pixelUnsafe(1, 1), Rgba32.unsafe(248, 249, 247))
    // Censored scans keep their source row and raw values in the image and sidecar.
    assertEquals(raster.scans, Vector(1, 2, 3).map(ScanIndex.unsafeOneBased))
    val lastRow = DesignMatrixRaster.toCsv(r).fold(error => fail(error.message), identity).linesIterator.toVector.last
    assertEquals(lastRow, "3,1,1.25,false,0,-1")
