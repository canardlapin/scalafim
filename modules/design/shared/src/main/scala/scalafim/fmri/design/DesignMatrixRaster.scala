package scalafim.fmri.design

import intaglio.*

/** All columns, scans downward, one pixel per observed design cell. Identified
  * values remain in the review for hover/export; no numeric dump enters SVG.
  */
final case class DesignMatrixRaster private[design] (
    fingerprint: DesignFingerprint,
    columns: Vector[ColumnId],
    scans: Vector[ScanIndex],
    image: RasterImage,
    scene: Scene
)

object DesignMatrixRaster:
  /** Raw values and stable scan/column identities for an export sidecar.
    * Scaling and colour are display-only and never enter this table.
    */
  def toCsv(review: DesignReview): String =
    def quoted(value: String): String = "\"" + value.replace("\"", "\"\"") + "\""
    val header = Vector("source_scan", "run", "time_seconds", "retained") ++ review.columns.map(_.id.value)
    val out = new StringBuilder(header.map(quoted).mkString(",") + "\n")
    review.scans.zipWithIndex.foreach: (scan, row) =>
      val values = Vector(scan.source.oneBased.toString, scan.run.oneBased.toString,
        scan.time.value.toString, scan.retained.toString) ++ review.columns.indices.map(review.raw(row, _).toString)
      val _ = out.append(values.mkString(",")).append('\n')
    out.result()

  def build(review: DesignReview, maximumCells: Int = 2000000): Either[GraphicsError, DesignMatrixRaster] =
    if review.columns.isEmpty || review.scans.isEmpty || maximumCells < 1 ||
        review.columns.size.toLong * review.scans.size > maximumCells then
      Left(GraphicsError.EmptyGeometry("design raster exceeds cell budget or has empty axes"))
    else
      RasterDimensions(review.columns.size, review.scans.size).map: dimensions =>
        def isTask(role: ColumnRole): Boolean = role match
          case ColumnRole.Task | ColumnRole.Trial | ColumnRole.TrialAggregate => true
          case _ => false
        val task = review.columns.map(c => c.origin match
          case StructuralColumnOrigin.Event(_, _, _, _, _, role, _) => isTask(role)
          case StructuralColumnOrigin.Sampled(_, role, _) => isTask(role)
          case StructuralColumnOrigin.Legacy(ModelSource.Event, _, _) => true
          case _ => false)
        val pixels = new Array[Int](dimensions.pixelCount)
        var row = 0
        while row < dimensions.height do
          var column = 0
          while column < dimensions.width do
            val value = review.scaled(row, column)
            val amount = math.min(1.0, math.abs(value)) * (if task(column) then 1.0 else 0.35)
            val (red, green, blue) =
              if !task(column) then (65, 82, 76)
              else if value < 0 then (63, 108, 143)
              else (181, 96, 55)
            def mix(paper: Int, ink: Int): Int = math.round(paper + amount * (ink - paper)).toInt
            pixels(row * dimensions.width + column) =
              (mix(248, red) << 24) | (mix(249, green) << 16) | (mix(247, blue) << 8) | 255
            column += 1
          row += 1
        val image = RasterImage.unsafeFromOwnedPackedArray(dimensions, pixels)
        val scene = Scene(Vector(Grob.imageUnsafe(image, Point.npcUnsafe(0.5, 0.5), Size.npcUnsafe(1.0, 1.0),
          interpolation = RasterInterpolation.Nearest)))
        DesignMatrixRaster(review.fingerprint, review.columns.map(_.id), review.scans.map(_.source), image, scene)
