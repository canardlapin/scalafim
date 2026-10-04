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

enum DesignMatrixRasterError:
  case EmptyAxes(columns: Int, scans: Int)
  case InvalidCellBudget(maximumCells: Int)
  case CellBudgetExceeded(cells: Long, maximumCells: Int)
  case ReservedColumnId(column: ColumnId)
  case NonFiniteValue(scan: ScanIndex, column: ColumnId)
  case Graphics(error: GraphicsError)

  def message: String = this match
    case EmptyAxes(columns, scans) => s"design raster needs at least one column and one scan, got $columns columns and $scans scans"
    case InvalidCellBudget(maximumCells) => s"design raster cell budget must be positive, got $maximumCells"
    case CellBudgetExceeded(cells, maximumCells) => s"design raster has $cells cells, exceeding the budget of $maximumCells; select fewer columns or scans"
    case ReservedColumnId(column) => s"design column '${column.value}' collides with a reserved CSV sidecar column"
    case NonFiniteValue(scan, column) => s"design value for scan ${scan.oneBased}, column '${column.value}' is not finite"
    case Graphics(error) => error.message

object DesignMatrixRaster:
  /** Sidecar columns that precede the design columns in [[toCsv]]. */
  val ReservedCsvColumns: Vector[String] = Vector("source_scan", "run", "time_seconds", "retained")

  /** Raw values and stable scan/column identities for an export sidecar.
    * Scaling and colour are display-only and never enter this table.
    *
    * The text is byte-identical on the JVM and Scala.js: numbers use
    * [[PortableNumber]], header fields are always RFC 4180 quoted, and rows end
    * with `\n`. A design column whose id equals a reserved sidecar column is
    * rejected rather than renamed.
    */
  def toCsv(review: DesignReview): Either[DesignMatrixRasterError, String] =
    review.columns.find(column => ReservedCsvColumns.contains(column.id.value)) match
      case Some(column) => Left(DesignMatrixRasterError.ReservedColumnId(column.id))
      case None =>
        def quoted(value: String): String = "\"" + value.replace("\"", "\"\"") + "\""
        val header = ReservedCsvColumns ++ review.columns.map(_.id.value)
        val out = new StringBuilder(header.map(quoted).mkString(",")).append('\n')
        var row = 0
        while row < review.scans.length do
          val scan = review.scans(row)
          out.append(scan.source.oneBased).append(',').append(scan.run.oneBased).append(',')
            .append(PortableNumber.format(scan.time.value)).append(',').append(if scan.retained then "true" else "false")
          var column = 0
          while column < review.columns.length do
            val value = review.raw(row, column)
            if !value.isFinite then return Left(DesignMatrixRasterError.NonFiniteValue(scan.source, review.columns(column).id))
            out.append(',').append(PortableNumber.format(value))
            column += 1
          out.append('\n')
          row += 1
        Right(out.result())

  private inline def mix(paper: Int, ink: Int, amount: Double): Int = math.round(paper + amount * (ink - paper)).toInt

  def build(review: DesignReview, maximumCells: Int = 2000000): Either[DesignMatrixRasterError, DesignMatrixRaster] =
    val cells = review.columns.size.toLong * review.scans.size
    if review.columns.isEmpty || review.scans.isEmpty then Left(DesignMatrixRasterError.EmptyAxes(review.columns.size, review.scans.size))
    else if maximumCells < 1 then Left(DesignMatrixRasterError.InvalidCellBudget(maximumCells))
    else if cells > maximumCells then Left(DesignMatrixRasterError.CellBudgetExceeded(cells, maximumCells))
    else
      RasterDimensions(review.columns.size, review.scans.size).left.map(DesignMatrixRasterError.Graphics(_)).map: dimensions =>
        def isTask(role: ColumnRole): Boolean = role match
          case ColumnRole.Task | ColumnRole.Trial | ColumnRole.TrialAggregate => true
          case _ => false
        val task = review.columns.map(c => c.origin match
          case StructuralColumnOrigin.Event(_, _, _, _, _, role, _) => isTask(role)
          case StructuralColumnOrigin.Sampled(_, role, _) => isTask(role)
          case StructuralColumnOrigin.Legacy(ModelSource.Event, _, _) => true
          case _ => false).toArray
        val pixels = new Array[Int](dimensions.pixelCount)
        var row = 0
        while row < dimensions.height do
          var column = 0
          while column < dimensions.width do
            val value = review.scaled(row, column)
            val isTaskColumn = task(column)
            val amount = math.min(1.0, math.abs(value)) * (if isTaskColumn then 1.0 else 0.35)
            val red = if !isTaskColumn then 65 else if value < 0 then 63 else 181
            val green = if !isTaskColumn then 82 else if value < 0 then 108 else 96
            val blue = if !isTaskColumn then 76 else if value < 0 then 143 else 55
            pixels(row * dimensions.width + column) =
              (mix(248, red, amount) << 24) | (mix(249, green, amount) << 16) | (mix(247, blue, amount) << 8) | 255
            column += 1
          row += 1
        val image = RasterImage.unsafeFromOwnedPackedArray(dimensions, pixels)
        val scene = Scene(Vector(Grob.imageUnsafe(image, Point.npcUnsafe(0.5, 0.5), Size.npcUnsafe(1.0, 1.0),
          interpolation = RasterInterpolation.Nearest)))
        DesignMatrixRaster(review.fingerprint, review.columns.map(_.id), review.scans.map(_.source), image, scene)
