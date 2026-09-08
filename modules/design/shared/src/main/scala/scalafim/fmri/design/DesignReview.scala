package scalafim.fmri.design

import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.design.SamplingFrame

enum DesignReviewScope:
  case SourceColumns, RunwiseFit

final case class ReviewScan(source: ScanIndex, run: RunIndex, withinRun: Int, time: Seconds, retained: Boolean):
  require(withinRun >= 1 && time.value.isFinite)

/** Values retain original row positions. Missing samples are not zeros. */
final case class ReviewSeries(name: String, units: String, values: Vector[Option[Double]],
    designColumn: Option[(DesignFingerprint,ColumnId)] = None):
  require(name.trim.nonEmpty && units.trim.nonEmpty)
  require(values.forall(_.forall(_.isFinite)), "review series must use missing values explicitly")

/** A cheap identified view, not an all-cell object export. Values are the raw
  * source design; peak scaling is solely for display, including constant columns.
  */
final class DesignReview private (
    val schema: DesignSchema,
    val run: RunIndex,
    val scope: DesignReviewScope,
    val scans: Vector[ReviewScan],
    val columns: Vector[StructuralColumn],
    val sourceColumnIndices: Vector[Int],
    val projection: Either[DesignError, RunCoefficientProjection],
    val repetitionTime: Seconds
):
  val fingerprint: DesignFingerprint = schema.fingerprint
  val peaks: Vector[Double] = columns.indices.map(c => scans.indices.map(r => math.abs(raw(r,c))).max).toVector
  def raw(row: Int, column: Int): Double = schema.matrix(scans(row).source.oneBased - 1, sourceColumnIndices(column))
  def scaled(row: Int, column: Int): Double = if peaks(column) == 0.0 then 0.0 else raw(row,column) / peaks(column)
  def trace(column: ColumnId): Either[DesignError, ReviewSeries] =
    val index = columns.indexWhere(_.id == column)
    if index < 0 then Left(DesignError.InvalidSchema("selected column is not in this design review"))
    else Right(ReviewSeries(columns(index).label, "design value", scans.indices.map(r => Some(raw(r,index))).toVector,Some(fingerprint -> column)))

object DesignReview:
  def forRun(
      schema: DesignSchema,
      frame: SamplingFrame,
      run: RunIndex,
      retained: Vector[ScanIndex],
      scope: DesignReviewScope = DesignReviewScope.RunwiseFit
  ): Either[DesignError, DesignReview] =
    if schema.rows.acquisitionTimes != frame.acquisitionOnsets() ||
        schema.rows.blockIds != frame.blockIdsPerSample.map(i => RunIndex.unsafeOneBased(i + 1)) then
      Left(DesignError.InvalidSchema("review sampling frame does not match the compiled design"))
    else if run.oneBased > frame.nBlocks then Left(DesignError.InvalidSchema("review run is outside the sample frame"))
    else if retained.isEmpty || retained.distinct.size != retained.size || retained.exists(_.oneBased > schema.matrix.rows) then
      Left(DesignError.InvalidSchema("review retained scans must be nonempty, unique source rows"))
    else
      val projection = schema.runwiseProjection(run, retained)
      val axis = scope match
        case DesignReviewScope.SourceColumns => Right(schema.columns -> schema.columns.indices.toVector)
        case DesignReviewScope.RunwiseFit => projection.map(p => p.axis.columns -> p.sourceColumnIndices)
      axis.map { (columns, indices) =>
        val rows = schema.rows.blockIds.indices.filter(i => schema.rows.blockIds(i) == run).toVector
        val times = frame.samples(blocks = Vector(run.oneBased - 1))
        val selected = retained.toSet
        val scans = rows.zipWithIndex.map { (row, local) =>
          val id = ScanIndex.unsafeOneBased(row + 1)
          ReviewScan(id, run, local + 1, times(local), selected(id))
        }
        new DesignReview(schema, run, scope, scans, columns, indices, projection, frame.tr(run.oneBased - 1))
      }
