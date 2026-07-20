package scalafim.spatial

class SpatialLazyContractSuite extends munit.FunSuite with SpatialLazyContractLaws:

  override val spatialLazyAdapter: SpatialLazyContractAdapter =
    ReferenceSpatialLazyAdapter()

  registerSpatialLazyContractLaws()

private final case class ReferenceRoot(id: String, values: Vector[Double])

private final case class ReferenceView(
  root: ReferenceRoot,
  shift: Double,
  requestedRows: Vector[Int]
)

private enum ReferenceFailure:
  case InvalidRow(row: Int, size: Int)

private final class ReferenceSpatialLazyAdapter extends SpatialLazyContractAdapter:
  override type Root = ReferenceRoot
  override type View = ReferenceView
  override type Failure = ReferenceFailure

  private var rootSequence = 0
  private var counters = SpatialLazyWork(0, 0, 0, 0)
  private var lastSupport = Map.empty[ReferenceView, Vector[Int]]
  private var completedPasses = Map.empty[ReferenceView, Int]

  override def freshRoot(): ReferenceRoot =
    rootSequence += 1
    ReferenceRoot(s"root-$rootSequence", Vector(0.0, 8.0, 0.0, 4.0))

  override def rootId(root: ReferenceRoot): String =
    root.id

  override def rootView(root: ReferenceRoot): ReferenceView =
    ReferenceView(root, shift = 0.0, requestedRows = root.values.indices.toVector)

  override def halfShift(view: ReferenceView): Either[ReferenceFailure, ReferenceView] =
    Right(view.copy(shift = view.shift + 0.5))

  override def directFullShift(view: ReferenceView): Either[ReferenceFailure, ReferenceView] =
    Right(view.copy(shift = view.shift + 1.0))

  override def selectRows(
    view: ReferenceView,
    rows: Vector[Int]
  ): Either[ReferenceFailure, ReferenceView] =
    rows.find(row => row < 0 || row >= view.root.values.length) match
      case Some(row) => Left(ReferenceFailure.InvalidRow(row, view.root.values.length))
      case None => Right(view.copy(requestedRows = rows))

  override def invalidSelection(view: ReferenceView): Either[ReferenceFailure, ReferenceView] =
    selectRows(view, Vector(view.root.values.length))

  override def materialize(view: ReferenceView): Either[ReferenceFailure, Vector[Double]] =
    val support = sourceSupport(view)
    counters = counters.copy(
      sourceReads = counters.sourceReads + support.length,
      compilations = counters.compilations + 1,
      resamplings = counters.resamplings + 1,
      cacheWrites = counters.cacheWrites + 1
    )
    lastSupport = lastSupport.updated(view, support)
    completedPasses = completedPasses.updated(view, 1)
    Right(view.requestedRows.map(row => sample(view.root.values, row + view.shift)))

  override def explain(view: ReferenceView): SpatialLazyExplanation =
    SpatialLazyExplanation(
      rootId = view.root.id,
      rootDomain = "root",
      targetDomain = if view.shift == 0.0 then "root" else "shifted",
      requestedRows = view.requestedRows,
      sourceSupport = lastSupport.getOrElse(view, sourceSupport(view)),
      resamplingPasses = completedPasses.getOrElse(view, 0)
    )

  override def cacheKey(view: ReferenceView): String =
    s"${view.root.id}|shift=${view.shift}|rows=${view.requestedRows.mkString(",")}|linear"

  override def work: SpatialLazyWork =
    counters

  override def resetWork(): Unit =
    counters = SpatialLazyWork(0, 0, 0, 0)
    lastSupport = Map.empty
    completedPasses = Map.empty

  private def sourceSupport(view: ReferenceView): Vector[Int] =
    view.requestedRows
      .flatMap { row =>
        val coordinate = row + view.shift
        val lower = math.floor(coordinate).toInt
        val upper = lower + 1
        val fraction = coordinate - lower.toDouble
        val candidates = if fraction == 0.0 then Vector(lower) else Vector(lower, upper)
        candidates.filter(index => index >= 0 && index < view.root.values.length)
      }
      .distinct
      .sorted

  private def sample(values: Vector[Double], coordinate: Double): Double =
    val lower = math.floor(coordinate).toInt
    val upper = lower + 1
    val fraction = coordinate - lower.toDouble
    val lowerValue = if lower >= 0 && lower < values.length then values(lower) else 0.0
    val upperValue = if upper >= 0 && upper < values.length then values(upper) else 0.0
    lowerValue * (1.0 - fraction) + upperValue * fraction

private object ReferenceSpatialLazyAdapter:
  def apply(): ReferenceSpatialLazyAdapter =
    new ReferenceSpatialLazyAdapter
