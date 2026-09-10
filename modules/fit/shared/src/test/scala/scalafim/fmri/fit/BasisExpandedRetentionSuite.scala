package scalafim.fmri.fit

import gale.linalg.Vec
import scalafim.dataset.{DataSelection, DatasetError, DatasetId, DatasetSeriesReader, FmriDataset, FmriSeries, InMemoryDatasetBackend}
import scalafim.fmri.design.ColumnId
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitEngine, FitPlan, FmriModel}
import scalafim.image.SampleSpaces
import scalafim.fmri.fit.GaleTestMatrix

class BasisExpandedRetentionSuite extends munit.FunSuite:
  private def right[A](value: Either[FitError, A]): A = value.fold(error => fail(error.message), identity)

  private val n = 24
  private val basisSize = 3
  private val taskColumns = 6
  private val voxels = 3

  /** Deterministic, well-conditioned basis curves: staggered Gaussian bumps. */
  private def taskValue(t: Int, column: Int): Double =
    val condition = column / basisSize
    val basis = column % basisSize
    val center = 3.0 + 9.0 * condition + 2.5 * basis
    math.exp(-math.pow(t - center, 2.0) / 8.0)

  private lazy val designRows: Vector[Vector[Double]] =
    Vector.tabulate(n)(t => Vector.tabulate(taskColumns)(column => taskValue(t, column)))

  /** voxel -> time. Known coefficients plus an intercept and noise. */
  private lazy val responses: Array[Array[Double]] =
    val rng = new scala.util.Random(11)
    val truth = Array.tabulate(voxels, taskColumns)((voxel, column) => 0.4 * (column + 1) - 0.3 * voxel)
    val intercepts = Array(1.5, -0.5, 0.25)
    Array.tabulate(voxels, n) { (voxel, t) =>
      var value = intercepts(voxel) + 0.3 * rng.nextGaussian()
      var column = 0
      while column < taskColumns do
        value += designRows(t)(column) * truth(voxel)(column)
        column += 1
      value
    }

  private lazy val model: FmriModel =
    val frame = SamplingFrame(blockLens = Seq(n), tr = Seq(1.0))
    val data = GaleTestMatrix.fromRows(Vector.tabulate(n)(t => Vector.tabulate(voxels)(voxel => responses(voxel)(t))))
    val dataset = FmriDataset.unsafe(
      InMemoryDatasetBackend(DatasetId("basis-expanded"), data, SampleSpaces(Vector(voxels, 1, 1))), frame)
    val event = EventModel(
      terms = Vector.empty, samplingFrame = frame,
      designMatrix = Mat.fromRows(designRows),
      columnNames = Vector("A:b0", "A:b1", "A:b2", "B:b0", "B:b1", "B:b2"),
      termSpans = Vector(0 -> taskColumns),
      colIndices = Map("cond" -> (0 until taskColumns).toVector)
    )
    FmriModel(event, BaselineModel.build(frame, basis = BaselineBasis.Constant, intercept = Intercept.Global), dataset)

  private class RecordingReader(val dataset: FmriDataset) extends DatasetSeriesReader:
    var selections = Vector.empty[DataSelection]
    def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
      selections :+= selection
      dataset.seriesEither(selection)

  private def structure(plan: FitPlan): TaskBasisStructure =
    val ids = plan.coefficientAxis.get.columnIds
    right(TaskBasisStructure.make(Vector(ids.slice(0, basisSize), ids.slice(basisSize, taskColumns))))

  /** Solve a small symmetric positive-definite system by Gaussian elimination
    * with partial pivoting — deliberately not the library's factorization.
    */
  private def solve(matrix: Array[Array[Double]], rhs: Array[Double]): Array[Double] =
    val size = rhs.length
    val a = Array.tabulate(size, size)((i, j) => matrix(i)(j))
    val b = rhs.clone()
    var pivot = 0
    while pivot < size do
      var best = pivot
      var row = pivot + 1
      while row < size do
        if math.abs(a(row)(pivot)) > math.abs(a(best)(pivot)) then best = row
        row += 1
      val swapRow = a(pivot); a(pivot) = a(best); a(best) = swapRow
      val swapValue = b(pivot); b(pivot) = b(best); b(best) = swapValue
      row = pivot + 1
      while row < size do
        val factor = a(row)(pivot) / a(pivot)(pivot)
        var col = pivot
        while col < size do
          a(row)(col) -= factor * a(pivot)(col)
          col += 1
        b(row) -= factor * b(pivot)
        row += 1
      pivot += 1
    val solution = new Array[Double](size)
    var row = size - 1
    while row >= 0 do
      var sum = b(row)
      var col = row + 1
      while col < size do
        sum -= a(row)(col) * solution(col)
        col += 1
      solution(row) = sum / a(row)(row)
      row -= 1
    solution

  /** Independent ground truth: literal OLS of each response on the collapsed
    * design `[T_A w, T_B w, 1]`, residuals formed in the time domain.
    */
  private def collapsedRefit(
      weights: Array[Double],
      contrast: Array[Double]
  ): (Array[Double], Array[Double], Double) =
    val collapsed = Array.tabulate(n, 3) { (t, column) =>
      if column == 2 then 1.0
      else
        var sum = 0.0
        var i = 0
        while i < basisSize do
          sum += weights(i) * designRows(t)(column * basisSize + i)
          i += 1
        sum
    }
    val normal = Array.tabulate(3, 3) { (a, b) =>
      var sum = 0.0
      var t = 0
      while t < n do
        sum += collapsed(t)(a) * collapsed(t)(b)
        t += 1
      sum
    }
    // c' M^-1 c on the task rows of the collapsed model.
    val fullContrast = Array(contrast(0), contrast(1), 0.0)
    val solvedContrast = solve(normal, fullContrast)
    var varianceScale = 0.0
    var index = 0
    while index < 3 do
      varianceScale += fullContrast(index) * solvedContrast(index)
      index += 1
    val df = (n - 3).toDouble
    val maps = new Array[Double](voxels)
    val ses = new Array[Double](voxels)
    var voxel = 0
    while voxel < voxels do
      val rhs = Array.tabulate(3) { column =>
        var sum = 0.0
        var t = 0
        while t < n do
          sum += collapsed(t)(column) * responses(voxel)(t)
          t += 1
        sum
      }
      val beta = solve(normal, rhs)
      var rss = 0.0
      var t = 0
      while t < n do
        val residual =
          responses(voxel)(t) - collapsed(t)(0) * beta(0) - collapsed(t)(1) * beta(1) - collapsed(t)(2) * beta(2)
        rss += residual * residual
        t += 1
      maps(voxel) = contrast(0) * beta(0) + contrast(1) * beta(1)
      ses(voxel) = math.sqrt(rss / df * varianceScale)
      voxel += 1
    (maps, ses, varianceScale)

  test("preparation reads nothing and the retained product reprojects to the collapsed refit") {
    val fitPlan = FitPlan(model)
    val reader = new RecordingReader(fitPlan.model.dataset)
    val plan = right(BasisExpandedRetention.prepare(fitPlan, structure(fitPlan), ChunkSize.unsafe(2)))
    assertEquals(reader.selections, Vector.empty)
    assertEquals(plan.rows, n)
    assertEquals(plan.nonTaskRank, 1)
    assertEqualsDouble(plan.residualDf, (n - 3).toDouble, 1e-12)
    val retained = right(plan.retainAll(reader))
    assertEquals(reader.selections.size, 2)
    assertEquals(retained.voxelIndices, Vector(0, 1, 2))
    assertEquals(retained.product.voxels, voxels)
    val weights = Array(0.8, 0.5, -0.3)
    val contrast = Array(1.0, -1.0)
    val maps = ContrastReprojection
      .reproject(retained.product, Vec(weights*), Vec(contrast*))
      .fold(error => fail(error.message), identity)
    val (expectedMaps, expectedSes, expectedScale) = collapsedRefit(weights, contrast)
    assertEqualsDouble(maps.contrastVarianceScale, expectedScale, 1e-10)
    assertEqualsDouble(maps.residualDf, (n - 3).toDouble, 1e-12)
    var voxel = 0
    while voxel < voxels do
      assertEqualsDouble(maps.contrast(voxel), expectedMaps(voxel), 1e-9, s"map voxel $voxel")
      assertEqualsDouble(maps.standardError(voxel), expectedSes(voxel), 1e-9, s"se voxel $voxel")
      voxel += 1
  }

  test("a second kernel from the same retained product matches its own refit") {
    val fitPlan = FitPlan(model)
    val plan = right(BasisExpandedRetention.prepare(fitPlan, structure(fitPlan), ChunkSize.unsafe(3)))
    val retained = right(plan.retainAll(new RecordingReader(fitPlan.model.dataset)))
    val weights = Array(0.1, -0.6, 1.2)
    val contrast = Array(0.5, 0.5)
    val maps = ContrastReprojection
      .reproject(retained.product, Vec(weights*), Vec(contrast*))
      .fold(error => fail(error.message), identity)
    val (expectedMaps, expectedSes, _) = collapsedRefit(weights, contrast)
    var voxel = 0
    while voxel < voxels do
      assertEqualsDouble(maps.contrast(voxel), expectedMaps(voxel), 1e-9, s"map voxel $voxel")
      assertEqualsDouble(maps.standardError(voxel), expectedSes(voxel), 1e-9, s"se voxel $voxel")
      voxel += 1
  }

  test("streamed blocks carry the same panel as the assembled product") {
    val fitPlan = FitPlan(model)
    val plan = right(BasisExpandedRetention.prepare(fitPlan, structure(fitPlan), ChunkSize.unsafe(2)))
    val retained = right(plan.retainAll(new RecordingReader(fitPlan.model.dataset)))
    var blocks = Vector.empty[BasisExpandedProductBlock]
    val outcome = right(plan.foreachBlock(new RecordingReader(fitPlan.model.dataset), block => { blocks :+= block; Right(()) }))
    assertEquals(outcome, EstimateExecutionOutcome.Completed(2, 3))
    assertEquals(blocks.map(_.voxelIndices), Vector(Vector(0, 1), Vector(2)))
    var offset = 0
    blocks.foreach { block =>
      var within = 0
      while within < block.product.voxels do
        var row = 0
        while row < block.product.taskColumns do
          assertEqualsDouble(
            block.product.crossProducts(row, within),
            retained.product.crossProducts(row, offset + within),
            1e-12, s"cross-product row $row voxel ${offset + within}")
          row += 1
        assertEqualsDouble(
          block.product.responseSquares(within),
          retained.product.responseSquares(offset + within),
          1e-12, s"response square voxel ${offset + within}")
        within += 1
      offset += block.product.voxels
    }
  }

  test("structure and preparation refuse malformed input before any read") {
    assert(TaskBasisStructure.make(Vector.empty).isLeft)
    assert(TaskBasisStructure.make(Vector(Vector.empty)).isLeft)
    val fitPlan = FitPlan(model)
    val ids = fitPlan.coefficientAxis.get.columnIds
    assert(TaskBasisStructure.make(Vector(ids.slice(0, 3), ids.slice(3, 5))).isLeft, "ragged basis counts")
    assert(TaskBasisStructure.make(Vector(ids.slice(0, 3), ids.slice(0, 3))).isLeft, "duplicate columns")
    val unknown = right(TaskBasisStructure.make(Vector(Vector(ColumnId.unsafe("missing")), Vector(ids(0)))))
    val reader = new RecordingReader(fitPlan.model.dataset)
    assert(BasisExpandedRetention.prepare(fitPlan, unknown, ChunkSize.unsafe(1)).isLeft)
    assertEquals(reader.selections, Vector.empty)
    val runwise = FitPlan(fitPlan.model, FitEngine.RunwiseLeastSquares)
    assert(BasisExpandedRetention.prepare(runwise, structure(fitPlan), ChunkSize.unsafe(1)).isLeft)
  }

  test("cancellation yields no partial assembled product") {
    val fitPlan = FitPlan(model)
    val plan = right(BasisExpandedRetention.prepare(fitPlan, structure(fitPlan), ChunkSize.unsafe(1)))
    val reader = new RecordingReader(fitPlan.model.dataset)
    assert(plan.retainAll(reader, cancelled = () => true).isLeft)
    assertEquals(reader.selections, Vector.empty)
  }
