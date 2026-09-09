package scalafim.fmri.fit

import gale.linalg.{CholeskyOptions, DMat, DVec, Matrix}
import scalafim.dataset.{DataSelection, DatasetSeriesReader}
import scalafim.fmri.design.ColumnId
import scalafim.fmri.model.{FitEngine, FitPlan}

/** The condition-major grouping of basis-expanded task columns: one entry per
  * condition, each naming that condition's basis columns in basis order. Every
  * condition must carry the same number of columns, and the grouping decides
  * the `column = condition * basisSize + basis` layout of the retained
  * product.
  */
final class TaskBasisStructure private (val conditions: Vector[Vector[ColumnId]]):
  def conditionCount: Int = conditions.length
  def basisSize: Int = conditions.head.length
  def taskColumnCount: Int = conditionCount * basisSize
  def columnIds: Vector[ColumnId] = conditions.flatten

object TaskBasisStructure:
  def make(conditions: Vector[Vector[ColumnId]]): Either[FitError, TaskBasisStructure] =
    if conditions.isEmpty then
      Left(FitError.InvalidFitAxis("task basis structure", "at least one condition is required"))
    else if conditions.exists(_.isEmpty) then
      Left(FitError.InvalidFitAxis("task basis structure", "every condition needs at least one basis column"))
    else if conditions.map(_.length).distinct.size != 1 then
      Left(FitError.InvalidFitAxis("task basis structure", "every condition must carry the same number of basis columns"))
    else
      val flat = conditions.flatten
      if flat.distinct.size != flat.size then
        Left(FitError.InvalidFitAxis("task basis structure", "task basis columns must be distinct"))
      else Right(new TaskBasisStructure(conditions))

/** One delivered block of the retained product. The executor retains no prior
  * blocks; each block is a complete [[BasisExpandedFitProduct]] for its own
  * voxels (the design-side Gram and degrees of freedom are shared).
  */
final case class BasisExpandedProductBlock private[fit] (
    ordinal: ChunkOrdinal,
    voxelIndices: Vector[Int],
    product: BasisExpandedFitProduct)

/** The whole selected voxel axis assembled from a completed stream. */
final case class RetainedBasisExpandedProduct private[fit] (
    voxelIndices: Vector[Int],
    product: BasisExpandedFitProduct)

/** Prepared only from the model and selected axes; no response is read during
  * preparation. Admission matches the compiled selective OLS route: shared
  * design, finite inputs, no temporal weighting. The realized design is split
  * into the declared task columns and everything else; the remainder is
  * partialled out of both sides (Frisch–Waugh–Lovell) once here, so streamed
  * blocks only ever touch response data.
  */
final class BasisExpandedRetentionPlan private[fit] (
    val fitPlan: FitPlan,
    val structure: TaskBasisStructure,
    val chunks: FitChunkPlan,
    val preparation: ResponsePreparationProvenance,
    val gram: DMat,
    val rows: Int,
    val nonTaskRank: Int,
    private val partialledTask: Array[Array[Double]],
    private val nuisance: Array[Array[Double]],
    private val nuisanceInverse: Array[Double]):

  def taskColumnCount: Int = structure.taskColumnCount
  def timepoints: Vector[Int] = chunks.timepoints
  def maxBlockVoxels: Int = chunks.iterator.map(_.voxelIndices.length).max
  def residualDf: Double = (rows - nonTaskRank - structure.conditionCount).toDouble

  /** Cancellation is checked before each read and before each delivery; the
    * caller owns reader, sink and backend lifetimes.
    */
  def foreachBlock(
      reader: DatasetSeriesReader,
      consume: BasisExpandedProductBlock => Either[FitError, Unit],
      cancelled: () => Boolean = () => false
  ): Either[FitError, EstimateExecutionOutcome] =
    EstimateBlockExecutor.foreachBlock(fitPlan, chunks, reader, evaluate, consume, cancelled)

  /** Stream every block and assemble one product over the full selected voxel
    * axis. Cancellation surfaces as the executor outcome; a cancelled stream
    * returns no partial product.
    */
  def retainAll(
      reader: DatasetSeriesReader,
      cancelled: () => Boolean = () => false
  ): Either[FitError, RetainedBasisExpandedProduct] =
    var blocks = Vector.empty[BasisExpandedProductBlock]
    foreachBlock(reader, block => { blocks :+= block; Right(()) }, cancelled).flatMap {
      case EstimateExecutionOutcome.Cancelled(chunksDone, voxelsDone) =>
        Left(FitError.InvalidFitAxis(
          "basis-expanded retention",
          s"cancelled after $chunksDone chunks / $voxelsDone voxels; no partial product is published"))
      case EstimateExecutionOutcome.Completed(_, _) =>
        val voxelIndices = blocks.flatMap(_.voxelIndices)
        val voxels = voxelIndices.length
        val offsets = blocks.scanLeft(0)(_ + _.voxelIndices.length)
        val columnOf = new Array[(BasisExpandedFitProduct, Int)](voxels)
        var blockIndex = 0
        while blockIndex < blocks.length do
          val product = blocks(blockIndex).product
          var within = 0
          while within < product.voxels do
            columnOf(offsets(blockIndex) + within) = (product, within)
            within += 1
          blockIndex += 1
        BasisExpandedFitProduct
          .make(
            conditions = structure.conditionCount,
            basisSize = structure.basisSize,
            gram = gram,
            crossProducts = DMat.tabulate(taskColumnCount, voxels) { (row, voxel) =>
              val (product, within) = columnOf(voxel)
              product.crossProducts(row, within)
            },
            responseSquares = DVec.tabulate(voxels) { voxel =>
              val (product, within) = columnOf(voxel)
              product.responseSquares(within)
            },
            rows = rows,
            nonTaskRank = nonTaskRank
          )
          .left
          .map(error => FitError.InvalidFitAxis("basis-expanded retention", error.message))
          .map(product => RetainedBasisExpandedProduct(voxelIndices, product))
    }

  private def evaluate(
      chunk: FitChunkSpec,
      response: ResponseBlock
  ): Either[FitError, BasisExpandedProductBlock] =
    val data = response.value
    val voxels = response.voxels
    val taskColumns = taskColumnCount
    val nuisanceColumns = nuisance.length
    val cross = new Array[Double](taskColumns * voxels)
    val squares = new Array[Double](voxels)
    val nuisanceProjection = new Array[Double](math.max(nuisanceColumns, 1))
    var voxel = 0
    while voxel < voxels do
      var column = 0
      while column < taskColumns do
        val curve = partialledTask(column)
        var sum = 0.0
        var t = 0
        while t < rows do
          sum += curve(t) * data(t, voxel)
          t += 1
        cross(column * voxels + voxel) = sum
        column += 1
      var total = 0.0
      var t = 0
      while t < rows do
        val value = data(t, voxel)
        total += value * value
        t += 1
      var adjustment = 0.0
      if nuisanceColumns > 0 then
        var i = 0
        while i < nuisanceColumns do
          val curve = nuisance(i)
          var sum = 0.0
          t = 0
          while t < rows do
            sum += curve(t) * data(t, voxel)
            t += 1
          nuisanceProjection(i) = sum
          i += 1
        i = 0
        while i < nuisanceColumns do
          var l = 0
          while l < nuisanceColumns do
            adjustment += nuisanceProjection(i) * nuisanceInverse(i * nuisanceColumns + l) * nuisanceProjection(l)
            l += 1
          i += 1
      squares(voxel) = math.max(total - adjustment, 0.0)
      voxel += 1
    BasisExpandedFitProduct
      .make(
        conditions = structure.conditionCount,
        basisSize = structure.basisSize,
        gram = gram,
        crossProducts = DMat.tabulate(taskColumns, voxels)((row, voxel) => cross(row * voxels + voxel)),
        responseSquares = DVec.tabulate(voxels)(squares(_)),
        rows = rows,
        nonTaskRank = nonTaskRank
      )
      .left
      .map(error => FitError.InvalidFitAxis("basis-expanded retention", error.message))
      .map(product => BasisExpandedProductBlock(chunk.ordinal, chunk.voxelIndices, product))

object BasisExpandedRetention:

  /** Prepare basis-expanded product retention without reading response data.
    * `structure` names the task columns of the realized design in
    * condition-major order; every other realized column (baseline, drift,
    * confounds) is partialled out. `blockSize` bounds each dataset request.
    */
  def prepare(
      plan: FitPlan,
      structure: TaskBasisStructure,
      blockSize: ChunkSize,
      selection: DataSelection = DataSelection.All,
      policy: OlsSolvePolicy = OlsSolvePolicy.Default
  ): Either[FitError, BasisExpandedRetentionPlan] =
    for
      preparation <- FirstLevelEstimates.admittedPreparation(plan, FitEngine.OrdinaryLeastSquares)
      schema <- plan.model.designSchema.toRight(
        FitError.InvalidFitAxis("basis-expanded design", "a structural design schema is required"))
      chunks <- FitChunkPlan.fromSelection(plan, selection, FitChunkingStrategy.ByVoxelCount(blockSize))
      design <- MatrixAdapters.designMatrix(plan.model, chunks.timepoints)
      built <- partition(schema.coefficientAxis.columnIds, structure, design, policy)
    yield
      val (gram, partialledTask, nuisanceColumns, nuisanceInverse) = built
      new BasisExpandedRetentionPlan(
        fitPlan = plan,
        structure = structure,
        chunks = chunks,
        preparation = preparation,
        gram = gram,
        rows = design.value.rows,
        nonTaskRank = nuisanceColumns.length,
        partialledTask = partialledTask,
        nuisance = nuisanceColumns,
        nuisanceInverse = nuisanceInverse
      )

  private def partition(
      axisColumns: Vector[ColumnId],
      structure: TaskBasisStructure,
      design: DesignMatrix,
      policy: OlsSolvePolicy
  ): Either[FitError, (DMat, Array[Array[Double]], Array[Array[Double]], Array[Double])] =
    val matrix = design.value
    val rows = matrix.rows
    val taskIds = structure.columnIds
    val taskIndices = new Array[Int](taskIds.length)
    var missing = Option.empty[ColumnId]
    var position = 0
    while position < taskIds.length && missing.isEmpty do
      val index = axisColumns.indexOf(taskIds(position))
      if index < 0 then missing = Some(taskIds(position))
      else taskIndices(position) = index
      position += 1
    missing match
      case Some(id) =>
        Left(FitError.InvalidFitAxis("task basis structure", s"unknown structural column '${id.value}'"))
      case None =>
        val minimumDf = rows - axisColumns.length
        if rows - (axisColumns.length - taskIds.length) - structure.conditionCount < 1 || minimumDf < 0 then
          Left(FitError.InvalidFitAxis(
            "basis-expanded design",
            s"$rows selected rows cannot support ${axisColumns.length} realized columns and ${structure.conditionCount} collapsed conditions"))
        else
          val taskSet = taskIndices.toSet
          val nuisanceIndices = axisColumns.indices.filterNot(taskSet.contains).toArray
          val task = Array.tabulate(taskIndices.length) { column =>
            val source = taskIndices(column)
            Array.tabulate(rows)(t => matrix(t, source))
          }
          val nuisanceColumns = Array.tabulate(nuisanceIndices.length) { column =>
            val source = nuisanceIndices(column)
            Array.tabulate(rows)(t => matrix(t, source))
          }
          partialInverse(nuisanceColumns, rows, policy).flatMap { nuisanceInverse =>
            val partialled = partialOut(task, nuisanceColumns, nuisanceInverse, rows)
            val taskColumns = partialled.length
            val gram = DMat.tabulate(taskColumns, taskColumns) { (a, b) =>
              val left = partialled(a)
              val right = partialled(b)
              var sum = 0.0
              var t = 0
              while t < rows do
                sum += left(t) * right(t)
                t += 1
              sum
            }
            gram
              .cholesky(CholeskyOptions(policy.choleskyTolerance))
              .left
              .map(FitError.SingularDesign.apply)
              .map(_ => (gram, partialled, nuisanceColumns, nuisanceInverse))
          }

  /** `(N'N)^-1` for the partialled-out columns, row-major; empty when there is
    * nothing to partial.
    */
  private def partialInverse(
      nuisanceColumns: Array[Array[Double]],
      rows: Int,
      policy: OlsSolvePolicy
  ): Either[FitError, Array[Double]] =
    val count = nuisanceColumns.length
    if count == 0 then Right(new Array[Double](0))
    else
      val crossProduct = DMat.tabulate(count, count) { (a, b) =>
        val left = nuisanceColumns(a)
        val right = nuisanceColumns(b)
        var sum = 0.0
        var t = 0
        while t < rows do
          sum += left(t) * right(t)
          t += 1
        sum
      }
      crossProduct
        .cholesky(CholeskyOptions(policy.choleskyTolerance))
        .left
        .map(FitError.SingularDesign.apply)
        .flatMap(_.solve(Matrix.eye(count)).left.map(FitError.SingularDesign.apply))
        .map { inverse =>
          val flat = new Array[Double](count * count)
          var a = 0
          while a < count do
            var b = 0
            while b < count do
              flat(a * count + b) = inverse(a, b)
              b += 1
            a += 1
          flat
        }

  /** `T - N (N'N)^-1 N'T`, computed column by column. */
  private def partialOut(
      task: Array[Array[Double]],
      nuisanceColumns: Array[Array[Double]],
      nuisanceInverse: Array[Double],
      rows: Int
  ): Array[Array[Double]] =
    val count = nuisanceColumns.length
    if count == 0 then task.map(_.clone())
    else
      task.map { column =>
        val projection = new Array[Double](count)
        var i = 0
        while i < count do
          val curve = nuisanceColumns(i)
          var sum = 0.0
          var t = 0
          while t < rows do
            sum += curve(t) * column(t)
            t += 1
          projection(i) = sum
          i += 1
        val solved = new Array[Double](count)
        i = 0
        while i < count do
          var sum = 0.0
          var l = 0
          while l < count do
            sum += nuisanceInverse(i * count + l) * projection(l)
            l += 1
          solved(i) = sum
          i += 1
        val result = new Array[Double](rows)
        var t = 0
        while t < rows do
          var value = column(t)
          i = 0
          while i < count do
            value -= nuisanceColumns(i)(t) * solved(i)
            i += 1
          result(t) = value
          t += 1
        result
      }
