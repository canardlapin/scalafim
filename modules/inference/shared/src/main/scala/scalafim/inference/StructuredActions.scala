package scalafim.inference

import scalafim.linalg.DoubleMatrix
import scalafim.multivar.RowProjector
import scalafim.multivar.RowWhitening

final case class RowPermutation private (
    sourceRows: Vector[RowIx]
):
  def rowCount: Int = sourceRows.length

  def applyTo(input: DoubleMatrix): Either[InferenceError, DoubleMatrix] =
    if input.rows != rowCount then
      Left(InferenceError.RowCountMismatch("row permutation input", rowCount, input.rows))
    else
      val out = new Array[Double](input.rows * input.cols)
      var targetRow = 0
      while targetRow < input.rows do
        val sourceRow = sourceRows(targetRow).value
        var col = 0
        while col < input.cols do
          out(targetRow * input.cols + col) = input(sourceRow, col)
          col += 1
        targetRow += 1
      Right(DoubleMatrix.fromRows(Vector.tabulate(input.rows) { row =>
        Vector.tabulate(input.cols)(col => out(row * input.cols + col))
      }))

object RowPermutation:
  def from(sourceRows: Iterable[Int]): Either[InferenceError, RowPermutation] =
    val values = sourceRows.iterator.toVector
    if values.isEmpty then Left(InferenceError.InvalidPartition("row permutation must be non-empty"))
    else
      val seen = Array.fill(values.length)(false)
      var i = 0
      var error = Option.empty[InferenceError]
      while i < values.length && error.isEmpty do
        val row = values(i)
        if row < 0 || row >= values.length then
          error = Some(InferenceError.InvalidPartition(
            s"permutation source row $row is outside [0, ${values.length})"
          ))
        else if seen(row) then
          error = Some(InferenceError.InvalidPartition(
            s"permutation source row $row appears more than once"
          ))
        else seen(row) = true
        i += 1
      error.toLeft(RowPermutation(values.map(RowIx.unsafe)))

  private[inference] def unsafe(sourceRows: Array[Int]): RowPermutation =
    RowPermutation(sourceRows.iterator.map(RowIx.unsafe).toVector)

sealed trait PermutationAction:
  def rowCount: RowCount
  def draw(seed: RootSeed, replicate: ReplicateId): Either[InferenceError, RowPermutation]

object PermutationAction:
  final case class Unrestricted private[inference] (
      rowCount: RowCount
  ) extends PermutationAction:
    override def draw(
        seed: RootSeed,
        replicate: ReplicateId
    ): Either[InferenceError, RowPermutation] =
      RandomSource.forReplicate(seed, replicate).permutation(rowCount.value).map { case (rows, _) =>
        RowPermutation.unsafe(rows.toArray)
      }

  final case class WithinGroups private[inference] (
      partition: RowPartition
  ) extends PermutationAction:
    override def rowCount: RowCount = partition.rowCount

    override def draw(
        seed: RootSeed,
        replicate: ReplicateId
    ): Either[InferenceError, RowPermutation] =
      val out = Array.tabulate(rowCount.value)(identity)
      var cursor = RandomSource.forReplicate(seed, replicate)
      var groupIndex = 0
      var error = Option.empty[InferenceError]
      while groupIndex < partition.groups.length && error.isEmpty do
        val group = partition.groups(groupIndex)
        cursor.permutation(group.length) match
          case Left(value) => error = Some(value)
          case Right((local, next)) =>
            var i = 0
            while i < group.length do
              out(group(i).value) = group(local(i)).value
              i += 1
            cursor = next
        groupIndex += 1
      error.toLeft(RowPermutation.unsafe(out))

  final case class WholeClusters private[inference] (
      partition: ClusterPartition,
      clusterSize: Int
  ) extends PermutationAction:
    override def rowCount: RowCount = partition.rowCount

    override def draw(
        seed: RootSeed,
        replicate: ReplicateId
    ): Either[InferenceError, RowPermutation] =
      RandomSource.forReplicate(seed, replicate).permutation(partition.clusters.length).map {
        case (clusterOrder, _) =>
          val out = new Array[Int](rowCount.value)
          var targetCluster = 0
          while targetCluster < partition.clusters.length do
            val target = partition.clusters(targetCluster)
            val source = partition.clusters(clusterOrder(targetCluster))
            var within = 0
            while within < clusterSize do
              out(target(within).value) = source(within).value
              within += 1
            targetCluster += 1
          RowPermutation.unsafe(out)
      }

  def unrestricted(rows: RowCount): PermutationAction =
    Unrestricted(rows)

  def withinBlocks(partition: RowPartition): PermutationAction =
    WithinGroups(partition)

  def withinStrata(partition: StrataPartition): PermutationAction =
    WithinGroups(partition.value)

  def wholeClusters(partition: ClusterPartition): Either[InferenceError, PermutationAction] =
    val sizes = partition.clusters.map(_.length).distinct
    if sizes.length != 1 then
      Left(InferenceError.UnsupportedProblem(
        "whole-cluster permutation requires equal cluster sizes"
      ))
    else Right(WholeClusters(partition, sizes.head))

  def forDesign[K <: DesignKind](
      design: ResamplingDesign[K]
  ): Either[InferenceError, PermutationAction] =
    (design.units, design.exchangeability) match
      case (SamplingUnits.Rows(rows), Exchangeability.Unrestricted) =>
        Right(unrestricted(rows))
      case (SamplingUnits.Rows(_), Exchangeability.WithinBlocks(partition)) =>
        Right(withinBlocks(partition))
      case (SamplingUnits.Rows(_), Exchangeability.WithinStrata(partition)) =>
        Right(withinStrata(partition))
      case (SamplingUnits.Clusters(partition), Exchangeability.Unrestricted) =>
        wholeClusters(partition)
      case _ =>
        Left(InferenceError.UnsupportedProblem(
          "cluster permutation within row-level blocks or strata has no proved action"
        ))

final case class ResidualPermutationAction private (
    reference: ConditioningRef,
    rowCount: RowCount,
    nuisance: RowProjector,
    residual: RowProjector,
    whitening: RowWhitening,
    permutation: PermutationAction
):
  def draw(
      input: DoubleMatrix,
      seed: RootSeed,
      replicate: ReplicateId
  ): Either[InferenceError, DoubleMatrix] =
    if input.rows != rowCount.value then
      Left(InferenceError.RowCountMismatch(
        "conditioned permutation input",
        rowCount.value,
        input.rows
      ))
    else
      firstNonFinite(input) match
        case Some((index, value)) =>
          Left(InferenceError.NonFiniteStatistic(
            s"conditioned permutation input entry $index",
            value
          ))
        case None =>
          for
            whitened <- adapt("row whitening", whitening.whiten(input))
            fitted <- adapt("nuisance projection", nuisance.project(whitened))
            residuals <- adapt("residual projection", residual.project(whitened))
            rowOrder <- permutation.draw(seed, replicate)
            permuted <- rowOrder.applyTo(residuals)
            randomized = add(fitted, permuted)
            restored <- adapt("row unwhitening", whitening.unwhiten(randomized))
          yield restored

  private def firstNonFinite(input: DoubleMatrix): Option[(Int, Double)] =
    val values = input.copyData
    var i = 0
    while i < values.length do
      if !values(i).isFinite then return Some((i, values(i)))
      i += 1
    None

  private def add(left: DoubleMatrix, right: DoubleMatrix): DoubleMatrix =
    val out = new Array[Double](left.rows * left.cols)
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        out(row * left.cols + col) = left(row, col) + right(row, col)
        col += 1
      row += 1
    DoubleMatrix.fromRows(Vector.tabulate(left.rows) { row =>
      Vector.tabulate(left.cols)(col => out(row * left.cols + col))
    })

  private def adapt[A](
      role: String,
      value: Either[scalafim.multivar.MultivarError, A]
  ): Either[InferenceError, A] =
    value.left.map(error => InferenceError.NumericalFailure(role, error.message))

object ResidualPermutationAction:
  def from(
      design: ResamplingDesign[DesignKind.ConditionedRows],
      reference: ConditioningRef,
      nuisance: RowProjector,
      whitening: Option[RowWhitening]
  ): Either[InferenceError, ResidualPermutationAction] =
    design.conditioning match
      case Conditioning.Unadjusted =>
        Left(InferenceError.UnsupportedProblem(
          "residual permutation requires a nuisance-conditioned design"
        ))
      case Conditioning.Nuisance(expectedReference, rows, requirement) =>
        if reference != expectedReference then
          Left(InferenceError.UnsupportedProblem(
            s"conditioning resource '${reference.value}' does not match '${expectedReference.value}'"
          ))
        else if nuisance.rows != rows.value then
          Left(InferenceError.RowCountMismatch(
            "nuisance projector",
            rows.value,
            nuisance.rows
          ))
        else if requirement == WhiteningRequirement.Required && whitening.isEmpty then
          Left(InferenceError.UnsupportedProblem(
            "conditioned design requires an explicit row-whitening capability"
          ))
        else
          for
            permutation <- PermutationAction.forDesign(design)
            checkedWhitening <- whitening match
              case Some(value) if value.rows != rows.value =>
                Left(InferenceError.RowCountMismatch(
                  "row whitening",
                  rows.value,
                  value.rows
                ))
              case Some(value) => Right(value)
              case None => adaptIdentity(RowWhitening.identity(rows.value))
          yield ResidualPermutationAction(
            reference,
            rows,
            nuisance,
            nuisance.complement,
            checkedWhitening,
            permutation
          )

  private def adaptIdentity(
      value: Either[scalafim.multivar.MultivarError, RowWhitening]
  ): Either[InferenceError, RowWhitening] =
    value.left.map(error => InferenceError.NumericalFailure("identity row whitening", error.message))
