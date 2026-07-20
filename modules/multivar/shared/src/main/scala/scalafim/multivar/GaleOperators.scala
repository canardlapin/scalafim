package scalafim.multivar

import gale.linalg.DoubleLinearOperator
import gale.linalg.DVec
import gale.linalg.LinAlgError
import gale.linalg.LinearOperator
import gale.linalg.MutableDVec
import gale.sparse.Sparse

private[multivar] final case class LinearOperatorBlock(
    rowBlock: Int,
    columnBlock: Int,
    operator: DoubleLinearOperator
)

private[multivar] trait BlockStructuredOperator extends DoubleLinearOperator

private final case class TaggedBlockOperator(delegate: DoubleLinearOperator) extends BlockStructuredOperator:
  override def rows: Int = delegate.rows
  override def cols: Int = delegate.cols
  override def applyTo(input: DVec, output: MutableDVec): Unit =
    delegate.applyTo(input, output)
  override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
    delegate.transposeApplyTo(input, output)
  override def adjoint: DoubleLinearOperator =
    TaggedBlockOperator(delegate.adjoint)

private[multivar] object GaleOperators:
  def blockDiagonal(
      operators: IndexedSeq[DoubleLinearOperator]
  ): Either[LinAlgError, DoubleLinearOperator] =
    LinearOperator.blockDiagonal(operators).map(TaggedBlockOperator.apply)

  def blockMatrix(
      rowSizes: IndexedSeq[Int],
      columnSizes: IndexedSeq[Int],
      blocks: IndexedSeq[LinearOperatorBlock]
  ): Either[LinAlgError, DoubleLinearOperator] =
    if rowSizes.isEmpty || columnSizes.isEmpty then
      Left(LinAlgError.InvalidArgument("block operator dimensions must be non-empty"))
    else if rowSizes.exists(_ < 0) || columnSizes.exists(_ < 0) then
      Left(LinAlgError.InvalidArgument("block operator dimensions must be non-negative"))
    else
      val grid = Array.tabulate(rowSizes.length, columnSizes.length): (row, col) =>
        Sparse.zero(rowSizes(row), columnSizes(col)): DoubleLinearOperator
      val seen = scala.collection.mutable.HashSet.empty[(Int, Int)]
      var index = 0
      var error = Option.empty[LinAlgError]
      while index < blocks.length && error.isEmpty do
        val block = blocks(index)
        val key = block.rowBlock -> block.columnBlock
        if block.rowBlock < 0 || block.rowBlock >= rowSizes.length then
          error = Some(LinAlgError.IndexOutOfBounds(block.rowBlock, rowSizes.length))
        else if block.columnBlock < 0 || block.columnBlock >= columnSizes.length then
          error = Some(LinAlgError.IndexOutOfBounds(block.columnBlock, columnSizes.length))
        else if seen.contains(key) then
          error = Some(LinAlgError.InvalidArgument(s"duplicate block operator at $key"))
        else if block.operator.rows != rowSizes(block.rowBlock) || block.operator.cols != columnSizes(block.columnBlock) then
          error = Some(LinAlgError.InvalidArgument(s"operator at $key does not match its block dimensions"))
        else
          seen += key
          grid(block.rowBlock)(block.columnBlock) = block.operator
        index += 1
      error match
        case Some(value) => Left(value)
        case None =>
          LinearOperator.block(grid.map(_.toIndexedSeq).toIndexedSeq).map(TaggedBlockOperator.apply)

  def scale(
      operator: DoubleLinearOperator,
      coefficient: Double
  ): Either[LinAlgError, DoubleLinearOperator] =
    if !coefficient.isFinite then Left(LinAlgError.InvalidArgument(s"operator scale must be finite: $coefficient"))
    else Right(operator.scaled(coefficient))
