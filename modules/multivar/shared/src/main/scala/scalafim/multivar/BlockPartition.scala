package scalafim.multivar

import scala.collection.mutable

final case class BlockSpec(id: BlockId, columns: IndexSet):
  require(
    columns.axis == IndexAxis.Column || columns.axis == IndexAxis.Feature,
    "block columns must use column or feature indices"
  )

  def size: Int =
    columns.length

  def globalColumns(localColumns: IndexSet): Either[MultivarError, IndexSet] =
    if localColumns.axis != IndexAxis.Column && localColumns.axis != IndexAxis.Feature then
      Left(MultivarError.InvalidBlockPartition("block-local columns must use column or feature indices"))
    else
      localColumns.requireWithin(size).flatMap { checked =>
        IndexSet.from(checked.indices.map(columns.indices), axis = columns.axis)
      }

  /** Map global column indices to block-local positions. Local indices are always
    * stamped with `IndexAxis.Feature`, whereas `globalColumns` preserves the block's
    * own axis.
    */
  def localColumns(globalColumns: IndexSet): Either[MultivarError, IndexSet] =
    MatrixView.requireColumnAxis(globalColumns).flatMap { checked =>
      val lookup = columns.indices.zipWithIndex.toMap
      val locals = Vector.newBuilder[Int]
      var i = 0
      var error = Option.empty[MultivarError]
      val indices = checked.indices
      while i < indices.length && error.isEmpty do
        lookup.get(indices(i)) match
          case Some(local) => locals += local
          case None        => error = Some(MultivarError.InvalidBlockPartition(s"column ${indices(i)} is not in block '${id.value}'"))
        i += 1
      error match
        case Some(value) => Left(value)
        case None        => IndexSet.from(locals.result(), axis = IndexAxis.Feature, limit = Some(size))
    }

final class BlockPartition private (
    val totalSize: Dimension,
    val blocks: Vector[BlockSpec]
):
  def blockCount: Int =
    blocks.length

  def blockFor(column: Int): Option[BlockSpec] =
    blocks.find(_.columns.contains(column))

  def block(id: BlockId): Option[BlockSpec] =
    blocks.find(_.id == id)

  def blockIndexFor(column: Int): Option[Int] =
    blocks.indexWhere(_.columns.contains(column)) match
      case -1    => None
      case index => Some(index)

  def columns: IndexSet =
    IndexSet.unsafe(blocks.flatMap(_.columns.indices), axis = IndexAxis.Column)

object BlockPartition:
  def from(totalSize: Dimension, blocks: Iterable[BlockSpec]): Either[MultivarError, BlockPartition] =
    val blockVector = blocks.toVector
    if blockVector.isEmpty then Left(MultivarError.EmptyBlockPartition)
    else
      val ids = mutable.HashSet.empty[String]
      val covered = Array.fill(totalSize.value)(false)
      var blockIndex = 0
      var error = Option.empty[MultivarError]

      while blockIndex < blockVector.length && error.isEmpty do
        val block = blockVector(blockIndex)
        if ids.contains(block.id.value) then error = Some(MultivarError.DuplicateBlock(block.id))
        else
          ids += block.id.value
          var i = 0
          val blockColumns = block.columns.indices
          while i < blockColumns.length && error.isEmpty do
            val column = blockColumns(i)
            if column < 0 || column >= totalSize.value then
              error = Some(MultivarError.IndexOutOfBounds(IndexAxis.Column, column, totalSize.value))
            else if covered(column) then error = Some(MultivarError.DuplicateIndex(IndexAxis.Column, column))
            else covered(column) = true
            i += 1
        blockIndex += 1

      if error.isEmpty then
        var column = 0
        while column < covered.length && error.isEmpty do
          if !covered(column) then error = Some(MultivarError.MissingBlockColumn(column))
          column += 1

      error match
        case Some(value) => Left(value)
        case None        => Right(new BlockPartition(totalSize, blockVector))

  def unsafe(totalSize: Dimension, blocks: Iterable[BlockSpec]): BlockPartition =
    from(totalSize, blocks).fold(error => throw new IllegalArgumentException(error.message), identity)
