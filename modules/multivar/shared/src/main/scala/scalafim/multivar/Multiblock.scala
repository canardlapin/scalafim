package scalafim.multivar

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

final case class BlockPreprocessSpec(id: BlockId, spec: PreprocessSpec)

final case class BlockwisePreprocessor(
    partition: BlockPartition,
    blockPreprocessors: Vector[(BlockSpec, FittedPreprocessor)],
    global: FittedColumnAffine
) extends FittedPreprocessor:
  override def inputCols: Int =
    partition.totalSize.value

  override def transform(
      input: MatrixView,
      columns: Option[IndexSet],
      policy: StoragePolicy
  ): Either[MultivarError, MatrixView] =
    global.transform(input, columns, policy)

  override def inverseTransform(
      input: MatrixView,
      columns: Option[IndexSet],
      policy: StoragePolicy
  ): Either[MultivarError, MatrixView] =
    global.inverseTransform(input, columns, policy)

  override def restrict(columns: IndexSet): Either[MultivarError, FittedPreprocessor] =
    global.restrict(columns)

  def preprocessorFor(blockId: BlockId): Option[FittedPreprocessor] =
    blockPreprocessors.find(_._1.id == blockId).map(_._2)

object BlockwisePreprocessor:
  def fit(
      input: MatrixView,
      partition: BlockPartition,
      specs: Vector[BlockPreprocessSpec],
      defaultSpec: PreprocessSpec = PreprocessSpec.Pass
  ): Either[MultivarError, BlockwisePreprocessor] =
    if input.cols != partition.totalSize.value then
      Left(MultivarError.MatrixShapeMismatch(s"input has ${input.cols} columns but partition covers ${partition.totalSize.value}"))
    else
      val specById = specs.map(spec => spec.id -> spec.spec).toMap
      val globalScale = Array.fill(partition.totalSize.value)(1.0)
      val globalShift = new Array[Double](partition.totalSize.value)
      val fitted = Vector.newBuilder[(BlockSpec, FittedPreprocessor)]
      var blockIndex = 0
      var error = Option.empty[MultivarError]

      while blockIndex < partition.blocks.length && error.isEmpty do
        val block = partition.blocks(blockIndex)
        val spec = specById.getOrElse(block.id, defaultSpec)
        val fitResult =
          for
            blockInput <- input.selectColumns(block.columns)
            blockFit <- spec.fit(blockInput)
          yield blockFit
        fitResult match
          case Left(value) =>
            error = Some(value)
          case Right(blockFit: FittedColumnAffine) =>
            fitted += block -> blockFit
            var local = 0
            while local < block.columns.length do
              val global = block.columns.indices(local)
              globalScale(global) = blockFit.scale(local)
              globalShift(global) = blockFit.shift(local)
              local += 1
          case Right(_) =>
            error = Some(MultivarError.InvalidBlockPartition("blockwise preprocessing currently requires column-affine block preprocessors"))
        blockIndex += 1

      error match
        case Some(value) => Left(value)
        case None =>
          Right(
            BlockwisePreprocessor(
              partition,
              fitted.result(),
              FittedColumnAffine(
                partition.totalSize.value,
                DoubleVector.unsafe(globalScale),
                DoubleVector.unsafe(globalShift)
              )
            )
          )

final case class BlockMapComponent(block: BlockSpec, map: MatrixMap):
  require(map.domain.size == block.size, "block map domain must match block width")

final case class BlockMap private (
    domain: MvSpace,
    codomain: MvSpace,
    partition: BlockPartition,
    components: Vector[BlockMapComponent]
) extends MvMap:
  override def forward(input: MatrixView): Either[MultivarError, DoubleMatrix] =
    if input.cols != domain.size then
      Left(MultivarError.MatrixShapeMismatch(s"block map expected ${domain.size} input columns, got ${input.cols}"))
    else
      val out = DoubleMatrix.zeros(input.rows, codomain.size)
      var i = 0
      var error = Option.empty[MultivarError]
      while i < components.length && error.isEmpty do
        val component = components(i)
        val result =
          for
            blockInput <- input.selectColumns(component.block.columns)
            blockScores <- component.map.forward(blockInput)
          yield blockScores
        result match
          case Left(value) =>
            error = Some(value)
          case Right(blockScores) =>
            addInPlace(out, blockScores)
        i += 1
      error match
        case Some(value) => Left(value)
        case None        => Right(out)

  override def restrictInput(columns: IndexSet): Either[MultivarError, MvMap] =
    val checked = MatrixView.requireColumnIndexSet(columns, domain.size)
    checked.flatMap { globalColumns =>
      components.find(component => globalColumns.indices.forall(component.block.columns.contains)) match
        case Some(component) =>
          component.block.localColumns(globalColumns).flatMap(component.map.restrictInput)
        case None =>
          Left(MultivarError.InvalidBlockPartition("restricted block map input must lie within one block"))
    }

  override def decoder(using solver: PseudoInverseSolver): Either[MultivarError, Decoder] =
    Left(MultivarError.DecoderUnavailable("block-map decoder is not defined without an explicit block-combination model"))

  def projectBlock(input: MatrixView, blockId: BlockId): Either[MultivarError, DoubleMatrix] =
    component(blockId) match
      case Some(value) =>
        input.selectColumns(value.block.columns).flatMap(value.map.forward)
      case None =>
        Left(MultivarError.InvalidBlockPartition(s"unknown block '${blockId.value}'"))

  def projectBlock(
      input: MatrixView,
      blockId: BlockId,
      localColumns: IndexSet
  ): Either[MultivarError, DoubleMatrix] =
    component(blockId) match
      case Some(value) =>
        for
          globalColumns <- value.block.globalColumns(localColumns)
          selectedInput <- input.selectColumns(globalColumns)
          restricted <- value.map.restrictInput(localColumns)
          scores <- restricted.forward(selectedInput)
        yield scores
      case None =>
        Left(MultivarError.InvalidBlockPartition(s"unknown block '${blockId.value}'"))

  private def component(blockId: BlockId): Option[BlockMapComponent] =
    components.find(_.block.id == blockId)

  private def addInPlace(left: DoubleMatrix, right: DoubleMatrix): Unit =
    require(left.rows == right.rows && left.cols == right.cols, "block outputs must have equal shape")
    var i = 0
    while i < left.dataArray.length do
      left.dataArray(i) += right.dataArray(i)
      i += 1

object BlockMap:
  def from(
      domain: MvSpace,
      codomain: MvSpace,
      partition: BlockPartition,
      components: Vector[BlockMapComponent]
  ): Either[MultivarError, BlockMap] =
    if domain.size != partition.totalSize.value then
      Left(MultivarError.InvalidMap(s"block map domain has ${domain.size} columns but partition covers ${partition.totalSize.value}"))
    else if components.length != partition.blocks.length then
      Left(MultivarError.InvalidMap("block map must provide exactly one map per partition block"))
    else
      val expectedIds = partition.blocks.map(_.id).toSet
      val actualIds = components.map(_.block.id).toSet
      if expectedIds != actualIds then Left(MultivarError.InvalidMap("block map components must match partition block ids"))
      else if components.exists(_.map.codomain != codomain) then
        Left(MultivarError.InvalidMap("all block maps must share the block-map codomain"))
      else Right(BlockMap(domain, codomain, partition, components))

final case class MultiblockProjection(blockMap: BlockMap, scores: DoubleMatrix):
  require(scores.cols == blockMap.codomain.size, "multiblock score columns must match codomain")

  def project(input: MatrixView): Either[MultivarError, DoubleMatrix] =
    blockMap.forward(input)

  def projectBlock(input: MatrixView, blockId: BlockId): Either[MultivarError, DoubleMatrix] =
    blockMap.projectBlock(input, blockId)

