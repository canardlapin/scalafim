package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

final case class BlockPreprocessSpec(id: BlockId, spec: PreprocessSpec)

/** Blockwise column preprocessing over a [[BlockPartition]].
  *
  * Construct via [[BlockwisePreprocessor.fit]], which guarantees that `global` is the
  * composition of `blockPreprocessors` laid out in global column order, so `transform`
  * (which uses `global`) and `preprocessorFor` (which uses the block list) always agree.
  */
final case class BlockwisePreprocessor private (
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
    else validateSpecIds(partition, specs).flatMap { _ =>
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
                GaleNumerics.vectorFromArray(globalScale),
                GaleNumerics.vectorFromArray(globalShift)
              )
            )
          )
    }

  private def validateSpecIds(
      partition: BlockPartition,
      specs: Vector[BlockPreprocessSpec]
  ): Either[MultivarError, Unit] =
    val knownIds = partition.blocks.map(_.id).toSet
    val seenIds = scala.collection.mutable.HashSet.empty[String]
    var i = 0
    var error = Option.empty[MultivarError]
    while i < specs.length && error.isEmpty do
      val id = specs(i).id
      if !knownIds.contains(id) then
        error = Some(MultivarError.InvalidBlockPartition(s"block preprocess spec references unknown block '${id.value}'"))
      else if seenIds.contains(id.value) then error = Some(MultivarError.DuplicateBlock(id))
      else seenIds += id.value
      i += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(())

final case class BlockMapComponent(block: BlockSpec, map: MatrixMap):
  require(map.domain.size == block.size, "block map domain must match block width")

enum BlockCombination:
  case Sum
  case Weighted(weights: Vector[(BlockId, Double)])

  def weightFor(blockId: BlockId): Either[MultivarError, Double] =
    this match
      case Sum =>
        Right(1.0)
      case Weighted(weights) =>
        weights.find(_._1 == blockId) match
          case Some((_, weight)) => Right(weight)
          case None              => Left(MultivarError.InvalidMap(s"missing combination weight for block '${blockId.value}'"))

  private[multivar] def validate(partition: BlockPartition): Either[MultivarError, Unit] =
    this match
      case Sum =>
        Right(())
      case Weighted(weights) =>
        val expectedIds = partition.blocks.map(_.id).toSet
        val actualIds = scala.collection.mutable.HashSet.empty[String]
        var i = 0
        var error = Option.empty[MultivarError]
        while i < weights.length && error.isEmpty do
          val (id, weight) = weights(i)
          if !expectedIds.contains(id) then error = Some(MultivarError.InvalidMap(s"combination weight references unknown block '${id.value}'"))
          else if actualIds.contains(id.value) then error = Some(MultivarError.DuplicateBlock(id))
          else if !weight.isFinite then error = Some(MultivarError.NonFiniteValue("block combination weight", i, weight))
          else actualIds += id.value
          i += 1
        if error.isEmpty && actualIds.size != expectedIds.size then
          error = Some(MultivarError.InvalidMap("weighted block combination must provide exactly one weight per block"))
        error match
          case Some(value) => Left(value)
          case None        => Right(())

final case class BlockMap private (
    domain: MvSpace,
    codomain: MvSpace,
    partition: BlockPartition,
    components: Vector[BlockMapComponent],
    combination: BlockCombination
) extends MvMap:
  override def forward(input: MatrixView): Either[MultivarError, DMat] =
    if input.cols != domain.size then
      Left(MultivarError.MatrixShapeMismatch(s"block map expected ${domain.size} input columns, got ${input.cols}"))
    else
      val out = new Array[Double](input.rows * codomain.size)
      var i = 0
      var error = Option.empty[MultivarError]
      while i < components.length && error.isEmpty do
        val component = components(i)
        val result =
          for
            blockInput <- input.selectColumns(component.block.columns)
            blockScores <- component.map.forward(blockInput)
            weight <- combination.weightFor(component.block.id)
          yield (blockScores, weight)
        result match
          case Left(value) =>
            error = Some(value)
          case Right((blockScores, weight)) =>
            addInPlace(out, input.rows, codomain.size, blockScores, weight)
        i += 1
      error match
        case Some(value) => Left(value)
        case None        => Right(GaleNumerics.matrixFromRowMajor(input.rows, codomain.size, out))

  /** Restricts the map to a column subset that lies within a single block.
    *
    * The restricted map carries the block's combination weight, so its `forward` equals
    * the block's weighted contribution of those columns in the full map's `forward`.
    */
  override def restrictInput(columns: IndexSet): Either[MultivarError, MvMap] =
    val checked = MatrixView.requireColumnIndexSet(columns, domain.size)
    checked.flatMap { globalColumns =>
      components.find(component => globalColumns.indices.forall(component.block.columns.contains)) match
        case Some(component) =>
          for
            localColumns <- component.block.localColumns(globalColumns)
            restricted <- component.map.restrictInput(localColumns)
            weight <- combination.weightFor(component.block.id)
          yield if weight == 1.0 then restricted else BlockMap.ScaledMap(restricted, weight)
        case None =>
          Left(MultivarError.InvalidBlockPartition("restricted block map input must lie within one block"))
    }

  override def decoder(using solver: PseudoInverseSolver): Either[MultivarError, Decoder] =
    Left(MultivarError.DecoderUnavailable("block-map decoder is not defined for combined block projections"))

  /** Projects one block through its component map, returning *unweighted* block scores.
    *
    * Under [[BlockCombination.Weighted]] this intentionally differs from the block's
    * contribution to `forward`, which scales these scores by the block's combination
    * weight. Use `restrictInput` when the weighted contribution is needed.
    */
  def projectBlock(input: MatrixView, blockId: BlockId): Either[MultivarError, DMat] =
    component(blockId) match
      case Some(value) =>
        input.selectColumns(value.block.columns).flatMap(value.map.forward)
      case None =>
        Left(MultivarError.InvalidBlockPartition(s"unknown block '${blockId.value}'"))

  /** Column-restricted variant of [[projectBlock]]; scores are likewise unweighted. */
  def projectBlock(
      input: MatrixView,
      blockId: BlockId,
      localColumns: IndexSet
  ): Either[MultivarError, DMat] =
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

  private def addInPlace(
      left: Array[Double],
      rows: Int,
      cols: Int,
      right: DMat,
      weight: Double
  ): Unit =
    require(right.rows == rows && right.cols == cols, "block outputs must have equal shape")
    val rightData = right.copyData
    var i = 0
    while i < left.length do
      left(i) += weight * rightData(i)
      i += 1

object BlockMap:
  def from(
      domain: MvSpace,
      codomain: MvSpace,
      partition: BlockPartition,
      components: Vector[BlockMapComponent],
      combination: BlockCombination = BlockCombination.Sum
  ): Either[MultivarError, BlockMap] =
    if domain.size != partition.totalSize.value then
      Left(MultivarError.InvalidMap(s"block map domain has ${domain.size} columns but partition covers ${partition.totalSize.value}"))
    else if components.length != partition.blocks.length then
      Left(MultivarError.InvalidMap("block map must provide exactly one map per partition block"))
    else
      var i = 0
      var error = Option.empty[MultivarError]
      while i < partition.blocks.length && error.isEmpty do
        val block = partition.blocks(i)
        components.find(_.block.id == block.id) match
          case None =>
            error = Some(MultivarError.InvalidMap("block map components must match partition block ids"))
          case Some(component) if component.block != block =>
            error = Some(
              MultivarError.InvalidMap(
                s"block map component '${block.id.value}' does not match the partition block columns"
              )
            )
          case Some(_) =>
            ()
        i += 1
      error match
        case Some(value) => Left(value)
        case None =>
          if components.exists(_.map.codomain != codomain) then
            Left(MultivarError.InvalidMap("all block maps must share the block-map codomain"))
          else combination.validate(partition).map(_ => BlockMap(domain, codomain, partition, components, combination))

  /** Wraps a restricted component map so its forward output carries the block's
    * combination weight, keeping `restrictInput` a true restriction of the full map.
    */
  private[multivar] final case class ScaledMap(base: MvMap, weight: Double) extends MvMap:
    override def domain: MvSpace =
      base.domain

    override def codomain: MvSpace =
      base.codomain

    override def forward(input: MatrixView): Either[MultivarError, DMat] =
      base.forward(input).map(scaledBy(weight))

    override def restrictInput(columns: IndexSet): Either[MultivarError, MvMap] =
      base.restrictInput(columns).map(ScaledMap(_, weight))

    override def decoder(using solver: PseudoInverseSolver): Either[MultivarError, Decoder] =
      val reciprocal = 1.0 / weight
      if !reciprocal.isFinite then
        Left(MultivarError.DecoderUnavailable("scaled block map combination weight has no finite reciprocal for a decoder"))
      else base.decoder.map(value => value.copy(weights = scaledBy(reciprocal)(value.weights)))

  private def scaledBy(factor: Double)(matrix: DMat): DMat =
    val data = matrix.copyData
    val out = new Array[Double](data.length)
    var i = 0
    while i < out.length do
      out(i) = factor * data(i)
      i += 1
    GaleNumerics.matrixFromRowMajor(matrix.rows, matrix.cols, out)
