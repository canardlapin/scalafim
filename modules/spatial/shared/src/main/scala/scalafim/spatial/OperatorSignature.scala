package scalafim.spatial

final case class OperatorShape private (rows: Int, cols: Int):
  require(rows >= 0 && cols >= 0, "operator shape dimensions must be non-negative")

  def label: String =
    s"${rows}x${cols}"

object OperatorShape:
  def build(rows: Int, cols: Int): Either[SpatialError, OperatorShape] =
    if rows < 0 || cols < 0 then
      Left(SpatialError.OperatorAssemblyFailed(s"operator shape must be non-negative, got ${rows}x${cols}"))
    else Right(new OperatorShape(rows, cols))

  private[spatial] def unsafe(rows: Int, cols: Int): OperatorShape =
    new OperatorShape(rows, cols)

final case class OperatorRecipe private (
  path: Vector[MorphismId],
  routing: RoutingPolicy,
  sampling: SamplingPolicy,
  rowSelection: RowSelection,
  allowInverses: Boolean,
  compiler: String
):
  require(path.nonEmpty, "operator recipe path must be non-empty")
  require(compiler.trim.nonEmpty, "operator compiler must be non-empty")

  def roi: Option[Vector[Int]] =
    rowSelection.roi

object OperatorRecipe:
  def build(
    path: Vector[MorphismId],
    routing: RoutingPolicy,
    sampling: SamplingPolicy,
    rowSelection: RowSelection,
    allowInverses: Boolean,
    compiler: String
  ): Either[SpatialError, OperatorRecipe] =
    val cleanCompiler = compiler.trim
    if path.isEmpty then Left(SpatialError.EmptyPath)
    else if cleanCompiler.isEmpty then Left(SpatialError.EmptyIdentifier("operator compiler"))
    else Right(new OperatorRecipe(path, routing, sampling, rowSelection, allowInverses, cleanCompiler))

  private[spatial] def unsafe(
    path: Vector[MorphismId],
    routing: RoutingPolicy,
    sampling: SamplingPolicy,
    rowSelection: RowSelection,
    allowInverses: Boolean,
    compiler: String
  ): OperatorRecipe =
    new OperatorRecipe(path, routing, sampling, rowSelection, allowInverses, compiler.trim)

final case class OperatorSignature private (
  source: DomainId,
  target: DomainId,
  shape: OperatorShape,
  recipe: OperatorRecipe
):
  def label: String =
    val pathLabel = recipe.path.map(_.value).mkString(">")
    val rowsLabel = recipe.rowSelection match
      case RowSelection.All => "all"
      case RowSelection.Rows(indices) => indices.mkString("[", ",", "]")
    s"${source.value}->${target.value}|$pathLabel|${recipe.routing}|${recipe.sampling}|rows=$rowsLabel|inv=${recipe.allowInverses}|${recipe.compiler}|${shape.label}"

object OperatorSignature:
  def build(
    source: DomainId,
    target: DomainId,
    shape: OperatorShape,
    recipe: OperatorRecipe
  ): Either[SpatialError, OperatorSignature] =
    Right(new OperatorSignature(source, target, shape, recipe))

  private[spatial] def unsafe(
    source: DomainId,
    target: DomainId,
    shape: OperatorShape,
    recipe: OperatorRecipe
  ): OperatorSignature =
    new OperatorSignature(source, target, shape, recipe)
