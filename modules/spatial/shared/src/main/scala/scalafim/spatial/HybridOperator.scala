package scalafim.spatial

import gale.linalg.{DMat, DoubleLinearOperator, LinAlgError, LinearOperator}

/** How per-part operators assemble around a hybrid domain.
  *
  *   - `ToHybrid`: one source to every part of a hybrid target; part rows are stacked in part order (neurofunctor
  *     `compile_to_hybrid`).
  *   - `FromHybrid`: every part of a hybrid source to one target; part columns are concatenated, so the target
  *     receives the sum of the parts' contributions (neurofunctor `compile_from_hybrid`).
  *   - `BlockDiagonal`: a hybrid source to a hybrid target, part i to part i.
  */
enum HybridLayout:
  case ToHybrid, FromHybrid, BlockDiagonal

/** Settings shared by every part compilation. */
final case class HybridCompileOptions(
  routing: RoutingPolicy = RoutingPolicy.Shortest,
  sampling: SamplingPolicy = SamplingPolicy.Trilinear,
  allowInverses: Boolean = false
)

/** One compiled part: its name, its offset in the hybrid domain's element order, and its own operator. */
final case class HybridPartOperator(name: PartName, offset: Int, operator: SpatialOperator)

/** Per-part operators assembled into one operator on a hybrid domain, with Gale's block assembly.
  *
  * Each part keeps its own compiled [[SpatialOperator]], so paths, coverage, provenance and stored weights stay
  * inspectable per part; `map` is the assembled operator and has a real adjoint.
  */
final case class HybridOperator private (
  source: DomainId,
  target: DomainId,
  layout: HybridLayout,
  parts: Vector[HybridPartOperator],
  map: DoubleLinearOperator
):
  def rows: Int =
    map.rows

  def cols: Int =
    map.cols

  def forward(input: DMat): Either[LinAlgError, DMat] =
    map.applyTo(input)

  def adjoint(input: DMat): Either[LinAlgError, DMat] =
    map.transposeApplyTo(input)

  /** Every morphism any part routes through, first occurrence first. */
  def path: Vector[MorphismId] =
    parts.flatMap(_.operator.path.ids).distinct

  def usedInverses: Boolean =
    parts.exists(_.operator.path.usedInverses)

  /** Mean of the parts' covered-row fractions, as neurofunctor reports hybrid coverage. */
  def meanPartCoverage: Double =
    parts.map(_.operator.qc.coverage.fraction).sum / parts.length.toDouble

object HybridOperator:
  /** Compile `source` to each part of the hybrid `target` and stack the part rows. */
  def toHybrid(
    graph: SpatialGraph,
    source: DomainId,
    target: Domain,
    options: HybridCompileOptions = HybridCompileOptions(),
    compiler: OperatorCompiler = OperatorCompiler.volumePullback
  ): Either[SpatialError, HybridOperator] =
    for
      targetParts <- hybridParts(graph, target)
      compiled <- compileParts(targetParts.map(part => PartJob(part, request(source, part.domain.id, options), Some(part), None)), graph, compiler)
      map <- assemble(LinearOperator.block(compiled.map(part => IndexedSeq(part.operator.map))))
    yield HybridOperator(source, target.id, HybridLayout.ToHybrid, compiled, map)

  /** Compile each part of the hybrid `source` to `target` and concatenate the part columns. */
  def fromHybrid(
    graph: SpatialGraph,
    source: Domain,
    target: DomainId,
    options: HybridCompileOptions = HybridCompileOptions(),
    compiler: OperatorCompiler = OperatorCompiler.volumePullback
  ): Either[SpatialError, HybridOperator] =
    for
      sourceParts <- hybridParts(graph, source)
      compiled <- compileParts(sourceParts.map(part => PartJob(part, request(part.domain.id, target, options), None, Some(part))), graph, compiler)
      map <- assemble(LinearOperator.block(IndexedSeq(compiled.map(_.operator.map))))
    yield HybridOperator(source.id, target, HybridLayout.FromHybrid, compiled, map)

  /** Compile part i of the hybrid `source` to part i of the hybrid `target`; parts must correspond by name and order.
    * Offsets recorded on each part are the target's.
    */
  def blockDiagonal(
    graph: SpatialGraph,
    source: Domain,
    target: Domain,
    options: HybridCompileOptions = HybridCompileOptions(),
    compiler: OperatorCompiler = OperatorCompiler.volumePullback
  ): Either[SpatialError, HybridOperator] =
    for
      sourceParts <- hybridParts(graph, source)
      targetParts <- hybridParts(graph, target)
      pairs <-
        if sourceParts.map(_.name.value) == targetParts.map(_.name.value) then Right(sourceParts.zip(targetParts))
        else
          Left(
            SpatialError.HybridLayoutMismatch(
              s"parts ${sourceParts.map(_.name.value).mkString(",")} do not correspond to ${targetParts.map(_.name.value).mkString(",")}"
            )
          )
      compiled <- compileParts(
        pairs.map((from, to) => PartJob(to, request(from.domain.id, to.domain.id, options), Some(to), Some(from))),
        graph,
        compiler
      )
      map <- assemble(LinearOperator.blockDiagonal(compiled.map(_.operator.map)))
    yield HybridOperator(source.id, target.id, HybridLayout.BlockDiagonal, compiled, map)

  private def request(source: DomainId, target: DomainId, options: HybridCompileOptions): CompileRequest =
    CompileRequest(source, target, options.routing, options.sampling, None, options.allowInverses)

  /** The parts of a hybrid domain, each registered in `graph` under the same definition. */
  private def hybridParts(graph: SpatialGraph, domain: Domain): Either[SpatialError, Vector[DomainPart]] =
    domain.geometry match
      case SamplingGeometry.Hybrid(parts) =>
        parts.foldLeft[Either[SpatialError, Vector[DomainPart]]](Right(Vector.empty)) { (acc, part) =>
          acc.flatMap { out =>
            graph.domain(part.domain.id).flatMap { registered =>
              if registered == part.domain then Right(out :+ part)
              else Left(SpatialError.HybridLayoutMismatch(s"part ${part.name.value} differs from graph domain ${part.domain.id.value}"))
            }
          }
        }
      case _ =>
        Left(SpatialError.HybridLayoutMismatch(s"domain ${domain.id.value} is not hybrid"))

  /** One part compilation: the part recorded on the result, its request, and the parts fixing its row and column
    * counts.
    */
  private final case class PartJob(
    recorded: DomainPart,
    request: CompileRequest,
    rowPart: Option[DomainPart],
    columnPart: Option[DomainPart]
  )

  private def compileParts(
    jobs: Vector[PartJob],
    graph: SpatialGraph,
    compiler: OperatorCompiler
  ): Either[SpatialError, Vector[HybridPartOperator]] =
    jobs.foldLeft[Either[SpatialError, Vector[HybridPartOperator]]](Right(Vector.empty)) { (acc, job) =>
      for
        out <- acc
        operator <- compiler.compile(graph, job.request)
        _ <- requireCount(job.rowPart, operator.rows)
        _ <- requireCount(job.columnPart, operator.cols)
      yield out :+ HybridPartOperator(job.recorded.name, job.recorded.offset, operator)
    }

  private def requireCount(part: Option[DomainPart], actual: Int): Either[SpatialError, Unit] =
    part match
      case Some(value) if value.nElements != actual =>
        Left(SpatialError.HybridLayoutMismatch(s"part ${value.name.value} has ${value.nElements} elements, operator has $actual"))
      case _ => Right(())

  private def assemble(result: Either[LinAlgError, DoubleLinearOperator]): Either[SpatialError, DoubleLinearOperator] =
    result.left.map(SpatialQc.linearError)
