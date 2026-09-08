package scalafim.fmri.mvpa

import gale.sparse.Sparse
import gale.linalg.DoubleLinearOperator
import multivar.core.CoordinateEvidence
import multivar.core.Lin
import multivar.core.Primal
import multivar.core.SemanticProvenance
import multivar.core.SemanticSpace
import multivar.core.SpaceEvidence
import multivar.core.ValueId
import multivar.core.ValueIdentity
import resample4s.core.Draw
import resample4s.core.Injection
import resample4s.core.Permutation
import resample4s.core.Reindexing
import resample4s.core.Selection

/** Unique coordinate of a resampling draw. `source` retains the scientific entity, while `drawPosition` and
  * `occurrence` distinguish repeated rows.
  */
final case class DrawOccurrence[K] private[mvpa] (
    source: K,
    sourcePosition: Int,
    occurrence: Int,
    drawPosition: Int
)

object DrawOccurrence:
  given [K](using sourceCodec: AxisKeyCodec[K]): AxisKeyCodec[DrawOccurrence[K]] with
    override def encode(value: DrawOccurrence[K]): AxisKey =
      val source = sourceCodec.encode(value.source).value
      AxisKey.unsafe(
        s"draw-v1:${value.drawPosition}:${value.sourcePosition}:${value.occurrence}:${source.length}:$source"
      )

/** A Resample4s reindexing bound to exact parent and child scientific axes. The concrete reindexing kind remains in
  * `R`; selection, injection, draw, and permutation are never weakened to one untyped integer vector.
  */
final class ReindexingLeg[
    P <: SemanticSpace,
    ParentKey,
    ChildKey,
    R <: Reindexing
] private (
    val parentIdentity: AxisIdentity,
    val parentEvidence: SpaceEvidence[P],
    val child: AxisRef[ChildKey],
    val reindexing: R,
    private[mvpa] val operator: DoubleLinearOperator
)(
    val leg: Lin[Primal[P], Primal[child.Id]]
):
  def size: Int =
    reindexing.domain

  def sourcePositionAt(position: Int): Either[AxisRefError, Int] =
    reindexing
      .at(position)
      .left
      .map(error =>
        AxisRefError.InvalidReindexing(
          s"position ${error.index} is outside [0, ${error.domain})"
        )
      )

object ReindexingLeg:
  def selection[K](
      parent: AxisRef[K],
      reindexing: Selection
  )(using AxisKeyCodec[K]): Either[AxisRefError, ReindexingLeg[parent.Id, K, K, Selection]] =
    injective(parent, reindexing, "selection")

  def injection[K](
      parent: AxisRef[K],
      reindexing: Injection
  )(using AxisKeyCodec[K]): Either[AxisRefError, ReindexingLeg[parent.Id, K, K, Injection]] =
    injective(parent, reindexing, "injection")

  def permutation[K](
      parent: AxisRef[K],
      reindexing: Permutation
  )(using AxisKeyCodec[K]): Either[AxisRefError, ReindexingLeg[parent.Id, K, K, Permutation]] =
    injective(parent, reindexing, "permutation")

  def draw[K](
      parent: AxisRef[K],
      reindexing: Draw
  )(using
      AxisKeyCodec[K]
  ): Either[
    AxisRefError,
    ReindexingLeg[parent.Id, K, DrawOccurrence[K], Draw]
  ] =
    validateCodomain(parent, reindexing).flatMap: _ =>
      val occurrences = Vector.newBuilder[DrawOccurrence[K]]
      val counts = new Array[Int](parent.size)
      var drawPosition = 0
      reindexing.foreachIndex: sourcePosition =>
        occurrences += DrawOccurrence(
          parent.keys(sourcePosition),
          sourcePosition,
          counts(sourcePosition),
          drawPosition
        )
        counts(sourcePosition) += 1
        drawPosition += 1
      val childKeys = occurrences.result()
      build(
        parent,
        childKeys,
        reindexing,
        "draw"
      )

  private def injective[K, R <: Reindexing](
      parent: AxisRef[K],
      reindexing: R,
      kind: String
  )(using AxisKeyCodec[K]): Either[AxisRefError, ReindexingLeg[parent.Id, K, K, R]] =
    validateCodomain(parent, reindexing).flatMap: _ =>
      val keys = Vector.newBuilder[K]
      reindexing.foreachIndex(sourcePosition => keys += parent.keys(sourcePosition))
      build(parent, keys.result(), reindexing, kind)

  private def build[K, C, R <: Reindexing](
      parent: AxisRef[K],
      childKeys: Vector[C],
      reindexing: R,
      kind: String
  )(using
      childCodec: AxisKeyCodec[C]
  ): Either[
    AxisRefError,
    ReindexingLeg[parent.Id, K, C, R]
  ] =
    val sourcePositions = reindexing.toIArray
    for
      childIdentity <- AxisIdentity
        .reindexed(
          parent.identity,
          kind,
          childKeys.map(childCodec.encode),
          sourcePositions
        )
        .left
        .map(AxisRefError.InvalidIdentity.apply)
      child <- AxisRef(childIdentity, childKeys)
      selector <- selector(parent.evidence, child.evidence, reindexing, childIdentity)
    yield new ReindexingLeg(
      parent.identity,
      parent.evidence,
      child,
      reindexing,
      selector._1
    )(selector._2)

  private def validateCodomain(
      parent: AxisRef[?],
      reindexing: Reindexing
  ): Either[AxisRefError, Unit] =
    if reindexing.codomain == parent.size then Right(())
    else
      Left(
        AxisRefError.ReindexingCodomainMismatch(
          parent.size,
          reindexing.codomain
        )
      )

  private def selector[P <: SemanticSpace, C <: SemanticSpace](
      parent: SpaceEvidence[P],
      child: SpaceEvidence[C],
      reindexing: Reindexing,
      childIdentity: AxisIdentity
  ): Either[AxisRefError, (DoubleLinearOperator, Lin[Primal[P], Primal[C]])] =
    val builder = Sparse.coo(reindexing.domain, reindexing.codomain)
    var row = 0
    reindexing.foreachIndex: sourcePosition =>
      val _ = builder.add(row, sourcePosition, 1.0)
      row += 1
    val operator = builder.toCSR()
    Lin
      .fromLinearMap(
        operator,
        CoordinateEvidence.primal(parent),
        CoordinateEvidence.primal(child),
        ValueIdentity.source(ValueId.unsafe(s"reindex-${childIdentity.fingerprint.value}")),
        SemanticProvenance.source("resample4s-axis-reindexing")
      )
      .left
      .map(AxisRefError.InvalidSemanticOperator.apply)
      .map(operator -> _)
