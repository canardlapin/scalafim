package scalafim.fmri.mvpa.dataset

import multivar.core.SemanticSpace
import resample4s.core.Reindexing
import scalafim.fmri.mvpa.{ReindexingKind, ReindexingLeg}
import scalafim.response.{DomainId, DuplicatePolicy, IndexError, OrderedIndices, TimeAxis, TimeDomain}

enum ResponseReindexingError:
  case AxisSizeMismatch(axis: Int, domain: Int)
  case InvalidIndices(error: IndexError)

  def message: String =
    this match
      case AxisSizeMismatch(axis, domain) =>
        s"reindexed MVPA axis has $axis coordinates but the response domain has $domain"
      case InvalidIndices(error) =>
        error.message

/** The response boundary for MVPA reindexing.
  *
  * MVPA core expresses selections and repeated draws as Resample4s reindexings
  * over identified axes and never names response types. This adapter is where a
  * leg whose parent axis enumerates a response domain, in that domain's order,
  * becomes an ordered response read. Draw occurrences are preserved in order;
  * every other reindexing kind remains a unique selection.
  */
object ResponseReindexing:
  def indices[A, P <: SemanticSpace, K, R <: Reindexing](
      leg: ReindexingLeg[P, K, R],
      domain: DomainId[A],
      domainSize: Int
  ): Either[ResponseReindexingError, OrderedIndices[A]] =
    if leg.parentAxis.size != domainSize then
      Left(ResponseReindexingError.AxisSizeMismatch(leg.parentAxis.size, domainSize))
    else
      val duplicates =
        leg.kind match
          case ReindexingKind.Draw => DuplicatePolicy.Allow
          case ReindexingKind.Selection | ReindexingKind.Injection | ReindexingKind.Permutation =>
            DuplicatePolicy.Reject
      OrderedIndices
        .fromInts(domain, domainSize, leg.ordinals.toVector, duplicates)
        .left
        .map(ResponseReindexingError.InvalidIndices.apply)

  def timepoints[P <: SemanticSpace, K, R <: Reindexing](
      leg: ReindexingLeg[P, K, R],
      time: TimeDomain
  ): Either[ResponseReindexingError, OrderedIndices[TimeAxis]] =
    indices(leg, time.id, time.count)
