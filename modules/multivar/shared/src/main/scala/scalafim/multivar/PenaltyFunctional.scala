package scalafim.multivar

/** Stable, parameter-free identity of a mathematical penalty functional.
  *
  * This identity is deliberately smaller than any executable penalty. Geometry,
  * grouping, tuning parameters, topology, evaluation behavior, and legal
  * targets remain in family-specific [[PenaltyFunctionalWitness]] types.
  */
enum PenaltyFunctionalIdentity(val stableKey: String):
  case SquaredNorm extends PenaltyFunctionalIdentity("squared_norm")
  case L1 extends PenaltyFunctionalIdentity("l1")
  case GroupL21 extends PenaltyFunctionalIdentity("group_l21")
  case GroupL2 extends PenaltyFunctionalIdentity("group_l2")
  case SparseGroup extends PenaltyFunctionalIdentity("sparse_group")
  case ElasticNet extends PenaltyFunctionalIdentity("elastic_net")
  case Huber extends PenaltyFunctionalIdentity("huber")
  case TotalVariation extends PenaltyFunctionalIdentity("total_variation")
  case NuclearNorm extends PenaltyFunctionalIdentity("nuclear_norm")
  case NegativeLogDet extends PenaltyFunctionalIdentity("negative_log_det")

object PenaltyFunctionalIdentity:
  def fromStableKey(value: String): Option[PenaltyFunctionalIdentity] =
    values.find(_.stableKey == value)

/** A family-specific witness that preserves richer semantics around one shared identity. */
trait PenaltyFunctionalWitness:
  def functionalIdentity: PenaltyFunctionalIdentity
