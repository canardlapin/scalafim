package scalafim.fmri.mvpa.relation

/** Permission for stable forward and adjoint applications during one acquired
  * source scope. Contains only immutable declaration metadata and expiration
  * state, never a reader, operator or resource callback. Providers must also
  * guard every application against resource closure.
  */
final class ScopeReplay private[mvpa] (
    val owner: String, val revision: String, val sourceBinding: String
):
  require(owner.trim.nonEmpty && revision.trim.nonEmpty && sourceBinding.trim.nonEmpty)
  private var active = true
  def isActive: Boolean = synchronized(active)
  private[mvpa] def expire(): Unit = synchronized:
    active = false

  // Dependency identity is immutable even after expiration. Separate scopes
  // with the same declaration still have independent lifetime state.
  override def equals(other: Any): Boolean = other match
    case that: ScopeReplay => owner == that.owner && revision == that.revision && sourceBinding == that.sourceBinding
    case _ => false
  override def hashCode(): Int = (owner, revision, sourceBinding).hashCode
