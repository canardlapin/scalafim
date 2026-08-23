package scalafim.locus

import locus4s.DomainError
import locus4s.DomainRecord
import locus4s.DomainRegistry
import locus4s.DomainResolution
import locus4s.DomainRestoreError

enum DomainFactoryError:
  case InvalidRecord(error: DomainError)
  case RestoreFailed(error: DomainRestoreError)

  def message: String =
    this match
      case InvalidRecord(error) => error.message
      case RestoreFailed(error) => error.message

object DomainFactory:

  /** Restore one live owner from a stable domain id in an explicit registry.
    *
    * The returned resolution contains both the unforgeable owner and the
    * updated immutable registry. A dataset, archive session, workflow, or
    * application service owns that returned snapshot and must pass it to the
    * next restoration in the same scope. Restoring through the returned
    * registry canonicalizes an existing key; starting from another registry
    * deliberately creates an independent live owner.
    *
    * Concurrent applications may serialize updates through a scoped effect
    * reference, but this module publishes no process-global mutable state and
    * no generic effectful store. Dropping the owning registry releases its
    * canonicalization scope.
    */
  def restore(
      registry: DomainRegistry,
      key: SpaceKey,
      size: Int,
      name: Option[String] = None
  ): Either[DomainFactoryError, DomainResolution] =
    for
      record <-
        DomainRecord
          .make(key, name.getOrElse(key.value), size)
          .left
          .map(DomainFactoryError.InvalidRecord.apply)
      resolution <-
        registry
          .restore(record)
          .left
          .map(DomainFactoryError.RestoreFailed.apply)
    yield resolution

  /** A derived, process-local domain with no persistent identity.
    *
    * Selection-position domains — the active voxels of a mask, a searchlight's
    * support, the rows of a fitted subset — exist only relative to a live
    * parent domain, and the injection back into that parent is what carries
    * their meaning. locus4s calls these ephemeral, and they are the right shape
    * here for two reasons.
    *
    * They are not registry-retained, so a process that builds one domain per
    * mask does not accumulate them. And they need no content-addressed key:
    * naming such a domain by its own index list meant building — and, once the
    * registry became canonical, permanently retaining — a string proportional
    * to the mask, on the order of a megabyte for a whole-brain one.
    *
    * The cost is that two separately built supports over identical content are
    * distinct owners. That is correct for a derived structure: compare them
    * through the parent domain, which *is* canonical.
    */
  def ephemeral(
      name: String,
      size: Int
  ): Either[DomainFactoryError, SomeFiniteDomain] =
    locus4s.FiniteDomain
      .ephemeral(name, size)
      .left
      .map(DomainFactoryError.InvalidRecord.apply)

  private[scalafim] def unsafeEphemeral(name: String, size: Int): SomeFiniteDomain =
    ephemeral(name, size)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private[scalafim] def unsafeRestore(
      registry: DomainRegistry,
      key: SpaceKey,
      size: Int,
      name: Option[String] = None
  ): DomainResolution =
    restore(registry, key, size, name)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
