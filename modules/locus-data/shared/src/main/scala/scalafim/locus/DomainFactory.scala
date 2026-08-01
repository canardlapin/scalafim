package scalafim.locus

import java.util.concurrent.atomic.AtomicReference
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

  /** The process-wide canonical registry.
    *
    * A [[SpaceKey]] is meant to name one domain, so restoring the same key
    * twice must yield the same live owner — otherwise two independently
    * constructed views of the same domain (a second load, a deserialization,
    * two code paths that each build it) would not interoperate, and every
    * checked operation between them would fail despite their persistent
    * identities agreeing.
    *
    * `DomainRegistry` is an immutable value whose `restore` returns the
    * *existing* owner when the id is already present, so canonicalization is
    * just a matter of keeping one. locus4s deliberately leaves that policy to
    * "an effectful/atomic factory"; this is it.
    *
    * A conflicting record for a known id still fails rather than silently
    * sharing: `DomainRegistry.restore` rejects a different key for the same id.
    */
  private val registry: AtomicReference[DomainRegistry] =
    new AtomicReference(DomainRegistry.empty)

  /** Restore one live, registry-created owner from a stable domain id.
    *
    * The returned abstract owner type must be carried by the domain adapter;
    * callers cannot choose or forge it. Two calls with the same key return
    * resolutions whose owner types are statically distinct but whose runtime
    * owner is identical, so the checked operations can realign them.
    */
  def restore(
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
        canonicalize(record).left.map(DomainFactoryError.RestoreFailed.apply)
    yield resolution

  /** Install `record` in the shared registry, or return the owner already
    * registered under its id.
    */
  private def canonicalize(
      record: DomainRecord
  ): Either[DomainRestoreError, DomainResolution] =
    @annotation.tailrec
    def attempt(): Either[DomainRestoreError, DomainResolution] =
      val current = registry.get()
      current.restore(record) match
        case Left(error) => Left(error)
        case Right(resolution) =>
          // When the id was already present the registry is returned
          // unchanged, so this compare-and-set is a no-op that still
          // serialises against a concurrent first registration.
          if registry.compareAndSet(current, resolution.registry) then Right(resolution)
          else attempt()
    attempt()

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

  /** Number of distinct domains canonicalized so far. Diagnostics only. */
  private[scalafim] def registeredCount: Int =
    registry.get().size

  private[scalafim] def unsafeRestore(
      key: SpaceKey,
      size: Int,
      name: Option[String] = None
  ): DomainResolution =
    restore(key, size, name)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
