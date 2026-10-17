package scalafim.locus

import locus4s.DomainError
import locus4s.DomainId
import locus4s.DomainRecord
import locus4s.DomainRegistry
import locus4s.DomainResolution
import locus4s.DomainRestoreError
import locus4s.SomeFiniteDomain

enum DomainFactoryError:
  case InvalidRecord(error: DomainError)
  case RestoreFailed(error: DomainRestoreError)

  def message: String =
    this match
      case InvalidRecord(error) => error.message
      case RestoreFailed(error) => error.message

object DomainFactory:

  /** Restore one live owner for a persistent domain key, independently.
    *
    * There is no process-wide registry: every call returns a fresh runtime owner.
    * Two restores of the same key are distinct owners with the same persistent
    * identity, so a consumer that combines them checks `left.align(right)` and
    * transports structures through the resulting `DomainAlignment`. A composition
    * that genuinely needs one shared live owner threads its own
    * `locus4s.DomainRegistry` value through that composition instead.
    *
    * The returned abstract owner type must be carried by the domain adapter;
    * callers cannot choose or forge it.
    */
  def restore(
      id: String,
      size: Int,
      name: Option[String] = None
  ): Either[DomainFactoryError, DomainResolution] =
    for
      key <- DomainId.parse(id).left.map(DomainFactoryError.InvalidRecord.apply)
      record <-
        DomainRecord
          .make(key, name.getOrElse(key.value), size)
          .left
          .map(DomainFactoryError.InvalidRecord.apply)
      resolution <-
        DomainRegistry.empty
          .restore(record)
          .left
          .map(DomainFactoryError.RestoreFailed.apply)
    yield resolution

  /** A derived, process-local domain with no persistent identity.
    *
    * Selection-position domains — the active voxels of a mask, a searchlight's
    * support, the rows of a fitted subset — exist only relative to a live
    * parent domain, and the injection back into that parent is what carries
    * their meaning. locus4s calls these ephemeral. They need no
    * content-addressed key: naming such a domain by its own index list would
    * build a string proportional to the mask, on the order of a megabyte for a
    * whole-brain one.
    *
    * The cost is that two separately built supports over identical content are
    * distinct owners that can never align. That is correct for a derived
    * structure: compare them through the parent domain.
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
      id: String,
      size: Int,
      name: Option[String] = None
  ): DomainResolution =
    restore(id, size, name)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
