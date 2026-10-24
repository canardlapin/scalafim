package providercontract.locus

import locus4s.{DomainRecord, DomainRegistry}

/** Synthetic fixture construction only; production callers use explicit provider admission. */
private[locus] object DomainFixtures:
  def restore(id: String, size: Int): locus4s.DomainResolution =
    val record = DomainRecord.parse(id, id, size).fold(error => throw new IllegalArgumentException(error.message), identity)
    DomainRegistry.empty.restore(record).fold(error => throw new IllegalArgumentException(error.message), identity)

  def ephemeral(name: String, size: Int): locus4s.SomeFiniteDomain =
    locus4s.FiniteDomain.ephemeral(name, size).fold(error => throw new IllegalArgumentException(error.message), identity)
