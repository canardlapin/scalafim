package scalafim.archive.zarr

import zarr4s.*

object JvmNeuroArchiveAudit:
  /** Fully verifies receipt-listed physical objects.
    *
    * Opening remains bounded and checks existence plus length. An explicit
    * audit is the operation that downloads every object and verifies its
    * physical SHA-256 digest.
    */
  def verifyObjects(
      store: ObjectReader,
      publication: PublicationReceipt,
      maxObjectBytes: ByteCount
  ): Either[NeuroArchiveZarrError, Unit] =
    var index = 0
    while index < publication.objects.length do
      val expected = publication.objects(index)
      store.readAll(expected.key, maxObjectBytes) match
        case Left(error) =>
          return Left(NeuroArchiveZarrError.IncompletePublication(error.message))
        case Right(bytes) =>
          val actual = Sha256.digest(bytes)
          if actual != expected.sha256 then
            return Left(NeuroArchiveZarrError.DigestMismatch(
              expected.key.value,
              expected.sha256,
              actual
            ))
      index += 1
    Right(())
