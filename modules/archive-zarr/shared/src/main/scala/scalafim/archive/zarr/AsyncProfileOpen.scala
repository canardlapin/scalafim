package scalafim.archive.zarr

import zarr4s.*
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

final class AsyncOpenedCanonicalBold private[archive] (
    val canonical: CanonicalBold,
    val array: AsyncOpenedArray,
    val publication: PublicationReceipt
)

object AsyncNeuroArchiveZarr:
  def openCanonical(
      store: AsyncObjectReader,
      capabilities: ZarrCapabilities = ZarrCapabilities(),
      limits: ProfileOpenLimits = ProfileOpenLimits.default,
      runtime: AsyncCodecRuntime = AsyncCodecRuntime.core
  )(using ExecutionContext): Future[Either[NeuroArchiveZarrError, AsyncOpenedCanonicalBold]] =
    readText(store, "publication.json", limits.maxPublicationBytes).flatMap:
      case Left(error) => Future.successful(Left(error))
      case Right(publicationJson) => PublicationReceiptCodec.parse(publicationJson) match
        case Left(error) => Future.successful(Left(error))
        case Right(publication) => ProfileOpenValidation.inventoryLimit(publication, limits.maxInventoryObjects) match
          case Left(error) => Future.successful(Left(error))
          case Right(_) => readDocuments(store, publication, capabilities, limits, runtime)

  private def readDocuments(
      store: AsyncObjectReader,
      publication: PublicationReceipt,
      capabilities: ZarrCapabilities,
      limits: ProfileOpenLimits,
      runtime: AsyncCodecRuntime
  )(using ExecutionContext): Future[Either[NeuroArchiveZarrError, AsyncOpenedCanonicalBold]] =
    val documents = for
      root <- readText(store, "zarr.json", limits.zarr.maxMetadataBytes)
      manifest <- readText(store, "neuroarchive.json", limits.maxManifestBytes)
      canonical <- readText(store, "canonical/zarr.json", limits.zarr.maxMetadataBytes)
    yield (root, manifest, canonical) match
      case (Right(rootJson), Right(manifestJson), Right(canonicalJson)) =>
        ProfileOpenValidation.documents(rootJson, manifestJson, canonicalJson, publication)
      case (Left(error), _, _) => Left(error)
      case (_, Left(error), _) => Left(error)
      case (_, _, Left(error)) => Left(error)
    documents.flatMap:
      case Left(error) => Future.successful(Left(error))
      case Right(manifest) => ZarrPath("canonical") match
        case Left(error) => Future.successful(Left(NeuroArchiveZarrError.Kernel(error)))
        case Right(path) => AsyncZarr.openArray(
          store,
          path,
          capabilities,
          limits.zarr,
          runtime
        ).flatMap:
          case Left(error) => Future.successful(Left(NeuroArchiveZarrError.Kernel(error)))
          case Right(opened) =>
            val refined = for
              canonical <- CanonicalBold.refine(opened.descriptor, manifest)
              _ <- ProfileOpenValidation.inventory(opened.descriptor, publication)
            yield canonical
            refined match
              case Left(error) => Future.successful(Left(error))
              case Right(canonical) => validateLengths(store, publication).map:
                case Left(error) => Left(error)
                case Right(_) => Right(new AsyncOpenedCanonicalBold(canonical, opened, publication))

  private def readText(
      store: AsyncObjectReader,
      keyValue: String,
      limit: ByteCount
  )(using ExecutionContext): Future[Either[NeuroArchiveZarrError, String]] =
    StoreKey.from(keyValue) match
      case Left(error) => Future.successful(Left(NeuroArchiveZarrError.Kernel(error)))
      case Right(key) => store.readAll(key, limit).map:
        case Left(error) => Left(NeuroArchiveZarrError.Kernel(ZarrError.StoreFailure(error)))
        case Right(bytes) => Right(new String(bytes.toArray, "UTF-8"))

  private def validateLengths(
      store: AsyncObjectReader,
      publication: PublicationReceipt
  )(using ExecutionContext): Future[Either[NeuroArchiveZarrError, Unit]] =
    def loop(index: Int): Future[Either[NeuroArchiveZarrError, Unit]] =
      if index >= publication.objects.length then Future.successful(Right(()))
      else
        val expected = publication.objects(index)
        store.length(expected.key).flatMap:
          case Left(error) => Future.successful(Left(NeuroArchiveZarrError.IncompletePublication(error.message)))
          case Right(found) if found != expected.length.toLong => Future.successful(Left(
            NeuroArchiveZarrError.IncompletePublication(
              s"${expected.key.value} has length $found, expected ${expected.length.toLong}"
            )
          ))
          case Right(_) => loop(index + 1)
    loop(0)
