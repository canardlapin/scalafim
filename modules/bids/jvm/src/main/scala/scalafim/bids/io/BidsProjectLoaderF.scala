package scalafim.bids.io

import scalafim.bids.*

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.effect.syntax.all.*
import cats.syntax.all.*

import java.nio.file.Path

final class BidsProjectLoaderF[F[_]] private[io] (
    observer: BidsReadObserver[F]
)(using F: Async[F]):
  def load(
      root: Path,
      config: BidsLoadConfig = BidsLoadConfig(),
      maxConcurrentReads: PositiveInt = PositiveInt.unsafe(4)
  ): F[Either[BidsError, BidsProject]] =
    val rootAbs = root.toAbsolutePath.normalize()
    val rootPath = BidsPath(rootAbs.toString.replace('\\', '/'))
    loadStore(
      BidsPathStore.observed(rootAbs, observer),
      rootPath,
      config,
      maxConcurrentReads
    )

  def loadStrict(
      root: Path,
      config: BidsLoadConfig = BidsLoadConfig(),
      maxConcurrentReads: PositiveInt = PositiveInt.unsafe(4)
  ): F[Either[BidsProjectLoadError, BidsProject]] =
    loadChecked(root, config, maxConcurrentReads).map(
      _.left
        .map(BidsProjectLoadError.Operation.apply)
        .flatMap(
          _.enforce(BidsValidationPolicy.Strict)
            .left
            .map(BidsProjectLoadError.Validation.apply)
            .map(_.value)
        )
    )

  def loadChecked(
      root: Path,
      config: BidsLoadConfig = BidsLoadConfig(),
      maxConcurrentReads: PositiveInt = PositiveInt.unsafe(4)
  ): F[Either[BidsError, BidsValidationReport[BidsProject]]] =
    val rootAbs = root.toAbsolutePath.normalize()
    val rootPath = BidsPath(rootAbs.toString.replace('\\', '/'))
    loadStoreChecked(
      BidsPathStore.observed(rootAbs, observer),
      rootPath,
      config,
      maxConcurrentReads
    )

  def loadStore(
      store: BidsStore[F],
      root: BidsPath,
      config: BidsLoadConfig = BidsLoadConfig(),
      maxConcurrentReads: PositiveInt = PositiveInt.unsafe(4)
  ): F[Either[BidsError, BidsProject]] =
    (for
      entries <- EitherT(store.entries)
      relPaths = entries.map(_.path.value).sorted
      derivatives = discoverDerivatives(relPaths, config.derivatives)
      description <- EitherT(readDatasetDescription(store, root, relPaths))
      participantsTable <- EitherT(loadParticipantsTable(store, relPaths, config.strictParticipants))
      participants <- EitherT.fromEither[F](readParticipants(relPaths, participantsTable))
      sidecars <- EitherT(readSidecars(store, relPaths, maxConcurrentReads.toInt))
    yield
      BidsProject(
        root = root,
        description = description.map(_.copy(parentDirectory = Some(root))),
        participants = participants,
        participantsTable = participantsTable,
        derivatives = derivatives,
        manifest = BidsManifest.fromRelativePaths(relPaths, derivatives),
        sidecars = sidecars
      )).value

  def loadStoreChecked(
      store: BidsStore[F],
      root: BidsPath,
      config: BidsLoadConfig = BidsLoadConfig(),
      maxConcurrentReads: PositiveInt = PositiveInt.unsafe(4)
  ): F[Either[BidsError, BidsValidationReport[BidsProject]]] =
    (for
      entries <- EitherT(store.entries)
      relPaths = entries.map(_.path.value).sorted
      derivatives = discoverDerivatives(relPaths, config.derivatives)
      descriptions <- EitherT(readDatasetDescriptionsChecked(store, root, relPaths))
      participantsText <- EitherT(readOptionalText(store, BidsPath("participants.tsv"), relPaths))
      participants = BidsCheckedContent.participants(participantsText, relPaths, config.strictParticipants)
      sidecars <- EitherT(readSidecarsChecked(store, relPaths, maxConcurrentReads.toInt))
    yield
      val manifest = BidsManifest.fromRelativePathsChecked(relPaths, derivatives)
      val project = BidsProject(
        root = root,
        description = descriptions.value,
        participants = participants.participants,
        participantsTable = participants.table,
        derivatives = derivatives,
        manifest = manifest.value,
        sidecars = sidecars.value
      )
      BidsValidationReport.from(
        project,
        manifest.issues ++ descriptions.issues ++ participants.issues ++ sidecars.issues ++
          BidsCheckedContent.resolvedMetadata(project)
      )).value

  private def readDatasetDescription(
      store: BidsStore[F],
      root: BidsPath,
      relPaths: Vector[String]
  ): F[Either[BidsError, Option[DatasetDescription]]] =
    val path = BidsPath("dataset_description.json")
    if !relPaths.contains(path.value) then F.pure(Right(None))
    else
      store.readUtf8(path).map(
        _.flatMap { text =>
          contextualize(path, "decode dataset description")(
            BidsJson
              .parseObject(text)
              .map(json => Some(DatasetDescription.fromJson(json, parentDirectory = Some(root))))
          )
        }
      )

  private def readDatasetDescriptionsChecked(
      store: BidsStore[F],
      root: BidsPath,
      relPaths: Vector[String]
  ): F[Either[BidsError, BidsCheckedValue[Option[DatasetDescription]]]] =
    val descriptionPaths = relPaths.filter(_.endsWith("dataset_description.json")).sorted
    descriptionPaths
      .traverse { rel =>
        val path = BidsPath(rel)
        store.readUtf8(path).map(_.map(text => path -> BidsCheckedContent.datasetDescription(path, text, root)))
      }
      .map(
        _.sequence.map { descriptions =>
          val rootDescription =
            descriptions.collectFirst {
              case (path, checked) if path.value == "dataset_description.json" => checked.value
            }.flatten
          val missingRoot =
            Option.when(!descriptionPaths.contains("dataset_description.json"))(
              BidsIssue.error(
                BidsIssueCode.MissingRequiredField,
                Some(BidsPath("dataset_description.json")),
                Some("dataset_description.json"),
                "dataset_description.json is missing"
              )
            ).toVector
          BidsCheckedValue(
            rootDescription,
            descriptions.flatMap(_._2.issues) ++ missingRoot
          )
        }
      )

  private def loadParticipantsTable(
      store: BidsStore[F],
      relPaths: Vector[String],
      strictParticipants: Boolean
  ): F[Either[BidsError, Option[BidsTable]]] =
    val path = BidsPath("participants.tsv")
    if relPaths.contains(path.value) then
      store.readUtf8(path).map(
        _.flatMap(text => contextualize(path, "decode participants table")(BidsTable.parse(text)).map(Some(_)))
      )
    else if strictParticipants then F.pure(Left(BidsError.MissingParticipants(path.value)))
    else F.pure(Right(None))

  private def readParticipants(
      relPaths: Vector[String],
      participantsTable: Option[BidsTable]
  ): Either[BidsError, Vector[String]] =
    participantsTable match
      case Some(table) =>
        table
          .columnNamed("participant_id")
          .map(_.values.flatten.map(_.stripPrefix("sub-")).filter(_.nonEmpty).distinct.sorted)
      case None =>
        Right(
          relPaths
            .flatMap { path =>
              path.split('/').toVector.dropRight(1).find(_.startsWith("sub-"))
            }
            .map(_.stripPrefix("sub-"))
            .distinct
            .sorted
        )

  private def discoverDerivatives(
      relPaths: Vector[String],
      mode: DerivativeMode
  ): Vector[DerivativeRoot] =
    mode match
      case DerivativeMode.None => Vector.empty
      case DerivativeMode.Auto =>
        val parts = relPaths.map(_.split('/').toVector)
        val pipelineRoots =
          parts
            .collect {
              case Vector("derivatives", pipeline, _*) if !pipeline.startsWith("sub-") && pipeline != "dataset_description.json" =>
                pipeline
            }
            .distinct
            .sorted
            .map(name => DerivativeRoot(BidsPath(s"derivatives/$name"), PipelineName(name)))
        val directDerivative =
          Option
            .when(
              relPaths.contains("derivatives/dataset_description.json") &&
                parts.exists {
                  case Vector("derivatives", subject, _*) => subject.startsWith("sub-")
                  case _                                  => false
                }
            )(DerivativeRoot(BidsPath("derivatives"), PipelineName("derivatives")))
            .toVector
        directDerivative ++ pipelineRoots

  private def readSidecars(
      store: BidsStore[F],
      relPaths: Vector[String],
      maxConcurrentReads: Int
  ): F[Either[BidsError, Map[BidsPath, JsonValue.Obj]]] =
    val jsonPaths =
      relPaths.filter(path => path.endsWith(".json") && !path.endsWith("dataset_description.json"))
    jsonPaths
      .parTraverseN(maxConcurrentReads) { rel =>
        val path = BidsPath(rel)
        store.readUtf8(path).map(
          _.flatMap(text => contextualize(path, "decode JSON sidecar")(BidsJson.parseObject(text)).map(path -> _))
        )
      }
      .map(_.sequence.map(_.toMap))

  private def readSidecarsChecked(
      store: BidsStore[F],
      relPaths: Vector[String],
      maxConcurrentReads: Int
  ): F[Either[BidsError, BidsCheckedValue[Map[BidsPath, JsonValue.Obj]]]] =
    val jsonPaths =
      relPaths.filter(path => path.endsWith(".json") && !path.endsWith("dataset_description.json"))
    jsonPaths
      .parTraverseN(maxConcurrentReads) { rel =>
        val path = BidsPath(rel)
        store.readUtf8(path).map(_.map(text => BidsCheckedContent.sidecar(path, text)))
      }
      .map(
        _.sequence.map { sidecars =>
          BidsCheckedValue(
            sidecars.flatMap(_.value).toMap,
            sidecars.flatMap(_.issues)
          )
        }
      )

  private def readOptionalText(
      store: BidsStore[F],
      path: BidsPath,
      relPaths: Vector[String]
  ): F[Either[BidsError, Option[String]]] =
    if relPaths.contains(path.value) then store.readUtf8(path).map(_.map(Some(_)))
    else F.pure(Right(None))

  private def contextualize[A](
      path: BidsPath,
      operation: String
  )(result: Either[BidsError, A]): Either[BidsError, A] =
    result.leftMap(error => BidsError.Io(path.value, s"$operation failed: ${error.message}"))

object BidsProjectLoaderF:
  def apply[F[_]: Async]: BidsProjectLoaderF[F] =
    new BidsProjectLoaderF(BidsReadObserver.noop[F])

  private[io] def observed[F[_]: Async](observer: BidsReadObserver[F]): BidsProjectLoaderF[F] =
    new BidsProjectLoaderF(observer)
