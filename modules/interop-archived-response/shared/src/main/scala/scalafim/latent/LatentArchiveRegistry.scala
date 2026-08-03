package scalafim.latent

import scalafim.archive.{ArchiveError, RunLabel}
import scalafim.archive.lna.{LnaArchive, LnaRun}

trait LatentArchiveBinding:
  def name: String

  def kinds: Vector[LatentArchiveKind]

  private[latent] def descriptorOption(
      archive: LnaArchive,
      runLabel: RunLabel
  ): Either[ArchiveError, Option[LatentArchiveDescriptor]]

  private[latent] def openPlan(
      archive: LnaArchive,
      runLabel: RunLabel,
      run: LnaRun,
      descriptor: LatentArchiveDescriptor
  ): Either[ArchiveError, LatentArchivePlan]

object LatentArchiveBinding:
  case object BoldZip extends LatentArchiveBinding:
    val name: String =
      "boldzip"

    val kinds: Vector[LatentArchiveKind] =
      Vector(LatentArchiveKind.BoldZip)

    private[latent] def descriptorOption(
        archive: LnaArchive,
        runLabel: RunLabel
    ): Either[ArchiveError, Option[LatentArchiveDescriptor]] =
      Right(
        BoldZipLatentArchiveCodec
          .descriptorOption(archive, runLabel)
          .map(LatentArchiveDescriptor.BoldZip(_))
      )

    private[latent] def openPlan(
        archive: LnaArchive,
        runLabel: RunLabel,
        run: LnaRun,
        descriptor: LatentArchiveDescriptor
    ): Either[ArchiveError, LatentArchivePlan] =
      descriptor match
        case descriptor @ LatentArchiveDescriptor.BoldZip(_) =>
          BoldZipLatentArchiveCodec
            .fromArchive(archive, runLabel)
            .map(response =>
              LatentArchivePlan.BoldZip(run, descriptor, response)
            )
        case other =>
          mismatch(this, other)

  case object Transport extends LatentArchiveBinding:
    val name: String =
      "transport"

    val kinds: Vector[LatentArchiveKind] =
      Vector(LatentArchiveKind.Transport)

    private[latent] def descriptorOption(
        archive: LnaArchive,
        runLabel: RunLabel
    ): Either[ArchiveError, Option[LatentArchiveDescriptor]] =
      Right(
        TransportLatentArchiveCodec
          .descriptorOption(archive, runLabel)
          .map(LatentArchiveDescriptor.Transport(_))
      )

    private[latent] def openPlan(
        archive: LnaArchive,
        runLabel: RunLabel,
        run: LnaRun,
        descriptor: LatentArchiveDescriptor
    ): Either[ArchiveError, LatentArchivePlan] =
      descriptor match
        case descriptor @ LatentArchiveDescriptor.Transport(_) =>
          TransportLatentArchiveCodec
            .fromArchive(archive, runLabel)
            .map(response =>
              LatentArchivePlan.Transport(run, descriptor, response)
            )
        case other =>
          mismatch(this, other)

  case object TemporalDct extends LatentArchiveBinding:
    val name: String =
      "temporal-dct"

    val kinds: Vector[LatentArchiveKind] =
      Vector(LatentArchiveKind.TemporalDct)

    private[latent] def descriptorOption(
        archive: LnaArchive,
        runLabel: RunLabel
    ): Either[ArchiveError, Option[LatentArchiveDescriptor]] =
      ExplicitLatentArchiveCodec
        .temporalDctDescriptor(archive, runLabel) match
        case Some(desc) =>
          ExplicitLatentArchiveCodec
            .explicitDescriptor(archive, runLabel)
            .map(embed =>
              LatentArchiveDescriptor.TemporalDct(desc, embed)
            )
            .map(Some(_))
            .toRight(
              ArchiveError.InvalidArchive(
                s"run '${runLabel.value}' has temporal DCT params but no explicit latent embed descriptor"
              )
            )
        case None =>
          Right(None)

    private[latent] def openPlan(
        archive: LnaArchive,
        runLabel: RunLabel,
        run: LnaRun,
        descriptor: LatentArchiveDescriptor
    ): Either[ArchiveError, LatentArchivePlan] =
      descriptor match
        case descriptor @ LatentArchiveDescriptor.TemporalDct(
              temporal,
              _
            ) =>
          ExplicitLatentArchiveCodec
            .fromArchive(archive, runLabel)
            .flatMap: response =>
              ExplicitLatentArchiveCodec
                .temporalDctSpec(archive, runLabel, temporal)
                .map: (spec, center, ridge) =>
                  LatentArchivePlan.TemporalDct(
                    run,
                    descriptor,
                    response,
                    spec,
                    center,
                    ridge
                  )
        case other =>
          mismatch(this, other)

  case object SharedBasis extends LatentArchiveBinding:
    val name: String =
      "shared-basis"

    val kinds: Vector[LatentArchiveKind] =
      Vector(LatentArchiveKind.SharedBasis)

    private[latent] def descriptorOption(
        archive: LnaArchive,
        runLabel: RunLabel
    ): Either[ArchiveError, Option[LatentArchiveDescriptor]] =
      Right(
        SharedBasisLatentArchiveCodec
          .descriptorOption(archive, runLabel)
          .map(LatentArchiveDescriptor.SharedBasis(_))
      )

    private[latent] def openPlan(
        archive: LnaArchive,
        runLabel: RunLabel,
        run: LnaRun,
        descriptor: LatentArchiveDescriptor
    ): Either[ArchiveError, LatentArchivePlan] =
      descriptor match
        case descriptor @ LatentArchiveDescriptor.SharedBasis(_) =>
          SharedBasisLatentArchiveCodec
            .fromArchive(archive, runLabel)
            .map(response =>
              LatentArchivePlan.SharedBasis(run, descriptor, response)
            )
        case other =>
          mismatch(this, other)

  case object Explicit extends LatentArchiveBinding:
    val name: String =
      "explicit"

    val kinds: Vector[LatentArchiveKind] =
      Vector(
        LatentArchiveKind.Explicit,
        LatentArchiveKind.TemporalHaar
      )

    private[latent] def descriptorOption(
        archive: LnaArchive,
        runLabel: RunLabel
    ): Either[ArchiveError, Option[LatentArchiveDescriptor]] =
      if ExplicitLatentArchiveCodec
          .temporalDctDescriptor(archive, runLabel)
          .isDefined
      then Right(None)
      else
        ExplicitLatentArchiveCodec
          .explicitDescriptor(archive, runLabel) match
          case None =>
            Right(None)
          case Some(desc) =>
            ExplicitLatentArchiveCodec
              .explicitDescriptorAlgebra(desc)
              .map(Some(_))

    private[latent] def openPlan(
        archive: LnaArchive,
        runLabel: RunLabel,
        run: LnaRun,
        descriptor: LatentArchiveDescriptor
    ): Either[ArchiveError, LatentArchivePlan] =
      descriptor match
        case descriptor @ LatentArchiveDescriptor.Explicit(_) =>
          ExplicitLatentArchiveCodec
            .fromArchive(archive, runLabel)
            .map(response =>
              LatentArchivePlan.Explicit(run, descriptor, response)
            )

        case descriptor @ LatentArchiveDescriptor.TemporalHaar(
              _,
              spec,
              center,
              ridge
            ) =>
          ExplicitLatentArchiveCodec
            .fromArchive(archive, runLabel)
            .map: response =>
              LatentArchivePlan.TemporalHaar(
                run,
                descriptor,
                response,
                spec,
                center,
                ridge
              )

        case other =>
          mismatch(this, other)

  private def mismatch(
      binding: LatentArchiveBinding,
      descriptor: LatentArchiveDescriptor
  ): Either[ArchiveError, LatentArchivePlan] =
    Left(
      ArchiveError.InvalidArchive(
        s"${binding.name} LNA binding cannot open ${descriptor.kind.toString} descriptor"
      )
    )

enum LatentArchiveRegistryError:
  case Empty
  case InvalidBinding(name: String, detail: String)
  case DuplicateName(name: String)
  case DuplicateKind(kind: LatentArchiveKind)

  def message: String =
    this match
      case Empty =>
        "latent archive registry must contain at least one binding"
      case InvalidBinding(name, detail) =>
        s"invalid latent archive binding '$name': $detail"
      case DuplicateName(name) =>
        s"duplicate latent archive binding name '$name'"
      case DuplicateKind(kind) =>
        s"duplicate latent archive binding for ${kind.toString}"

final class LatentArchiveRegistry private (
    val bindings: Vector[LatentArchiveBinding],
    val supportedKinds: Vector[LatentArchiveKind]
):
  def openPlan(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, LatentArchivePlan] =
    maybeOpenPlan(archive, runLabel).flatMap:
      case Some(plan) =>
        Right(plan)
      case None =>
        Left(
          ArchiveError.InvalidArchive(
            s"run '${runLabel.value}' has no latent response descriptor"
          )
        )

  def maybeOpenPlan(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, Option[LatentArchivePlan]] =
    archive.validate.flatMap: valid =>
      valid.run(runLabel) match
        case None =>
          Left(
            ArchiveError.InvalidArchive(
              s"run '${runLabel.value}' not found"
            )
          )
        case Some(run) =>
          bindingAndDescriptor(valid, runLabel).flatMap:
            case None =>
              Right(None)
            case Some((binding, descriptor)) =>
              binding
                .openPlan(valid, runLabel, run, descriptor)
                .map(Some(_))

  def openDescriptor(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, LatentArchiveDescriptor] =
    maybeOpenDescriptor(archive, runLabel).flatMap:
      case Some(descriptor) =>
        Right(descriptor)
      case None =>
        Left(
          ArchiveError.InvalidArchive(
            s"run '${runLabel.value}' has no latent response descriptor"
          )
        )

  def maybeOpenDescriptor(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, Option[LatentArchiveDescriptor]] =
    archive.validate.flatMap: valid =>
      valid.run(runLabel) match
        case None =>
          Left(
            ArchiveError.InvalidArchive(
              s"run '${runLabel.value}' not found"
            )
          )
        case Some(_) =>
          bindingAndDescriptor(valid, runLabel)
            .map(_.map(_._2))

  def fromArchive(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, LatentArchiveResponse] =
    openPlan(archive, runLabel).map(_.archiveResponse)

  private def bindingAndDescriptor(
      archive: LnaArchive,
      runLabel: RunLabel
  ): Either[
    ArchiveError,
    Option[(LatentArchiveBinding, LatentArchiveDescriptor)]
  ] =
    val matches =
      Vector.newBuilder[
        (LatentArchiveBinding, LatentArchiveDescriptor)
      ]
    val iterator = bindings.iterator
    var failure = Option.empty[ArchiveError]
    while iterator.hasNext && failure.isEmpty do
      val binding = iterator.next()
      binding.descriptorOption(archive, runLabel) match
        case Left(error) =>
          failure = Some(error)
        case Right(Some(descriptor)) =>
          matches += binding -> descriptor
        case Right(None) =>
          ()

    failure match
      case Some(error) =>
        Left(error)
      case None =>
        matches.result() match
          case Vector() =>
            Right(None)
          case Vector(found) =>
            Right(Some(found))
          case many =>
            Left(
              ArchiveError.InvalidArchive(
                s"run '${runLabel.value}' matches multiple latent archive bindings: " +
                  many.map(_._1.name).sorted.mkString(", ")
              )
            )

object LatentArchiveRegistry:
  val standard: LatentArchiveRegistry =
    build(
      LatentArchiveBinding.Explicit,
      LatentArchiveBinding.TemporalDct,
      LatentArchiveBinding.SharedBasis,
      LatentArchiveBinding.Transport,
      LatentArchiveBinding.BoldZip
    ).fold(
      error => throw new IllegalStateException(error.message),
      identity
    )

  def build(
      bindings: LatentArchiveBinding*
  ): Either[LatentArchiveRegistryError, LatentArchiveRegistry] =
    val ordered =
      bindings.toVector.sortBy: binding =>
        (
          binding.name,
          binding.kinds.map(_.toString).mkString(",")
        )
    if ordered.isEmpty then
      Left(LatentArchiveRegistryError.Empty)
    else
      ordered.find(binding =>
        binding.name.trim.isEmpty ||
          binding.name != binding.name.trim
      ) match
        case Some(binding) =>
          Left(LatentArchiveRegistryError.InvalidBinding(
            binding.name,
            "name must be non-empty and have no surrounding whitespace"
          ))
        case None =>
          ordered.find(_.kinds.isEmpty) match
            case Some(binding) =>
              Left(LatentArchiveRegistryError.InvalidBinding(
                binding.name,
                "at least one archive kind is required"
              ))
            case None =>
              val kinds =
                ordered.flatMap(_.kinds).sortBy(_.toString)
              firstDuplicateKind(kinds) match
                case Some(kind) =>
                  Left(
                    LatentArchiveRegistryError.DuplicateKind(kind)
                  )
                case None =>
                  firstDuplicateName(ordered.map(_.name)) match
                    case Some(name) =>
                      Left(
                        LatentArchiveRegistryError
                          .DuplicateName(name)
                      )
                    case None =>
                      Right(
                        new LatentArchiveRegistry(ordered, kinds)
                      )

  private def firstDuplicateName(
      ordered: Vector[String]
  ): Option[String] =
    val sorted = ordered.sorted
    var index = 1
    while index < sorted.length do
      if sorted(index - 1) == sorted(index) then
        return Some(sorted(index))
      index += 1
    None

  private def firstDuplicateKind(
      ordered: Vector[LatentArchiveKind]
  ): Option[LatentArchiveKind] =
    var index = 1
    while index < ordered.length do
      if ordered(index - 1) == ordered(index) then
        return Some(ordered(index))
      index += 1
    None
