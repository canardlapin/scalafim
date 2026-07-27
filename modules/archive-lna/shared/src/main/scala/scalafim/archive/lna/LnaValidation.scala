package scalafim.archive.lna

import scalafim.archive.{
  ArchiveError,
  ArchivePath,
  ArchiveValidationIssue,
  ArchiveValidationLayer,
  RunScopedPath
}

type ValidationLayer = ArchiveValidationLayer

object ValidationLayer:
  val Structure: ArchiveValidationLayer = ArchiveValidationLayer.Structure
  val Descriptors: ArchiveValidationLayer = ArchiveValidationLayer.Descriptors
  val References: ArchiveValidationLayer = ArchiveValidationLayer.References
  val Shapes: ArchiveValidationLayer = ArchiveValidationLayer.Shapes
  val Checksum: ArchiveValidationLayer = ArchiveValidationLayer.Checksum

type ValidationIssue = ArchiveValidationIssue

object ValidationIssue:
  def apply(
      layer: ValidationLayer,
      message: String,
      path: Option[ArchivePath] = None
  ): ValidationIssue =
    ArchiveValidationIssue(layer, message, path)

object LnaValidator:
  def validateArchive(archive: LnaArchive): Either[ArchiveError, LnaArchive] =
    val issues = validate(archive)
    if issues.isEmpty then Right(archive)
    else Left(ArchiveError.ValidationFailed(issues))

  def validate(archive: LnaArchive): Vector[ValidationIssue] =
    validateStructure(archive) ++
      validateDescriptors(archive) ++
      validateReferences(archive) ++
      validateChecksum(archive)

  private def validateStructure(archive: LnaArchive): Vector[ValidationIssue] =
    val m = archive.manifest
    val b = Vector.newBuilder[ValidationIssue]
    if m.version != LnaVersion.V2 then
      b += ValidationIssue(ValidationLayer.Structure, s"unsupported LNA version ${m.version.id}")
    if m.creator.trim.isEmpty then
      b += ValidationIssue(ValidationLayer.Structure, "creator must be non-empty")
    if m.runs.isEmpty then
      b += ValidationIssue(ValidationLayer.Structure, "manifest must contain at least one run")
    if m.datasets.map(_.path).distinct.length != m.datasets.length then
      b += ValidationIssue(ValidationLayer.Structure, "dataset refs must not contain duplicate paths")
    b.result()

  private def validateDescriptors(archive: LnaArchive): Vector[ValidationIssue] =
    val b = Vector.newBuilder[ValidationIssue]
    archive.manifest.transforms.zipWithIndex.foreach { case (desc, i) =>
      if !desc.name.endsWith(".json") then
        b += ValidationIssue(ValidationLayer.Descriptors, s"descriptor $i name must end in .json")
      if desc.kind.value.trim.isEmpty then
        b += ValidationIssue(ValidationLayer.Descriptors, s"descriptor ${desc.name} has empty transform kind")
      if desc.inputs.isEmpty then
        b += ValidationIssue(ValidationLayer.Descriptors, s"descriptor ${desc.name} has no input keys")
      if desc.outputs.isEmpty then
        b += ValidationIssue(ValidationLayer.Descriptors, s"descriptor ${desc.name} has no output keys")
      if desc.params != TransformParams.Empty && desc.params.kind != desc.kind then
        b += ValidationIssue(ValidationLayer.Descriptors, s"descriptor ${desc.name} params kind does not match transform kind")
    }
    b.result()

  private def validateReferences(archive: LnaArchive): Vector[ValidationIssue] =
    val b = Vector.newBuilder[ValidationIssue]
    val declared = archive.manifest.datasets.map(ref => ref.path -> ref).toMap

    archive.manifest.transforms.foreach { desc =>
      desc.datasets.foreach { ref =>
        if !declared.contains(ref.path) then
          b += ValidationIssue(ValidationLayer.References, s"descriptor ${desc.name} references undeclared dataset", Some(ref.path))
      }
      validateTemporalDescriptor(desc, declared).foreach(b += _)
      validateExplicitLatentDescriptor(desc, declared, archive.manifest.runs).foreach(b += _)
      validateSharedBasisEmbedDescriptor(desc, declared, archive.manifest.runs).foreach(b += _)
    }

    archive.manifest.runs.foreach { run =>
      if !declared.contains(run.output) then
        b += ValidationIssue(ValidationLayer.References, s"run ${run.label.value} output is not declared", Some(run.output))
    }

    val runLabels = archive.manifest.runs.map(_.label).toSet
    declared.values.foreach { ref =>
      RunScopedPath.from(ref.path).foreach { scoped =>
        if !runLabels.contains(scoped.label) then
          b += ValidationIssue(
            ValidationLayer.References,
            s"dataset path belongs to unknown run ${scoped.label.value}",
            Some(ref.path)
          )
      }

      archive.payloads.get(ref.path) match
        case None =>
          b += ValidationIssue(ValidationLayer.References, "declared dataset has no payload", Some(ref.path))
        case Some(payload) =>
          if payload.dims != ref.dims then
            b += ValidationIssue(
              ValidationLayer.Shapes,
              s"declared dims ${ref.dims.mkString("x")} do not match payload dims ${payload.dims.mkString("x")}",
              Some(ref.path)
            )
          ref.dtype.foreach { dtype =>
            if dtype != payload.dtype then
              b += ValidationIssue(
                ValidationLayer.Shapes,
                s"declared dtype $dtype does not match payload dtype ${payload.dtype}",
                Some(ref.path)
              )
          }
    }

    b.result()

  private def validateTemporalDescriptor(
      desc: TransformDescriptor,
      declared: Map[ArchivePath, DatasetRef]
  ): Vector[ValidationIssue] =
    if desc.kind != TransformKind.Temporal then Vector.empty
    else
      val b = Vector.newBuilder[ValidationIssue]
      val params =
        desc.params match
          case TransformParams.TemporalDct(value) => Some(value)
          case _ =>
            b += ValidationIssue(ValidationLayer.Descriptors, s"descriptor ${desc.name} missing temporal DCT params")
            None

      val basisRef = desc.datasets.find(_.role == DatasetRole.TemporalBasis)
      if basisRef.isEmpty then
        b += ValidationIssue(ValidationLayer.Descriptors, s"descriptor ${desc.name} missing temporal basis dataset")

      basisRef.foreach { ref =>
        declared.get(ref.path) match
          case None =>
            b += ValidationIssue(ValidationLayer.References, s"descriptor ${desc.name} temporal basis path is undeclared", Some(ref.path))
          case Some(declaredRef) if declaredRef.role != DatasetRole.TemporalBasis =>
            b += ValidationIssue(ValidationLayer.Descriptors, s"descriptor ${desc.name} basis path must reference a temporal_basis dataset", Some(ref.path))
          case _ =>
            ()

        if ref.dims.length != 2 then
          b += ValidationIssue(ValidationLayer.Shapes, "temporal basis dataset must be two-dimensional", Some(ref.path))

        params.foreach { p =>
          if ref.dims.length == 2 then
            if ref.dims(1) != p.components then
              b += ValidationIssue(
                ValidationLayer.Shapes,
                s"temporal DCT components ${p.components} do not match basis columns ${ref.dims(1)}",
                Some(ref.path)
              )
            if p.components > ref.dims.head then
              b += ValidationIssue(
                ValidationLayer.Shapes,
                s"temporal DCT components ${p.components} exceed basis timepoints ${ref.dims.head}",
                Some(ref.path)
              )
        }
      }

      b.result()

  private def validateExplicitLatentDescriptor(
      desc: TransformDescriptor,
      declared: Map[ArchivePath, DatasetRef],
      runs: Vector[LnaRun]
  ): Vector[ValidationIssue] =
    if !ExplicitLatentDescriptor.matches(desc) then Vector.empty
    else
      val b = Vector.newBuilder[ValidationIssue]
      val params =
        desc.params match
          case p: TransformParams.Embed => Some(p)
          case _ =>
            b += ValidationIssue(ValidationLayer.Descriptors, s"descriptor ${desc.name} missing explicit latent embed params")
            None

      params.foreach { p =>
        if p.sourceDomain.forall(_.trim.isEmpty) then
          b += ValidationIssue(ValidationLayer.Descriptors, s"descriptor ${desc.name} missing source domain")
        if p.targetDomain.forall(_.trim.isEmpty) then
          b += ValidationIssue(ValidationLayer.Descriptors, s"descriptor ${desc.name} missing target domain")

        declared.get(p.basisPath) match
          case None =>
            b += ValidationIssue(ValidationLayer.References, s"descriptor ${desc.name} explicit latent basis path is undeclared", Some(p.basisPath))
          case Some(ref) if ref.role != DatasetRole.TemporalBasis =>
            b += ValidationIssue(ValidationLayer.Descriptors, s"descriptor ${desc.name} basis path must reference a temporal_basis dataset", Some(p.basisPath))
          case _ =>
            ()
      }

      val loadingsRef = desc.datasets.find(_.role == DatasetRole.Loadings)
      if loadingsRef.isEmpty then
        b += ValidationIssue(ValidationLayer.Descriptors, s"descriptor ${desc.name} missing loadings dataset")

      for
        p <- params
        basisRef <- declared.get(p.basisPath)
        loadRef <- loadingsRef
      do
        if basisRef.dims.length != 2 then
          b += ValidationIssue(ValidationLayer.Shapes, "temporal basis dataset must be two-dimensional", Some(basisRef.path))
        if loadRef.dims.length != 2 then
          b += ValidationIssue(ValidationLayer.Shapes, "loadings dataset must be two-dimensional", Some(loadRef.path))
        if basisRef.dims.length == 2 && loadRef.dims.length == 2 && basisRef.dims(1) != loadRef.dims(1) then
          b += ValidationIssue(
            ValidationLayer.Shapes,
            s"temporal basis coefficients ${basisRef.dims(1)} do not match loadings coefficients ${loadRef.dims(1)}",
            Some(loadRef.path)
          )

        desc.datasets.find(_.role == DatasetRole.SampleOffset).foreach { offsetRef =>
          if offsetRef.dims.length != 1 then
            b += ValidationIssue(ValidationLayer.Shapes, "sample offset dataset must be one-dimensional", Some(offsetRef.path))
          else if loadRef.dims.length == 2 && offsetRef.dims.head != loadRef.dims.head then
            b += ValidationIssue(
              ValidationLayer.Shapes,
              s"sample offset length ${offsetRef.dims.head} does not match loadings samples ${loadRef.dims.head}",
              Some(offsetRef.path)
            )
        }

        runForPath(loadRef.path, runs).foreach { run =>
          if basisRef.dims.length == 2 && basisRef.dims.head != run.shape.timepoints then
            b += ValidationIssue(
              ValidationLayer.Shapes,
              s"temporal basis rows ${basisRef.dims.head} do not match run timepoints ${run.shape.timepoints}",
              Some(basisRef.path)
            )
          if loadRef.dims.length == 2 && loadRef.dims.head != run.shape.spatialSize then
            b += ValidationIssue(
              ValidationLayer.Shapes,
              s"loadings samples ${loadRef.dims.head} do not match run samples ${run.shape.spatialSize}",
              Some(loadRef.path)
            )
        }

      b.result()

  private def validateSharedBasisEmbedDescriptor(
      desc: TransformDescriptor,
      declared: Map[ArchivePath, DatasetRef],
      runs: Vector[LnaRun]
  ): Vector[ValidationIssue] =
    desc.params match
      case _: TransformParams.SharedBasisEmbed =>
        val b = Vector.newBuilder[ValidationIssue]
        val coeffRef = desc.datasets.find(_.role == DatasetRole.Coefficients)

        if desc.kind != TransformKind.Embed then
          b += ValidationIssue(ValidationLayer.Descriptors, s"descriptor ${desc.name} shared basis params must use embed transform kind")

        if coeffRef.isEmpty then
          b += ValidationIssue(ValidationLayer.Descriptors, s"descriptor ${desc.name} missing coefficients dataset")

        coeffRef.foreach { ref =>
          declared.get(ref.path) match
            case None =>
              b += ValidationIssue(ValidationLayer.References, s"descriptor ${desc.name} coefficients path is undeclared", Some(ref.path))
            case Some(declaredRef) if declaredRef.role != DatasetRole.Coefficients =>
              b += ValidationIssue(ValidationLayer.Descriptors, s"descriptor ${desc.name} coefficients path must reference a coefficients dataset", Some(ref.path))
            case _ =>
              ()

          if ref.dims.length != 2 then
            b += ValidationIssue(ValidationLayer.Shapes, "shared basis coefficients dataset must be two-dimensional", Some(ref.path))

          runForPath(ref.path, runs).foreach { run =>
            if ref.dims.length == 2 && ref.dims.head != run.shape.timepoints then
              b += ValidationIssue(
                ValidationLayer.Shapes,
                s"coefficient rows ${ref.dims.head} do not match run timepoints ${run.shape.timepoints}",
                Some(ref.path)
              )
          }
        }

        desc.datasets.find(_.role == DatasetRole.Offset).foreach { ref =>
          declared.get(ref.path) match
            case None =>
              b += ValidationIssue(ValidationLayer.References, s"descriptor ${desc.name} offset path is undeclared", Some(ref.path))
            case Some(declaredRef) if declaredRef.role != DatasetRole.Offset =>
              b += ValidationIssue(ValidationLayer.Descriptors, s"descriptor ${desc.name} offset path must reference an offset dataset", Some(ref.path))
            case _ =>
              ()

          if ref.dims.length != 1 then
            b += ValidationIssue(ValidationLayer.Shapes, "shared basis offset dataset must be one-dimensional", Some(ref.path))

          runForPath(ref.path, runs).foreach { run =>
            if ref.dims.length == 1 && ref.dims.head > run.shape.spatialSize then
              b += ValidationIssue(
                ValidationLayer.Shapes,
                s"offset length ${ref.dims.head} exceeds run samples ${run.shape.spatialSize}",
                Some(ref.path)
              )
          }
        }

        b.result()
      case _ =>
        Vector.empty

  private def runForPath(path: ArchivePath, runs: Vector[LnaRun]): Option[LnaRun] =
    runs.find(_.output == path).orElse {
      RunScopedPath.from(path).flatMap(scoped => runs.find(_.label == scoped.label))
    }

  private def validateChecksum(archive: LnaArchive): Vector[ValidationIssue] =
    archive.manifest.checksum match
      case None => Vector.empty
      case Some(value) if value.matches("[A-Fa-f0-9]{64}") => Vector.empty
      case Some(_) => Vector(ValidationIssue(ValidationLayer.Checksum, "checksum must be a 64-character SHA-256 hex string"))
