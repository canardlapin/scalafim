package scalafim.archive.lna

import scalafim.archive.{ArchiveError, ArchivePath, RunLabel}
import scalafim.image.{DMat, NeuroSpace}

object LnaPipeline:
  def quantArchive(
      data: DMat,
      space: NeuroSpace,
      runLabel: RunLabel = RunLabel.indexed(0),
      params: QuantParams = QuantParams(),
      creator: String = "scalafim-archive"
  ): Either[ArchiveError, LnaArchive] =
    val shape = LnaShape(space, data.rows)
    if data.cols != shape.spatialSize then
      Left(ArchiveError.ShapeMismatch(s"matrix has ${data.cols} columns but space has ${shape.spatialSize} voxels"))
    else
      Quant.encode(data, params).map { encoded =>
        val base = ArchivePath(s"/scans/${runLabel.value}/step_00_quant")
        val dataPath = base / "values"
        val scalePath = base / "scale"
        val offsetPath = base / "offset"

        val dataRef = DatasetRef(dataPath, DatasetRole.Quantized, encoded.quantized.dims, Some(encoded.quantized.dtype))
        val scaleRef = DatasetRef(scalePath, DatasetRole.Scale, encoded.scale.dims, Some(encoded.scale.dtype))
        val offsetRef = DatasetRef(offsetPath, DatasetRole.Offset, encoded.offset.dims, Some(encoded.offset.dtype))

        val descriptor = TransformDescriptor(
          name = "00_quant.json",
          kind = TransformKind.Quant,
          params = TransformParams.Quant(params),
          inputs = Vector("input"),
          outputs = Vector("quantized"),
          datasets = Vector(dataRef, scaleRef, offsetRef),
          report = Some(TransformReport.Quant(encoded.report))
        )

        LnaArchive(
          manifest = LnaManifest(
            creator = creator,
            requiredTransforms = Vector(TransformKind.Quant),
            transforms = Vector(descriptor),
            runs = Vector(LnaRun(runLabel, shape, dataPath)),
            datasets = Vector(dataRef, scaleRef, offsetRef),
            header = Map(
              "space.dims" -> space.spatialDims.mkString("x"),
              "timepoints" -> data.rows.toString
            )
          ),
          payloads = Map(
            dataPath -> encoded.quantized,
            scalePath -> encoded.scale,
            offsetPath -> encoded.offset
          )
        )
      }

  def deltaArchive(
      data: DMat,
      space: NeuroSpace,
      runLabel: RunLabel = RunLabel.indexed(0),
      params: DeltaParams = DeltaParams(),
      creator: String = "scalafim-archive"
  ): Either[ArchiveError, LnaArchive] =
    val shape = LnaShape(space, data.rows)
    if data.cols != shape.spatialSize then
      Left(ArchiveError.ShapeMismatch(s"matrix has ${data.cols} columns but space has ${shape.spatialSize} voxels"))
    else
      Delta.encode(data, params).map { encoded =>
        val base = ArchivePath(s"/scans/${runLabel.value}/step_00_delta")
        val deltaPath = base / "delta_stream"
        val firstPath = base / "first_values"

        val deltaRef = DatasetRef(deltaPath, DatasetRole.DeltaStream, encoded.deltas.dims, Some(encoded.deltas.dtype))
        val firstRef = DatasetRef(firstPath, DatasetRole.FirstValues, encoded.firstValues.dims, Some(encoded.firstValues.dtype))

        val descriptor = TransformDescriptor(
          name = "00_delta.json",
          kind = TransformKind.Delta,
          params = TransformParams.Delta(params),
          inputs = Vector("input"),
          outputs = Vector("delta_stream"),
          datasets = Vector(deltaRef, firstRef)
        )

        LnaArchive(
          manifest = LnaManifest(
            creator = creator,
            requiredTransforms = Vector(TransformKind.Delta),
            transforms = Vector(descriptor),
            runs = Vector(LnaRun(runLabel, shape, deltaPath)),
            datasets = Vector(deltaRef, firstRef),
            header = Map(
              "space.dims" -> space.spatialDims.mkString("x"),
              "timepoints" -> data.rows.toString
            )
          ),
          payloads = Map(
            deltaPath -> encoded.deltas,
            firstPath -> encoded.firstValues
          )
        )
      }

  def deltaQuantArchive(
      data: DMat,
      space: NeuroSpace,
      runLabel: RunLabel = RunLabel.indexed(0),
      deltaParams: DeltaParams = DeltaParams(),
      quantParams: QuantParams = QuantParams(),
      creator: String = "scalafim-archive"
  ): Either[ArchiveError, LnaArchive] =
    val shape = LnaShape(space, data.rows)
    if data.cols != shape.spatialSize then
      Left(ArchiveError.ShapeMismatch(s"matrix has ${data.cols} columns but space has ${shape.spatialSize} voxels"))
    else
      for
        delta <- Delta.encode(data, deltaParams)
        quant <- Quant.encode(delta.deltas.data, quantParams)
      yield
        val deltaBase = ArchivePath(s"/scans/${runLabel.value}/step_00_delta")
        val quantBase = ArchivePath(s"/scans/${runLabel.value}/step_01_quant")
        val firstPath = deltaBase / "first_values"
        val dataPath = quantBase / "values"
        val scalePath = quantBase / "scale"
        val offsetPath = quantBase / "offset"

        val firstRef = DatasetRef(firstPath, DatasetRole.FirstValues, delta.firstValues.dims, Some(delta.firstValues.dtype))
        val dataRef = DatasetRef(dataPath, DatasetRole.Quantized, quant.quantized.dims, Some(quant.quantized.dtype))
        val scaleRef = DatasetRef(scalePath, DatasetRole.Scale, quant.scale.dims, Some(quant.scale.dtype))
        val offsetRef = DatasetRef(offsetPath, DatasetRole.Offset, quant.offset.dims, Some(quant.offset.dtype))

        val deltaDescriptor = TransformDescriptor(
          name = "00_delta.json",
          kind = TransformKind.Delta,
          params = TransformParams.Delta(deltaParams),
          inputs = Vector("input"),
          outputs = Vector("delta_stream"),
          datasets = Vector(firstRef)
        )
        val quantDescriptor = TransformDescriptor(
          name = "01_quant.json",
          kind = TransformKind.Quant,
          params = TransformParams.Quant(quantParams),
          inputs = Vector("delta_stream"),
          outputs = Vector("quantized_delta"),
          datasets = Vector(dataRef, scaleRef, offsetRef),
          report = Some(TransformReport.Quant(quant.report))
        )

        LnaArchive(
          manifest = LnaManifest(
            creator = creator,
            requiredTransforms = Vector(TransformKind.Delta, TransformKind.Quant),
            transforms = Vector(deltaDescriptor, quantDescriptor),
            runs = Vector(LnaRun(runLabel, shape, dataPath)),
            datasets = Vector(firstRef, dataRef, scaleRef, offsetRef),
            header = Map(
              "space.dims" -> space.spatialDims.mkString("x"),
              "timepoints" -> data.rows.toString
            )
          ),
          payloads = Map(
            firstPath -> delta.firstValues,
            dataPath -> quant.quantized,
            scalePath -> quant.scale,
            offsetPath -> quant.offset
          )
        )

  def basisEmbedArchive(
      data: DMat,
      space: NeuroSpace,
      basis: DMat,
      runLabel: RunLabel = RunLabel.indexed(0),
      creator: String = "scalafim-archive"
  ): Either[ArchiveError, LnaArchive] =
    val shape = LnaShape(space, data.rows)
    if data.cols != shape.spatialSize then
      Left(ArchiveError.ShapeMismatch(s"matrix has ${data.cols} columns but space has ${shape.spatialSize} voxels"))
    else if basis.cols != data.cols then
      Left(ArchiveError.ShapeMismatch(s"basis has ${basis.cols} voxel columns but data has ${data.cols} columns"))
    else
      val coeff = multiplyByBasisTranspose(data, basis)
      val basisPath = ArchivePath("/basis/00_basis/matrix")
      val coeffPath = ArchivePath(s"/scans/${runLabel.value}/step_01_embed/coefficients")

      val basisRef = DatasetRef(basisPath, DatasetRole.BasisMatrix, Vector(basis.rows, basis.cols), Some(LnaDType.Float64))
      val coeffRef = DatasetRef(coeffPath, DatasetRole.Coefficients, Vector(coeff.rows, coeff.cols), Some(LnaDType.Float64))

      val basisDescriptor = TransformDescriptor(
        name = "00_basis.json",
        kind = TransformKind.Basis,
        params = TransformParams.Basis(method = "provided", k = basis.rows, center = false, scale = false),
        inputs = Vector("input"),
        outputs = Vector("basis"),
        datasets = Vector(basisRef)
      )
      val embedDescriptor = TransformDescriptor(
        name = "01_embed.json",
        kind = TransformKind.Embed,
        params = TransformParams.Embed(basisPath = basisPath),
        inputs = Vector("input", "basis"),
        outputs = Vector("coefficients"),
        datasets = Vector(coeffRef)
      )

      Right(
        LnaArchive(
          manifest = LnaManifest(
            creator = creator,
            requiredTransforms = Vector(TransformKind.Basis, TransformKind.Embed),
            transforms = Vector(basisDescriptor, embedDescriptor),
            runs = Vector(LnaRun(runLabel, shape, coeffPath)),
            datasets = Vector(basisRef, coeffRef),
            header = Map(
              "space.dims" -> space.spatialDims.mkString("x"),
              "timepoints" -> data.rows.toString,
              "basis.components" -> basis.rows.toString
            )
          ),
          payloads = Map(
            basisPath -> Payload.DoubleMatrix(basis),
            coeffPath -> Payload.DoubleMatrix(coeff)
          )
        )
      )

  def sharedBasisEmbedArchive(
      data: DMat,
      space: NeuroSpace,
      basis: SharedBasisArtifact,
      basisId: SharedBasisId,
      locator: Option[SharedBasisLocator] = None,
      runLabel: RunLabel = RunLabel.indexed(0),
      creator: String = "scalafim-archive"
  ): Either[ArchiveError, LnaArchive] =
    val shape = LnaShape(space, data.rows)
    if data.cols != shape.spatialSize then
      Left(ArchiveError.ShapeMismatch(s"matrix has ${data.cols} columns but space has ${shape.spatialSize} voxels"))
    else if basis.nVoxels != data.cols then
      Left(ArchiveError.ShapeMismatch(s"shared basis has ${basis.nVoxels} voxels but data has ${data.cols} columns"))
    else
      SharedBasisArtifact.validateFinite(basis).flatMap { validBasis =>
        val coeff = multiplyBySharedBasisLoadings(data, validBasis.loadings)
        sharedBasisEmbedArchiveFromCoefficients(
          coefficients = coeff,
          space = space,
          basis = validBasis,
          basisId = basisId,
          locator = locator,
          offset = None,
          runLabel = runLabel,
          creator = creator
        )
      }

  def sharedBasisEmbedArchiveFromCoefficients(
      coefficients: DMat,
      space: NeuroSpace,
      basis: SharedBasisArtifact,
      basisId: SharedBasisId,
      locator: Option[SharedBasisLocator] = None,
      offset: Option[Vector[Double]] = None,
      runLabel: RunLabel = RunLabel.indexed(0),
      creator: String = "scalafim-archive",
      sourceDomain: Option[String] = Some("voxels"),
      targetDomain: Option[String] = Some("shared_basis.coefficients"),
      label: Option[String] = None,
      metadata: Map[String, String] = Map.empty
  ): Either[ArchiveError, LnaArchive] =
    val shape = LnaShape(space, coefficients.rows)
    if basis.mask.values.length != shape.spatialSize then
      Left(ArchiveError.ShapeMismatch(s"shared basis mask has ${basis.mask.values.length} entries but space has ${shape.spatialSize} voxels"))
    else if coefficients.cols != basis.nAtoms then
      Left(ArchiveError.ShapeMismatch(s"coefficients have ${coefficients.cols} columns but shared basis has ${basis.nAtoms} atoms"))
    else
      offset match
        case Some(values) if values.length != basis.nVoxels =>
          Left(ArchiveError.ShapeMismatch(s"offset has ${values.length} values but shared basis has ${basis.nVoxels} active voxels"))
        case Some(values) if values.exists(!_.isFinite) =>
          Left(ArchiveError.InvalidArchive("shared basis offset contains non-finite values"))
        case _ =>
          SharedBasisArtifact.validateFinite(basis).map { validBasis =>
            val coeffPath = ArchivePath(s"/scans/${runLabel.value}/step_00_shared_basis_embed/coefficients")
            val offsetPath = ArchivePath(s"/scans/${runLabel.value}/step_00_shared_basis_embed/offset")
            val coeffRef = DatasetRef(coeffPath, DatasetRole.Coefficients, Vector(coefficients.rows, coefficients.cols), Some(LnaDType.Float64))
            val offsetRef = offset.map(values => DatasetRef(offsetPath, DatasetRole.Offset, Vector(values.length), Some(LnaDType.Float64)))
            val basisRef = SharedBasisRef(basisId, validBasis.checksum, locator)

            val embedDescriptor = TransformDescriptor(
              name = "00_shared_basis_embed.json",
              kind = TransformKind.Embed,
              params = TransformParams.SharedBasisEmbed(
                basis = basisRef,
                centerDataWith = offsetRef.map(_.path),
                sourceDomain = sourceDomain,
                targetDomain = targetDomain,
                label = label,
                metadata = metadata ++ Map(
                  "basis.kind" -> validBasis.kind,
                  "basis.n_atoms" -> validBasis.nAtoms.toString,
                  "basis.n_voxels" -> validBasis.nVoxels.toString,
                  "basis.mask_size" -> validBasis.mask.values.length.toString,
                  "basis.mask_active" -> validBasis.mask.activeCount.toString,
                  "center" -> offset.nonEmpty.toString
                )
              ),
              inputs = Vector("input", "shared_basis"),
              outputs = Vector("coefficients"),
              datasets = Vector(coeffRef) ++ offsetRef.toVector
            )

            LnaArchive(
              manifest = LnaManifest(
                creator = creator,
                requiredTransforms = Vector(TransformKind.Embed),
                transforms = Vector(embedDescriptor),
                runs = Vector(LnaRun(runLabel, shape, coeffPath)),
                datasets = Vector(coeffRef) ++ offsetRef.toVector,
                header = Map(
                  "space.dims" -> space.spatialDims.mkString("x"),
                  "timepoints" -> coefficients.rows.toString,
                  "basis.shared.id" -> basisId.value,
                  "basis.shared.checksum" -> validBasis.checksum.value,
                  "basis.shared.kind" -> validBasis.kind,
                  "basis.shared.atoms" -> validBasis.nAtoms.toString,
                  "basis.shared.mask_size" -> validBasis.mask.values.length.toString,
                  "basis.shared.mask_active" -> validBasis.mask.activeCount.toString,
                  "basis.shared.centered" -> offset.nonEmpty.toString
                ) ++ locator.map(value => "basis.shared.locator" -> value.value).toMap
              ),
              payloads = Map(coeffPath -> Payload.DoubleMatrix(coefficients)) ++
                offset.map(values => offsetPath -> Payload.DoubleVector(values)).toMap
            )
          }

  def reconstruct(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, DMat] =
    archive.validate.flatMap { valid =>
      valid.run(runLabel) match
        case None =>
          Left(ArchiveError.InvalidArchive(s"run '${runLabel.value}' not found"))
        case Some(run) =>
          if valid.manifest.transforms.isEmpty then
            Left(ArchiveError.InvalidArchive("archive has no transform descriptors"))
          else
            invertPlan(valid).flatMap { dense =>
              if dense.rows == run.shape.timepoints && dense.cols == run.shape.spatialSize then Right(dense)
              else Left(ArchiveError.ShapeMismatch("reconstructed run shape does not match manifest"))
            }
    }

  private def invertPlan(archive: LnaArchive): Either[ArchiveError, DMat] =
    val it = archive.manifest.transforms.reverseIterator
    var current = Option.empty[DMat]
    var error = Option.empty[ArchiveError]

    while it.hasNext && error.isEmpty do
      invertStep(archive, it.next(), current) match
        case Left(err) => error = Some(err)
        case Right(next) => current = Some(next)

    error match
      case Some(err) => Left(err)
      case None =>
        current.toRight(ArchiveError.InvalidArchive("archive did not materialize a dense reconstruction"))

  private def invertStep(
      archive: LnaArchive,
      desc: TransformDescriptor,
      current: Option[DMat]
  ): Either[ArchiveError, DMat] =
    desc.kind match
      case TransformKind.Quant =>
        current match
          case Some(_) =>
            Left(ArchiveError.InvalidArchive("quant descriptor cannot invert with an already materialized matrix"))
          case None =>
            reconstructQuant(archive, desc)
      case TransformKind.Delta =>
        reconstructDelta(archive, desc, current)
      case TransformKind.Embed =>
        current match
          case Some(_) =>
            Left(ArchiveError.UnsupportedTransform("embed inversion after a materialized matrix"))
          case None =>
            reconstructEmbed(archive, desc)
      case TransformKind.Basis =>
        current.toRight(ArchiveError.InvalidArchive("basis descriptor has no downstream matrix to reconstruct"))
      case TransformKind.Temporal =>
        current.toRight(ArchiveError.InvalidArchive("temporal descriptor has no downstream matrix to reconstruct"))
      case TransformKind.Custom(name) =>
        Left(ArchiveError.UnsupportedTransform(name))

  private def reconstructDelta(
      archive: LnaArchive,
      desc: TransformDescriptor,
      current: Option[DMat]
  ): Either[ArchiveError, DMat] =
    def params: Either[ArchiveError, DeltaParams] =
      desc.params match
        case TransformParams.Delta(p) => Right(p)
        case _ => Left(ArchiveError.InvalidArchive("delta descriptor missing typed delta params"))

    def deltaPayload: Either[ArchiveError, DMat] =
      current match
        case Some(data) => Right(data)
        case None =>
          for
            deltaPath <- byRole(desc, DatasetRole.DeltaStream)
            data <- archive.payload(deltaPath) match
              case Some(Payload.DoubleMatrix(data, _)) => Right(data)
              case Some(_) => Left(ArchiveError.ShapeMismatch("delta payload is not a double matrix"))
              case None => Left(ArchiveError.MissingPayload(deltaPath))
          yield data

    for
      p <- params
      deltas <- deltaPayload
      firstPath <- byRole(desc, DatasetRole.FirstValues)
      first <- archive.payload(firstPath) match
        case Some(Payload.DoubleMatrix(data, _)) => Right(data)
        case Some(_) => Left(ArchiveError.ShapeMismatch("delta first-values payload is not a double matrix"))
        case None => Left(ArchiveError.MissingPayload(firstPath))
      dense <- Delta.decodeChecked(deltas, first, p)
    yield dense

  private def reconstructQuant(
      archive: LnaArchive,
      desc: TransformDescriptor
  ): Either[ArchiveError, DMat] =
    for
      dataPath <- byRole(desc, DatasetRole.Quantized)
      scalePath <- byRole(desc, DatasetRole.Scale)
      offsetPath <- byRole(desc, DatasetRole.Offset)
      q <- archive.payload(dataPath) match
        case Some(p: Payload.IntMatrix) => Right(p)
        case Some(_) => Left(ArchiveError.ShapeMismatch("quantized payload is not an integer matrix"))
        case None => Left(ArchiveError.MissingPayload(dataPath))
      scale <- archive.payload(scalePath) match
        case Some(Payload.DoubleVector(values, _)) => Right(values)
        case Some(_) => Left(ArchiveError.ShapeMismatch("scale payload is not a double vector"))
        case None => Left(ArchiveError.MissingPayload(scalePath))
      offset <- archive.payload(offsetPath) match
        case Some(Payload.DoubleVector(values, _)) => Right(values)
        case Some(_) => Left(ArchiveError.ShapeMismatch("offset payload is not a double vector"))
        case None => Left(ArchiveError.MissingPayload(offsetPath))
      dense <- Quant.decodeChecked(q, scale, offset)
    yield dense

  private def reconstructEmbed(
      archive: LnaArchive,
      desc: TransformDescriptor
  ): Either[ArchiveError, DMat] =
    if LnaExplicitLatent.isExplicitEmbed(desc) then
      LnaExplicitLatent.reconstructEmbed(archive, desc)
    else
      val basisPath =
        desc.params match
          case TransformParams.Embed(path, _, _, _, _, _, _) => Right(path)
          case _: TransformParams.SharedBasisEmbed => Left(ArchiveError.UnsupportedTransform("shared_basis_embed requires an external shared-basis resolver"))
          case _ => Left(ArchiveError.InvalidArchive("embed descriptor missing typed embed params"))

      val coeffPath =
        byRole(desc, DatasetRole.Coefficients)

      for
        bp <- basisPath
        cp <- coeffPath
        basis <- archive.payload(bp) match
          case Some(Payload.DoubleMatrix(data, _)) => Right(data)
          case Some(_) => Left(ArchiveError.ShapeMismatch("basis payload is not a double matrix"))
          case None => Left(ArchiveError.MissingPayload(bp))
        coeff <- archive.payload(cp) match
          case Some(Payload.DoubleMatrix(data, _)) => Right(data)
          case Some(_) => Left(ArchiveError.ShapeMismatch("coefficient payload is not a double matrix"))
          case None => Left(ArchiveError.MissingPayload(cp))
        dense <-
          if coeff.cols == basis.rows then Right(multiply(coeff, basis))
          else Left(ArchiveError.ShapeMismatch(s"coefficients have ${coeff.cols} columns but basis has ${basis.rows} rows"))
      yield dense

  private def byRole(desc: TransformDescriptor, role: DatasetRole): Either[ArchiveError, ArchivePath] =
    desc.datasets
      .find(_.role == role)
      .map(_.path)
      .toRight(ArchiveError.InvalidArchive(s"${desc.kind.value} descriptor missing ${role.value} dataset"))

  private def multiplyByBasisTranspose(data: DMat, basis: DMat): DMat =
    val rows =
      Vector.tabulate(data.rows) { r =>
        Vector.tabulate(basis.rows) { k =>
          var sum = 0.0
          var c = 0
          while c < data.cols do
            sum += data(r, c) * basis(k, c)
            c += 1
          sum
        }
      }
    DMat.fromRows(rows)

  private def multiplyBySharedBasisLoadings(data: DMat, loadings: DMat): DMat =
    require(data.cols == loadings.rows, "matrix columns must match shared basis loading rows")
    val rows =
      Vector.tabulate(data.rows) { r =>
        Vector.tabulate(loadings.cols) { k =>
          var sum = 0.0
          var c = 0
          while c < data.cols do
            sum += data(r, c) * loadings(c, k)
            c += 1
          sum
        }
      }
    DMat.fromRows(rows)

  private def multiply(left: DMat, right: DMat): DMat =
    require(left.cols == right.rows, "matrix inner dimensions must agree")
    val rows =
      Vector.tabulate(left.rows) { r =>
        Vector.tabulate(right.cols) { c =>
          var sum = 0.0
          var k = 0
          while k < left.cols do
            sum += left(r, k) * right(k, c)
            k += 1
          sum
        }
      }
    DMat.fromRows(rows)
