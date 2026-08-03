package scalafim.archive.lna

import scalafim.archive.{ArchiveError, ArchivePath, RunLabel}
import scalafim.image.NeuroSpace

import scala.collection.mutable
import scala.util.control.NonFatal

object LnaManifestCodec:
  def render(manifest: LnaManifest): String =
    obj(
      "version" -> str(manifest.version.id),
      "creator" -> str(manifest.creator),
      "required_transforms" -> arr(manifest.requiredTransforms.map(kindJson)),
      "transforms" -> arr(manifest.transforms.map(transformJson)),
      "runs" -> arr(manifest.runs.map(runJson)),
      "datasets" -> arr(manifest.datasets.map(datasetJson)),
      "header" -> obj(manifest.header.toVector.sortBy(_._1).map { case (k, v) => k -> str(v) }),
      "checksum" -> manifest.checksum.fold("null")(str)
    )

  def parse(text: String): Either[ArchiveError, LnaManifest] =
    Json.parse(text).flatMap(asObj(_, "manifest")).flatMap { root =>
      for
        version <- stringField(root, "version").flatMap(parseVersion)
        creator <- stringField(root, "creator")
        required <- arrayField(root, "required_transforms").flatMap(values => traverse(values)(asString(_, "required transform").map(parseKind)))
        transforms <- arrayField(root, "transforms").flatMap(values => traverse(values)(parseTransform))
        runs <- arrayField(root, "runs").flatMap(values => traverse(values)(parseRun))
        datasets <- arrayField(root, "datasets").flatMap(values => traverse(values)(parseDatasetRef))
        header <- objectField(root, "header").flatMap(parseHeader)
        checksum <- optionalNullableString(root, "checksum")
        manifest <- catchInvalid("manifest")(LnaManifest(version, creator, required, transforms, runs, datasets, header, checksum))
      yield manifest
    }

  def renderTransform(descriptor: TransformDescriptor): String =
    transformJson(descriptor)

  private def transformJson(desc: TransformDescriptor): String =
    obj(
      "name" -> str(desc.name),
      "kind" -> kindJson(desc.kind),
      "params" -> paramsJson(desc.params),
      "inputs" -> arr(desc.inputs.map(str)),
      "outputs" -> arr(desc.outputs.map(str)),
      "datasets" -> arr(desc.datasets.map(datasetJson)),
      "report" -> desc.report.fold("null")(reportJson)
    )

  private def datasetJson(ref: DatasetRef): String =
    obj(
      "path" -> str(ref.path.value),
      "role" -> str(ref.role.value),
      "dims" -> arr(ref.dims.map(_.toString)),
      "dtype" -> ref.dtype.fold("null")(dtype => str(dtypeJson(dtype)))
    )

  private def runJson(run: LnaRun): String =
    obj(
      "label" -> str(run.label.value),
      "space_dims" -> arr(run.shape.space.spatialDims.map(_.toString)),
      "timepoints" -> run.shape.timepoints.toString,
      "output" -> str(run.output.value),
      "mask" -> run.mask.fold("null")(path => str(path.value))
    )

  private def paramsJson(params: TransformParams): String =
    params match
      case TransformParams.Empty =>
        obj("type" -> str("empty"))
      case TransformParams.Quant(p) =>
        obj(
          "type" -> str("quant"),
          "bits" -> p.bits.toString,
          "method" -> str(quantMethodJson(p.method)),
          "center" -> p.center.toString,
          "scale_scope" -> str(scaleScopeJson(p.scaleScope)),
          "allow_clip" -> p.allowClip.toString
        )
      case TransformParams.Delta(p) =>
        obj(
          "type" -> str("delta"),
          "order" -> p.order.toString,
          "axis" -> str(deltaAxisJson(p.axis)),
          "reference_value_storage" -> str(deltaReferenceStorageJson(p.referenceValueStorage)),
          "coding_method" -> str(deltaCodingMethodJson(p.codingMethod))
        )
      case TransformParams.TemporalDct(p) =>
        obj(
          "type" -> str("temporal_dct"),
          "components" -> p.components.toString,
          "norm" -> str(temporalDctNormJson(p.norm)),
          "center" -> p.center.toString,
          "ridge" -> p.ridge.toString
        )
      case TransformParams.Basis(method, k, center, scale) =>
        obj(
          "type" -> str("basis"),
          "method" -> str(method),
          "k" -> k.toString,
          "center" -> center.toString,
          "scale" -> scale.toString
        )
      case TransformParams.Embed(basisPath, centerDataWith, scaleDataWith, sourceDomain, targetDomain, label, metadata) =>
        obj(
          "type" -> str("embed"),
          "basis_path" -> str(basisPath.value),
          "center_data_with" -> centerDataWith.fold("null")(path => str(path.value)),
          "scale_data_with" -> scaleDataWith.fold("null")(path => str(path.value)),
          "source_domain" -> sourceDomain.fold("null")(str),
          "target_domain" -> targetDomain.fold("null")(str),
          "label" -> label.fold("null")(str),
          "metadata" -> stringMapJson(metadata)
        )
      case TransformParams.SharedBasisEmbed(basis, centerDataWith, scaleDataWith, sourceDomain, targetDomain, label, metadata) =>
        obj(
          "type" -> str("shared_basis_embed"),
          "basis_id" -> str(basis.basisId.value),
          "checksum" -> str(basis.checksum.value),
          "locator" -> basis.locator.fold("null")(locator => str(locator.value)),
          "center_data_with" -> centerDataWith.fold("null")(path => str(path.value)),
          "scale_data_with" -> scaleDataWith.fold("null")(path => str(path.value)),
          "source_domain" -> sourceDomain.fold("null")(str),
          "target_domain" -> targetDomain.fold("null")(str),
          "label" -> label.fold("null")(str),
          "metadata" -> stringMapJson(metadata)
        )
      case TransformParams.Custom(name, sourceDomain, targetDomain, label, metadata) =>
        obj(
          "type" -> str("custom"),
          "name" -> str(name),
          "source_domain" -> sourceDomain.fold("null")(str),
          "target_domain" -> targetDomain.fold("null")(str),
          "label" -> label.fold("null")(str),
          "metadata" -> stringMapJson(metadata)
        )

  private def parseTransform(json: J): Either[ArchiveError, TransformDescriptor] =
    asObj(json, "transform").flatMap { obj =>
      for
        name <- stringField(obj, "name")
        kind <- stringField(obj, "kind").map(parseKind)
        params <- objectField(obj, "params").flatMap(parseParams)
        inputs <- arrayField(obj, "inputs").flatMap(values => traverse(values)(asString(_, "transform input")))
        outputs <- arrayField(obj, "outputs").flatMap(values => traverse(values)(asString(_, "transform output")))
        datasets <- arrayField(obj, "datasets").flatMap(values => traverse(values)(parseDatasetRef))
        report <- optionalNullableObject(obj, "report").flatMap(_.fold(Right(None))(value => parseReport(value).map(Some(_))))
        desc <- catchInvalid(s"transform $name")(TransformDescriptor(name, kind, params, inputs, outputs, datasets, report))
      yield desc
    }

  private def parseDatasetRef(json: J): Either[ArchiveError, DatasetRef] =
    asObj(json, "dataset ref").flatMap { obj =>
      for
        path <- archivePathField(obj, "path")
        role <- stringField(obj, "role").map(parseRole)
        dims <- arrayField(obj, "dims").flatMap(values => traverse(values)(asInt(_, "dataset dim")))
        dtype <- optionalNullableString(obj, "dtype").flatMap(_.fold(Right(None))(value => parseDType(value).map(Some(_))))
        ref <- catchInvalid(s"dataset ${path.value}")(DatasetRef(path, role, dims, dtype))
      yield ref
    }

  private def parseRun(json: J): Either[ArchiveError, LnaRun] =
    asObj(json, "run").flatMap { obj =>
      for
        label <- stringField(obj, "label").flatMap(value => catchInvalid(s"run label $value")(RunLabel(value)))
        dims <- arrayField(obj, "space_dims").flatMap(values => traverse(values)(asInt(_, "space dim")))
        timepoints <- intField(obj, "timepoints")
        output <- archivePathField(obj, "output")
        mask <- optionalNullableString(obj, "mask").flatMap(_.fold(Right(None))(value => catchInvalid(s"mask path $value")(Some(ArchivePath(value)))))
        run <- catchInvalid(s"run ${label.value}")(LnaRun(label, LnaShape(NeuroSpace(dims), timepoints), output, mask))
      yield run
    }

  private def parseParams(obj: Map[String, J]): Either[ArchiveError, TransformParams] =
    stringField(obj, "type").flatMap {
      case "empty" => Right(TransformParams.Empty)
      case "quant" =>
        for
          bits <- intField(obj, "bits")
          method <- stringField(obj, "method").flatMap(parseQuantMethod)
          center <- boolField(obj, "center")
          scaleScope <- stringField(obj, "scale_scope").flatMap(parseScaleScope)
          allowClip <- boolField(obj, "allow_clip")
          params <- catchInvalid("quant params")(TransformParams.Quant(QuantParams(bits, method, center, scaleScope, allowClip)))
        yield params
      case "delta" =>
        for
          order <- intField(obj, "order")
          axis <- stringField(obj, "axis").flatMap(parseDeltaAxis)
          referenceValueStorage <- stringField(obj, "reference_value_storage").flatMap(parseDeltaReferenceStorage)
          codingMethod <- stringField(obj, "coding_method").flatMap(parseDeltaCodingMethod)
          params <- catchInvalid("delta params")(TransformParams.Delta(DeltaParams(order, axis, referenceValueStorage, codingMethod)))
        yield params
      case "temporal_dct" =>
        for
          components <- intField(obj, "components")
          norm <- stringField(obj, "norm").flatMap(parseTemporalDctNorm)
          center <- boolField(obj, "center")
          ridge <- doubleField(obj, "ridge")
          params <- catchInvalid("temporal DCT params")(TransformParams.TemporalDct(TemporalDctParams(components, norm, center, ridge)))
        yield params
      case "basis" =>
        for
          method <- stringField(obj, "method")
          k <- intField(obj, "k")
          center <- boolField(obj, "center")
          scale <- boolField(obj, "scale")
          params <- catchInvalid("basis params")(TransformParams.Basis(method, k, center, scale))
        yield params
      case "embed" =>
        for
          basisPath <- archivePathField(obj, "basis_path")
          centerDataWith <- optionalNullableString(obj, "center_data_with")
            .flatMap(_.fold(Right(None))(value => catchInvalid(s"center data path $value")(Some(ArchivePath(value)))))
          scaleDataWith <- optionalNullableString(obj, "scale_data_with")
            .flatMap(_.fold(Right(None))(value => catchInvalid(s"scale data path $value")(Some(ArchivePath(value)))))
          sourceDomain <- optionalNullableString(obj, "source_domain")
          targetDomain <- optionalNullableString(obj, "target_domain")
          label <- optionalNullableString(obj, "label")
          metadata <- optionalNullableObject(obj, "metadata").flatMap(_.fold(Right(Map.empty[String, String]))(parseHeader))
          params <- catchInvalid("embed params")(TransformParams.Embed(basisPath, centerDataWith, scaleDataWith, sourceDomain, targetDomain, label, metadata))
        yield params
      case "shared_basis_embed" =>
        for
          basisId <- stringField(obj, "basis_id").flatMap(SharedBasisId.apply)
          checksum <- stringField(obj, "checksum").flatMap(SharedBasisChecksum.apply)
          locator <- optionalNullableString(obj, "locator")
            .flatMap(_.fold(Right(None))(value => SharedBasisLocator(value).map(Some(_))))
          centerDataWith <- optionalNullableString(obj, "center_data_with")
            .flatMap(_.fold(Right(None))(value => catchInvalid(s"center data path $value")(Some(ArchivePath(value)))))
          scaleDataWith <- optionalNullableString(obj, "scale_data_with")
            .flatMap(_.fold(Right(None))(value => catchInvalid(s"scale data path $value")(Some(ArchivePath(value)))))
          sourceDomain <- optionalNullableString(obj, "source_domain")
          targetDomain <- optionalNullableString(obj, "target_domain")
          label <- optionalNullableString(obj, "label")
          metadata <- optionalNullableObject(obj, "metadata").flatMap(_.fold(Right(Map.empty[String, String]))(parseHeader))
          params <- catchInvalid("shared basis embed params")(
            TransformParams.SharedBasisEmbed(
              SharedBasisRef(basisId, checksum, locator),
              centerDataWith,
              scaleDataWith,
              sourceDomain,
              targetDomain,
              label,
              metadata
            )
          )
        yield params
      case "custom" =>
        for
          name <- stringField(obj, "name")
          sourceDomain <- optionalNullableString(obj, "source_domain")
          targetDomain <- optionalNullableString(obj, "target_domain")
          label <- optionalNullableString(obj, "label")
          metadata <- optionalNullableObject(obj, "metadata").flatMap(_.fold(Right(Map.empty[String, String]))(parseHeader))
          params <- catchInvalid("custom params")(TransformParams.Custom(name, sourceDomain, targetDomain, label, metadata))
        yield params
      case other =>
        Left(ArchiveError.InvalidArchive(s"unknown transform params type '$other'"))
    }

  private def parseHeader(obj: Map[String, J]): Either[ArchiveError, Map[String, String]] =
    traverse(obj.toVector) { case (key, value) => asString(value, s"header $key").map(key -> _) }.map(_.toMap)

  private def reportJson(report: TransformReport): String =
    report match
      case TransformReport.Quant(r) =>
        obj(
          "type" -> str("quant"),
          "bits" -> r.bits.toString,
          "method" -> str(quantMethodJson(r.method)),
          "scale_scope" -> str(scaleScopeJson(r.scaleScope)),
          "n_clipped_total" -> r.nClippedTotal.toString,
          "clip_pct" -> r.clipPct.toString
        )

  private def parseReport(obj: Map[String, J]): Either[ArchiveError, TransformReport] =
    stringField(obj, "type").flatMap {
      case "quant" =>
        for
          bits <- intField(obj, "bits")
          method <- stringField(obj, "method").flatMap(parseQuantMethod)
          scaleScope <- stringField(obj, "scale_scope").flatMap(parseScaleScope)
          nClippedTotal <- intField(obj, "n_clipped_total")
          clipPct <- doubleField(obj, "clip_pct")
          report <- catchInvalid("quant report")(TransformReport.Quant(QuantReport(bits, method, scaleScope, nClippedTotal, clipPct)))
        yield report
      case other =>
        Left(ArchiveError.InvalidArchive(s"unknown transform report type '$other'"))
    }

  private def parseVersion(value: String): Either[ArchiveError, LnaVersion] =
    if value == LnaVersion.V2.id then Right(LnaVersion.V2)
    else Left(ArchiveError.InvalidArchive(s"unsupported LNA version '$value'"))

  private def parseKind(value: String): TransformKind =
    value match
      case "quant" => TransformKind.Quant
      case "basis" => TransformKind.Basis
      case "embed"  => TransformKind.Embed
      case "delta"  => TransformKind.Delta
      case "temporal" => TransformKind.Temporal
      case other    => TransformKind.Custom(other)

  private def parseRole(value: String): DatasetRole =
    value match
      case "raw_data"     => DatasetRole.RawData
      case "quantized"    => DatasetRole.Quantized
      case "scale"        => DatasetRole.Scale
      case "offset"       => DatasetRole.Offset
      case "basis_matrix" => DatasetRole.BasisMatrix
      case "coefficients" => DatasetRole.Coefficients
      case "temporal_basis" => DatasetRole.TemporalBasis
      case "loadings"       => DatasetRole.Loadings
      case "sample_offset"  => DatasetRole.SampleOffset
      case "delta_stream" => DatasetRole.DeltaStream
      case "first_values" => DatasetRole.FirstValues
      case "mask"         => DatasetRole.Mask
      case "metadata"     => DatasetRole.Metadata
      case other          => DatasetRole.Other(other)

  private def parseDType(value: String): Either[ArchiveError, LnaDType] =
    value match
      case "float32" => Right(LnaDType.Float32)
      case "float64" => Right(LnaDType.Float64)
      case "uint8"   => Right(LnaDType.UInt8)
      case "uint16"  => Right(LnaDType.UInt16)
      case "int32"   => Right(LnaDType.Int32)
      case other     => Left(ArchiveError.InvalidArchive(s"unknown dtype '$other'"))

  private def parseQuantMethod(value: String): Either[ArchiveError, QuantMethod] =
    value match
      case "range" => Right(QuantMethod.Range)
      case "sd"    => Right(QuantMethod.Sd)
      case other   => Left(ArchiveError.InvalidArchive(s"unknown quant method '$other'"))

  private def parseScaleScope(value: String): Either[ArchiveError, QuantScaleScope] =
    value match
      case "global" => Right(QuantScaleScope.Global)
      case "voxel"  => Right(QuantScaleScope.Voxel)
      case other    => Left(ArchiveError.InvalidArchive(s"unknown quant scale scope '$other'"))

  private def parseDeltaAxis(value: String): Either[ArchiveError, DeltaAxis] =
    value match
      case "time"    => Right(DeltaAxis.Time)
      case "feature" => Right(DeltaAxis.Feature)
      case other     => Left(ArchiveError.InvalidArchive(s"unknown delta axis '$other'"))

  private def parseDeltaReferenceStorage(value: String): Either[ArchiveError, DeltaReferenceStorage] =
    value match
      case "first_value_verbatim" => Right(DeltaReferenceStorage.FirstValueVerbatim)
      case other => Left(ArchiveError.InvalidArchive(s"unknown delta reference storage '$other'"))

  private def parseDeltaCodingMethod(value: String): Either[ArchiveError, DeltaCodingMethod] =
    value match
      case "none" => Right(DeltaCodingMethod.None)
      case other  => Left(ArchiveError.InvalidArchive(s"unknown delta coding method '$other'"))

  private def parseTemporalDctNorm(value: String): Either[ArchiveError, TemporalDctNorm] =
    value match
      case "ortho" => Right(TemporalDctNorm.Ortho)
      case "none"  => Right(TemporalDctNorm.None)
      case other   => Left(ArchiveError.InvalidArchive(s"unknown temporal DCT norm '$other'"))

  private def kindJson(kind: TransformKind): String = str(kind.value)

  private def dtypeJson(dtype: LnaDType): String =
    dtype match
      case LnaDType.Float32 => "float32"
      case LnaDType.Float64 => "float64"
      case LnaDType.UInt8   => "uint8"
      case LnaDType.UInt16  => "uint16"
      case LnaDType.Int32   => "int32"

  private def quantMethodJson(method: QuantMethod): String =
    method match
      case QuantMethod.Range => "range"
      case QuantMethod.Sd    => "sd"

  private def scaleScopeJson(scope: QuantScaleScope): String =
    scope match
      case QuantScaleScope.Global => "global"
      case QuantScaleScope.Voxel  => "voxel"

  private def deltaAxisJson(axis: DeltaAxis): String =
    axis.value

  private def deltaReferenceStorageJson(storage: DeltaReferenceStorage): String =
    storage.value

  private def deltaCodingMethodJson(method: DeltaCodingMethod): String =
    method.value

  private def temporalDctNormJson(norm: TemporalDctNorm): String =
    norm.value

  private def archivePathField(obj: Map[String, J], key: String): Either[ArchiveError, ArchivePath] =
    stringField(obj, key).flatMap(value => catchInvalid(s"archive path $value")(ArchivePath(value)))

  private def stringField(obj: Map[String, J], key: String): Either[ArchiveError, String] =
    field(obj, key).flatMap(asString(_, key))

  private def intField(obj: Map[String, J], key: String): Either[ArchiveError, Int] =
    field(obj, key).flatMap(asInt(_, key))

  private def boolField(obj: Map[String, J], key: String): Either[ArchiveError, Boolean] =
    field(obj, key).flatMap(asBool(_, key))

  private def doubleField(obj: Map[String, J], key: String): Either[ArchiveError, Double] =
    field(obj, key).flatMap(asDouble(_, key))

  private def arrayField(obj: Map[String, J], key: String): Either[ArchiveError, Vector[J]] =
    field(obj, key).flatMap(asArr(_, key))

  private def objectField(obj: Map[String, J], key: String): Either[ArchiveError, Map[String, J]] =
    field(obj, key).flatMap(asObj(_, key))

  private def optionalNullableObject(obj: Map[String, J], key: String): Either[ArchiveError, Option[Map[String, J]]] =
    obj.get(key) match
      case None | Some(J.Null) => Right(None)
      case Some(value)         => asObj(value, key).map(Some(_))

  private def optionalNullableString(obj: Map[String, J], key: String): Either[ArchiveError, Option[String]] =
    obj.get(key) match
      case None | Some(J.Null) => Right(None)
      case Some(value)         => asString(value, key).map(Some(_))

  private def field(obj: Map[String, J], key: String): Either[ArchiveError, J] =
    obj.get(key).toRight(ArchiveError.InvalidArchive(s"manifest missing '$key'"))

  private def asObj(value: J, label: String): Either[ArchiveError, Map[String, J]] =
    value match
      case J.Obj(fields) => Right(fields)
      case _             => Left(ArchiveError.InvalidArchive(s"$label must be a JSON object"))

  private def asArr(value: J, label: String): Either[ArchiveError, Vector[J]] =
    value match
      case J.Arr(values) => Right(values)
      case _             => Left(ArchiveError.InvalidArchive(s"$label must be a JSON array"))

  private def asString(value: J, label: String): Either[ArchiveError, String] =
    value match
      case J.Str(s) => Right(s)
      case _        => Left(ArchiveError.InvalidArchive(s"$label must be a JSON string"))

  private def asInt(value: J, label: String): Either[ArchiveError, Int] =
    value match
      case J.Num(n) if n.isValidInt && n == n.toInt.toDouble => Right(n.toInt)
      case _ => Left(ArchiveError.InvalidArchive(s"$label must be an integer"))

  private def asDouble(value: J, label: String): Either[ArchiveError, Double] =
    value match
      case J.Num(n) if n.isFinite => Right(n)
      case _ => Left(ArchiveError.InvalidArchive(s"$label must be a finite number"))

  private def asBool(value: J, label: String): Either[ArchiveError, Boolean] =
    value match
      case J.Bool(value) => Right(value)
      case _             => Left(ArchiveError.InvalidArchive(s"$label must be a boolean"))

  private def catchInvalid[A](label: String)(body: => A): Either[ArchiveError, A] =
    try Right(body)
    catch case NonFatal(e) => Left(ArchiveError.InvalidArchive(s"$label: ${e.getMessage}"))

  private def traverse[A, B](values: Iterable[A])(f: A => Either[ArchiveError, B]): Either[ArchiveError, Vector[B]] =
    val out = Vector.newBuilder[B]
    val it = values.iterator
    var error = Option.empty[ArchiveError]
    while it.hasNext && error.isEmpty do
      f(it.next()) match
        case Left(err) => error = Some(err)
        case Right(ok) => out += ok
    error.fold(Right(out.result()))(Left(_))

  private def obj(fields: (String, String)*): String =
    obj(fields.toVector)

  private def obj(fields: Vector[(String, String)]): String =
    fields.map { case (key, value) => s"${str(key)}:$value" }.mkString("{", ",", "}")

  private def arr(values: Iterable[String]): String =
    values.mkString("[", ",", "]")

  private def stringMapJson(values: Map[String, String]): String =
    obj(values.toVector.sortBy(_._1).map { case (key, value) => key -> str(value) })

  private def str(value: String): String =
    val out = new StringBuilder("\"")
    value.foreach {
      case '"'  => out.append("\\\"")
      case '\\' => out.append("\\\\")
      case '\b' => out.append("\\b")
      case '\f' => out.append("\\f")
      case '\n' => out.append("\\n")
      case '\r' => out.append("\\r")
      case '\t' => out.append("\\t")
      case c if c < ' ' => out.append(f"\\u${c.toInt}%04x")
      case c => out.append(c)
    }
    out.append('"').toString

  private enum J:
    case Obj(fields: Map[String, J])
    case Arr(values: Vector[J])
    case Str(value: String)
    case Num(value: Double)
    case Bool(value: Boolean)
    case Null

  private object Json:
    def parse(text: String): Either[ArchiveError, J] =
      val parser = Parser(text)
      parser.parseValue().flatMap { value =>
        parser.skipWhitespace()
        if parser.atEnd then Right(value)
        else Left(ArchiveError.InvalidArchive(s"unexpected trailing JSON input at offset ${parser.offset}"))
      }

  private final class Parser(input: String):
    private var index = 0

    def offset: Int = index
    def atEnd: Boolean = index >= input.length

    def skipWhitespace(): Unit =
      while !atEnd && input.charAt(index).isWhitespace do index += 1

    def parseValue(): Either[ArchiveError, J] =
      skipWhitespace()
      if atEnd then Left(ArchiveError.InvalidArchive("unexpected end of JSON input"))
      else
        input.charAt(index) match
          case '{' => parseObject()
          case '[' => parseArray()
          case '"' => parseString().map(J.Str.apply)
          case 't' => parseLiteral("true", J.Bool(true))
          case 'f' => parseLiteral("false", J.Bool(false))
          case 'n' => parseLiteral("null", J.Null)
          case c if c == '-' || c.isDigit => parseNumber()
          case c => Left(ArchiveError.InvalidArchive(s"unexpected JSON character '$c' at offset $index"))

    private def parseObject(): Either[ArchiveError, J] =
      index += 1
      skipWhitespace()
      val fields = mutable.LinkedHashMap.empty[String, J]
      if consume('}') then return Right(J.Obj(fields.toMap))

      var done = false
      while !done do
        skipWhitespace()
        val key =
          parseString() match
            case Left(err) => return Left(err)
            case Right(k)  => k
        skipWhitespace()
        if !consume(':') then return Left(ArchiveError.InvalidArchive(s"expected ':' after JSON object key at offset $index"))
        val value =
          parseValue() match
            case Left(err) => return Left(err)
            case Right(v)  => v
        fields.update(key, value)
        skipWhitespace()
        if consume('}') then done = true
        else if !consume(',') then return Left(ArchiveError.InvalidArchive(s"expected ',' or '}' at JSON offset $index"))

      Right(J.Obj(fields.toMap))

    private def parseArray(): Either[ArchiveError, J] =
      index += 1
      skipWhitespace()
      val values = Vector.newBuilder[J]
      if consume(']') then return Right(J.Arr(Vector.empty))

      var done = false
      while !done do
        parseValue() match
          case Left(err) => return Left(err)
          case Right(v)  => values += v
        skipWhitespace()
        if consume(']') then done = true
        else if !consume(',') then return Left(ArchiveError.InvalidArchive(s"expected ',' or ']' at JSON offset $index"))

      Right(J.Arr(values.result()))

    private def parseString(): Either[ArchiveError, String] =
      if !consume('"') then return Left(ArchiveError.InvalidArchive(s"expected JSON string at offset $index"))
      val out = new StringBuilder
      while !atEnd do
        val c = input.charAt(index)
        index += 1
        c match
          case '"' => return Right(out.toString)
          case '\\' =>
            if atEnd then return Left(ArchiveError.InvalidArchive("unterminated JSON escape sequence"))
            val esc = input.charAt(index)
            index += 1
            esc match
              case '"'  => out.append('"')
              case '\\' => out.append('\\')
              case '/'  => out.append('/')
              case 'b'  => out.append('\b')
              case 'f'  => out.append('\f')
              case 'n'  => out.append('\n')
              case 'r'  => out.append('\r')
              case 't'  => out.append('\t')
              case 'u' =>
                if index + 4 > input.length then return Left(ArchiveError.InvalidArchive("short JSON unicode escape"))
                val hex = input.substring(index, index + 4)
                if !hex.forall(c => c.isDigit || "abcdefABCDEF".contains(c)) then
                  return Left(ArchiveError.InvalidArchive(s"invalid JSON unicode escape '$hex'"))
                out.append(Integer.parseInt(hex, 16).toChar)
                index += 4
              case other => return Left(ArchiveError.InvalidArchive(s"invalid JSON escape '\\$other'"))
          case other => out.append(other)
      Left(ArchiveError.InvalidArchive("unterminated JSON string"))

    private def parseNumber(): Either[ArchiveError, J] =
      val start = index
      if !atEnd && input.charAt(index) == '-' then index += 1
      consumeDigits()
      if !atEnd && input.charAt(index) == '.' then
        index += 1
        consumeDigits()
      if !atEnd && (input.charAt(index) == 'e' || input.charAt(index) == 'E') then
        index += 1
        if !atEnd && (input.charAt(index) == '+' || input.charAt(index) == '-') then index += 1
        consumeDigits()
      val raw = input.substring(start, index)
      try Right(J.Num(raw.toDouble))
      catch case _: NumberFormatException => Left(ArchiveError.InvalidArchive(s"invalid JSON number '$raw'"))

    private def parseLiteral(lit: String, value: J): Either[ArchiveError, J] =
      if input.startsWith(lit, index) then
        index += lit.length
        Right(value)
      else Left(ArchiveError.InvalidArchive(s"expected JSON literal '$lit' at offset $index"))

    private def consume(c: Char): Boolean =
      if !atEnd && input.charAt(index) == c then
        index += 1
        true
      else false

    private def consumeDigits(): Unit =
      while !atEnd && input.charAt(index).isDigit do index += 1
