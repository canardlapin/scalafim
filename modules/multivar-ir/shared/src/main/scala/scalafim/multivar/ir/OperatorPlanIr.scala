package scalafim.multivar.ir

import scalafim.multivar.*

enum OperatorPlanSourceIr:
  case InMemory
  case MvpaPatternSource(ref: String)
  case DatasetSelection(ref: String)
  case External(ref: String)

final case class OperatorPlanInputIr(
    id: String,
    samples: Int,
    features: Int,
    source: OperatorPlanSourceIr
)

final case class OperatorPlanRoiIr(
    id: String,
    columns: Vector[Int],
    label: Option[String]
)

enum OperatorPlanExecutionModeIr:
  case Local
  case RoiParallel
  case DistributedReady

enum OperatorPlanPartitionAxisIr:
  case WholeInput
  case Roi
  case Block

final case class OperatorPlanExecutionIr(
    mode: OperatorPlanExecutionModeIr,
    partitionAxis: OperatorPlanPartitionAxisIr,
    broadcastSmallFits: Boolean
)

/** Binds a lifecycle partition to the semantic programs realized for it. */
final case class OperatorPlanBindingIr(
    roiId: String,
    programIds: Vector[String]
)

/** Portable, realized lifecycle plan for typed operator programs.
  *
  * Unlike the compatibility `MultivarPlan` request, this representation does
  * not serialize a method enum or solver backend. It records the input and
  * partition lifecycle, then binds every partition to inspectable semantic
  * programs in `programDocument`.
  */
final case class OperatorPlanIr(
    schema: String,
    id: String,
    input: OperatorPlanInputIr,
    rois: Vector[OperatorPlanRoiIr],
    execution: OperatorPlanExecutionIr,
    bindings: Vector[OperatorPlanBindingIr],
    programDocument: OperatorProgramDocumentIr
)

object OperatorPlanIr:
  val schemaV01: String = "scalafim-operator-plan-ir/0.1"

  def from(
      plan: MultivarPlan,
      programDocument: OperatorProgramDocumentIr,
      bindings: Vector[OperatorPlanBindingIr]
  ): Either[IrError, OperatorPlanIr] =
    val input = OperatorPlanInputIr(
      plan.input.id.value,
      plan.input.sampleCount,
      plan.input.featureCount,
      source(plan.input.source)
    )
    val rois = plan.roiPlan.rois.map: roi =>
      OperatorPlanRoiIr(roi.id.value, roi.columns.indices, roi.label)
    val execution = OperatorPlanExecutionIr(
      executionMode(plan.execution.mode),
      partitionAxis(plan.execution.partitionAxis),
      plan.execution.broadcastSmallFits
    )
    OperatorPlanIrValidator.validate(
      OperatorPlanIr(schemaV01, plan.id.value, input, rois, execution, bindings, programDocument)
    )

  private def source(value: MultivarSourceRef): OperatorPlanSourceIr =
    value match
      case MultivarSourceRef.InMemory => OperatorPlanSourceIr.InMemory
      case MultivarSourceRef.MvpaPatternSource(ref) => OperatorPlanSourceIr.MvpaPatternSource(ref)
      case MultivarSourceRef.DatasetSelection(ref) => OperatorPlanSourceIr.DatasetSelection(ref)
      case MultivarSourceRef.External(ref) => OperatorPlanSourceIr.External(ref)

  private def executionMode(value: MultivarExecutionMode): OperatorPlanExecutionModeIr =
    value match
      case MultivarExecutionMode.Local => OperatorPlanExecutionModeIr.Local
      case MultivarExecutionMode.RoiParallel => OperatorPlanExecutionModeIr.RoiParallel
      case MultivarExecutionMode.DistributedReady => OperatorPlanExecutionModeIr.DistributedReady

  private def partitionAxis(value: MultivarPartitionAxis): OperatorPlanPartitionAxisIr =
    value match
      case MultivarPartitionAxis.WholeInput => OperatorPlanPartitionAxisIr.WholeInput
      case MultivarPartitionAxis.Roi => OperatorPlanPartitionAxisIr.Roi
      case MultivarPartitionAxis.Block => OperatorPlanPartitionAxisIr.Block

object OperatorPlanIrCodec:
  def encode(plan: OperatorPlanIr): String =
    IrJson.render(OperatorPlanIrEncoder.plan(plan))

  def decode(text: String): Either[IrError, OperatorPlanIr] =
    IrJson.parse(text).flatMap(OperatorPlanIrDecoder.plan).flatMap(OperatorPlanIrValidator.validate)

object OperatorPlanIrValidator:
  def validate(plan: OperatorPlanIr): Either[IrError, OperatorPlanIr] =
    for
      _ <- requireValue(
        plan.schema == OperatorPlanIr.schemaV01,
        RejectionCategory.SchemaVersionMismatch,
        "$.schema",
        s"expected ${OperatorPlanIr.schemaV01}, got ${plan.schema}"
      )
      _ <- requireValue(plan.id.trim.nonEmpty, RejectionCategory.Malformed, "$.id", "plan id must be non-empty")
      _ <- validateInput(plan.input)
      roiIds <- unique(plan.rois.map(_.id), "$.rois")
      _ <- requireValue(plan.rois.nonEmpty, RejectionCategory.Malformed, "$.rois", "plan requires at least one ROI")
      _ <- plan.rois.zipWithIndex.foldLeft[Either[IrError, Unit]](Right(())): (result, entry) =>
        result.flatMap(_ => validateRoi(entry._1, entry._2, plan.input.features))
      bindingIds <- unique(plan.bindings.map(_.roiId), "$.bindings")
      _ <- OperatorProgramIrValidator.validate(plan.programDocument)
      programIds <- unique(plan.programDocument.programs.map(_.id), "$.program_document.programs")
      _ <- requireValue(
        plan.programDocument.programs.nonEmpty && plan.programDocument.programs.forall(_.id.trim.nonEmpty),
        RejectionCategory.Malformed,
        "$.program_document.programs",
        "realized plan requires at least one semantic program with a non-empty id"
      )
      _ <- requireValue(
        roiIds == bindingIds,
        RejectionCategory.Malformed,
        "$.bindings",
        "every ROI requires exactly one program binding"
      )
      _ <- validateBindings(plan.bindings, programIds)
    yield plan

  private def validateInput(value: OperatorPlanInputIr): Either[IrError, Unit] =
    val sourceValid =
      value.source match
        case OperatorPlanSourceIr.InMemory => true
        case OperatorPlanSourceIr.MvpaPatternSource(ref) => ref.trim.nonEmpty
        case OperatorPlanSourceIr.DatasetSelection(ref) => ref.trim.nonEmpty
        case OperatorPlanSourceIr.External(ref) => ref.trim.nonEmpty
    requireValue(
      value.id.trim.nonEmpty && value.samples > 0 && value.features > 0 && sourceValid,
      RejectionCategory.Malformed,
      "$.input",
      "input id, dimensions, and source reference must be valid"
    )

  private def validateRoi(value: OperatorPlanRoiIr, index: Int, featureCount: Int): Either[IrError, Unit] =
    requireValue(
      value.id.trim.nonEmpty && value.columns.nonEmpty && value.columns.distinct.length == value.columns.length &&
        value.columns.forall(column => column >= 0 && column < featureCount) && value.label.forall(_.trim.nonEmpty),
      RejectionCategory.Malformed,
      s"$$.rois[$index]",
      "ROI id, columns, and optional label must be valid"
    )

  private def validateBindings(
      values: Vector[OperatorPlanBindingIr],
      programIds: Set[String]
  ): Either[IrError, Unit] =
    val referenced = values.flatMap(_.programIds)
    for
      _ <- values.zipWithIndex.foldLeft[Either[IrError, Unit]](Right(())): (result, entry) =>
        result.flatMap: _ =>
          val binding = entry._1
          requireValue(
            binding.programIds.nonEmpty && binding.programIds.distinct.length == binding.programIds.length &&
              binding.programIds.forall(programIds.contains),
            RejectionCategory.Malformed,
            s"$$.bindings[${entry._2}]",
            "binding must reference one or more distinct known programs"
          )
      _ <- requireValue(
        referenced.toSet == programIds,
        RejectionCategory.Malformed,
        "$.bindings",
        "every semantic program must be bound to a lifecycle partition"
      )
    yield ()

  private def unique(values: Vector[String], path: String): Either[IrError, Set[String]] =
    requireValue(
      values.distinct.length == values.length,
      RejectionCategory.Malformed,
      path,
      "identifiers must be unique"
    ).map(_ => values.toSet)

  private def requireValue(
      condition: Boolean,
      category: RejectionCategory,
      path: String,
      detail: String
  ): Either[IrError, Unit] =
    if condition then Right(()) else Left(IrError(category, path, detail))

private object OperatorPlanIrEncoder:
  import IrJson.*

  def plan(value: OperatorPlanIr): IrJson =
    obj(
      "schema" -> Str(value.schema),
      "id" -> Str(value.id),
      "input" -> input(value.input),
      "rois" -> Arr(value.rois.map(roi)),
      "execution" -> execution(value.execution),
      "bindings" -> Arr(value.bindings.map(binding)),
      "program_document" -> Str(OperatorProgramDocumentIrCodec.encode(value.programDocument))
    )

  private def input(value: OperatorPlanInputIr): IrJson =
    obj(
      "id" -> Str(value.id),
      "samples" -> Num(value.samples),
      "features" -> Num(value.features),
      "source" -> source(value.source)
    )

  private def source(value: OperatorPlanSourceIr): IrJson =
    value match
      case OperatorPlanSourceIr.InMemory => obj("kind" -> Str("in_memory"))
      case OperatorPlanSourceIr.MvpaPatternSource(ref) => obj("kind" -> Str("mvpa_pattern_source"), "ref" -> Str(ref))
      case OperatorPlanSourceIr.DatasetSelection(ref) => obj("kind" -> Str("dataset_selection"), "ref" -> Str(ref))
      case OperatorPlanSourceIr.External(ref) => obj("kind" -> Str("external"), "ref" -> Str(ref))

  private def roi(value: OperatorPlanRoiIr): IrJson =
    obj(
      "id" -> Str(value.id),
      "columns" -> Arr(value.columns.map(column => Num(column))),
      "label" -> value.label.fold[IrJson](Null)(Str.apply)
    )

  private def execution(value: OperatorPlanExecutionIr): IrJson =
    obj(
      "mode" -> Str(executionMode(value.mode)),
      "partition_axis" -> Str(partitionAxis(value.partitionAxis)),
      "broadcast_small_fits" -> Bool(value.broadcastSmallFits)
    )

  private def binding(value: OperatorPlanBindingIr): IrJson =
    obj(
      "roi_id" -> Str(value.roiId),
      "program_ids" -> Arr(value.programIds.map(Str.apply))
    )

  private def executionMode(value: OperatorPlanExecutionModeIr): String =
    value match
      case OperatorPlanExecutionModeIr.Local => "local"
      case OperatorPlanExecutionModeIr.RoiParallel => "roi_parallel"
      case OperatorPlanExecutionModeIr.DistributedReady => "distributed_ready"

  private def partitionAxis(value: OperatorPlanPartitionAxisIr): String =
    value match
      case OperatorPlanPartitionAxisIr.WholeInput => "whole_input"
      case OperatorPlanPartitionAxisIr.Roi => "roi"
      case OperatorPlanPartitionAxisIr.Block => "block"

  private def obj(fields: (String, IrJson)*): IrJson =
    Obj(fields.toVector)

private object OperatorPlanIrDecoder:
  import IrJson.*

  def plan(value: IrJson): Either[IrError, OperatorPlanIr] =
    for
      current <- fields(
        value,
        "$",
        Set("schema", "id", "input", "rois", "execution", "bindings", "program_document")
      )
      schema <- required(current, "schema", "$", string(_, "$.schema"))
      id <- required(current, "id", "$", string(_, "$.id"))
      inputValue <- required(current, "input", "$", input(_, "$.input"))
      rois <- required(current, "rois", "$", vector(_, "$.rois", roi))
      executionValue <- required(current, "execution", "$", execution(_, "$.execution"))
      bindings <- required(current, "bindings", "$", vector(_, "$.bindings", binding))
      documentText <- required(current, "program_document", "$", string(_, "$.program_document"))
      document <- OperatorProgramDocumentIrCodec.decode(documentText).left.map(error =>
        error.copy(path = s"$$.program_document${error.path.drop(1)}")
      )
    yield OperatorPlanIr(schema, id, inputValue, rois, executionValue, bindings, document)

  private def input(value: IrJson, path: String): Either[IrError, OperatorPlanInputIr] =
    for
      current <- fields(value, path, Set("id", "samples", "features", "source"))
      id <- required(current, "id", path, string(_, s"$path.id"))
      samples <- required(current, "samples", path, integer(_, s"$path.samples"))
      features <- required(current, "features", path, integer(_, s"$path.features"))
      sourceValue <- required(current, "source", path, source(_, s"$path.source"))
    yield OperatorPlanInputIr(id, samples, features, sourceValue)

  private def source(value: IrJson, path: String): Either[IrError, OperatorPlanSourceIr] =
    for
      current <- fields(value, path, Set("kind", "ref"))
      kind <- required(current, "kind", path, string(_, s"$path.kind"))
      result <- kind match
        case "in_memory" =>
          exact(current, path, Set("kind")).map(_ => OperatorPlanSourceIr.InMemory)
        case "mvpa_pattern_source" =>
          required(current, "ref", path, string(_, s"$path.ref")).map(OperatorPlanSourceIr.MvpaPatternSource.apply)
        case "dataset_selection" =>
          required(current, "ref", path, string(_, s"$path.ref")).map(OperatorPlanSourceIr.DatasetSelection.apply)
        case "external" =>
          required(current, "ref", path, string(_, s"$path.ref")).map(OperatorPlanSourceIr.External.apply)
        case other => malformed(s"$path.kind", s"unknown source '$other'")
    yield result

  private def roi(value: IrJson, path: String): Either[IrError, OperatorPlanRoiIr] =
    for
      current <- fields(value, path, Set("id", "columns", "label"))
      id <- required(current, "id", path, string(_, s"$path.id"))
      columns <- required(current, "columns", path, vector(_, s"$path.columns", integer))
      label <- required(current, "label", path, optionalString(_, s"$path.label"))
    yield OperatorPlanRoiIr(id, columns, label)

  private def execution(value: IrJson, path: String): Either[IrError, OperatorPlanExecutionIr] =
    for
      current <- fields(value, path, Set("mode", "partition_axis", "broadcast_small_fits"))
      mode <- required(current, "mode", path, string(_, s"$path.mode").flatMap(executionMode(_, s"$path.mode")))
      axis <- required(current, "partition_axis", path, string(_, s"$path.partition_axis").flatMap(partitionAxis(_, s"$path.partition_axis")))
      broadcast <- required(current, "broadcast_small_fits", path, boolean(_, s"$path.broadcast_small_fits"))
    yield OperatorPlanExecutionIr(mode, axis, broadcast)

  private def binding(value: IrJson, path: String): Either[IrError, OperatorPlanBindingIr] =
    for
      current <- fields(value, path, Set("roi_id", "program_ids"))
      roiId <- required(current, "roi_id", path, string(_, s"$path.roi_id"))
      programIds <- required(current, "program_ids", path, vector(_, s"$path.program_ids", string))
    yield OperatorPlanBindingIr(roiId, programIds)

  private def executionMode(value: String, path: String): Either[IrError, OperatorPlanExecutionModeIr] =
    value match
      case "local" => Right(OperatorPlanExecutionModeIr.Local)
      case "roi_parallel" => Right(OperatorPlanExecutionModeIr.RoiParallel)
      case "distributed_ready" => Right(OperatorPlanExecutionModeIr.DistributedReady)
      case other => malformed(path, s"unknown execution mode '$other'")

  private def partitionAxis(value: String, path: String): Either[IrError, OperatorPlanPartitionAxisIr] =
    value match
      case "whole_input" => Right(OperatorPlanPartitionAxisIr.WholeInput)
      case "roi" => Right(OperatorPlanPartitionAxisIr.Roi)
      case "block" => Right(OperatorPlanPartitionAxisIr.Block)
      case other => malformed(path, s"unknown partition axis '$other'")

  private def fields(value: IrJson, path: String, allowed: Set[String]): Either[IrError, Map[String, IrJson]] =
    value match
      case Obj(values) =>
        values.find((key, _) => !allowed.contains(key)) match
          case Some((key, _)) => Left(IrError(RejectionCategory.UnknownField, s"$path.$key", "unknown field"))
          case None => Right(values.toMap)
      case _ => malformed(path, "expected object")

  private def exact(current: Map[String, IrJson], path: String, allowed: Set[String]): Either[IrError, Unit] =
    current.keys.find(key => !allowed.contains(key)) match
      case Some(key) => Left(IrError(RejectionCategory.UnknownField, s"$path.$key", "unknown field"))
      case None => Right(())

  private def required[A](
      current: Map[String, IrJson],
      key: String,
      path: String,
      decode: IrJson => Either[IrError, A]
  ): Either[IrError, A] =
    current.get(key).toRight(IrError(RejectionCategory.Malformed, s"$path.$key", "missing required field")).flatMap(decode)

  private def vector[A](
      value: IrJson,
      path: String,
      decode: (IrJson, String) => Either[IrError, A]
  ): Either[IrError, Vector[A]] =
    value match
      case Arr(values) =>
        values.zipWithIndex.foldLeft[Either[IrError, Vector[A]]](Right(Vector.empty)): (result, entry) =>
          result.flatMap(current => decode(entry._1, s"$path[${entry._2}]").map(current :+ _))
      case _ => malformed(path, "expected array")

  private def string(value: IrJson, path: String): Either[IrError, String] =
    value match
      case Str(current) => Right(current)
      case _ => malformed(path, "expected string")

  private def optionalString(value: IrJson, path: String): Either[IrError, Option[String]] =
    value match
      case Null => Right(None)
      case Str(current) => Right(Some(current))
      case _ => malformed(path, "expected string or null")

  private def boolean(value: IrJson, path: String): Either[IrError, Boolean] =
    value match
      case Bool(current) => Right(current)
      case _ => malformed(path, "expected boolean")

  private def integer(value: IrJson, path: String): Either[IrError, Int] =
    value match
      case Num(current) if current == Math.rint(current) && current >= Int.MinValue && current <= Int.MaxValue =>
        Right(current.toInt)
      case _ => malformed(path, "expected integer")

  private def malformed[A](path: String, detail: String): Either[IrError, A] =
    Left(IrError(RejectionCategory.Malformed, path, detail))
