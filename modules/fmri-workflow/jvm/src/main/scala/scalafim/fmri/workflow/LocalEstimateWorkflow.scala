package scalafim.fmri.workflow

import java.net.URI
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal
import scalafim.dataset.DatasetSeriesReader
import scalafim.estimates.{EstimateError, EstimateUnit, ProductKind}
import scalafim.estimates.io.LocalEstimateStore
import scalafim.fmri.fit.estimates.FitEstimateProducer
import scalafim.fmri.group.{EstimateGroup, GroupEstimateAdmission}
import gale.backend.Backend

/** JVM boundary for the narrow first-level estimate lifecycle.  The store is
  * reopened by the caller before group admission, so an in-memory sink or its
  * mutable destination cannot stand in for the persisted scientific output.
  */
object LocalEstimateWorkflow:
  def produce(
      producer: FitEstimateProducer,
      reader: DatasetSeriesReader,
      store: LocalEstimateStore,
      maximumBlockCells: Int,
      output: PlannedEstimateOutput,
      selection: EstimateProductSelection,
      cancelled: () => Boolean = () => false
  )(using Backend): Either[EstimateError, SealedEstimateReference] =
    validate(output, store).flatMap(_ => validate(selection, producer.unit)).flatMap: _ =>
      store.newSink(producer.unit, maximumBlockCells)
      .flatMap(sink => producer.write(reader, sink, cancelled))
      .flatMap: pinned =>
        SealedEstimateReference.make(pinned, selection)
          .left.map(error => EstimateError.Invalid(error.message))

  /** Opened-from-storage group preparation.  `EstimateGroup.prepare` inspects
    * every manifest and the explicit geometry admission before payload reads.
    */
  def prepareGroup(
      store: LocalEstimateStore,
      references: Vector[SealedEstimateReference],
      admission: GroupEstimateAdmission,
      maximumBlockCells: Int
  ): Either[EstimateError, EstimateGroup] =
    if references.isEmpty then Left(EstimateError.Invalid("group handoff requires sealed estimates"))
    else
      val estimands = references.head.selection.estimands
      if references.exists(_.selection.estimands != estimands) then
        Left(EstimateError.Invalid("group handoff requires one explicit estimand selection"))
      else EstimateGroup.prepare(store, references.map(_.groupInput), estimands, admission, maximumBlockCells)

  /** The present JVM adapter writes exactly the canonical NIfTI estimate-set
    * representation. Refuse an unimplemented target before allocating a sink
    * or reading a response block.
    */
  private def validate(output: PlannedEstimateOutput, store: LocalEstimateStore): Either[EstimateError, Unit] =
    if output.format != OutputFormatId.CoreNifti then
      Left(EstimateError.Unsupported(s"workflow output format '${output.format.value}' is not implemented by LocalEstimateWorkflow"))
    else if output.layout != EstimateMapLayout.BundledMaps then
      Left(EstimateError.Unsupported(s"workflow output layout '${output.layout}' is not implemented by LocalEstimateWorkflow"))
    else localPath(output).flatMap: expected =>
      sameLocalTarget(expected, store.root).flatMap: same =>
        if same then Right(())
        else Left(EstimateError.Invalid(s"planned output '${output.location.value}' does not name local estimate store '${store.root.toUri}'"))

  private def sameLocalTarget(expected: Path, actual: Path): Either[EstimateError, Boolean] =
    try Right(expected == actual.toAbsolutePath.normalize || Files.isSameFile(expected, actual))
    catch case NonFatal(error) => Left(EstimateError.Invalid(s"cannot compare planned local output: ${error.getMessage}"))

  private def localPath(output: PlannedEstimateOutput): Either[EstimateError, Path] =
    try
      val uri = URI(output.location.value)
      if uri.getScheme != "file" || uri.getRawQuery != null || uri.getRawFragment != null then
        Left(EstimateError.Unsupported("LocalEstimateWorkflow requires a plain file URI output location"))
      else Right(Path.of(uri).toAbsolutePath.normalize)
    catch case NonFatal(error) => Left(EstimateError.Invalid(s"invalid local output location: ${error.getMessage}"))

  /** Validate selected scientific axes before opening a sink or reading the
    * response. The later group reopen repeats these checks against the stored
    * manifest, so a sealed reference remains a verified handoff rather than a
    * claim made by a caller-provided selection.
    */
  private def validate(selection: EstimateProductSelection, unit: EstimateUnit): Either[EstimateError, Unit] =
    val observation = unit.observations.find(_.id == selection.observation)
    val effect = unit.products.find(_.id == selection.effect)
    if observation.isEmpty then Left(EstimateError.Invalid("selected observation is not declared by the producer unit"))
    else if effect.isEmpty || effect.get.kind != ProductKind.Effect then Left(EstimateError.Invalid("selected effect product is not declared as an effect"))
    else if !effect.get.observations.contains(selection.observation) then Left(EstimateError.Invalid("selected effect does not contain the selected observation"))
    else if selection.estimands.exists(id => unit.catalog.entry(id).isEmpty || !effect.get.targets.estimands.contains(id)) then
      Left(EstimateError.Invalid("selected estimands are not declared by the selected effect"))
    else selection.uncertainty match
      case None => Right(())
      case Some(requested) =>
        val kind = requested match
          case scalafim.fmri.group.GroupMarginalUncertainty.Variance(_) => ProductKind.Variance
          case scalafim.fmri.group.GroupMarginalUncertainty.StandardError(_) => ProductKind.StandardError
        val product = unit.products.find(_.id == requested.product)
        val associated = unit.marginalUncertainty.exists(descriptor =>
          descriptor.product == requested.product && descriptor.effects == selection.effect)
        if product.isEmpty || product.get.kind != kind || !associated ||
            product.get.observations != effect.get.observations || product.get.targets != effect.get.targets then
          Left(EstimateError.Invalid("selected marginal uncertainty is not associated with the selected effect axes"))
        else Right(())
