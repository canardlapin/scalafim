package scalafim.multivar
package family.paired

import scalafim.multivar.core.*
import scalafim.multivar.capability.*

import gale.linalg.DMat

opaque type ComponentScalingId = String

object ComponentScalingId:
  def apply(value: String): Either[MultivarError, ComponentScalingId] =
    Identifier.validate("component scaling id", value)

  def unsafe(value: String): ComponentScalingId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: ComponentScalingId)
    inline def value: String = id

final case class ComponentScaling private (
    values: DMat,
    valueIdentity: ValueIdentity
)

object ComponentScaling:
  def from(values: DMat, id: ComponentScalingId): Either[MultivarError, ComponentScaling] =
    MatrixOps
      .checkFinite("component scaling", values)
      .map(_ => ComponentScaling(values, ValueIdentity.source(ValueId.unsafe(id.value))))

  def identity(
      size: Int,
      id: ComponentScalingId = ComponentScalingId.unsafe("identity-component-scaling")
  ): Either[MultivarError, ComponentScaling] =
    if size <= 0 then Left(MultivarError.InvalidDimension("component scaling", size))
    else from(DMat.eye(size), id)

enum PairedTransferEstimand:
  case Plsc
  case Cca

final case class TransferOrientation(source: MvSpace, target: MvSpace):
  require(source != target, "transfer orientation must connect distinct spaces")

final case class TransferProvenance(
    estimand: PairedTransferEstimand,
    orientation: TransferOrientation,
    sourceFrame: ValueIdentity,
    targetDecoder: ValueIdentity,
    scaling: ValueIdentity,
    semantic: SemanticProvenance
)

final case class TransferResult private[multivar] (
    values: DMat,
    provenance: TransferProvenance
)

final class PairedTransfer private (
    val estimand: PairedTransferEstimand,
    val source: FittedBidirectionalTransform,
    val target: FittedBidirectionalTransform,
    val scaling: ComponentScaling,
    val orientation: TransferOrientation,
    val provenance: TransferProvenance
):
  def apply(
      input: MatrixView,
      coordinate: ReconstructionCoordinate = ReconstructionCoordinate.Original,
      targetFeatures: Option[IndexSet] = None
  ): Either[MultivarError, TransferResult] =
    for
      sourceScores <- source.analysis.project(input)
      scaled = GaleNumerics.multiply(sourceScores, scaling.values)
      reconstructed <- target.synthesizeTransfer(scaled, coordinate, targetFeatures)
    yield TransferResult(reconstructed.values, provenance)

object PairedTransfer:
  def forPlsc(
      fit: PlscFit,
      source: FittedBidirectionalTransform,
      target: FittedBidirectionalTransform,
      scaling: ComponentScaling
  ): Either[MultivarError, PairedTransfer] =
    requireFitTransforms(fit.sourceTransform, fit.targetTransform, source, target)
      .flatMap(_ => from(PairedTransferEstimand.Plsc, source, target, scaling))

  def forCca(
      fit: CcaFit,
      source: FittedBidirectionalTransform,
      target: FittedBidirectionalTransform,
      scaling: ComponentScaling
  ): Either[MultivarError, PairedTransfer] =
    requireFitTransforms(fit.sourceTransform, fit.targetTransform, source, target)
      .flatMap(_ => from(PairedTransferEstimand.Cca, source, target, scaling))

  private[multivar] def from(
      estimand: PairedTransferEstimand,
      source: FittedBidirectionalTransform,
      target: FittedBidirectionalTransform,
      scaling: ComponentScaling
  ): Either[MultivarError, PairedTransfer] =
    val sourceComponents = source.analysis.componentSpace.descriptor.size
    val targetComponents = target.analysis.componentSpace.descriptor.size
    if source.analysis.featureSpace.descriptor == target.analysis.featureSpace.descriptor then
      Left(MultivarError.InvalidMap("paired transfer source and target domains must differ"))
    else if scaling.values.rows != sourceComponents || scaling.values.cols != targetComponents then
      Left(
        MultivarError.MatrixShapeMismatch(
          s"component scaling is ${scaling.values.rows}x${scaling.values.cols}, expected ${sourceComponents}x$targetComponents"
        )
      )
    else
      val orientation = TransferOrientation(
        source.analysis.featureSpace.descriptor,
        target.analysis.featureSpace.descriptor
      )
      val semantic = (source.provenance ++ target.provenance).append(
        SemanticProvenanceEvent.Derived(
          "paired-transfer",
          Vector(source.analysis.frame.weights.valueIdentity, target.decoder.valueIdentity, scaling.valueIdentity)
        )
      )
      val record = TransferProvenance(
        estimand,
        orientation,
        source.analysis.frame.weights.valueIdentity,
        target.decoder.valueIdentity,
        scaling.valueIdentity,
        semantic
      )
      Right(new PairedTransfer(estimand, source, target, scaling, orientation, record))

  private def requireFitTransforms(
      expectedSource: FittedFrameTransform,
      expectedTarget: FittedFrameTransform,
      source: FittedBidirectionalTransform,
      target: FittedBidirectionalTransform
  ): Either[MultivarError, Unit] =
    if !(source.analysis eq expectedSource) || !(target.analysis eq expectedTarget)
    then Left(MultivarError.DecoderUnavailable("paired transfer synthesis does not belong to the supplied paired fit"))
    else Right(())
