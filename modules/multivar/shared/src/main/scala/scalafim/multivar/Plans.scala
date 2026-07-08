package scalafim.multivar

import scalafim.linalg.DoubleMatrix

opaque type MultivarPlanId = String

object MultivarPlanId:
  def apply(value: String): Either[MultivarError, MultivarPlanId] =
    Identifier.validate("multivar plan id", value)

  def unsafe(value: String): MultivarPlanId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: MultivarPlanId)
    inline def value: String = id

enum MultivarSourceRef:
  case InMemory
  case MvpaPatternSource(ref: String)
  case DatasetSelection(ref: String)
  case External(ref: String)

  def label: String =
    this match
      case InMemory               => "in-memory"
      case MvpaPatternSource(ref) => s"mvpa:$ref"
      case DatasetSelection(ref)  => s"dataset:$ref"
      case External(ref)          => s"external:$ref"

final case class SampleByFeatureInput(
    id: SpaceId,
    samples: Dimension,
    features: Dimension,
    source: MultivarSourceRef
):
  def sampleCount: Int =
    samples.value

  def featureCount: Int =
    features.value

object SampleByFeatureInput:
  def of(
      id: String,
      samples: Int,
      features: Int,
      source: MultivarSourceRef = MultivarSourceRef.InMemory
  ): Either[MultivarError, SampleByFeatureInput] =
    for
      spaceId <- SpaceId(id)
      sampleDim <- Dimension.from(samples, "sample-by-feature sample count")
      featureDim <- Dimension.from(features, "sample-by-feature feature count")
    yield SampleByFeatureInput(spaceId, sampleDim, featureDim, source)

final case class RoiPlan private (
    id: BlockId,
    columns: IndexSet,
    label: Option[String] = None
):
  require(columns.axis == IndexAxis.Column || columns.axis == IndexAxis.Feature, "ROI columns must use column or feature indices")
  require(label.forall(_.nonEmpty), "ROI label must be non-empty")

  def size: Int =
    columns.length

object RoiPlan:
  def of(id: String, columns: Iterable[Int], label: Option[String] = None): Either[MultivarError, RoiPlan] =
    for
      blockId <- BlockId(id)
      columnSet <- IndexSet.from(columns, IndexAxis.Feature)
      checkedLabel <- checkedLabel(label)
    yield new RoiPlan(blockId, columnSet, checkedLabel)

  private def checkedLabel(label: Option[String]): Either[MultivarError, Option[String]] =
    label match
      case None => Right(None)
      case Some(value) =>
        val trimmed = value.trim
        if trimmed.isEmpty then Left(MultivarError.InvalidId("ROI label", value, "must be non-empty"))
        else Right(Some(trimmed))

final case class RoiPlanSet private (name: String, rois: Vector[RoiPlan]):
  require(name.nonEmpty, "ROI plan name must be non-empty")
  require(rois.nonEmpty, "ROI plan must contain at least one ROI")

  def size: Int =
    rois.length

  def toBlockPartition(features: Dimension): Either[MultivarError, BlockPartition] =
    BlockPartition.from(
      features,
      rois.map(roi => BlockSpec(roi.id, roi.columns))
    )

object RoiPlanSet:
  def of(name: String, rois: Iterable[RoiPlan], featureCount: Int): Either[MultivarError, RoiPlanSet] =
    val trimmed = name.trim
    val values = rois.toVector
    if trimmed.isEmpty then Left(MultivarError.InvalidRowGeometry("ROI plan name must be non-empty"))
    else if values.isEmpty then Left(MultivarError.InvalidRowGeometry("ROI plan must contain at least one ROI"))
    else
      val seen = scala.collection.mutable.HashSet.empty[String]
      var i = 0
      var error = Option.empty[MultivarError]
      while i < values.length && error.isEmpty do
        val roi = values(i)
        if seen.contains(roi.id.value) then error = Some(MultivarError.DuplicateBlock(roi.id))
        else
          seen += roi.id.value
          roi.columns.requireWithin(featureCount) match
            case Left(value) => error = Some(value)
            case Right(_)    =>
        i += 1
      error match
        case Some(value) => Left(value)
        case None        => Right(new RoiPlanSet(trimmed, values))

enum MultivarExecutionMode:
  case Local
  case RoiParallel
  case SparkReady

enum MultivarPartitionAxis:
  case WholeInput
  case Roi
  case Block

final case class MultivarExecutionPlan(
    mode: MultivarExecutionMode,
    partitionAxis: MultivarPartitionAxis,
    broadcastSmallFits: Boolean
):
  def isDistributedIntent: Boolean =
    mode != MultivarExecutionMode.Local

object MultivarExecutionPlan:
  val local: MultivarExecutionPlan =
    MultivarExecutionPlan(MultivarExecutionMode.Local, MultivarPartitionAxis.WholeInput, broadcastSmallFits = false)

  val roiLocal: MultivarExecutionPlan =
    MultivarExecutionPlan(MultivarExecutionMode.RoiParallel, MultivarPartitionAxis.Roi, broadcastSmallFits = false)

  val sparkReadyRoi: MultivarExecutionPlan =
    MultivarExecutionPlan(MultivarExecutionMode.SparkReady, MultivarPartitionAxis.Roi, broadcastSmallFits = true)

enum MultivarEstimator:
  case Pca(components: ComponentCount, preprocessing: PreprocessSpec = PreprocessSpec.Center)
  case Svd(components: ComponentCount, preprocessing: PreprocessSpec = PreprocessSpec.Pass)
  case Nystrom(
      components: ComponentCount,
      landmarks: Vector[Int],
      kernel: KernelSpec = KernelSpec("linear"),
      preprocessing: PreprocessSpec = PreprocessSpec.Pass,
      method: NystromMethod = NystromMethod.Standard
  )

  def componentCount: ComponentCount =
    this match
      case Pca(value, _)              => value
      case Svd(value, _)              => value
      case Nystrom(value, _, _, _, _) => value

  def kind: FitArtifactKind =
    this match
      case Pca(_, _)              => FitArtifactKind.Pca
      case Svd(_, _)              => FitArtifactKind.Svd
      case Nystrom(_, _, _, _, _) => FitArtifactKind.Nystrom

final case class MultivarPlan private (
    id: MultivarPlanId,
    input: SampleByFeatureInput,
    roiPlan: RoiPlanSet,
    estimator: MultivarEstimator,
    execution: MultivarExecutionPlan
):
  def roiCount: Int =
    roiPlan.size

  def inspectableSummary: String =
    s"${id.value}:${estimator.kind.label}:${input.sampleCount}x${input.featureCount}:${roiCount} roi(s):${execution.mode}"

object MultivarPlan:
  def of(
      id: String,
      input: SampleByFeatureInput,
      roiPlan: RoiPlanSet,
      estimator: MultivarEstimator,
      execution: MultivarExecutionPlan = MultivarExecutionPlan.local
  ): Either[MultivarError, MultivarPlan] =
    for
      planId <- MultivarPlanId(id)
      _ <- validateEstimator(estimator, input.sampleCount, roiPlan)
    yield new MultivarPlan(planId, input, roiPlan, estimator, execution)

  private def validateEstimator(
      estimator: MultivarEstimator,
      sampleCount: Int,
      roiPlan: RoiPlanSet
  ): Either[MultivarError, Unit] =
    estimator match
      case MultivarEstimator.Nystrom(components, landmarks, _, _, method) =>
        val landmarkSet = LandmarkSet.from(landmarks, sampleCount)
        landmarkSet.flatMap { checked =>
          if components.value > checked.length then Left(MultivarError.InvalidComponentRequest(components.value, checked.length))
          else
            method match
              case NystromMethod.Standard => Right(())
              case NystromMethod.DoubleNystrom(intermediateRank) =>
                if intermediateRank.value > checked.length then
                  Left(MultivarError.InvalidComponentRequest(intermediateRank.value, checked.length))
                else Right(())
        }
      case _ =>
        val minRoiSize = roiPlan.rois.map(_.size).min
        if estimator.componentCount.value > Math.min(sampleCount, minRoiSize) then
          Left(MultivarError.InvalidComponentRequest(estimator.componentCount.value, Math.min(sampleCount, minRoiSize)))
        else Right(())

enum FitArtifactKind:
  case Pca
  case Svd
  case Nystrom

  def label: String =
    this match
      case Pca     => "pca"
      case Svd     => "svd"
      case Nystrom => "nystrom"

final case class FitArtifactShape(
    planId: MultivarPlanId,
    roiId: BlockId,
    kind: FitArtifactKind,
    samples: Int,
    features: Int,
    components: Int,
    executionMode: MultivarExecutionMode,
    source: MultivarSourceRef
):
  require(samples > 0, "artifact samples must be positive")
  require(features > 0, "artifact features must be positive")
  require(components >= 0, "artifact components must be non-negative")

enum FitArtifact:
  case BiProjectionArtifact(artifactShape: FitArtifactShape, projection: BiProjection)
  case KernelArtifact(artifactShape: FitArtifactShape, fit: NystromFit)

  def shape: FitArtifactShape =
    this match
      case BiProjectionArtifact(value, _) => value
      case KernelArtifact(value, _)       => value

final case class LocalMultivarResult(plan: MultivarPlan, artifacts: Vector[FitArtifact]):
  require(artifacts.nonEmpty, "local multivar result must contain at least one artifact")

object LocalMultivarExecutor:
  def run(plan: MultivarPlan, input: MatrixView): Either[MultivarError, LocalMultivarResult] =
    if input.rows != plan.input.sampleCount then
      Left(MultivarError.MatrixShapeMismatch(s"plan expected ${plan.input.sampleCount} samples, got ${input.rows}"))
    else if input.cols != plan.input.featureCount then
      Left(MultivarError.MatrixShapeMismatch(s"plan expected ${plan.input.featureCount} features, got ${input.cols}"))
    else
      PlanOps.traverse(plan.roiPlan.rois) { roi =>
        for
          selected <- input.selectColumns(roi.columns)
          artifact <- fitRoi(plan, roi, selected)
        yield artifact
      }.map(artifacts => LocalMultivarResult(plan, artifacts))

  private def fitRoi(plan: MultivarPlan, roi: RoiPlan, input: MatrixView): Either[MultivarError, FitArtifact] =
    plan.estimator match
      case MultivarEstimator.Pca(components, preprocessing) =>
        Pca.fit(input, components, preprocessing).map { fit =>
          FitArtifact.BiProjectionArtifact(shape(plan, roi, FitArtifactKind.Pca, input, fit.projection.map.codomain.size), fit.projection)
        }
      case MultivarEstimator.Svd(components, preprocessing) =>
        Svd.fit(input, components, preprocessing).map { fit =>
          FitArtifact.BiProjectionArtifact(shape(plan, roi, FitArtifactKind.Svd, input, fit.projection.map.codomain.size), fit.projection)
        }
      case MultivarEstimator.Nystrom(components, landmarks, kernelSpec, preprocessing, method) =>
        for
          kernel <- PlanOps.kernelFromSpec(kernelSpec)
          fit <- Nystrom.fit(input, components, landmarks, kernel, preprocessing, method)
        yield FitArtifact.KernelArtifact(shape(plan, roi, FitArtifactKind.Nystrom, input, fit.eigen.components), fit)

  private def shape(
      plan: MultivarPlan,
      roi: RoiPlan,
      kind: FitArtifactKind,
      input: MatrixView,
      components: Int
  ): FitArtifactShape =
    FitArtifactShape(
      planId = plan.id,
      roiId = roi.id,
      kind = kind,
      samples = input.rows,
      features = input.cols,
      components = components,
      executionMode = plan.execution.mode,
      source = plan.input.source
    )

private[multivar] object PlanOps:
  def traverse[A, B](values: Vector[A])(f: A => Either[MultivarError, B]): Either[MultivarError, Vector[B]] =
    val out = Vector.newBuilder[B]
    var i = 0
    var error = Option.empty[MultivarError]
    while i < values.length && error.isEmpty do
      f(values(i)) match
        case Left(value)  => error = Some(value)
        case Right(value) => out += value
      i += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(out.result())

  def kernelFromSpec(spec: KernelSpec): Either[MultivarError, Kernel] =
    spec.name match
      case "linear" =>
        Right(LinearKernel())
      case "rbf" =>
        spec.parameters.get("gamma") match
          case Some(gamma) if gamma.isFinite && gamma > 0.0 => Right(RbfKernel(gamma))
          case _ => Left(MultivarError.InvalidKernelFit("RBF kernel spec requires a positive finite gamma"))
      case other =>
        Left(MultivarError.InvalidKernelFit(s"unsupported kernel spec '$other'"))
