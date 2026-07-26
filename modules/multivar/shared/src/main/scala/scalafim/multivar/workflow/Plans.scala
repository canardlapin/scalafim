package scalafim.multivar
package workflow

import scalafim.multivar.core.*
import scalafim.multivar.optimization.*
import scalafim.multivar.capability.*
import scalafim.multivar.family.spectral.*
import scalafim.multivar.family.paired.*
import scalafim.multivar.family.cpca.*

import scalafim.multivar.family.kernel.*


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
  case DistributedReady

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

  val distributedReadyRoi: MultivarExecutionPlan =
    MultivarExecutionPlan(MultivarExecutionMode.DistributedReady, MultivarPartitionAxis.Roi, broadcastSmallFits = true)

  val sparkReadyRoi: MultivarExecutionPlan =
    distributedReadyRoi

enum MultivarEstimator:
  case Pca(components: ComponentCount, preprocessing: PreprocessSpec = PreprocessSpec.Center)
  case Svd(components: ComponentCount, preprocessing: PreprocessSpec = PreprocessSpec.Pass)
  case Gpca(
      components: ComponentCount,
      preprocessing: PreprocessSpec = PreprocessSpec.Center,
      rowMetric: Option[MetricSpec] = None,
      columnMetric: Option[MetricSpec] = None,
      backend: GpcaBackend = GpcaBackend.Auto,
      storagePolicy: StoragePolicy = StoragePolicy.AllowDense
  )
  case Cpca(spec: CpcaEstimatorSpec)
  case Nystrom(
      components: ComponentCount,
      landmarks: Vector[Int],
      kernel: KernelSpec = KernelSpec("linear"),
      preprocessing: PreprocessSpec = PreprocessSpec.Pass,
      method: NystromMethod = NystromMethod.Standard
  )

  /** Concrete requested component count, when the estimator states one.
    *
    * CPCA requests are per-block and may be "full" (rank-determined only at fit
    * time) or structurally invalid; those honestly report `None` instead of a
    * fabricated count. `componentRequestSummary` renders the full picture.
    */
  def componentCount: Option[ComponentCount] =
    this match
      case Pca(value, _)                => Some(value)
      case Svd(value, _)                => Some(value)
      case Gpca(value, _, _, _, _, _) => Some(value)
      case Cpca(spec)                   => spec.requestedComponentUpperBound
      case Nystrom(value, _, _, _, _)   => Some(value)

  def componentRequestSummary: String =
    this match
      case Pca(value, _)                => value.value.toString
      case Svd(value, _)                => value.value.toString
      case Gpca(value, _, _, _, _, _) => value.value.toString
      case Cpca(spec)                   => spec.requestedComponentSummary
      case Nystrom(value, _, _, _, _)   => value.value.toString

  def kind: FitArtifactKind =
    this match
      case Pca(_, _)                => FitArtifactKind.Pca
      case Svd(_, _)                => FitArtifactKind.Svd
      case Gpca(_, _, _, _, _, _) => FitArtifactKind.Gpca
      case Cpca(_)                  => FitArtifactKind.Cpca
      case Nystrom(_, _, _, _, _)   => FitArtifactKind.Nystrom

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
      _ <- MatrixOps.traverse(roiPlan.rois)(roi => roi.columns.requireWithin(input.featureCount))
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
      case MultivarEstimator.Gpca(components, _, rowMetric, columnMetric, _, _) =>
        for
          _ <- validateComponentRequest(components, sampleCount, roiPlan)
          _ <- validateOptionalMetric(IndexAxis.Row, sampleCount, rowMetric)
          _ <- MatrixOps.traverse(roiPlan.rois) { roi =>
            validateOptionalMetric(IndexAxis.Feature, roi.size, columnMetric)
          }
        yield ()
      case MultivarEstimator.Cpca(spec) =>
        MatrixOps.traverse(roiPlan.rois) { roi =>
          spec.validate(sampleCount, roi.size)
        }.map(_ => ())
      case _ =>
        estimator.componentCount match
          case Some(components) => validateComponentRequest(components, sampleCount, roiPlan)
          case None             => Right(())

  private def validateComponentRequest(
      components: ComponentCount,
      sampleCount: Int,
      roiPlan: RoiPlanSet
  ): Either[MultivarError, Unit] =
    val minRoiSize = roiPlan.rois.map(_.size).min
    val limit = Math.min(sampleCount, minRoiSize)
    if components.value > limit then Left(MultivarError.InvalidComponentRequest(components.value, limit))
    else Right(())

  private def validateOptionalMetric(
      axis: IndexAxis,
      expected: Int,
      metric: Option[MetricSpec]
  ): Either[MultivarError, Unit] =
    metric match
      case Some(value) if value.dim != expected =>
        Left(MultivarError.MetricShapeMismatch(axis, expected, value.dim))
      case _ =>
        Right(())

final case class PairedSampleByFeatureInput private (
    x: SampleByFeatureInput,
    y: SampleByFeatureInput
):
  def sampleCount: Int =
    x.sampleCount

  def xFeatureCount: Int =
    x.featureCount

  def yFeatureCount: Int =
    y.featureCount

object PairedSampleByFeatureInput:
  def of(x: SampleByFeatureInput, y: SampleByFeatureInput): Either[MultivarError, PairedSampleByFeatureInput] =
    if x.sampleCount != y.sampleCount then
      Left(MultivarError.MatrixShapeMismatch(s"paired input expected equal samples, got ${x.sampleCount} and ${y.sampleCount}"))
    else Right(new PairedSampleByFeatureInput(x, y))

enum PairedFeatureScope:
  case WholeInputPair

  def label: String =
    this match
      case WholeInputPair => "whole-input-pair"

enum PairedMultivarEstimator:
  case Plsc(
      components: ComponentCount,
      xPreprocessing: PreprocessSpec = PreprocessSpec.Center,
      yPreprocessing: PreprocessSpec = PreprocessSpec.Center
  )
  case Cca(
      components: ComponentCount,
      regularization: CcaRegularization = CcaRegularization.default,
      xPreprocessing: PreprocessSpec = PreprocessSpec.Center,
      yPreprocessing: PreprocessSpec = PreprocessSpec.Center
  )
  case ReducedRankRegression(
      components: ComponentCount,
      regularization: RegressionRegularization = RegressionRegularization.Ols,
      direction: RegressionDirection = RegressionDirection.XToY,
      xPreprocessing: PreprocessSpec = PreprocessSpec.Center,
      yPreprocessing: PreprocessSpec = PreprocessSpec.Center
  )

  def componentCount: ComponentCount =
    this match
      case Plsc(value, _, _)                       => value
      case Cca(value, _, _, _)                     => value
      case ReducedRankRegression(value, _, _, _, _) => value

  def method: PairedLatentMethod =
    this match
      case Plsc(_, _, _) =>
        PairedLatentMethod.Plsc
      case Cca(_, regularization, _, _) =>
        PairedLatentMethod.Cca(regularization)
      case ReducedRankRegression(_, regularization, direction, _, _) =>
        PairedLatentMethod.ReducedRankRegression(direction, regularization)

  def label: String =
    method.label

/** Inspectable plan boundary for paired latent analyses.
  *
  * This plan intentionally supports only whole-input X/Y pairs. ROI-by-ROI X
  * with global Y and paired ROI sets remain future executor designs rather than
  * implicit variants of the current single-input `MultivarPlan`.
  */
final case class PairedMultivarPlan private (
    id: MultivarPlanId,
    input: PairedSampleByFeatureInput,
    estimator: PairedMultivarEstimator,
    featureScope: PairedFeatureScope,
    execution: MultivarExecutionPlan
):
  def inspectableSummary: String =
    s"${id.value}:${estimator.label}:${input.sampleCount}x${input.xFeatureCount}->${input.yFeatureCount}:${featureScope.label}:${execution.mode}"

object PairedMultivarPlan:
  def of(
      id: String,
      input: PairedSampleByFeatureInput,
      estimator: PairedMultivarEstimator,
      execution: MultivarExecutionPlan = MultivarExecutionPlan.local,
      featureScope: PairedFeatureScope = PairedFeatureScope.WholeInputPair
  ): Either[MultivarError, PairedMultivarPlan] =
    for
      planId <- MultivarPlanId(id)
      _ <- validateWholeInputExecution(execution)
      _ <- validateComponentRequest(estimator.componentCount, input)
    yield new PairedMultivarPlan(planId, input, estimator, featureScope, execution)

  private def validateWholeInputExecution(execution: MultivarExecutionPlan): Either[MultivarError, Unit] =
    if execution.partitionAxis == MultivarPartitionAxis.WholeInput then Right(())
    else Left(MultivarError.InvalidRowGeometry("paired multivar plans currently support whole-input execution only"))

  private def validateComponentRequest(
      components: ComponentCount,
      input: PairedSampleByFeatureInput
  ): Either[MultivarError, Unit] =
    val limit = Math.min(input.sampleCount, Math.min(input.xFeatureCount, input.yFeatureCount))
    if components.value > limit then Left(MultivarError.InvalidComponentRequest(components.value, limit))
    else Right(())

enum FitArtifactKind:
  case Pca
  case Svd
  case Gpca
  case Cpca
  case Nystrom

  def label: String =
    this match
      case Pca     => "pca"
      case Svd     => "svd"
      case Gpca    => "gpca"
      case Cpca    => "cpca"
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
  case FrameTransformArtifact(artifactShape: FitArtifactShape, transform: FittedFrameTransform)
  case OperatorArtifact(artifactShape: FitArtifactShape, fits: Vector[OperatorFitBundle])
  case CpcaArtifact(
      artifactShape: FitArtifactShape,
      fit: PreparedCpcaOperatorFit,
      fits: Vector[OperatorFitBundle]
  )
  case KernelArtifact(artifactShape: FitArtifactShape, fit: NystromFit)

  def shape: FitArtifactShape =
    this match
      case FrameTransformArtifact(value, _) => value
      case OperatorArtifact(value, _)     => value
      case CpcaArtifact(value, _, _)      => value
      case KernelArtifact(value, _)       => value

  def operatorFits: Vector[OperatorFitBundle] =
    this match
      case OperatorArtifact(_, values) => values
      case CpcaArtifact(_, _, values)  => values
      case _                           => Vector.empty

final case class LocalMultivarResult(plan: MultivarPlan, artifacts: Vector[FitArtifact]):
  require(artifacts.nonEmpty, "local multivar result must contain at least one artifact")

object LocalMultivarExecutor:
  def run(plan: MultivarPlan, input: MatrixView): Either[MultivarError, LocalMultivarResult] =
    if input.rows != plan.input.sampleCount then
      Left(MultivarError.MatrixShapeMismatch(s"plan expected ${plan.input.sampleCount} samples, got ${input.rows}"))
    else if input.cols != plan.input.featureCount then
      Left(MultivarError.MatrixShapeMismatch(s"plan expected ${plan.input.featureCount} features, got ${input.cols}"))
    else
      MatrixOps.traverse(plan.roiPlan.rois) { roi =>
        for
          selected <- input.selectColumns(roi.columns)
          artifact <- fitRoi(plan, roi, selected)
        yield artifact
      }.map(artifacts => LocalMultivarResult(plan, artifacts))

  private def fitRoi(plan: MultivarPlan, roi: RoiPlan, input: MatrixView): Either[MultivarError, FitArtifact] =
    plan.estimator match
      case MultivarEstimator.Pca(components, preprocessing) =>
        Pca.fit(input, components, preprocessing).map { fit =>
          FitArtifact.FrameTransformArtifact(
            shape(plan, roi, FitArtifactKind.Pca, input, fit.transform.componentSpace.descriptor.size),
            fit.transform
          )
        }
      case MultivarEstimator.Svd(components, preprocessing) =>
        Svd.fit(input, components, preprocessing).map { fit =>
          FitArtifact.FrameTransformArtifact(
            shape(plan, roi, FitArtifactKind.Svd, input, fit.transform.componentSpace.descriptor.size),
            fit.transform
          )
        }
      case MultivarEstimator.Gpca(components, preprocessing, rowMetric, columnMetric, backend, policy) =>
        val rowSpace = MvSpace(plan.input.id, SpaceRole.Samples, plan.input.samples)
        val columnSpace = MvSpace(
          SpaceId.unsafe(s"${plan.input.id.value}.${roi.id.value}"),
          SpaceRole.Observed,
          Dimension.unsafe(input.cols)
        )
        for
          preprocessor <- preprocessing.fit(input)
          transformed <- preprocessor.transform(input, policy = policy)
          rowGeometry <- rowMetric match
            case Some(value) => Right(value)
            case None        => MetricSpec.identity(input.rows, Some(rowSpace))
          featureGeometry <- columnMetric match
            case Some(value) => Right(value)
            case None        => MetricSpec.identity(input.cols, Some(columnSpace))
          tolerance <- GpcaRankTolerance.fromBackend(backend)
          problem <- DynamicGpcaProblem.from(
            transformed,
            rowSpace,
            columnSpace,
            rowGeometry,
            featureGeometry,
            ValueIdentity.source(ValueId.unsafe(s"${plan.id.value}.${roi.id.value}.planned-gpca")),
            SemanticProvenance.source(s"planned-gpca:${plan.id.value}:${roi.id.value}")
          )
          fit <- problem.fit(components, tolerance, DenseSolvers.generalizedEigen)
          bundle <- fit.toBundle(problem.value.table)
        yield FitArtifact.OperatorArtifact(
          shape(plan, roi, FitArtifactKind.Gpca, input, fit.generalizedEigenvalues.length),
          Vector(bundle)
        )
      case MultivarEstimator.Cpca(spec) =>
        val rowSpace = MvSpace(plan.input.id, SpaceRole.Samples, plan.input.samples)
        val columnSpace = MvSpace(
          SpaceId.unsafe(s"${plan.input.id.value}.${roi.id.value}"),
          SpaceRole.Observed,
          Dimension.unsafe(input.cols)
        )
        for
          problem <- CpcaOperatorProblem.fromMatrices(
            input,
            spec.rowMetric,
            spec.columnMetric,
            spec.rowConstraint,
            spec.columnConstraint,
            rowSpace,
            columnSpace,
            DenseSolvers.symmetricEigen,
            spec.rankTolerance,
            spec.storagePolicy,
            s"planned-cpca:${plan.id.value}:${roi.id.value}"
          )
          blockRequest <- spec.blockRequest
          fit <- problem.fit(
            blockRequest,
            eigenSolver = DenseSolvers.symmetricEigen,
            svdSolver = DenseSolvers.svd,
            rankTolerance = spec.rankTolerance,
            policy = spec.storagePolicy
          )
          bundles <- MatrixOps.traverse(fit.operatorBlocks)(_.toBundle)
        yield FitArtifact.CpcaArtifact(
          shape(plan, roi, FitArtifactKind.Cpca, input, cpcaComponentCount(fit)),
          fit,
          bundles
        )
      case MultivarEstimator.Nystrom(components, landmarks, kernelSpec, preprocessing, method) =>
        for
          kernel <- PlanOps.kernelFromSpec(kernelSpec)
          fit <- Nystrom.fit(input, components, landmarks, kernel, preprocessing, method)
        yield FitArtifact.KernelArtifact(shape(plan, roi, FitArtifactKind.Nystrom, input, fit.eigen.components), fit)

  private def cpcaComponentCount(fit: PreparedCpcaOperatorFit): Int =
    fit.blocks.valuesIterator.map(_.rank).sum

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
