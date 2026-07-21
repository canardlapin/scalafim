package scalafim.multivar

import gale.backend.Backend.given
import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.LinAlgError
import gale.linalg.Matrix
import gale.linalg.Vec
import gale.spectral.Eigen
import gale.spectral.EigenOrder
import gale.spectral.EigenSelection

/** Non-negative leading generalized root of a canonical effect problem. */
opaque type CanonicalRoot = Double

object CanonicalRoot:
  def apply(value: Double): Either[MultivarError, CanonicalRoot] =
    if !value.isFinite then Left(MultivarError.NonFiniteValue("canonical root", 0, value))
    else if value < 0.0 then Left(MultivarError.NonPositiveSemiDefinite("canonical root", value))
    else Right(value)

  private[multivar] def unsafe(value: Double): CanonicalRoot =
    require(value.isFinite && value >= 0.0, "canonical root must be finite and non-negative")
    value

  extension (root: CanonicalRoot)
    inline def value: Double = root

    def correlation: Double =
      Math.sqrt(root / (1.0 + root))

/** Positive fraction used by trace-scaled residual ridge regularization. */
opaque type TraceRidgeFraction = Double

object TraceRidgeFraction:
  def apply(value: Double): Either[MultivarError, TraceRidgeFraction] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(MultivarError.InvalidRegularization("trace ridge fraction", value, "must be finite and strictly positive"))

  def unsafe(value: Double): TraceRidgeFraction =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (fraction: TraceRidgeFraction)
    inline def value: Double = fraction

enum ResidualRegularization:
  case Unregularized
  case TraceScaled(fraction: TraceRidgeFraction)

final case class ResidualRegularizationFit(
    specification: ResidualRegularization,
    traceScale: Double,
    ridgeAmount: Double
):
  require(traceScale.isFinite && traceScale >= 0.0, "residual trace scale must be finite and non-negative")
  require(ridgeAmount.isFinite && ridgeAmount >= 0.0, "residual ridge amount must be finite and non-negative")

enum DirectionOrientation:
  case LargestMagnitudePositive(anchorFeature: Int, signFlipped: Boolean)

/** Identifiable result of the leading generalized root.
  *
  * A repeated root deliberately has no preferred direction. Its `basis` is
  * B-orthonormal for evaluation, while `projector` is the Euclidean orthogonal
  * projector used for representation-invariant comparisons.
  */
enum CanonicalEffectSolution:
  case Simple(direction: DVec, orientation: DirectionOrientation)
  case LeadingSubspace(basis: DMat, projector: DMat, multiplicity: Int)

final case class CanonicalEffectDiagnostics(
    effectRank: Int,
    residualRank: Int,
    regularizedResidualCondition: Double,
    leadingMultiplicity: Int,
    eigengap: Double,
    generalizedResidual: Double,
    bOrthonormalityError: Double
):
  require(effectRank >= 0 && residualRank >= 0, "numerical ranks must be non-negative")
  require(leadingMultiplicity > 0, "leading multiplicity must be positive")
  require(
    regularizedResidualCondition >= 0.0 && !regularizedResidualCondition.isNaN,
    "condition estimate must be non-negative"
  )
  require(eigengap >= 0.0 && !eigengap.isNaN, "eigengap must be non-negative")
  require(generalizedResidual.isFinite && generalizedResidual >= 0.0, "generalized residual must be finite and non-negative")
  require(bOrthonormalityError.isFinite && bOrthonormalityError >= 0.0, "orthonormality error must be finite and non-negative")

final case class CanonicalEffectProvenance(
    effect: ValueIdentity,
    residual: ValueIdentity,
    regularizedResidual: ValueIdentity,
    solver: String,
    semantic: SemanticProvenance
)

final case class CanonicalEffectFit[
    Feature <: SemanticSpace,
    Component <: SemanticSpace
](
    root: CanonicalRoot,
    solution: CanonicalEffectSolution,
    functionalFrame: FunctionalFrame[Feature, Component, UncheckedEvidence],
    programFit: OperatorProgramFit,
    regularizedResidual: OpCovariance[Feature, CertifiedSpd],
    regularization: ResidualRegularizationFit,
    diagnostics: CanonicalEffectDiagnostics,
    provenance: CanonicalEffectProvenance
)

/** A certified feature-space generalized-Rayleigh problem `E w = lambda B w`.
  *
  * Construction contains no scan, fold, contrast, or ROI concepts. Those layers
  * are responsible for producing the effect and residual operators and for
  * keeping held-out data outside this training problem.
  */
final class CanonicalEffectProblem[Feature <: SemanticSpace] private (
    val featureSpace: SpaceEvidence[Feature],
    val effect: OpCovariance[Feature, CertifiedPsd],
    val residual: OpCovariance[Feature, CertifiedPsd],
    val regularization: ResidualRegularization,
    val tolerance: CertificateTolerance,
    val provenance: SemanticProvenance
):
  def fit: Either[MultivarError, CanonicalEffectFit[Feature, ? <: SemanticSpace]] =
    for
      effectDense <- semantic(effect.toDense)
      residualDense <- semantic(residual.toDense)
      _ <- MatrixOps.checkFinite("canonical effect", effectDense)
      _ <- MatrixOps.checkFinite("canonical residual", residualDense)
      prepared <- prepareResidual(residualDense)
      (regularizedDense, regularizationFit) = prepared
      regularized <- certifySpd(regularizedDense)
      spectrum <- adaptGale(
        Eigen.eigSymmetricGeneralized(
          effectDense,
          regularizedDense,
          EigenSelection.Count(featureSpace.dimension, EigenOrder.LargestAlgebraic)
        )
      )
      converged <- adaptGale(spectrum.requireExtremeCertified)
      _ <- validateSpectrum(converged.eigenvalues)
      leading = converged.eigenvalues(converged.size - 1)
      scale = matrixFrobenius(effectDense) + Math.abs(leading) * matrixFrobenius(regularizedDense)
      residualThreshold = tolerance.threshold(scale)
      worstResidual = converged.diagnostics.worstResidual
      _ <-
        if worstResidual <= residualThreshold then Right(())
        else Left(MultivarError.NumericalResidualExceeded("canonical generalized eigensolve", worstResidual, residualThreshold))
      root <- canonicalRoot(leading, tolerance.threshold(Math.max(1.0, Math.abs(leading))))
      multiplicity = leadingMultiplicity(converged.eigenvalues, tolerance)
      basis = leadingBasis(converged.eigenvectors, multiplicity)
      solution <- identifiedSolution(basis, multiplicity)
      condition <- adaptGale(regularizedDense.conditionEstimate)
      component <- componentSpace(multiplicity)
      fit <- assembleFit(
        component,
        solutionBasis(solution),
        solution,
        root,
        regularized,
        regularizationFit,
        CanonicalEffectDiagnostics(
          effectDense.rankEstimate,
          residualDense.rankEstimate,
          condition,
          multiplicity,
          eigenGap(converged.eigenvalues, multiplicity),
          worstResidual,
          converged.diagnostics.orthogonalityError
        )
      )
    yield fit

  /** Fit the leading generalized-root spectrum for a declared hypothesis rank.
    * The returned frame identifies the retained subspace; repeated roots are
    * represented by projector-valued clusters rather than arbitrary axes.
    */
  def fitSpectrum(
      components: Int
  ): Either[MultivarError, CanonicalSpectrumFit[Feature, ? <: SemanticSpace]] =
    if components <= 0 || components > featureSpace.dimension then
      Left(MultivarError.InvalidComponentRequest(components, featureSpace.dimension))
    else
      for
        effectDense <- semantic(effect.toDense)
        residualDense <- semantic(residual.toDense)
        _ <- MatrixOps.checkFinite("canonical effect", effectDense)
        _ <- MatrixOps.checkFinite("canonical residual", residualDense)
        prepared <- prepareResidual(residualDense)
        (regularizedDense, regularizationFit) = prepared
        regularized <- certifySpd(regularizedDense)
        spectrum <- adaptGale(
          Eigen.eigSymmetricGeneralized(
            effectDense,
            regularizedDense,
            EigenSelection.Count(featureSpace.dimension, EigenOrder.LargestAlgebraic)
          )
        )
        converged <- adaptGale(spectrum.requireExtremeCertified)
        _ <- validateSpectrum(converged.eigenvalues)
        scale = matrixFrobenius(effectDense) +
          canonicalMaxAbs(converged.eigenvalues) * matrixFrobenius(regularizedDense)
        residualThreshold = tolerance.threshold(scale)
        worstResidual = converged.diagnostics.worstResidual
        _ <-
          if worstResidual <= residualThreshold then Right(())
          else Left(MultivarError.NumericalResidualExceeded("canonical generalized eigensolve", worstResidual, residualThreshold))
        roots <- retainedRoots(converged.eigenvalues, components, tolerance)
        basis = spectrumBasis(converged.eigenvectors, components)
        clusters = rootClusters(roots, basis, tolerance)
        condition <- adaptGale(regularizedDense.conditionEstimate)
        component <- SpaceRef.of(s"${featureSpace.id.value}.canonical-spectrum", SpaceRole.Latent, components)
        fit <- assembleSpectrumFit(
          component,
          basis,
          roots,
          clusters,
          regularized,
          regularizationFit,
          CanonicalSpectrumDiagnostics(
            effectDense.rankEstimate,
            residualDense.rankEstimate,
            condition,
            clusters,
            worstResidual,
            converged.diagnostics.orthogonalityError
          )
        )
      yield fit

  private def prepareResidual(residualDense: DMat): Either[MultivarError, (DMat, ResidualRegularizationFit)] =
    prepareCanonicalResidual(residualDense, featureSpace.dimension, regularization)

  private def certifySpd(matrix: DMat): Either[MultivarError, OpCovariance[Feature, CertifiedSpd]] =
    certifyCanonicalResidual(featureSpace, residual, matrix, regularization, tolerance, provenance)

  private def componentSpace(multiplicity: Int): Either[MultivarError, SpaceRef] =
    SpaceRef.of(s"${featureSpace.id.value}.canonical", SpaceRole.Latent, multiplicity)

  private def assembleFit(
      component: SpaceRef,
      basis: DMat,
      solution: CanonicalEffectSolution,
      root: CanonicalRoot,
      regularized: OpCovariance[Feature, CertifiedSpd],
      regularizationFit: ResidualRegularizationFit,
      diagnostics: CanonicalEffectDiagnostics
  ): Either[MultivarError, CanonicalEffectFit[Feature, component.Id]] =
    val parameterId = ParameterId.unsafe(s"${featureSpace.id.value}.canonical-frame")
    for
      variable <- program(FrameVariable.from(parameterId, featureSpace, component.evidence))
      frameOperator <- semantic(
        Op.fromDense(
          basis,
          CoordinateEvidence.primal(component.evidence),
          CoordinateEvidence.dual(featureSpace),
          OperatorRoleWitness.frame,
          ValueIdentity.derived("canonical-frame", effect.valueIdentity, regularized.valueIdentity),
          provenance.append(SemanticProvenanceEvent.Derived("canonical-generalized-eigenframe", Vector(effect.valueIdentity, regularized.valueIdentity)))
        )
      )
      parameterization = FrameParameterization.identity(variable)
      normalization = FrameNormalization(variable, regularized)
      operatorProgram <- program(OperatorPrograms.ldaRayleigh(parameterization, effect, regularized, normalization))
      functionalFrame = FunctionalFrame(frameOperator)
      context <- certificateContext("canonical-generalized-eigenfit")
      identifiability = NumericalIdentifiability(
        diagnostics.leadingMultiplicity,
        Vector((0 until diagnostics.leadingMultiplicity).toVector),
        diagnostics.generalizedResidual,
        context
      )
      fitProvenance = provenance.append(
        SemanticProvenanceEvent.Derived("gale-symmetric-definite-generalized-eigen", Vector(effect.valueIdentity, regularized.valueIdentity))
      )
      operatorFit <- program(
        OperatorProgramFit.from(
          operatorProgram,
          Vector(FittedFrame(variable, functionalFrame)),
          root.value,
          identifiability,
          fitProvenance
        )
      )
    yield
      CanonicalEffectFit(
        root,
        solution,
        functionalFrame,
        operatorFit,
        regularized,
        regularizationFit,
        diagnostics,
        CanonicalEffectProvenance(
          effect.valueIdentity,
          residual.valueIdentity,
          regularized.valueIdentity,
          "gale.spectral.Eigen.eigSymmetricGeneralized",
          fitProvenance
        )
      )

  private def assembleSpectrumFit(
      component: SpaceRef,
      basis: DMat,
      roots: CanonicalRootSpectrum,
      clusters: Vector[CanonicalRootCluster],
      regularized: OpCovariance[Feature, CertifiedSpd],
      regularizationFit: ResidualRegularizationFit,
      diagnostics: CanonicalSpectrumDiagnostics
  ): Either[MultivarError, CanonicalSpectrumFit[Feature, component.Id]] =
    val parameterId = ParameterId.unsafe(s"${featureSpace.id.value}.canonical-spectrum-frame")
    for
      variable <- program(FrameVariable.from(parameterId, featureSpace, component.evidence))
      frameOperator <- semantic(
        Op.fromDense(
          basis,
          CoordinateEvidence.primal(component.evidence),
          CoordinateEvidence.dual(featureSpace),
          OperatorRoleWitness.frame,
          ValueIdentity.derived("canonical-spectrum-frame", effect.valueIdentity, regularized.valueIdentity),
          provenance.append(
            SemanticProvenanceEvent.Derived(
              "canonical-generalized-eigenframe",
              Vector(effect.valueIdentity, regularized.valueIdentity)
            )
          )
        )
      )
      parameterization = FrameParameterization.identity(variable)
      normalization = FrameNormalization(variable, regularized)
      operatorProgram <- program(OperatorPrograms.gpca(parameterization, effect, normalization))
      functionalFrame = FunctionalFrame(frameOperator)
      context <- certificateContext("canonical-generalized-spectrum-fit")
      identifiability = NumericalIdentifiability(
        roots.rank,
        clusters.map(cluster => (cluster.firstRoot until cluster.firstRoot + cluster.multiplicity).toVector),
        diagnostics.generalizedResidual,
        context
      )
      fitProvenance = provenance.append(
        SemanticProvenanceEvent.Derived(
          "gale-symmetric-definite-generalized-spectrum",
          Vector(effect.valueIdentity, regularized.valueIdentity)
        )
      )
      statistics = ManovaStatistics.from(roots)
      operatorFit <- program(
        OperatorProgramFit.from(
          operatorProgram,
          Vector(FittedFrame(variable, functionalFrame)),
          statistics.hotellingLawleyTrace,
          identifiability,
          fitProvenance
        )
      )
    yield
      CanonicalSpectrumFit(
        roots,
        statistics,
        functionalFrame,
        operatorFit,
        regularizationFit,
        diagnostics,
        CanonicalEffectProvenance(
          effect.valueIdentity,
          residual.valueIdentity,
          regularized.valueIdentity,
          "gale.spectral.Eigen.eigSymmetricGeneralized",
          fitProvenance
        )
      )

  private def certificateContext(method: String): Either[MultivarError, CertificateContext] =
    semantic(
      CertificateContext.from(
        tolerance,
        CertificateNorm.Frobenius,
        method,
        "gale",
        NumericalPrecision.Float64,
        Some(regularizationLabel)
      )
    )

  private def regularizationLabel: String =
    regularization match
      case ResidualRegularization.Unregularized => "unregularized"
      case ResidualRegularization.TraceScaled(fraction) => s"trace-scaled-${fraction.value}"

object CanonicalEffectProblem:
  def fromOperators[Feature <: SemanticSpace](
      featureSpace: SpaceEvidence[Feature],
      effect: OpCovariance[Feature, CertifiedPsd],
      residual: OpCovariance[Feature, CertifiedPsd],
      regularization: ResidualRegularization,
      tolerance: CertificateTolerance = CertificateTolerance.strict,
      provenance: SemanticProvenance = SemanticProvenance.source("canonical-effect-problem")
  ): Either[MultivarError, CanonicalEffectProblem[Feature]] =
    if effect.domain.descriptor.space != featureSpace.descriptor then
      Left(MultivarError.MatrixShapeMismatch("canonical effect operator does not belong to the declared feature space"))
    else if residual.domain.descriptor.space != featureSpace.descriptor then
      Left(MultivarError.MatrixShapeMismatch("canonical residual operator does not belong to the declared feature space"))
    else Right(new CanonicalEffectProblem(featureSpace, effect, residual, regularization, tolerance, provenance))

  def fromDense[Feature <: SemanticSpace](
      featureSpace: SpaceEvidence[Feature],
      effect: DMat,
      residual: DMat,
      regularization: ResidualRegularization,
      tolerance: CertificateTolerance = CertificateTolerance.strict,
      provenance: SemanticProvenance = SemanticProvenance.source("canonical-effect-dense")
  ): Either[MultivarError, CanonicalEffectProblem[Feature]] =
    for
      effectOperator <- certifiedCovariance(featureSpace, effect, "canonical-effect", tolerance, provenance)
      residualOperator <- certifiedCovariance(featureSpace, residual, "canonical-residual", tolerance, provenance)
      problem <- fromOperators(featureSpace, effectOperator, residualOperator, regularization, tolerance, provenance)
    yield problem

  private def certifiedCovariance[Feature <: SemanticSpace](
      featureSpace: SpaceEvidence[Feature],
      value: DMat,
      label: String,
      tolerance: CertificateTolerance,
      provenance: SemanticProvenance
  ): Either[MultivarError, OpCovariance[Feature, CertifiedPsd]] =
    val identity = ValueIdentity.source(ValueId.unsafe(s"${featureSpace.id.value}.$label"))
    for
      _ <- MatrixOps.checkFinite(label, value)
      context <- semantic(
        CertificateContext.from(tolerance, CertificateNorm.Frobenius, s"$label-psd", "gale", NumericalPrecision.Float64)
      )
      linear <- semantic(
        Lin.fromDenseMatrix(
          value,
          CoordinateEvidence.dual(featureSpace),
          CoordinateEvidence.primal(featureSpace),
          identity,
          provenance
        )
      )
      certificate <- semantic(FormCertificates.psd(linear, context))
      certified <- semantic(Op.certifiedPsd(Op.fromLin(linear, OperatorRoleWitness.covariance), certificate))
    yield certified

private[multivar] def prepareCanonicalResidual(
    residualDense: DMat,
    dimension: Int,
    regularization: ResidualRegularization
): Either[MultivarError, (DMat, ResidualRegularizationFit)] =
  val trace = matrixTrace(residualDense)
  val scale = trace / dimension.toDouble
  regularization match
    case ResidualRegularization.Unregularized =>
      if !trace.isFinite then Left(MultivarError.NonFiniteValue("residual trace", 0, trace))
      else Right((residualDense, ResidualRegularizationFit(regularization, Math.max(scale, 0.0), 0.0)))
    case ResidualRegularization.TraceScaled(fraction) =>
      if !trace.isFinite then Left(MultivarError.NonFiniteValue("residual trace", 0, trace))
      else if trace <= 0.0 then Left(MultivarError.NonInvertibleValue("residual trace", 0, trace))
      else
        val ridge = fraction.value * scale
        Right((residualDense.addToDiagonal(ridge), ResidualRegularizationFit(regularization, scale, ridge)))

private[multivar] def certifyCanonicalResidual[Feature <: SemanticSpace](
    featureSpace: SpaceEvidence[Feature],
    residual: OpCovariance[Feature, CertifiedPsd],
    matrix: DMat,
    regularization: ResidualRegularization,
    tolerance: CertificateTolerance,
    provenance: SemanticProvenance
): Either[MultivarError, OpCovariance[Feature, CertifiedSpd]] =
  val identity = ValueIdentity.derived("canonical-regularized-residual", residual.valueIdentity)
  for
    context <- semantic(
      CertificateContext.from(
        tolerance,
        CertificateNorm.Frobenius,
        "canonical-residual-spd",
        "gale",
        NumericalPrecision.Float64,
        Some(canonicalRegularizationLabel(regularization))
      )
    )
    linear <- semantic(
      Lin.fromDenseMatrix(
        matrix,
        CoordinateEvidence.dual(featureSpace),
        CoordinateEvidence.primal(featureSpace),
        identity,
        provenance.append(SemanticProvenanceEvent.Derived("residual-regularization", Vector(residual.valueIdentity)))
      )
    )
    certificate <- semantic(FormCertificates.spd(linear, context))
    certified <- semantic(Op.certifiedSpd(Op.fromLin(linear, OperatorRoleWitness.covariance), certificate))
  yield certified

private def canonicalRegularizationLabel(regularization: ResidualRegularization): String =
  regularization match
    case ResidualRegularization.Unregularized => "unregularized"
    case ResidualRegularization.TraceScaled(fraction) => s"trace-scaled-${fraction.value}"

private def canonicalRoot(value: Double, noise: Double): Either[MultivarError, CanonicalRoot] =
  if value >= 0.0 then CanonicalRoot(value)
  else if value >= -noise then Right(CanonicalRoot.unsafe(0.0))
  else Left(MultivarError.NonPositiveSemiDefinite("canonical effect generalized root", value))

private def validateSpectrum(values: DVec): Either[MultivarError, Unit] =
  if values.length == 0 then Left(MultivarError.SolverFailed("generalized eigensolver returned an empty spectrum"))
  else
    var index = 0
    var invalid = Option.empty[(Int, Double)]
    while index < values.length && invalid.isEmpty do
      if !values(index).isFinite then invalid = Some(index -> values(index))
      index += 1
    invalid match
      case Some((at, value)) => Left(MultivarError.NonFiniteValue("generalized eigenvalue", at, value))
      case None              => Right(())

private def leadingMultiplicity(values: DVec, tolerance: CertificateTolerance): Int =
  val leading = values(values.length - 1)
  val threshold = tolerance.threshold(Math.abs(leading))
  var count = 1
  var index = values.length - 2
  while index >= 0 && Math.abs(leading - values(index)) <= threshold do
    count += 1
    index -= 1
  count

private def eigenGap(values: DVec, multiplicity: Int): Double =
  val next = values.length - multiplicity - 1
  if next < 0 then Double.PositiveInfinity
  else Math.max(0.0, values(values.length - 1) - values(next))

private def leadingBasis(vectors: DMat, multiplicity: Int): DMat =
  val out = Matrix.newBuilder(vectors.rows, multiplicity)
  var col = 0
  while col < multiplicity do
    val source = vectors.cols - col - 1
    var row = 0
    while row < vectors.rows do
      out(row, col) = vectors(row, source)
      row += 1
    col += 1
  out.result()

private def identifiedSolution(basis: DMat, multiplicity: Int): Either[MultivarError, CanonicalEffectSolution] =
  if multiplicity == 1 then
    val direction = Vec.newBuilder(basis.rows)
    var anchor = 0
    var largest = -1.0
    var row = 0
    while row < basis.rows do
      val magnitude = Math.abs(basis(row, 0))
      if magnitude > largest then
        largest = magnitude
        anchor = row
      row += 1
    val flipped = basis(anchor, 0) < 0.0
    row = 0
    while row < basis.rows do
      direction(row) = if flipped then -basis(row, 0) else basis(row, 0)
      row += 1
    Right(CanonicalEffectSolution.Simple(direction.result(), DirectionOrientation.LargestMagnitudePositive(anchor, flipped)))
  else
    val orthogonal = basis.qr.q
    val q = MatrixOps.takeColumns(orthogonal, multiplicity)
    Right(CanonicalEffectSolution.LeadingSubspace(basis, q * q.t, multiplicity))

private def solutionBasis(solution: CanonicalEffectSolution): DMat =
  solution match
    case CanonicalEffectSolution.Simple(direction, _) =>
      val out = Matrix.newBuilder(direction.length, 1)
      var row = 0
      while row < direction.length do
        out(row, 0) = direction(row)
        row += 1
      out.result()
    case CanonicalEffectSolution.LeadingSubspace(basis, _, _) => basis

private def retainedRoots(
    values: DVec,
    components: Int,
    tolerance: CertificateTolerance
): Either[MultivarError, CanonicalRootSpectrum] =
  val roots = Vector.newBuilder[ManovaRoot]
  var component = 0
  while component < components do
    val value = values(values.length - component - 1)
    val threshold = tolerance.threshold(Math.max(1.0, Math.abs(value)))
    if value >= 0.0 then roots += ManovaRoot.unsafe(value)
    else if value >= -threshold then roots += ManovaRoot.unsafe(0.0)
    else return Left(MultivarError.NonPositiveSemiDefinite("canonical effect generalized root", value))
    component += 1
  Right(CanonicalRootSpectrum.unsafe(roots.result()))

private def spectrumBasis(vectors: DMat, components: Int): DMat =
  val out = Matrix.newBuilder(vectors.rows, components)
  var component = 0
  while component < components do
    val source = vectors.cols - component - 1
    var row = 0
    while row < vectors.rows do
      out(row, component) = vectors(row, source)
      row += 1
    component += 1
  out.result()

private def rootClusters(
    roots: CanonicalRootSpectrum,
    basis: DMat,
    tolerance: CertificateTolerance
): Vector[CanonicalRootCluster] =
  val clusters = Vector.newBuilder[CanonicalRootCluster]
  var first = 0
  while first < roots.rank do
    val representative = roots.values(first)
    val threshold = tolerance.threshold(Math.max(1.0, representative.value))
    var end = first + 1
    while end < roots.rank && Math.abs(roots.values(end).value - representative.value) <= threshold do
      end += 1
    val block = basisBlock(basis, first, end - first)
    val orthogonal = block.qr.q
    val q = MatrixOps.takeColumns(orthogonal, end - first)
    clusters += CanonicalRootCluster(first, end - first, representative, q * q.t)
    first = end
  clusters.result()

private def basisBlock(matrix: DMat, first: Int, count: Int): DMat =
  val out = Matrix.newBuilder(matrix.rows, count)
  var row = 0
  while row < matrix.rows do
    var col = 0
    while col < count do
      out(row, col) = matrix(row, first + col)
      col += 1
    row += 1
  out.result()

private def canonicalMaxAbs(values: DVec): Double =
  var largest = 0.0
  var index = 0
  while index < values.length do
    largest = Math.max(largest, Math.abs(values(index)))
    index += 1
  largest

private def matrixTrace(matrix: DMat): Double =
  var trace = 0.0
  var index = 0
  while index < matrix.rows do
    trace += matrix(index, index)
    index += 1
  trace

private def matrixFrobenius(matrix: DMat): Double =
  var squared = 0.0
  var row = 0
  while row < matrix.rows do
    var col = 0
    while col < matrix.cols do
      squared += matrix(row, col) * matrix(row, col)
      col += 1
    row += 1
  Math.sqrt(squared)

private def semantic[A](result: Either[SemanticError, A]): Either[MultivarError, A] =
  result.left.map:
    case SemanticError.MultivarFailure(error) => error
    case SemanticError.LinearMapFailure(error) => LinalgErrorAdapter.toMultivarError(error)
    case SemanticError.CertificateRejected("spd", _) =>
      MultivarError.NonInvertibleValue("positive-definite residual geometry", 0, 0.0)
    case error => MultivarError.SolverFailed(error.message)

private def adaptGale[A](result: Either[LinAlgError, A]): Either[MultivarError, A] =
  LinalgErrorAdapter.adapt(result)

private def program[A](result: Either[ProgramError, A]): Either[MultivarError, A] =
  result.left.map(error => MultivarError.SolverFailed(error.message))
