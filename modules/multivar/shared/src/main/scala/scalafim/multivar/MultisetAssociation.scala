package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

final case class ViewAssociationScore(
    viewId: BlockId,
    rowSpace: MvSpace,
    values: DMat
)

final case class MultisetAssociationDiagnostics(
    featureRank: Int,
    requestedComponents: Int,
    returnedComponents: Int,
    rowOperatorRepresentation: OperatorRepresentation,
    featureOperatorRepresentation: OperatorRepresentation,
    tableRepresentation: OperatorRepresentation,
    generalizedResidual: Double,
    normalizationResidual: Double,
    formulation: String
)

final case class MultisetAssociationFit[
    Feature <: SemanticSpace,
    Component <: SemanticSpace
](
    eigenvalues: DVec,
    functionalFrame: FunctionalFrame[Feature, Component, UncheckedEvidence],
    programFit: OperatorProgramFit,
    association: DirectSumAssociationOperator[Feature],
    componentAssociation: Op[Primal[Component], Dual[Component], ComponentOperatorRole, UncheckedEvidence],
    operatorBundle: OperatorFitBundle,
    featureAxes: DMat,
    metricLoadings: DMat,
    directSumScores: DMat,
    viewScores: Vector[ViewAssociationScore],
    objective: ObjectiveDefinition,
    diagnostics: MultisetAssociationDiagnostics
)

/** Executable covariance-style multiset association over a typed direct sum.
  *
  * The row and feature block assemblies remain matrix-free. Dense realization
  * occurs only at the explicit finite feature-space generalized Rayleigh
  * lowering. The fitted parameter is one functional frame `W`; axes and scores
  * are derived as `Q W` and `X W`.
  */
object MultisetAssociation:
  def fit(
      study: DirectSumStudy,
      problem: MaximizeAssociation[study.rowSpace.Id],
      components: ComponentCount,
      policy: StoragePolicy,
      eigenSolver: GeneralizedEigenSolver = DenseSolvers.generalizedEigen,
      tolerance: Double = 1e-10
  ): Either[DirectSumError, MultisetAssociationFit[study.featureSpace.Id, ? <: SemanticSpace]] =
    if components.value > study.featureSpace.evidence.dimension then
      Left(
        DirectSumError.Multivar(
          MultivarError.InvalidComponentRequest(components.value, study.featureSpace.evidence.dimension)
        )
      )
    else if !tolerance.isFinite || tolerance < 0.0 then
      Left(DirectSumError.Multivar(MultivarError.InvalidTolerance("multiset association", tolerance)))
    else if policy != StoragePolicy.AllowDense then
      Left(
        DirectSumError.Multivar(
          MultivarError.DensificationRejected("multiset feature-space generalized eigensystem", StorageKind.Operator)
        )
      )
    else
      for
        metric <- study.columnGeometry.certifiedMetric.toRight(
          DirectSumError.InvalidStudy(
            "multiset association requires certified SPD block column geometries; apply an explicit singular policy first"
          )
        )
        cometric <- study.columnGeometry.cometric.toRight(
          DirectSumError.InvalidStudy("multiset association requires an explicit certified direct-sum cometric")
        )
        association <- DirectSumFeatureOperators.association(study, problem.objective)
        numerator <- association.operator.toDense.left.map(DirectSumError.Semantic.apply)
        denominator <- cometric.toDense.left.map(DirectSumError.Semantic.apply)
        rankTolerance <- SpectralRankTolerance
          .from(tolerance)
          .left
          .map(DirectSumError.Multivar.apply)
        rayleigh <- GeneralizedRayleighRitz
          .solve(
            numerator,
            denominator,
            components,
            rankTolerance,
            solver = eigenSolver
          )
          .left
          .map(DirectSumError.Multivar.apply)
        component <- SpaceRef
          .of(
            s"${study.featureSpace.descriptor.id.value}.multiset-components",
            SpaceRole.Latent,
            rayleigh.values.length
          )
          .left
          .map(DirectSumError.Multivar.apply)
        fit <- assemble(
          study,
          problem,
          components,
          metric,
          cometric,
          association,
          component.evidence,
          rayleigh,
          tolerance
        )
      yield fit

  private def assemble[Component <: SemanticSpace](
      study: DirectSumStudy,
      problem: MaximizeAssociation[study.rowSpace.Id],
      requested: ComponentCount,
      metric: OpMetric[study.featureSpace.Id, CertifiedSpd],
      cometric: OpCometric[study.featureSpace.Id, CertifiedSpd],
      association: DirectSumAssociationOperator[study.featureSpace.Id],
      component: SpaceEvidence[Component],
      rayleigh: RayleighRitzResult,
      tolerance: Double
  ): Either[DirectSumError, MultisetAssociationFit[study.featureSpace.Id, Component]] =
    val frameIdentity = ValueIdentity.derived(
      "multiset-functional-frame",
      association.operator.valueIdentity,
      cometric.valueIdentity
    )
    val provenance = study.provenance.append(
      SemanticProvenanceEvent.Derived(
        "multiset-generalized-rayleigh-ritz",
        Vector(association.operator.valueIdentity, cometric.valueIdentity)
      )
    )
    for
      variable <- FrameVariable
        .from(
          ParameterId.unsafe(s"${study.featureSpace.descriptor.id.value}.multiset-frame"),
          study.featureSpace.evidence,
          component
        )
        .left
        .map(programError)
      frameOperator <- Op
        .fromDense(
          rayleigh.vectors,
          CoordinateEvidence.primal(component),
          CoordinateEvidence.dual(study.featureSpace.evidence),
          OperatorRoleWitness.frame,
          frameIdentity,
          provenance
        )
        .left
        .map(DirectSumError.Semantic.apply)
      functionalFrame = FunctionalFrame(frameOperator, Some(cometric))
      componentAssociation = OperatorAlgebra.compress(frameOperator, association.operator, frameOperator)
      componentDense <- componentAssociation.toDense.left.map(DirectSumError.Semantic.apply)
      parameterization = FrameParameterization.identity(variable)
      normalization = FrameNormalization(variable, cometric)
      operatorProgram <- OperatorPrograms
        .multiset(parameterization, association.operator, normalization)
        .left
        .map(programError)
      context <- CertificateContext
        .from(
          CertificateTolerance.strict,
          CertificateNorm.Frobenius,
          "multiset-generalized-eigenfit",
          "gale",
          NumericalPrecision.Float64,
          Some(s"rank-tolerance=$tolerance")
        )
        .left
        .map(DirectSumError.Semantic.apply)
      programFit <- OperatorProgramFit
        .from(
          operatorProgram,
          Vector(FittedFrame(variable, functionalFrame)),
          trace(componentDense),
          NumericalIdentifiability(
            rayleigh.values.length,
            rayleigh.diagnostics.spectralClusters,
            Math.max(
              rayleigh.diagnostics.generalizedResidual,
              rayleigh.diagnostics.normalizationResidual
            ),
            context
          ),
          provenance
        )
        .left
        .map(programError)
      scoresOperator = functionalFrame.scores(study.table)
      axesOperator = functionalFrame.axes.get
      scores <- scoresOperator.toDense.left.map(DirectSumError.Semantic.apply)
      axes <- axesOperator.toDense.left.map(DirectSumError.Semantic.apply)
      associationSnapshot <- OperatorSnapshot
        .from("association", DerivedOperatorKind.SecondOrder, association.operator)
        .left
        .map(DirectSumError.Multivar.apply)
      directSnapshot <- OperatorSnapshot
        .from("association-direct", DerivedOperatorKind.SecondOrder, association.direct)
        .left
        .map(DirectSumError.Multivar.apply)
      componentSnapshot <- OperatorSnapshot
        .from("component-association", DerivedOperatorKind.Component, componentAssociation)
        .left
        .map(DirectSumError.Multivar.apply)
      scoresSnapshot <- OperatorSnapshot
        .from("scores", DerivedOperatorKind.Scores, scoresOperator)
        .left
        .map(DirectSumError.Multivar.apply)
      axesSnapshot <- OperatorSnapshot
        .from("axes", DerivedOperatorKind.Axes, axesOperator)
        .left
        .map(DirectSumError.Multivar.apply)
      generalizedDiagnostic <- FitDiagnostic
        .from("generalized-residual", rayleigh.diagnostics.generalizedResidual)
        .left
        .map(DirectSumError.Multivar.apply)
      normalizationDiagnostic <- FitDiagnostic
        .from("normalization-residual", rayleigh.diagnostics.normalizationResidual)
        .left
        .map(DirectSumError.Multivar.apply)
      bundle <- OperatorFitBundle
        .from(
          programFit,
          Vector(associationSnapshot, directSnapshot, componentSnapshot, scoresSnapshot, axesSnapshot),
          Vector(generalizedDiagnostic, normalizationDiagnostic),
          provenance
        )
        .left
        .map(DirectSumError.Multivar.apply)
    yield
      val viewScores = study.blocks.map: block =>
        val rows = block.rowOffset until (block.rowOffset + block.rowSpace.size)
        ViewAssociationScore(block.id, block.rowSpace, scores.selectRows(rows))
      MultisetAssociationFit(
        rayleigh.values,
        functionalFrame,
        programFit,
        association,
        componentAssociation,
        bundle,
        axes,
        rayleigh.vectors,
        scores,
        viewScores,
        problem.definition,
        MultisetAssociationDiagnostics(
          metric.rows,
          requested.value,
          rayleigh.values.length,
          problem.objective.operator.representation,
          association.operator.representation,
          study.table.representation,
          rayleigh.diagnostics.generalizedResidual,
          rayleigh.diagnostics.normalizationResidual,
          "OperatorProgram MaximizeTrace over pairwise secondOrder blocks; generalized by the direct-sum cometric"
        )
      )

  private def trace(values: DMat): Double =
    var out = 0.0
    var index = 0
    while index < Math.min(values.rows, values.cols) do
      out += values(index, index)
      index += 1
    out

  private def programError(error: ProgramError): DirectSumError =
    DirectSumError.InvalidStudy(error.message)
