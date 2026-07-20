package scalafim.multivar

import gale.linalg.DMat

enum DiagramCertificate:
  case Numerical(value: NumericalCertificate)
  case CenteringLaws(value: CenteringLawCertificate)
  case AlreadyCentered(value: CenteredDataCertificate)

final class PreparedSemanticDiagram[Rows <: SemanticSpace, Columns <: SemanticSpace] private (
    val source: SemanticDualityDiagram[Rows, Columns, CompleteCells],
    val table: MatrixView,
    val rowSpace: MvSpace,
    val columnSpace: MvSpace,
    val rowMetric: MvMetric,
    val columnMetric: MvMetric,
    val rowResolution: GeometryResolution[Rows],
    val columnResolution: GeometryResolution[Columns],
    val evidence: Vector[DiagramCertificate],
    val audit: DiagramAudit,
    val provenance: SemanticProvenance
)

object PreparedSemanticDiagram:
  def prepare[Rows <: SemanticSpace, Columns <: SemanticSpace](
      diagram: SemanticDualityDiagram[Rows, Columns, CompleteCells],
      storagePolicy: StoragePolicy = StoragePolicy.Operator,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen
  ): Either[DiagramError, PreparedSemanticDiagram[Rows, Columns]] =
    for
      base <- DiagramNumerics.tableView(diagram.core.table, storagePolicy)
      centered <- applyCentering(diagram, base)
      (preparedTable, centeringEvidence) = centered
      rowResolution <- GeometryResolution.resolve(
        diagram.core.rowGeometry,
        diagram.preparation.geometryPolicies.rows,
        IndexAxis.Row,
        eigenSolver
      )
      columnResolution <- GeometryResolution.resolve(
        diagram.core.columnGeometry,
        diagram.preparation.geometryPolicies.columns,
        IndexAxis.Column,
        eigenSolver
      )
      effectiveTable <- applySupport(preparedTable, rowResolution, columnResolution, storagePolicy)
      audit = preparationAudit(diagram.audit, centeringEvidence, rowResolution, columnResolution)
      evidence =
        rowResolution.certificates.map(DiagramCertificate.Numerical.apply) ++
          columnResolution.certificates.map(DiagramCertificate.Numerical.apply) ++
          centeringEvidence.toVector
    yield
      new PreparedSemanticDiagram(
        diagram,
        effectiveTable,
        rowResolution.effectiveSpace,
        columnResolution.effectiveSpace,
        rowResolution.effectiveMetric,
        columnResolution.effectiveMetric,
        rowResolution,
        columnResolution,
        evidence,
        audit,
        diagram.provenance.append(
          SemanticProvenanceEvent.Derived("prepare-duality-diagram", Vector(diagram.core.table.valueIdentity))
        )
      )

  private def applyCentering[Rows <: SemanticSpace, Columns <: SemanticSpace](
      diagram: SemanticDualityDiagram[Rows, Columns, CompleteCells],
      table: MatrixView
  ): Either[DiagramError, (MatrixView, Option[DiagramCertificate])] =
    diagram.preparation.centering match
      case NoCentering() =>
        Right((table, None))
      case CenterByMeasure(measure) =>
        for
          projection <- CenteringProjection.byMeasure(measure)
          centered <- projection.applyTo(table)
        yield (centered, Some(DiagramCertificate.CenteringLaws(projection.lawCertificate)))
      case CenterOrthogonally(metric) =>
        for
          projection <- CenteringProjection.orthogonal(metric)
          centered <- projection.applyTo(table)
        yield (centered, Some(DiagramCertificate.CenteringLaws(projection.lawCertificate)))
      case AlreadyCenteredBy(_, certificate) =>
        Right((table, Some(DiagramCertificate.AlreadyCentered(certificate))))

  private def applySupport[Rows <: SemanticSpace, Columns <: SemanticSpace](
      table: MatrixView,
      rows: GeometryResolution[Rows],
      columns: GeometryResolution[Columns],
      storagePolicy: StoragePolicy
  ): Either[DiagramError, MatrixView] =
    if rows.support.isEmpty && columns.support.isEmpty then Right(table)
    else if storagePolicy != StoragePolicy.AllowDense then
      Left(DiagramError.DensificationRequired("support-restricted diagram preparation"))
    else
      table.toDense(StoragePolicy.AllowDense).left.map(DiagramError.Multivar.apply).map { dense =>
        val restrictedRows =
          rows.support match
            case Some(support) => GaleNumerics.multiply(support.restrictionMatrix, dense)
            case None          => dense
        val restricted =
          columns.support match
            case Some(support) => GaleNumerics.multiply(restrictedRows, support.embeddingMatrix)
            case None          => restrictedRows
        DenseMatrixView(restricted)
      }

  private def preparationAudit[Rows <: SemanticSpace, Columns <: SemanticSpace](
      initial: DiagramAudit,
      centering: Option[DiagramCertificate],
      rows: GeometryResolution[Rows],
      columns: GeometryResolution[Columns]
  ): DiagramAudit =
    val centeringEffect =
      centering match
        case Some(_) => CertificateEffect("centering", CertificateValidity.Preserved, "centering laws certified")
        case None    => CertificateEffect("centering", CertificateValidity.Preserved, "no centering requested")
    initial.append(
      DiagramAuditRecord(
        "prepare",
        Vector(rows.original.space.id, columns.original.space.id),
        "typed-duality-preparation",
        Vector(
          centeringEffect,
          CertificateEffect("row-geometry", CertificateValidity.Preserved, rows.kind.toString),
          CertificateEffect("column-geometry", CertificateValidity.Preserved, columns.kind.toString)
        )
      )
    )

final case class SemanticGenPcaFit[Rows <: SemanticSpace, Columns <: SemanticSpace](
    preparedDiagram: PreparedSemanticDiagram[Rows, Columns],
    operatorResult: GpcaOperatorFit[? <: SemanticSpace, ? <: SemanticSpace, ? <: SemanticSpace],
    numericalFit: GenPcaFit,
    result: GenPcaSemanticResult,
    lawDiagnostics: GenPcaLawDiagnostics,
    evidence: Vector[DiagramCertificate]
)

object SemanticGenPca:
  def fit[Rows <: SemanticSpace, Columns <: SemanticSpace](
      diagram: SemanticDualityDiagram[Rows, Columns, CompleteCells],
      components: ComponentCount,
      backend: GmdBackend = GmdBackend.Auto,
      storagePolicy: StoragePolicy = StoragePolicy.AllowDense,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      svdSolver: SvdSolver = DenseSolvers.svd,
      lawTolerance: NumericalLawTolerance = NumericalLawTolerance.strict
  ): Either[DiagramError, SemanticGenPcaFit[Rows, Columns]] =
    for
      prepared <- PreparedSemanticDiagram.prepare(diagram, storagePolicy, eigenSolver)
      tolerance <- GpcaRankTolerance.fromBackend(backend).left.map(DiagramError.Multivar.apply)
      problem <- PreparedGpcaProblem.from(prepared)
      operatorFit <- problem
        .fit(components, tolerance)
        .left
        .map(DiagramError.Multivar.apply)
      fit = operatorFit.compatibility
      diagnostics <- GenPcaLaws
        .evaluate(prepared.table, fit, lawTolerance)
        .left
        .map(DiagramError.Multivar.apply)
    yield
      SemanticGenPcaFit(
        prepared,
        operatorFit,
        fit,
        fit.semanticResult,
        diagnostics,
        prepared.evidence
      )
