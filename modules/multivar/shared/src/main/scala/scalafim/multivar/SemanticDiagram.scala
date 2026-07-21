package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

enum DiagramError:
  case Semantic(error: SemanticError)
  case Multivar(error: MultivarError)
  case InvalidMeasure(detail: String)
  case InvalidCentering(detail: String)
  case InvalidGeometry(detail: String)
  case SingularGeometryRejected(axis: IndexAxis, nullity: Int)
  case EmptySupport(axis: IndexAxis, threshold: Double)
  case DensificationRequired(operation: String)
  case MissingCellsUnsupported

  def message: String =
    this match
      case Semantic(error) => error.message
      case Multivar(error) => error.message
      case InvalidMeasure(detail) => detail
      case InvalidCentering(detail) => detail
      case InvalidGeometry(detail) => detail
      case SingularGeometryRejected(axis, nullity) =>
        s"${axis.label} geometry is singular with nullity $nullity; choose an explicit singular-geometry policy"
      case EmptySupport(axis, threshold) =>
        s"${axis.label} geometry has empty numerical support at threshold $threshold"
      case DensificationRequired(operation) =>
        s"$operation requires explicit StoragePolicy.AllowDense"
      case MissingCellsUnsupported =>
        "semantic generalized PCA requires complete cell data"

enum RowMeasureNormalization:
  case UnitMass(originalMass: Double)

final case class RowMeasureDescriptor(
    space: MvSpace,
    normalization: RowMeasureNormalization,
    valueIdentity: ValueIdentity
)

/** A non-negative normalized statistical measure on one row space. It is not row geometry. */
final class RowMeasure[S <: SemanticSpace] private (
    val space: SpaceEvidence[S],
    val weights: DVec,
    val descriptor: RowMeasureDescriptor,
    val provenance: SemanticProvenance
)

object RowMeasure:
  def fromWeights[S <: SemanticSpace](
      space: SpaceEvidence[S],
      weights: DVec,
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance = SemanticProvenance.source("row-measure")
  ): Either[DiagramError, RowMeasure[S]] =
    if weights.length != space.dimension then
      Left(
        DiagramError.InvalidMeasure(
          s"row measure for '${space.id.value}' has length ${weights.length}, expected ${space.dimension}"
        )
      )
    else
      var index = 0
      var mass = 0.0
      var error = Option.empty[DiagramError]
      while index < weights.length && error.isEmpty do
        val weight = weights(index)
        if !weight.isFinite then
          error = Some(DiagramError.InvalidMeasure(s"row measure weight $index is not finite: $weight"))
        else if weight < 0.0 then
          error = Some(DiagramError.InvalidMeasure(s"row measure weight $index is negative: $weight"))
        else mass += weight
        index += 1
      error match
        case Some(value) => Left(value)
        case None if !mass.isFinite || mass <= 0.0 =>
          Left(DiagramError.InvalidMeasure(s"row measure must have finite positive mass, got $mass"))
        case None =>
          val normalized = new Array[Double](weights.length)
          index = 0
          while index < weights.length do
            normalized(index) = weights(index) / mass
            index += 1
          Right(
            new RowMeasure(
              space,
              GaleNumerics.vectorFromArray(normalized),
              RowMeasureDescriptor(space.descriptor, RowMeasureNormalization.UnitMass(mass), valueIdentity),
              provenance
            )
          )

  def uniform[S <: SemanticSpace](
      space: SpaceEvidence[S],
      valueIdentity: ValueIdentity
  ): RowMeasure[S] =
    val weight = 1.0 / space.dimension
    new RowMeasure(
      space,
      DVec.fromSeq(Vector.fill(space.dimension)(weight)),
      RowMeasureDescriptor(space.descriptor, RowMeasureNormalization.UnitMass(1.0), valueIdentity),
      SemanticProvenance.source("uniform-row-measure")
    )

enum RowFunctionalKind:
  case ProbabilityMeasure
  case MetricDerived

final case class RowFunctionalDescriptor(
    space: MvSpace,
    kind: RowFunctionalKind,
    valueIdentity: ValueIdentity
)

/** A normalized row functional mu with mu*1 = 1. Metric-orthogonal centering may
  * produce signed entries, so this is deliberately broader than [[RowMeasure]].
  */
final class NormalizedRowFunctional[S <: SemanticSpace] private[multivar] (
    val space: SpaceEvidence[S],
    val weights: DVec,
    val descriptor: RowFunctionalDescriptor,
    val provenance: SemanticProvenance
)

object NormalizedRowFunctional:
  def fromMeasure[S <: SemanticSpace](measure: RowMeasure[S]): NormalizedRowFunctional[S] =
    new NormalizedRowFunctional(
      measure.space,
      measure.weights,
      RowFunctionalDescriptor(
        measure.space.descriptor,
        RowFunctionalKind.ProbabilityMeasure,
        measure.descriptor.valueIdentity
      ),
      measure.provenance
    )

  private[multivar] def metricDerived[S <: SemanticSpace](
      space: SpaceEvidence[S],
      weights: DVec,
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance
  ): NormalizedRowFunctional[S] =
    new NormalizedRowFunctional(
      space,
      weights,
      RowFunctionalDescriptor(space.descriptor, RowFunctionalKind.MetricDerived, valueIdentity),
      provenance
    )

final case class CenteringLawCertificate(
    functional: ValueIdentity,
    idempotenceResidual: Double,
    annihilatesOneResidual: Double,
    leftAnnihilationResidual: Double,
    metricSelfAdjointResidual: Option[Double],
    context: CertificateContext
)

final case class CenteredDataCertificate(
    table: ValueIdentity,
    functional: ValueIdentity,
    residual: Double,
    scale: Double,
    context: CertificateContext
)

final class CenteringProjection[S <: SemanticSpace] private (
    val functional: NormalizedRowFunctional[S],
    val lawCertificate: CenteringLawCertificate
):
  def matrix: DMat =
    val size = functional.space.dimension
    val out = DMat.eye(size).copyData
    var row = 0
    while row < size do
      var col = 0
      while col < size do
        out(row * size + col) -= functional.weights(col)
        col += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(size, size, out)

  def applyTo(
      table: MatrixView,
      policy: StoragePolicy = StoragePolicy.Operator
  ): Either[DiagramError, MatrixView] =
    if table.rows != functional.space.dimension then
      Left(
        DiagramError.InvalidCentering(
          s"centering functional has ${functional.space.dimension} rows but table has ${table.rows}"
        )
      )
    else
      val weights = GaleNumerics.matrixFromRowMajor(functional.weights.length, 1, functional.weights.copyData)
      table
        .transposeMultiply(DenseMatrixView(weights))
        .left
        .map(DiagramError.Multivar.apply)
        .flatMap { weightedMeans =>
          val shift = new Array[Double](table.cols)
          var col = 0
          while col < table.cols do
            shift(col) = -weightedMeans(col, 0)
            col += 1
          MatrixView
            .affine(
              table,
              MatrixView.ones(table.cols),
              GaleNumerics.vectorFromArray(shift),
              policy,
              "measure centering"
            )
            .left
            .map(DiagramError.Multivar.apply)
        }

object CenteringProjection:
  def byMeasure[S <: SemanticSpace](
      measure: RowMeasure[S],
      context: CertificateContext = CertificateContext.portableFloat64
  ): Either[DiagramError, CenteringProjection[S]] =
    val functional = NormalizedRowFunctional.fromMeasure(measure)
    certify(functional, None, context).map(new CenteringProjection(functional, _))

  def orthogonal[S <: SemanticSpace](
      metric: MetricForm[S, CertifiedSpd],
      context: CertificateContext = CertificateContext.portableFloat64
  ): Either[DiagramError, CenteringProjection[S]] =
    val size = metric.space.dimension
    val ones = GaleNumerics.matrixFromRowMajor(size, 1, Array.fill(size)(1.0))
    metric.operator(ones).left.map(DiagramError.Semantic.apply).flatMap { applied =>
      val weights = applied.copyData
      var denominator = 0.0
      var index = 0
      while index < weights.length do
        denominator += weights(index)
        index += 1
      if !denominator.isFinite || denominator <= context.tolerance.threshold(Math.abs(denominator)) then
        Left(DiagramError.InvalidCentering(s"row metric gives invalid centering denominator $denominator"))
      else
        index = 0
        while index < weights.length do
          weights(index) /= denominator
          index += 1
        val identity = ValueIdentity.derived(
          "metric-centering-functional",
          metric.operator.valueIdentity,
          ValueIdentity.source(ValueId.unsafe(s"${metric.space.id.value}.ones"))
        )
        val functional = NormalizedRowFunctional.metricDerived(
          metric.space,
          GaleNumerics.vectorFromArray(weights),
          identity,
          metric.provenance.append(
            SemanticProvenanceEvent.Derived("metric-orthogonal-centering-functional", Vector(metric.operator.valueIdentity))
          )
        )
        certify(functional, Some(metric), context).map(new CenteringProjection(functional, _))
    }

  private def certify[S <: SemanticSpace](
      functional: NormalizedRowFunctional[S],
      metric: Option[MetricForm[S, CertifiedSpd]],
      context: CertificateContext
  ): Either[DiagramError, CenteringLawCertificate] =
    val size = functional.space.dimension
    val projection = new Array[Double](size * size)
    var row = 0
    while row < size do
      var col = 0
      while col < size do
        projection(row * size + col) = (if row == col then 1.0 else 0.0) - functional.weights(col)
        col += 1
      row += 1
    val h = GaleNumerics.matrixFromRowMajor(size, size, projection)
    val hSquared = GaleNumerics.multiply(h, h)
    val idempotence = differenceNorm(hSquared, h)
    val ones = GaleNumerics.matrixFromRowMajor(size, 1, Array.fill(size)(1.0))
    val annihilatesOne = frobeniusNorm(GaleNumerics.multiply(h, ones))
    val mu = GaleNumerics.matrixFromRowMajor(1, size, functional.weights.copyData)
    val leftAnnihilation = frobeniusNorm(GaleNumerics.multiply(mu, h))
    val metricResidual = metric.map { value =>
      value.operator(DMat.eye(size)) match
        case Left(_) => Double.PositiveInfinity
        case Right(a) =>
          val left = GaleNumerics.multiply(h.transpose, a)
          val right = GaleNumerics.multiply(a, h)
          differenceNorm(left, right)
    }
    val scale = Math.max(1.0, frobeniusNorm(h))
    val threshold = context.tolerance.threshold(scale)
    if idempotence > threshold || annihilatesOne > threshold || leftAnnihilation > threshold ||
        metricResidual.exists(_ > context.tolerance.threshold(Math.max(scale, 1.0)))
    then
      Left(
        DiagramError.InvalidCentering(
          s"centering projection law residuals exceed tolerance: idempotence=$idempotence, " +
            s"one=$annihilatesOne, left=$leftAnnihilation, metric=$metricResidual"
        )
      )
    else
      Right(
        CenteringLawCertificate(
          functional.descriptor.valueIdentity,
          idempotence,
          annihilatesOne,
          leftAnnihilation,
          metricResidual,
          context
        )
      )

  private def differenceNorm(left: DMat, right: DMat): Double =
    val difference = left.copyData
    val rightValues = right.copyData
    var index = 0
    while index < difference.length do
      difference(index) -= rightValues(index)
      index += 1
    frobeniusNorm(GaleNumerics.matrixFromRowMajor(left.rows, left.cols, difference))

  private def frobeniusNorm(matrix: DMat): Double =
    val values = matrix.copyData
    var sum = 0.0
    var index = 0
    while index < values.length do
      sum += values(index) * values(index)
      index += 1
    Math.sqrt(sum)

object CenteredDataCertificate:
  def certify[Rows <: SemanticSpace, Columns <: SemanticSpace](
      table: Table[Rows, Columns],
      functional: NormalizedRowFunctional[Rows],
      context: CertificateContext = CertificateContext.portableFloat64
  ): Either[DiagramError, CenteredDataCertificate] =
    table(DMat.eye(table.cols)).left.map(DiagramError.Semantic.apply).flatMap { matrix =>
      val residuals = new Array[Double](matrix.cols)
      var row = 0
      while row < matrix.rows do
        var col = 0
        while col < matrix.cols do
          residuals(col) += functional.weights(row) * matrix(row, col)
          col += 1
        row += 1
      var residualSum = 0.0
      var scaleSum = 0.0
      var index = 0
      val values = matrix.copyData
      while index < residuals.length do
        residualSum += residuals(index) * residuals(index)
        index += 1
      index = 0
      while index < values.length do
        scaleSum += values(index) * values(index)
        index += 1
      val residual = Math.sqrt(residualSum)
      val scale = Math.sqrt(scaleSum)
      if residual > context.tolerance.threshold(scale) then
        Left(
          DiagramError.InvalidCentering(
            s"table is not centered by the declared functional: residual $residual exceeds " +
              s"${context.tolerance.threshold(scale)}"
          )
        )
      else
        Right(
          CenteredDataCertificate(
            table.valueIdentity,
            functional.descriptor.valueIdentity,
            residual,
            scale,
            context
          )
        )
    }

sealed trait CenteringPlan[S <: SemanticSpace]
final case class NoCentering[S <: SemanticSpace]() extends CenteringPlan[S]
final case class CenterByMeasure[S <: SemanticSpace](measure: RowMeasure[S]) extends CenteringPlan[S]
final case class CenterOrthogonally[S <: SemanticSpace](metric: MetricForm[S, CertifiedSpd]) extends CenteringPlan[S]
final case class AlreadyCenteredBy[S <: SemanticSpace](
    functional: NormalizedRowFunctional[S],
    certificate: CenteredDataCertificate
) extends CenteringPlan[S]

opaque type SupportThreshold = Double

object SupportThreshold:
  def apply(value: Double): Either[DiagramError, SupportThreshold] =
    if value.isFinite && value >= 0.0 then Right(value)
    else Left(DiagramError.InvalidGeometry(s"support threshold must be finite and non-negative, got $value"))

  val default: SupportThreshold = 1e-10

  extension (value: SupportThreshold)
    inline def toDouble: Double = value

opaque type GeometryRidge = Double

object GeometryRidge:
  def apply(value: Double): Either[DiagramError, GeometryRidge] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(DiagramError.InvalidGeometry(s"geometry ridge must be finite and positive, got $value"))

  extension (value: GeometryRidge)
    inline def toDouble: Double = value

enum SingularGeometryPolicy:
  case RejectSingularGeometry
  case RestrictToSupport(threshold: SupportThreshold)
  case RegularizeWith(amount: GeometryRidge)
  case WorkInQuotientSpace(threshold: SupportThreshold)

final case class GeometryPolicies(
    rows: SingularGeometryPolicy,
    columns: SingularGeometryPolicy
)

object GeometryPolicies:
  val reject: GeometryPolicies =
    GeometryPolicies(
      SingularGeometryPolicy.RejectSingularGeometry,
      SingularGeometryPolicy.RejectSingularGeometry
    )

final class DiagramGeometry[S <: SemanticSpace] private (
    val operator: Lin[Primal[S], Dual[S]],
    val space: SpaceEvidence[S],
    val descriptor: FormDescriptor
):
  def certificates: Vector[NumericalCertificate] =
    descriptor.certificates

  def isSpd: Boolean =
    descriptor.positivity == PositivityStatus.Spd

object DiagramGeometry:
  def metric[S <: SemanticSpace](form: MetricForm[S, CertifiedSpd]): DiagramGeometry[S] =
    new DiagramGeometry(form.operator, form.space, form.descriptor)

  def semiMetric[S <: SemanticSpace](form: SemiMetric[S, CertifiedPsd]): DiagramGeometry[S] =
    new DiagramGeometry(form.operator, form.space, form.descriptor)

final case class DiagramCore[Rows <: SemanticSpace, Columns <: SemanticSpace] private (
    table: Table[Rows, Columns],
    rowGeometry: DiagramGeometry[Rows],
    columnGeometry: DiagramGeometry[Columns]
)

object DiagramCore:
  def from[Rows <: SemanticSpace, Columns <: SemanticSpace](
      table: Table[Rows, Columns],
      rowGeometry: DiagramGeometry[Rows],
      columnGeometry: DiagramGeometry[Columns]
  ): Either[DiagramError, DiagramCore[Rows, Columns]] =
    if table.codomain.descriptor.space != rowGeometry.space.descriptor then
      Left(
        DiagramError.InvalidGeometry(
          s"table row space '${table.codomain.descriptor.space.id.value}' does not match row geometry " +
            s"'${rowGeometry.space.id.value}'"
        )
      )
    else if table.domain.descriptor.space != columnGeometry.space.descriptor then
      Left(
        DiagramError.InvalidGeometry(
          s"table column space '${table.domain.descriptor.space.id.value}' does not match column geometry " +
            s"'${columnGeometry.space.id.value}'"
        )
      )
    else Right(DiagramCore(table, rowGeometry, columnGeometry))

final case class DiagramPreparation[S <: SemanticSpace] private (
    rowMeasure: Option[RowMeasure[S]],
    centering: CenteringPlan[S],
    geometryPolicies: GeometryPolicies
)

object DiagramPreparation:
  def noCentering[S <: SemanticSpace](
      rowMeasure: Option[RowMeasure[S]],
      geometryPolicies: GeometryPolicies
  ): DiagramPreparation[S] =
    DiagramPreparation(rowMeasure, NoCentering(), geometryPolicies)

  def centerByMeasure[S <: SemanticSpace](
      measure: RowMeasure[S],
      geometryPolicies: GeometryPolicies
  ): DiagramPreparation[S] =
    DiagramPreparation(Some(measure), CenterByMeasure(measure), geometryPolicies)

  def centerOrthogonally[S <: SemanticSpace](
      metric: MetricForm[S, CertifiedSpd],
      rowMeasure: Option[RowMeasure[S]],
      geometryPolicies: GeometryPolicies
  ): DiagramPreparation[S] =
    DiagramPreparation(rowMeasure, CenterOrthogonally(metric), geometryPolicies)

  def alreadyCenteredBy[S <: SemanticSpace](
      functional: NormalizedRowFunctional[S],
      certificate: CenteredDataCertificate,
      rowMeasure: Option[RowMeasure[S]],
      geometryPolicies: GeometryPolicies
  ): Either[DiagramError, DiagramPreparation[S]] =
    if certificate.functional != functional.descriptor.valueIdentity then
      Left(DiagramError.InvalidCentering("already-centered certificate belongs to a different row functional"))
    else
      Right(
        DiagramPreparation(
          rowMeasure,
          AlreadyCenteredBy(functional, certificate),
          geometryPolicies
        )
      )

sealed trait CellDataState
sealed trait CompleteCells extends CellDataState
sealed trait MissingCells extends CellDataState

enum CellDataKind:
  case Complete
  case Missing(maskIdentity: ValueIdentity)

final case class CellDataSemantics[D <: CellDataState] private (kind: CellDataKind)

object CellDataSemantics:
  val complete: CellDataSemantics[CompleteCells] =
    CellDataSemantics(CellDataKind.Complete)

  def missing(maskIdentity: ValueIdentity): CellDataSemantics[MissingCells] =
    CellDataSemantics(CellDataKind.Missing(maskIdentity))

enum CertificateValidity:
  case Preserved
  case Invalidated

final case class CertificateEffect(
    property: String,
    validity: CertificateValidity,
    reason: String
)

final case class DiagramAuditRecord(
    operation: String,
    affectedSpaces: Vector[SpaceId],
    method: String,
    certificateEffects: Vector[CertificateEffect]
)

final case class DiagramAudit(records: Vector[DiagramAuditRecord]):
  def append(record: DiagramAuditRecord): DiagramAudit =
    copy(records = records :+ record)

object DiagramAudit:
  val empty: DiagramAudit =
    DiagramAudit(Vector.empty)

final class SemanticDualityDiagram[
    Rows <: SemanticSpace,
    Columns <: SemanticSpace,
    Data <: CellDataState
] private (
    val core: DiagramCore[Rows, Columns],
    val preparation: DiagramPreparation[Rows],
    val dataSemantics: CellDataSemantics[Data],
    val audit: DiagramAudit,
    val provenance: SemanticProvenance
):
  def withCentering(
      updated: DiagramPreparation[Rows]
  ): SemanticDualityDiagram[Rows, Columns, Data] =
    new SemanticDualityDiagram(
      core,
      updated,
      dataSemantics,
      audit.append(
        DiagramAuditRecord(
          "with-centering",
          Vector(core.rowGeometry.space.id),
          "replace-centering-plan",
          Vector(CertificateEffect("prepared-table", CertificateValidity.Invalidated, "centering plan changed"))
        )
      ),
      provenance.append(SemanticProvenanceEvent.Derived("with-centering", Vector(core.table.valueIdentity)))
    )

  def withRowGeometry(
      updated: DiagramGeometry[Rows]
  ): Either[DiagramError, SemanticDualityDiagram[Rows, Columns, Data]] =
    DiagramCore.from(core.table, updated, core.columnGeometry).map { nextCore =>
      new SemanticDualityDiagram(
        nextCore,
        preparation,
        dataSemantics,
        audit.append(
          DiagramAuditRecord(
            "with-row-geometry",
            Vector(updated.space.id),
            "replace-row-form",
            Vector(
              CertificateEffect("row-geometry", CertificateValidity.Invalidated, "row geometry changed"),
              CertificateEffect("metric-orthogonal-centering", CertificateValidity.Invalidated, "row geometry changed")
            )
          )
        ),
        provenance.append(SemanticProvenanceEvent.Derived("with-row-geometry", Vector(updated.operator.valueIdentity)))
      )
    }

  def withColumnGeometry(
      updated: DiagramGeometry[Columns]
  ): Either[DiagramError, SemanticDualityDiagram[Rows, Columns, Data]] =
    DiagramCore.from(core.table, core.rowGeometry, updated).map { nextCore =>
      new SemanticDualityDiagram(
        nextCore,
        preparation,
        dataSemantics,
        audit.append(
          DiagramAuditRecord(
            "with-column-geometry",
            Vector(updated.space.id),
            "replace-column-form",
            Vector(CertificateEffect("column-geometry", CertificateValidity.Invalidated, "column geometry changed"))
          )
        ),
        provenance.append(SemanticProvenanceEvent.Derived("with-column-geometry", Vector(updated.operator.valueIdentity)))
      )
    }

object SemanticDualityDiagram:
  def from[
      Rows <: SemanticSpace,
      Columns <: SemanticSpace,
      Data <: CellDataState
  ](
      core: DiagramCore[Rows, Columns],
      preparation: DiagramPreparation[Rows],
      dataSemantics: CellDataSemantics[Data],
      provenance: SemanticProvenance = SemanticProvenance.source("semantic-duality-diagram")
  ): Either[DiagramError, SemanticDualityDiagram[Rows, Columns, Data]] =
    preparation.centering match
      case AlreadyCenteredBy(_, certificate) if certificate.table != core.table.valueIdentity =>
        Left(DiagramError.InvalidCentering("already-centered certificate belongs to a different table"))
      case CenterOrthogonally(metric) if metric.space.descriptor != core.rowGeometry.space.descriptor =>
        Left(DiagramError.InvalidCentering("orthogonal centering metric belongs to a different row space"))
      case _ =>
        Right(new SemanticDualityDiagram(core, preparation, dataSemantics, DiagramAudit.empty, provenance))

final class SupportRestriction[S <: SemanticSpace] private (
    val sourceSpace: SpaceEvidence[S],
    val effectiveSpace: SpaceRef
)(
    val embedding: Lin[Primal[effectiveSpace.Id], Primal[S]],
    val restriction: Lin[Primal[S], Primal[effectiveSpace.Id]],
    val reducedMetric: MetricForm[effectiveSpace.Id, CertifiedSpd],
    val embeddingMatrix: DMat,
    val restrictionMatrix: DMat,
    val discardedNullity: Int,
    val threshold: SupportThreshold,
    val rankCertificate: Certificate[RankProperty]
)

object SupportRestriction:
  def fromGeometry[S <: SemanticSpace](
      geometry: DiagramGeometry[S],
      threshold: SupportThreshold,
      axis: IndexAxis,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen
  ): Either[DiagramError, SupportRestriction[S]] =
    for
      matrix <- DiagramNumerics.formMatrix(geometry)
      eigen <- LinalgErrorAdapter
        .adapt(eigenSolver.decompose(MatrixOps.symmetrize(matrix)))
        .left
        .map(DiagramError.Multivar.apply)
      largest = Math.max(eigen.values(0), 0.0)
      cutoff = threshold.toDouble * Math.max(1.0, largest)
      rank = numericalRank(eigen.values, cutoff)
      _ <-
        if rank > 0 then Right(())
        else Left(DiagramError.EmptySupport(axis, cutoff))
      restriction <- build(geometry, eigen, rank, cutoff, threshold)
    yield restriction

  private def build[S <: SemanticSpace](
      geometry: DiagramGeometry[S],
      eigen: SymmetricEigenResult,
      rank: Int,
      cutoff: Double,
      threshold: SupportThreshold
  ): Either[DiagramError, SupportRestriction[S]] =
    val thresholdKey = threshold.toDouble.toString.replace('.', '_').replace('-', 'm')
    val supportKey = geometry.operator.valueIdentity.stableKey
    val effectiveDescriptor = MvSpace(
      SpaceId.unsafe(s"${geometry.space.id.value}.support.$supportKey.r$rank.t$thresholdKey"),
      geometry.space.descriptor.role,
      Dimension.unsafe(rank)
    )
    val effective = SpaceRef(effectiveDescriptor)
    type Effective = effective.Id
    val embeddingMatrix = MatrixOps.takeColumns(eigen.vectors, rank)
    val restrictionMatrix = embeddingMatrix.transpose
    val embeddingIdentity = ValueIdentity.source(
      ValueId.unsafe(s"${geometry.space.id.value}.support.$supportKey.embedding.$rank.$thresholdKey")
    )
    val restrictionIdentity = embeddingIdentity.star
    for
      embedding <- Lin
        .fromDenseMatrix[Primal[Effective], Primal[S]](
          embeddingMatrix,
          CoordinateEvidence.primal(effective.evidence),
          CoordinateEvidence.primal(geometry.space),
          embeddingIdentity,
          SemanticProvenance.source("support-embedding")
        )
        .left
        .map(DiagramError.Semantic.apply)
      restriction <- Lin
        .fromDenseMatrix[Primal[S], Primal[Effective]](
          restrictionMatrix,
          CoordinateEvidence.primal(geometry.space),
          CoordinateEvidence.primal(effective.evidence),
          restrictionIdentity,
          SemanticProvenance.source("support-restriction")
        )
        .left
        .map(DiagramError.Semantic.apply)
      reducedLegacy <- MetricSpec
        .diagonal(
          MatrixOps.takeVector(eigen.values, rank),
          Some(effectiveDescriptor)
        )
        .left
        .map(DiagramError.Multivar.apply)
      reducedOperator <- FormOperator
        .primal(
          reducedLegacy,
          effective.evidence,
          ValueIdentity.source(
            ValueId.unsafe(s"${geometry.space.id.value}.support.$supportKey.metric.$rank.$thresholdKey")
          )
        )
        .left
        .map(DiagramError.Semantic.apply)
      spd <- FormCertificates.spd(reducedOperator).left.map(DiagramError.Semantic.apply)
      reduced <- Form.metric(reducedOperator, effective.evidence, spd).left.map(DiagramError.Semantic.apply)
      supportTolerance <- CertificateTolerance
        .from(0.0, threshold.toDouble)
        .left
        .map(DiagramError.Semantic.apply)
      supportContext <- CertificateContext
        .from(
          supportTolerance,
          CertificateNorm.Spectral,
          "support-eigendecomposition",
          "gale",
          NumericalPrecision.Float64
        )
        .left
        .map(DiagramError.Semantic.apply)
      rankCertificate <- Certificate
        .rank(
          geometry.operator.valueIdentity,
          rank,
          geometry.space.dimension,
          0.0,
          Math.max(1.0, eigen.values(0)),
          supportContext
        )
        .left
        .map(DiagramError.Semantic.apply)
    yield
      new SupportRestriction(geometry.space, effective)(
        embedding,
        restriction,
        reduced,
        embeddingMatrix,
        restrictionMatrix,
        geometry.space.dimension - rank,
        threshold,
        rankCertificate
      )

  private def numericalRank(values: DVec, cutoff: Double): Int =
    var rank = 0
    while rank < values.length && values(rank) > cutoff do
      rank += 1
    rank

enum GeometryResolutionKind:
  case Strict
  case Regularized(amount: GeometryRidge)
  case Restricted
  case Quotient

final case class GeometryResolution[S <: SemanticSpace](
    original: DiagramGeometry[S],
    kind: GeometryResolutionKind,
    effectiveSpace: MvSpace,
    effectiveMetric: MetricSpec,
    support: Option[SupportRestriction[S]],
    certificates: Vector[NumericalCertificate]
)

object GeometryResolution:
  def resolve[S <: SemanticSpace](
      geometry: DiagramGeometry[S],
      policy: SingularGeometryPolicy,
      axis: IndexAxis,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen
  ): Either[DiagramError, GeometryResolution[S]] =
    if geometry.isSpd then
      DiagramNumerics.legacyMetric(geometry, StoragePolicy.AllowDense).map { metric =>
        GeometryResolution(
          geometry,
          GeometryResolutionKind.Strict,
          geometry.space.descriptor,
          metric,
          None,
          geometry.certificates
        )
      }
    else
      policy match
        case SingularGeometryPolicy.RejectSingularGeometry =>
          SupportRestriction.fromGeometry(geometry, SupportThreshold.default, axis, eigenSolver).flatMap { support =>
            Left(DiagramError.SingularGeometryRejected(axis, support.discardedNullity))
          }
        case SingularGeometryPolicy.RestrictToSupport(threshold) =>
          fromSupport(geometry, threshold, axis, GeometryResolutionKind.Restricted, eigenSolver)
        case SingularGeometryPolicy.WorkInQuotientSpace(threshold) =>
          fromSupport(geometry, threshold, axis, GeometryResolutionKind.Quotient, eigenSolver)
        case SingularGeometryPolicy.RegularizeWith(amount) =>
          regularize(geometry, amount, eigenSolver)

  private def fromSupport[S <: SemanticSpace](
      geometry: DiagramGeometry[S],
      threshold: SupportThreshold,
      axis: IndexAxis,
      kind: GeometryResolutionKind,
      eigenSolver: SymmetricEigenSolver
  ): Either[DiagramError, GeometryResolution[S]] =
    SupportRestriction.fromGeometry(geometry, threshold, axis, eigenSolver).flatMap { support =>
      DiagramNumerics.legacyMetric(
        DiagramGeometry.metric(support.reducedMetric),
        StoragePolicy.AllowDense
      ).map { metric =>
        GeometryResolution(
          geometry,
          kind,
          support.effectiveSpace.descriptor,
          metric,
          Some(support),
          geometry.certificates :+ support.rankCertificate.runtime
        )
      }
    }

  private def regularize[S <: SemanticSpace](
      geometry: DiagramGeometry[S],
      amount: GeometryRidge,
      eigenSolver: SymmetricEigenSolver
  ): Either[DiagramError, GeometryResolution[S]] =
    for
      matrix <- DiagramNumerics.formMatrix(geometry)
      regularized = matrix.addToDiagonal(amount.toDouble)
      legacy <- MetricSpec
        .denseSymmetric(regularized, MetricValidation.Structural, Some(geometry.space.descriptor))
        .left
        .map(DiagramError.Multivar.apply)
      operator <- FormOperator
        .primal(
          legacy,
          geometry.space,
          ValueIdentity.derived(
            "ridge-regularization",
            geometry.operator.valueIdentity,
            ValueIdentity.source(ValueId.unsafe(s"ridge.${amount.toDouble}".replace('.', '_')))
          ),
          geometry.operator.provenance.append(
            SemanticProvenanceEvent.Derived("ridge-regularization", Vector(geometry.operator.valueIdentity))
          )
        )
        .left
        .map(DiagramError.Semantic.apply)
      certificateContext <- CertificateContext
        .from(
          CertificateTolerance.strict,
          CertificateNorm.Frobenius,
          "ridge-spd-check",
          "gale",
          NumericalPrecision.Float64,
          Some(s"ridge=${amount.toDouble}")
        )
        .left
        .map(DiagramError.Semantic.apply)
      certificate <- FormCertificates
        .spd(operator, certificateContext, eigenSolver)
        .left
        .map(DiagramError.Semantic.apply)
      metric <- Form.metric(operator, geometry.space, certificate).left.map(DiagramError.Semantic.apply)
    yield
      GeometryResolution(
        geometry,
        GeometryResolutionKind.Regularized(amount),
        geometry.space.descriptor,
        legacy,
        None,
        geometry.certificates ++ metric.descriptor.certificates
      )

private[multivar] object DiagramNumerics:
  def formMatrix[S <: SemanticSpace](geometry: DiagramGeometry[S]): Either[DiagramError, DMat] =
    geometry.operator(DMat.eye(geometry.space.dimension)).left.map(DiagramError.Semantic.apply)

  def legacyMetric[S <: SemanticSpace](
      geometry: DiagramGeometry[S],
      policy: StoragePolicy
  ): Either[DiagramError, MetricSpec] =
    geometry.operator.kernel match
      case MetricKernel(metric) => Right(metric)
      case _ if policy != StoragePolicy.AllowDense =>
        Left(DiagramError.DensificationRequired("geometry adaptation"))
      case _ =>
        formMatrix(geometry).flatMap(
          MetricSpec
            .denseSymmetric(_, MetricValidation.Structural, Some(geometry.space.descriptor))
            .left
            .map(DiagramError.Multivar.apply)
        )

  def tableView[Rows <: SemanticSpace, Columns <: SemanticSpace](
      table: Table[Rows, Columns],
      policy: StoragePolicy
  ): Either[DiagramError, MatrixView] =
    table.kernel match
      case MatrixViewKernel(view) => Right(view)
      case _ if policy != StoragePolicy.AllowDense =>
        Left(DiagramError.DensificationRequired("table adaptation"))
      case _ =>
        table(DMat.eye(table.cols)).left.map(DiagramError.Semantic.apply).map(DenseMatrixView(_))
