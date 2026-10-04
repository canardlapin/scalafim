package scalafim.fmri.mvpa.decomposition

import gale.linalg.DMat
import multivar.core.*
import multivar.family.paired.*
import multivar.family.spectral.*
import scala.util.control.NonFatal
import scalafim.fmri.mvpa.{AxisDescriptor, AxisDigest, AxisRecord, AxisRef, EvidenceError, Observations}
import scalafim.fmri.mvpa.analysis.{ObservationProductWork, PreparedObservationProduct}
import scalafim.fmri.mvpa.relation.{Relation, RelationAccess}
import scalafim.fmri.mvpa.relation.{EffectForm as RelationEffectForm, NeuralForm as RelationNeuralForm}

enum DecompositionPurpose:
  case Descriptive

enum CrossFormObject:
  case Effect, Neural

/** Admission ceiling for the declared dense materializations and retained
  * factors. Solver-private workspace, provider memory and process RSS are
  * outside this ceiling; it is not a whole-fit peak-memory certificate. */
final case class DecompositionBudget(maximumCells: Long):
  require(maximumCells >= 0L)

enum GlobalDecompositionError:
  case Budget(required: BigInt, allowed: Long)
  case Capacity(required: BigInt)
  case Axis(field: String)
  case Access(detail: String)
  case Evidence(error: SemanticError)
  case Multivar(error: MultivarError)
  case Replay(error: EvidenceError)

/** The SVD's left and right matrices retain their actual nominal endpoints.
  * They make no covariance, PSD, or signed-eigenvalue claim. */
final class SvdLeftFactor[E <: SemanticSpace] private (
    val space: SpaceEvidence[E], val axis: AxisDescriptor, val record: AxisRecord, val components: Int, val values: DMat
)
object SvdLeftFactor:
  private[decomposition] def apply[K](axis: AxisRef[K], components: Int, values: DMat): SvdLeftFactor[axis.Id] =
    require(values.rows == axis.size && values.cols == components)
    new SvdLeftFactor(axis.evidence, axis.descriptor, axis.toRecord, components, values)

final class SvdRightFactor[N <: SemanticSpace] private (
    val space: SpaceEvidence[N], val axis: AxisDescriptor, val record: AxisRecord, val components: Int, val values: DMat
)
object SvdRightFactor:
  private[decomposition] def apply[K](axis: AxisRef[K], components: Int, values: DMat): SvdRightFactor[axis.Id] =
    require(values.rows == axis.size && values.cols == components)
    new SvdRightFactor(axis.evidence, axis.descriptor, axis.toRecord, components, values)

final case class ObservationPcaArtifact[S <: SemanticSpace, N <: SemanticSpace] private[decomposition] (
    samples: SpaceEvidence[S], neural: SpaceEvidence[N], sampleAxis: AxisDescriptor, neuralAxis: AxisDescriptor, fit: PcaFit,
    centering: PreprocessSpec, evidenceIdentity: String, contentIdentity: String, work: ObservationProductWork
):
  /** Ordinary PCA uses Euclidean sample and neural coordinates after this
    * explicitly retained preprocessing policy. */
  val metric: String = "Euclidean sample and neural coordinates"
  val purpose: DecompositionPurpose = DecompositionPurpose.Descriptive

final case class PairedLatentArtifact[
    SR <: SemanticSpace, TR <: SemanticSpace, SF <: SemanticSpace, TF <: SemanticSpace
](
    sourceRows: SpaceEvidence[SR], targetRows: SpaceEvidence[TR],
    sourceFeatures: SpaceEvidence[SF], targetFeatures: SpaceEvidence[TF],
    fit: PairedOperatorFit[SF, TF, ? <: SemanticSpace], method: PairedLatentMethod,
    scale: Double, geometry: PairedGeometryReceipt[SR, TR], evidenceIdentity: String, crossContentIdentity: String
):
  val purpose: DecompositionPurpose = DecompositionPurpose.Descriptive

/** Geometry receipts contain no live operators. They preserve the actual row
  * domains, value declarations, numerical claims and transformation provenance
  * defining the marginals and pairing after a provider scope closes.
  */
final case class RowGeometryReceipt[S <: SemanticSpace, T <: SemanticSpace](
    source: SpaceEvidence[S], target: SpaceEvidence[T], valueIdentity: ValueIdentity,
    representation: OperatorRepresentation, evidence: EvidenceStatus,
    numericalClaims: Vector[NumericalCertificate], provenance: SemanticProvenance
)
final case class PairedGeometryReceipt[SR <: SemanticSpace, TR <: SemanticSpace](
    sourceMarginal: RowGeometryReceipt[SR, SR], targetMarginal: RowGeometryReceipt[TR, TR],
    relationship: RowGeometryReceipt[SR, TR]
)

final case class RelationSvdArtifact[E <: SemanticSpace, N <: SemanticSpace] private[decomposition] (
    effects: SpaceEvidence[E], neural: SpaceEvidence[N], effectAxis: AxisDescriptor, neuralAxis: AxisDescriptor, fit: SvdFit,
    left: SvdLeftFactor[E], right: SvdRightFactor[N],
    origins: scalafim.fmri.mvpa.relation.RelationOrigins, contentIdentity: String
):
  val purpose: DecompositionPurpose = DecompositionPurpose.Descriptive

final case class CrossFormSvdArtifact[L <: SemanticSpace, R <: SemanticSpace](
    left: SpaceEvidence[L], right: SpaceEvidence[R], fit: SvdFit,
    leftScores: DMat, rightLoadings: DMat, decomposedObject: CrossFormObject,
    leftOrigins: scalafim.fmri.mvpa.relation.RelationOrigins, rightOrigins: scalafim.fmri.mvpa.relation.RelationOrigins,
    contentIdentity: String
):
  val purpose: DecompositionPurpose = DecompositionPurpose.Descriptive
  val metric: String = "Euclidean left and right coordinates; no centering"

object GlobalDecompositions:
  /** Fits PCA only while an already admitted observation product is live. The
    * returned Multivar fit owns dense matrices and does not retain an operator.
    */
  def pca[SK, NK](
      samples: AxisRef[SK], neural: AxisRef[NK], product: PreparedObservationProduct[samples.Id, neural.Id],
      components: Int, centering: PreprocessSpec, budget: DecompositionBudget
  ): Either[GlobalDecompositionError, ObservationPcaArtifact[samples.Id, neural.Id]] =
    if product.observations.sampleAxis != samples.descriptor || product.observations.neuralAxis != neural.descriptor then
      Left(GlobalDecompositionError.Axis("PCA observations"))
    else if product.observations.rows < 2 || components <= 0 || components > math.min(samples.size, neural.size) then
      Left(GlobalDecompositionError.Axis("PCA rows or component count"))
    else
      try product.fit:
        materialize(product.observations, budget).flatMap: values =>
          Pca.fit(values, components, centering).left.map(error => GlobalDecompositionError.Multivar(error)).map: fit =>
            ObservationPcaArtifact(samples.evidence, neural.evidence, samples.descriptor, neural.descriptor, fit, centering, product.evidenceIdentity, contentIdentity(values), product.work)
      catch case NonFatal(error) => Left(GlobalDecompositionError.Access(Option(error.getMessage).getOrElse(error.getClass.getSimpleName)))

  /** PLSC has no implicit pairing: both marginal geometries and the cross-row
    * relationship are explicit pinned-Multivar operators. */
  def plsc[SR <: SemanticSpace, TR <: SemanticSpace, SF <: SemanticSpace, TF <: SemanticSpace,
      ES <: OperatorEvidence, ET <: OperatorEvidence, EL <: OperatorEvidence](
      sourceRows: SpaceEvidence[SR], targetRows: SpaceEvidence[TR],
      sourceFeatures: SpaceEvidence[SF], targetFeatures: SpaceEvidence[TF],
      source: OpTable[SR, SF, UncheckedEvidence], target: OpTable[TR, TF, UncheckedEvidence],
      sourceGeometry: OpRowLink[SR, SR, ES], targetGeometry: OpRowLink[TR, TR, ET],
      relationship: OpRowLink[SR, TR, EL], sourceAccess: RelationAccess, targetAccess: RelationAccess,
      components: ComponentCount, crossScale: Double, budget: DecompositionBudget
  ): Either[GlobalDecompositionError, PairedLatentArtifact[SR, TR, SF, TF]] =
    pairedAdmission(sourceRows, targetRows, sourceFeatures, targetFeatures, components, crossScale, sourceAccess, targetAccess, budget).flatMap: _ =>
      val problem = PairedOperatorProblem.fromTables(sourceRows, targetRows, sourceFeatures, targetFeatures,
        source, target, sourceGeometry, targetGeometry, relationship)
      problem.fitPlsc(components, crossScale).left.map(error => GlobalDecompositionError.Multivar(error)).flatMap: fit =>
        ownPairedFit(fit).map: (owned, crossContent) =>
          PairedLatentArtifact(sourceRows, targetRows, sourceFeatures, targetFeatures, owned, PairedLatentMethod.Plsc,
            crossScale, geometryReceipt(sourceRows, targetRows, sourceGeometry, targetGeometry, relationship),
            pairedIdentity(source, target, sourceGeometry, targetGeometry, relationship), crossContent)

  /** CCA uses the same explicit pairing but additionally retains both marginal
    * covariance geometries through the supplied regularization. */
  def cca[SR <: SemanticSpace, TR <: SemanticSpace, SF <: SemanticSpace, TF <: SemanticSpace,
      ES <: OperatorEvidence, ET <: OperatorEvidence, EL <: OperatorEvidence](
      sourceRows: SpaceEvidence[SR], targetRows: SpaceEvidence[TR],
      sourceFeatures: SpaceEvidence[SF], targetFeatures: SpaceEvidence[TF],
      source: OpTable[SR, SF, UncheckedEvidence], target: OpTable[TR, TF, UncheckedEvidence],
      sourceGeometry: OpRowLink[SR, SR, ES], targetGeometry: OpRowLink[TR, TR, ET],
      relationship: OpRowLink[SR, TR, EL], components: ComponentCount,
      regularization: CcaRegularization, covarianceScale: Double,
      sourceAccess: RelationAccess, targetAccess: RelationAccess, budget: DecompositionBudget
  ): Either[GlobalDecompositionError, PairedLatentArtifact[SR, TR, SF, TF]] =
    pairedAdmission(sourceRows, targetRows, sourceFeatures, targetFeatures, components, covarianceScale, sourceAccess, targetAccess, budget).flatMap: _ =>
      val problem = PairedOperatorProblem.fromTables(sourceRows, targetRows, sourceFeatures, targetFeatures,
        source, target, sourceGeometry, targetGeometry, relationship)
      problem.fitCca(components, regularization, covarianceScale).left.map(error => GlobalDecompositionError.Multivar(error)).flatMap: fit =>
        ownPairedFit(fit).map: (owned, crossContent) =>
          PairedLatentArtifact(sourceRows, targetRows, sourceFeatures, targetFeatures, owned, PairedLatentMethod.Cca(regularization),
            covarianceScale, geometryReceipt(sourceRows, targetRows, sourceGeometry, targetGeometry, relationship),
            pairedIdentity(source, target, sourceGeometry, targetGeometry, relationship), crossContent)

  /** Neutral rectangular SVD of one admitted relation estimate. `Pass` keeps
    * signs and offsets as supplied; this is not a covariance decomposition. */
  def svd[EK, NK](
      effects: AxisRef[EK], neural: AxisRef[NK], relation: Relation[effects.Id, neural.Id],
      components: Int, budget: DecompositionBudget
  ): Either[GlobalDecompositionError, RelationSvdArtifact[effects.Id, neural.Id]] =
    if relation.effectAxis != effects.descriptor || relation.neuralAxis != neural.descriptor then
      Left(GlobalDecompositionError.Axis("relation SVD endpoints"))
    else if components <= 0 || components > math.min(effects.size, neural.size) then
      Left(GlobalDecompositionError.Axis("relation SVD component count"))
    else RelationAccess.admitImmediate(relation.origins.access).left.map(error => GlobalDecompositionError.Replay(error)).flatMap: _ =>
      materializeRelation(relation, budget).flatMap: values =>
        Svd.fit(values, components, PreprocessSpec.Pass).left.map(error => GlobalDecompositionError.Multivar(error)).map: fit =>
          val rank = fit.effectiveComponents
          RelationSvdArtifact(effects.evidence, neural.evidence, effects.descriptor, neural.descriptor, fit,
            SvdLeftFactor(effects, rank, fit.scores), SvdRightFactor(neural, rank, fit.loadings), relation.origins, contentIdentity(values))

  /** Neutral SVD overload for the actual signed relational effect form. */
  def svd[L <: SemanticSpace, R <: SemanticSpace](
      form: RelationEffectForm[L, R], components: Int, budget: DecompositionBudget
  ): Either[GlobalDecompositionError, CrossFormSvdArtifact[L, R]] =
    if components <= 0 || components > math.min(form.left.dimension, form.right.dimension) then Left(GlobalDecompositionError.Axis("effect form SVD component count"))
    else RelationAccess.admitImmediate(form.leftOrigins.access).left.map(error => GlobalDecompositionError.Replay(error)).flatMap: _ =>
      RelationAccess.admitImmediate(form.rightOrigins.access).left.map(error => GlobalDecompositionError.Replay(error)).flatMap: _ =>
        materializeTable(form.values, form.left.dimension, form.right.dimension, budget).flatMap: values =>
          Svd.fit(values, components, PreprocessSpec.Pass).left.map(error => GlobalDecompositionError.Multivar(error)).map: fit =>
            CrossFormSvdArtifact(form.left, form.right, fit, fit.scores, fit.loadings, CrossFormObject.Effect, form.leftOrigins, form.rightOrigins, contentIdentity(values))

  /** Neutral SVD overload for the actual signed relational neural form. */
  def svd[L <: SemanticSpace, R <: SemanticSpace](
      form: RelationNeuralForm[L, R], components: Int, budget: DecompositionBudget
  ): Either[GlobalDecompositionError, CrossFormSvdArtifact[L, R]] =
    if components <= 0 || components > math.min(form.left.dimension, form.right.dimension) then Left(GlobalDecompositionError.Axis("neural form SVD component count"))
    else RelationAccess.admitImmediate(form.leftOrigins.access).left.map(error => GlobalDecompositionError.Replay(error)).flatMap: _ =>
      RelationAccess.admitImmediate(form.rightOrigins.access).left.map(error => GlobalDecompositionError.Replay(error)).flatMap: _ =>
        materializeTable(form.values, form.left.dimension, form.right.dimension, budget).flatMap: values =>
          Svd.fit(values, components, PreprocessSpec.Pass).left.map(error => GlobalDecompositionError.Multivar(error)).map: fit =>
            CrossFormSvdArtifact(form.left, form.right, fit, fit.scores, fit.loadings, CrossFormObject.Neural, form.leftOrigins, form.rightOrigins, contentIdentity(values))

  private def materialize[S <: SemanticSpace, N <: SemanticSpace](
      observations: Observations[S, N], budget: DecompositionBudget
  ): Either[GlobalDecompositionError, DMat] =
    checkedCells(observations.rows, observations.columns, budget).flatMap: _ =>
      val data = new Array[Double](observations.rows * observations.columns)
      var column = 0
      var failure: Option[GlobalDecompositionError] = None
      while column < observations.columns && failure.isEmpty do
        val basis = DMat.tabulate(observations.columns, 1)((index, _) => if index == column then 1.0 else 0.0)
        observations.patterns(basis) match
          case Left(error) => failure = Some(GlobalDecompositionError.Evidence(error))
          case Right(value) =>
            var row = 0
            while row < observations.rows do
              data(row * observations.columns + column) = value(row, 0)
              row += 1
        column += 1
      failure.toLeft(DMat.dense(observations.rows, observations.columns, data.toVector))

  private def materializeRelation[E <: SemanticSpace, N <: SemanticSpace](
      relation: Relation[E, N], budget: DecompositionBudget
  ): Either[GlobalDecompositionError, DMat] =
    checkedCells(relation.estimate.rows, relation.estimate.cols, budget).flatMap: _ =>
      val data = new Array[Double](relation.estimate.rows * relation.estimate.cols)
      var column = 0
      var failure: Option[GlobalDecompositionError] = None
      while column < relation.estimate.cols && failure.isEmpty do
        val basis = DMat.tabulate(relation.estimate.cols, 1)((index, _) => if index == column then 1.0 else 0.0)
        relation.estimate(basis) match
          case Left(error) => failure = Some(GlobalDecompositionError.Evidence(error))
          case Right(value) =>
            var row = 0
            while row < relation.estimate.rows do
              data(row * relation.estimate.cols + column) = value(row, 0)
              row += 1
        column += 1
      failure.toLeft(DMat.dense(relation.estimate.rows, relation.estimate.cols, data.toVector))

  private def materializeTable[L <: SemanticSpace, R <: SemanticSpace](
      table: Table[L, R], rows: Int, columns: Int, budget: DecompositionBudget
  ): Either[GlobalDecompositionError, DMat] =
    checkedCells(rows, columns, budget).flatMap: _ =>
      val data = new Array[Double](rows * columns)
      var column = 0
      var failure: Option[GlobalDecompositionError] = None
      while column < columns && failure.isEmpty do
        val basis = DMat.tabulate(columns, 1)((index, _) => if index == column then 1.0 else 0.0)
        table(basis) match
          case Left(error) => failure = Some(GlobalDecompositionError.Evidence(error))
          case Right(value) =>
            var row = 0
            while row < rows do
              data(row * columns + column) = value(row, 0)
              row += 1
        column += 1
      failure.toLeft(DMat.dense(rows, columns, data.toVector))

  private def checkedCells(rows: Int, columns: Int, budget: DecompositionBudget): Either[GlobalDecompositionError, Unit] =
    val cells = BigInt(rows) * columns
    // Array, Vector conversion, DMat and fitted retained input can overlap.
    val required = cells * 4
    if cells > Int.MaxValue then Left(GlobalDecompositionError.Capacity(cells))
    else if required > budget.maximumCells then Left(GlobalDecompositionError.Budget(required, budget.maximumCells))
    else Right(())

  /** Paired Multivar may materialize the p-by-p and q-by-q marginals plus the
    * p-by-q cross form. Provider-private source buffers and operator internals
    * are outside this declared adapter allocation and must be admitted by the
    * provider that owns them. */
  private def pairedAdmission[SR <: SemanticSpace, TR <: SemanticSpace, SF <: SemanticSpace, TF <: SemanticSpace](
      sourceRows: SpaceEvidence[SR], targetRows: SpaceEvidence[TR],
      sourceFeatures: SpaceEvidence[SF], targetFeatures: SpaceEvidence[TF],
      components: ComponentCount, scale: Double,
      sourceAccess: RelationAccess, targetAccess: RelationAccess, budget: DecompositionBudget
  ): Either[GlobalDecompositionError, Unit] =
    val p = sourceFeatures.dimension
    val q = targetFeatures.dimension
    val columns = BigInt(math.max(p, q))
    val sourceRectangle = BigInt(sourceRows.dimension) * columns
    val targetRectangle = BigInt(targetRows.dimension) * columns
    val largestArray = Vector(BigInt(p) * p, BigInt(q) * q, BigInt(p) * q, sourceRectangle, targetRectangle).max
    val required = 4 * (BigInt(p) * p + BigInt(q) * q + BigInt(p) * q + sourceRectangle + targetRectangle) +
      (BigInt(p) + q) * components.value
    if sourceRows.dimension < 2 || targetRows.dimension < 2 then Left(GlobalDecompositionError.Axis("paired row count"))
    else if components.value > math.min(p, q) then Left(GlobalDecompositionError.Axis("paired component count"))
    else if !scale.isFinite || scale <= 0.0 then Left(GlobalDecompositionError.Axis("paired scale"))
    else if largestArray > Int.MaxValue then Left(GlobalDecompositionError.Capacity(largestArray))
    else if required > budget.maximumCells then Left(GlobalDecompositionError.Budget(required, budget.maximumCells))
    else RelationAccess.admitImmediate(sourceAccess).left.map(error => GlobalDecompositionError.Replay(error)).flatMap: _ =>
      RelationAccess.admitImmediate(targetAccess).left.map(error => GlobalDecompositionError.Replay(error))

  private def pairedIdentity[SR <: SemanticSpace, TR <: SemanticSpace, SF <: SemanticSpace, TF <: SemanticSpace,
      ES <: OperatorEvidence, ET <: OperatorEvidence, EL <: OperatorEvidence](
      source: OpTable[SR, SF, UncheckedEvidence], target: OpTable[TR, TF, UncheckedEvidence],
      sourceGeometry: OpRowLink[SR, SR, ES], targetGeometry: OpRowLink[TR, TR, ET], relationship: OpRowLink[SR, TR, EL]
  ): String =
    AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.decomposition.paired-evidence.v1")
      writer.string(source.domain.descriptor.toString)
      writer.string(source.codomain.descriptor.toString)
      writer.string(target.domain.descriptor.toString)
      writer.string(target.codomain.descriptor.toString)
      writeIdentity(writer, source.valueIdentity)
      writeIdentity(writer, target.valueIdentity)
      writeIdentity(writer, sourceGeometry.valueIdentity)
      writeIdentity(writer, targetGeometry.valueIdentity)
      writeIdentity(writer, relationship.valueIdentity)

  private def geometryReceipt[SR <: SemanticSpace, TR <: SemanticSpace,
      ES <: OperatorEvidence, ET <: OperatorEvidence, EL <: OperatorEvidence](
      sourceRows: SpaceEvidence[SR], targetRows: SpaceEvidence[TR],
      source: OpRowLink[SR, SR, ES], target: OpRowLink[TR, TR, ET], relationship: OpRowLink[SR, TR, EL]
  ): PairedGeometryReceipt[SR, TR] =
    def receipt[S <: SemanticSpace, T <: SemanticSpace, E <: OperatorEvidence](
        left: SpaceEvidence[S], right: SpaceEvidence[T], op: OpRowLink[S, T, E]
    ): RowGeometryReceipt[S, T] =
      RowGeometryReceipt(left, right, op.valueIdentity, op.representation, op.certificate.status, op.certificate.claims, op.provenance)
    PairedGeometryReceipt(receipt(sourceRows, sourceRows, source), receipt(targetRows, targetRows, target), receipt(sourceRows, targetRows, relationship))

  /** The upstream fit's cross operator can retain its input tables. Own its
    * dense cross values before returning, so an admitted scoped provider may
    * close without invalidating the artifact. */
  private def ownPairedFit[SF <: SemanticSpace, TF <: SemanticSpace, C <: SemanticSpace](
      fit: PairedOperatorFit[SF, TF, C]
  ): Either[GlobalDecompositionError, (PairedOperatorFit[SF, TF, C], String)] =
    for
      cross <- fit.cross.toDense.left.map(error => GlobalDecompositionError.Evidence(error))
      owned <- Op.fromDense(cross, fit.cross.domain, fit.cross.codomain, fit.cross.role, fit.cross.valueIdentity, fit.cross.provenance)
        .left.map(error => GlobalDecompositionError.Evidence(error))
    yield fit.copy(cross = owned) -> contentIdentity(cross)

  private def writeIdentity(writer: AxisDigest.Writer, value: ValueIdentity): Unit =
    value match
      case ValueIdentity.Source(id) => writer.string("source"); writer.string(id.value)
      case ValueIdentity.Adjoint(of) => writer.string("adjoint"); writeIdentity(writer, of)
      case ValueIdentity.Composition(first, second) =>
        writer.string("composition"); writeIdentity(writer, first); writeIdentity(writer, second)
      case ValueIdentity.Derived(operation, inputs) =>
        writer.string("derived"); writer.string(operation); writer.intLE(inputs.length); inputs.foreach(writeIdentity(writer, _))

  private def contentIdentity(values: DMat): String =
    AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.decomposition.dense-content.v1")
      writer.intLE(values.rows)
      writer.intLE(values.cols)
      var row = 0
      while row < values.rows do
        var column = 0
        while column < values.cols do
          writer.string(java.lang.Double.toHexString(values(row, column)))
          column += 1
        row += 1
