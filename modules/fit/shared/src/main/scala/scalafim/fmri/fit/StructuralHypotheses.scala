package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix, QROptions, QRPivoting}
import scalafim.fmri.design.{
  BasisElementRef,
  CellKey,
  CoefficientAxis,
  ColumnId,
  ColumnRole,
  DesignSchema,
  ModulatorId,
  PhaseId,
  RunScope,
  RankToleranceConvention,
  RunwiseDesignSlice,
  StructuralColumn,
  StructuralColumnOrigin,
  TermId
}
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.hrf.{ResponseFunctional, ResponseUnits}

/** Failure classes emitted while lowering a semantic hypothesis. */
enum StructuralHypothesisErrorKind:
  case UnknownColumn
  case UnknownTerm
  case UnknownFactor
  case UnknownLevel
  case UnknownBasis
  case EmptySelection
  case DeclaredEmpty
  case DuplicateIdentity
  case InvalidWeight
  case IncompatibleDesign
  case NonEstimable

/** Where a lowered hypothesis came from.  Name-keyed values are retained only
  * as an explicitly labelled compatibility source. */
enum HypothesisSource:
  case Structural
  case CompatibilityNameKeyed
  case AttachedDesign
  case GeneratedDesign

enum StructuralEstimabilityDecision:
  case Estimable
  case NonEstimable

/** How an event selector treats the optional modulator axis.
  *
  * `Any` is useful for audits over a whole term.  Scientific hypotheses should
  * normally choose `Unmodulated` or one exact `ById` value so the same cell's
  * main-effect and parametric-modulator columns cannot be conflated.
  */
enum ModulatorSelection:
  case Any
  case Unmodulated
  case ById(id: ModulatorId)

  def matches(value: Option[ModulatorId]): Boolean =
    this match
      case Any             => true
      case Unmodulated     => value.isEmpty
      case ById(expected)  => value.contains(expected)

  def describe: String =
    this match
      case Any            => "any"
      case Unmodulated    => "unmodulated"
      case ById(id)       => s"id=${id.value}"

/** Evidence for the row-space decision made during hypothesis compilation. */
final case class StructuralEstimabilityEvidence(
    designRank: Int,
    contrastRank: Int,
    augmentedRank: Int,
    toleranceConvention: RankToleranceConvention,
    designTolerance: Double,
    contrastTolerance: Double,
    augmentedTolerance: Double,
    decision: StructuralEstimabilityDecision,
    detail: String
):
  require(designRank >= 0, "design rank must be non-negative")
  require(contrastRank > 0, "contrast rank must be positive")
  require(augmentedRank >= designRank, "augmented rank cannot be below design rank")
  require(designTolerance >= 0.0 && designTolerance.isFinite, "design estimability tolerance must be finite and non-negative")
  require(contrastTolerance >= 0.0 && contrastTolerance.isFinite, "contrast estimability tolerance must be finite and non-negative")
  require(augmentedTolerance >= 0.0 && augmentedTolerance.isFinite, "augmented estimability tolerance must be finite and non-negative")
  require(detail.trim.nonEmpty, "estimability detail must be non-empty")

  def rowSpacePreserved: Boolean = augmentedRank == designRank

/** The identity and lowering receipt retained by a compiled hypothesis and
  * copied into contrast result/artifact values. */
final case class HypothesisMetadata(
    id: ContrastId,
    description: String,
    designFingerprint: scalafim.fmri.design.DesignFingerprint,
    selectedColumnIds: Vector[ColumnId],
    weights: DMat,
    source: HypothesisSource,
    estimability: StructuralEstimabilityEvidence,
    responseFunctionals: Vector[StructuralResponseReceipt] = Vector.empty
):
  require(id.value.trim.nonEmpty, "hypothesis id must be non-empty")
  require(description.trim.nonEmpty, "hypothesis description must be non-empty")
  require(selectedColumnIds.nonEmpty, "hypothesis must select at least one structural column")
  require(selectedColumnIds.distinct.length == selectedColumnIds.length, "hypothesis structural columns must be unique")
  require(weights.rows > 0 && weights.cols > 0, "hypothesis weights must be non-empty")
  require(weights.valuesRowMajor.forall(_.isFinite), "hypothesis weights must be finite")

/** A structural query over compiled column provenance.  Queries return all
  * matching columns in the axis order; basis-specific hypotheses should use
  * `basis = Some(...)` rather than relying on a rendered suffix. */
enum StructuralColumnSelector:
  case Id(id: ColumnId)
  case RenderedLabel(label: String)
  case Event(
      term: Option[TermId],
      phase: Option[PhaseId],
      cell: Option[CellKey],
      modulator: ModulatorSelection,
      basis: Option[BasisElementRef],
      role: Option[ColumnRole],
      runScope: Option[RunScope]
  )
  case Sampled(
      regressor: Option[ModulatorId],
      role: Option[ColumnRole],
      runScope: Option[RunScope]
  )
  case Intercept(runScope: Option[RunScope])
  case Drift(
      term: Option[TermId],
      basis: Option[BasisElementRef],
      runScope: Option[RunScope]
  )
  case Nuisance(
      term: Option[TermId],
      regressor: Option[ModulatorId],
      runScope: Option[RunScope]
  )
  case Baseline(
      term: Option[TermId],
      role: Option[ColumnRole],
      component: Option[BasisElementRef],
      runScope: Option[RunScope]
  )

  def describe: String =
    this match
      case StructuralColumnSelector.Id(id) => s"column id '${id.value}'"
      case StructuralColumnSelector.RenderedLabel(label) => s"rendered label '$label'"
      case StructuralColumnSelector.Event(term, phase, cell, modulator, basis, role, runScope) =>
        s"event(term=${term.map(_.value)}, phase=${phase.map(_.value)}, cell=${cell.map(_.canonical)}, modulator=${modulator.describe}, basis=${basis.map(_.id)}, role=$role, runScope=$runScope)"
      case StructuralColumnSelector.Sampled(regressor, role, runScope) =>
        s"sampled(regressor=${regressor.map(_.value)}, role=$role, runScope=$runScope)"
      case StructuralColumnSelector.Intercept(runScope) => s"intercept(runScope=$runScope)"
      case StructuralColumnSelector.Drift(term, basis, runScope) =>
        s"drift(term=${term.map(_.value)}, basis=${basis.map(_.id)}, runScope=$runScope)"
      case StructuralColumnSelector.Nuisance(term, regressor, runScope) =>
        s"nuisance(term=${term.map(_.value)}, regressor=${regressor.map(_.value)}, runScope=$runScope)"
      case StructuralColumnSelector.Baseline(term, role, component, runScope) =>
        s"baseline(term=${term.map(_.value)}, role=$role, component=${component.map(_.id)}, runScope=$runScope)"

object StructuralColumnSelector:
  def event(
      term: Option[TermId] = None,
      phase: Option[PhaseId] = None,
      cell: Option[CellKey] = None,
      modulator: ModulatorSelection = ModulatorSelection.Any,
      basis: Option[BasisElementRef] = None,
      role: Option[ColumnRole] = None,
      runScope: Option[RunScope] = None
  ): StructuralColumnSelector =
    Event(term, phase, cell, modulator, basis, role, runScope)

  def sampled(
      regressor: Option[ModulatorId] = None,
      role: Option[ColumnRole] = None,
      runScope: Option[RunScope] = None
  ): StructuralColumnSelector =
    Sampled(regressor, role, runScope)

  def intercept(runScope: Option[RunScope] = None): StructuralColumnSelector =
    Intercept(runScope)

  def drift(
      term: Option[TermId] = None,
      basis: Option[BasisElementRef] = None,
      runScope: Option[RunScope] = None
  ): StructuralColumnSelector =
    Drift(term, basis, runScope)

  def nuisance(
      term: Option[TermId] = None,
      regressor: Option[ModulatorId] = None,
      runScope: Option[RunScope] = None
  ): StructuralColumnSelector =
    Nuisance(term, regressor, runScope)

  def baseline(
      term: Option[TermId] = None,
      role: Option[ColumnRole] = None,
      component: Option[BasisElementRef] = None,
      runScope: Option[RunScope] = None
  ): StructuralColumnSelector =
    Baseline(term, role, component, runScope)

final case class StructuralWeight(
    selector: StructuralColumnSelector,
    value: Double
):
  /** Validate at the construction boundary when callers want an immediate
    * checked value.  The case-class constructor intentionally remains total
    * so compilation can return a typed [[FitError]] for malformed weights
    * rather than throwing from a semantic hypothesis definition.
    */
  def validate: Either[FitError, StructuralWeight] =
    if !value.isFinite then
      Left(FitError.StructuralHypothesisFailure("<uncompiled>", StructuralHypothesisErrorKind.InvalidWeight, s"${selector.describe} has non-finite weight $value"))
    else if value == 0.0 then
      Left(FitError.StructuralHypothesisFailure("<uncompiled>", StructuralHypothesisErrorKind.InvalidWeight, s"${selector.describe} has zero weight"))
    else Right(this)

/** One coefficient-space weight in a response-level functional. */
final case class BasisFunctionalWeight(
    basis: BasisElementRef,
    value: Double
):
  require(basis.basisId.trim.nonEmpty, "response-functional basis id must be non-empty")

/** A response-level linear functional lowered over semantic basis elements.
  *
  * The caller supplies weights produced by `ResponseBasis.responseFunctional`;
  * this record carries the functional and units alongside those weights so a
  * point response, a window mean, and a window integral cannot be conflated.
  */
final case class StructuralResponseWeight(
    selector: StructuralColumnSelector,
    functional: ResponseFunctional,
    units: ResponseUnits,
    basisWeights: Vector[BasisFunctionalWeight]
):
  require(basisWeights.nonEmpty, "response-functional basis weights must be non-empty")

final case class StructuralResponseReceipt(
    functional: ResponseFunctional,
    units: ResponseUnits,
    basis: Vector[BasisElementRef]
)

final case class StructuralTContrast(
    id: ContrastId,
    description: String,
    terms: Vector[StructuralWeight],
    source: HypothesisSource = HypothesisSource.Structural,
    responseTerms: Vector[StructuralResponseWeight] = Vector.empty
):
  require(id.value.trim.nonEmpty, "structural T contrast id must be non-empty")
  require(description.trim.nonEmpty, "structural T contrast description must be non-empty")
  require(terms.nonEmpty || responseTerms.nonEmpty, "structural T contrast must contain at least one term")

  def compile(
      schema: DesignSchema,
      rankTolerance: OlsRankTolerance = StructuralHypothesis.DefaultRankTolerance
  ): Either[FitError, CompiledTContrast] =
    StructuralHypothesis.compile(schema, this, rankTolerance)

  /** Compile against one run's structural axis and row-selected design. */
  def compile(
      slice: RunwiseDesignSlice
  ): Either[FitError, CompiledTContrast] =
    compile(slice, StructuralHypothesis.DefaultRankTolerance)

  def compile(
      slice: RunwiseDesignSlice,
      rankTolerance: OlsRankTolerance
  ): Either[FitError, CompiledTContrast] =
    StructuralHypothesis.compile(slice, this, rankTolerance)

object StructuralTContrast:
  def fromColumn(
      id: ContrastId,
      description: String,
      column: ColumnId,
      value: Double = 1.0,
      source: HypothesisSource = HypothesisSource.Structural
  ): StructuralTContrast =
    StructuralTContrast(id, description, Vector(StructuralWeight(StructuralColumnSelector.Id(column), value)), source)

  def fromIds(
      id: ContrastId,
      description: String,
      weights: Map[ColumnId, Double],
      source: HypothesisSource = HypothesisSource.Structural
  ): StructuralTContrast =
    StructuralTContrast(
      id,
      description,
      weights.toVector.sortBy(_._1.value).map { case (column, value) =>
        StructuralWeight(StructuralColumnSelector.Id(column), value)
      },
      source
    )

  /** Explicit compatibility adapter for the legacy rendered-name API. */
  def fromNameKeyed(contrast: TContrast): StructuralTContrast =
    StructuralTContrast(
      ContrastId.unsafe(contrast.name),
      s"compatibility name-keyed contrast '${contrast.name}'",
      contrast.weights.toVector.sortBy(_._1).map { case (name, value) =>
        StructuralWeight(StructuralColumnSelector.RenderedLabel(name), value)
      },
      HypothesisSource.CompatibilityNameKeyed
    )

  def fromResponseFunctional(
      id: ContrastId,
      description: String,
      selector: StructuralColumnSelector,
      functional: ResponseFunctional,
      units: ResponseUnits,
      basisWeights: Vector[BasisFunctionalWeight],
      source: HypothesisSource = HypothesisSource.Structural
  ): StructuralTContrast =
    StructuralTContrast(
      id = id,
      description = description,
      terms = Vector.empty,
      source = source,
      responseTerms = Vector(StructuralResponseWeight(selector, functional, units, basisWeights))
    )

final case class StructuralFContrast(
    id: ContrastId,
    description: String,
    rows: Vector[Vector[StructuralWeight]],
    source: HypothesisSource = HypothesisSource.Structural,
    responseRows: Vector[Vector[StructuralResponseWeight]] = Vector.empty
):
  require(id.value.trim.nonEmpty, "structural F contrast id must be non-empty")
  require(description.trim.nonEmpty, "structural F contrast description must be non-empty")
  require(rows.nonEmpty || responseRows.nonEmpty, "structural F contrast must contain at least one row")
  require(rows.forall(_.nonEmpty), "structural F contrast rows must be non-empty")
  require(responseRows.forall(_.nonEmpty), "structural F response rows must be non-empty")

  def compile(
      schema: DesignSchema,
      rankTolerance: OlsRankTolerance = StructuralHypothesis.DefaultRankTolerance
  ): Either[FitError, CompiledFContrast] =
    StructuralHypothesis.compile(schema, this, rankTolerance)

  /** Compile against one run's structural axis and row-selected design. */
  def compile(
      slice: RunwiseDesignSlice
  ): Either[FitError, CompiledFContrast] =
    compile(slice, StructuralHypothesis.DefaultRankTolerance)

  def compile(
      slice: RunwiseDesignSlice,
      rankTolerance: OlsRankTolerance
  ): Either[FitError, CompiledFContrast] =
    StructuralHypothesis.compile(slice, this, rankTolerance)

object StructuralFContrast:
  def fromRows(
      id: ContrastId,
      description: String,
      rows: Vector[Map[ColumnId, Double]],
      source: HypothesisSource = HypothesisSource.Structural
  ): StructuralFContrast =
    StructuralFContrast(
      id,
      description,
      rows.map(_.toVector.sortBy(_._1.value).map { case (column, value) =>
        StructuralWeight(StructuralColumnSelector.Id(column), value)
      }),
      source
    )

  /** Explicit compatibility adapter for the legacy rendered-name API. */
  def fromNameKeyed(contrast: FContrast): StructuralFContrast =
    StructuralFContrast(
      ContrastId.unsafe(contrast.name),
      s"compatibility name-keyed contrast '${contrast.name}'",
      contrast.weights.map { row =>
        row.toVector.sortBy(_._1).map { case (name, value) =>
          StructuralWeight(StructuralColumnSelector.RenderedLabel(name), value)
        }
      },
      HypothesisSource.CompatibilityNameKeyed
    )

  def fromResponseRows(
      id: ContrastId,
      description: String,
      rows: Vector[Vector[StructuralResponseWeight]],
      source: HypothesisSource = HypothesisSource.Structural
  ): StructuralFContrast =
    StructuralFContrast(
      id = id,
      description = description,
      rows = Vector.empty,
      source = source,
      responseRows = rows
    )

final case class CompiledTContrast private[fit] (
    metadata: HypothesisMetadata,
    coefficientAxis: CoefficientAxis,
    aligned: AlignedTContrast
):
  def id: ContrastId = metadata.id
  def description: String = metadata.description
  def designFingerprint: scalafim.fmri.design.DesignFingerprint = metadata.designFingerprint
  def selectedColumnIds: Vector[ColumnId] = metadata.selectedColumnIds
  def weights: DMat = metadata.weights
  def source: HypothesisSource = metadata.source
  def estimability: StructuralEstimabilityEvidence = metadata.estimability

  def evaluate(result: DenseFmriFitResult): Either[FitError, TContrastResult] =
    result.inferenceReady.flatMap: ready =>
      StructuralHypothesis.validateResultAxis(metadata.id, coefficientAxis, result.coefficientAxis).flatMap: _ =>
        TContrast.evaluateAligned(ready, aligned, Some(metadata))

  /** Evaluate against a separate-runs-then-fixed-effects result. */
  def evaluate(result: FixedEffectsFmriFitResult): Either[FitError, TContrastResult] =
    for
      ready <- result.inferenceReady
      projected <- StructuralHypothesis.alignTForResult(metadata.id, coefficientAxis, result.coefficientAxis, metadata.weights)
      evaluated <- TContrast.evaluateAligned(ready, projected, Some(metadata))
    yield evaluated

  /** Evaluate a run-scoped hypothesis against one independently fitted run. */
  def evaluate(result: RunwiseFmriFitResult, runIndex: Int): Either[FitError, TContrastResult] =
    for
      run <- result.run(runIndex).toRight(FitError.EmptyRunPartition(runIndex))
      _ <- StructuralHypothesis.validateResultAxis(metadata.id, coefficientAxis, run.coefficientAxis)
      dense <- run.denseResult(result.columnNames, result.voxelIndices, result.summary)
      ready <- dense.inferenceReady
      evaluated <- TContrast.evaluateAligned(ready, aligned, Some(metadata))
    yield evaluated

final case class CompiledFContrast private[fit] (
    metadata: HypothesisMetadata,
    coefficientAxis: CoefficientAxis,
    aligned: AlignedFContrast
):
  def id: ContrastId = metadata.id
  def description: String = metadata.description
  def designFingerprint: scalafim.fmri.design.DesignFingerprint = metadata.designFingerprint
  def selectedColumnIds: Vector[ColumnId] = metadata.selectedColumnIds
  def weights: DMat = metadata.weights
  def source: HypothesisSource = metadata.source
  def estimability: StructuralEstimabilityEvidence = metadata.estimability
  def numeratorRank: Int = estimability.contrastRank

  def evaluate(result: DenseFmriFitResult): Either[FitError, FContrastResult] =
    result.inferenceReady.flatMap: ready =>
      StructuralHypothesis.validateResultAxis(metadata.id, coefficientAxis, result.coefficientAxis).flatMap: _ =>
        FContrast.evaluateAligned(ready, aligned, Some(metadata))

  /** Evaluate against a separate-runs-then-fixed-effects result. */
  def evaluate(result: FixedEffectsFmriFitResult): Either[FitError, FContrastResult] =
    for
      ready <- result.inferenceReady
      projected <- StructuralHypothesis.alignFForResult(metadata.id, coefficientAxis, result.coefficientAxis, metadata.weights, estimability.contrastRank)
      evaluated <- FContrast.evaluateAligned(ready, projected, Some(metadata))
    yield evaluated

  /** Evaluate a run-scoped hypothesis against one independently fitted run. */
  def evaluate(result: RunwiseFmriFitResult, runIndex: Int): Either[FitError, FContrastResult] =
    for
      run <- result.run(runIndex).toRight(FitError.EmptyRunPartition(runIndex))
      _ <- StructuralHypothesis.validateResultAxis(metadata.id, coefficientAxis, run.coefficientAxis)
      dense <- run.denseResult(result.columnNames, result.voxelIndices, result.summary)
      ready <- dense.inferenceReady
      evaluated <- FContrast.evaluateAligned(ready, aligned, Some(metadata))
    yield evaluated

object StructuralHypothesis:
  val DefaultRankTolerance: OlsRankTolerance = OlsRankTolerance.ScaleAware

  def compile(
      schema: DesignSchema,
      hypothesis: StructuralTContrast,
      rankTolerance: OlsRankTolerance
  ): Either[FitError, CompiledTContrast] =
    for
      _ <- validateSchema(schema, hypothesis.id)
      compiled <- compile(schema.coefficientAxis, schema.matrix, hypothesis, rankTolerance)
    yield compiled

  def compile(
      slice: RunwiseDesignSlice,
      hypothesis: StructuralTContrast,
      rankTolerance: OlsRankTolerance
  ): Either[FitError, CompiledTContrast] =
    for
      _ <- validateAxisDesign(slice.axis, slice.matrix, hypothesis.id)
      compiled <- compile(slice.axis, slice.matrix, hypothesis, rankTolerance)
    yield compiled

  private def compile(
      axis: CoefficientAxis,
      design: Mat,
      hypothesis: StructuralTContrast,
      rankTolerance: OlsRankTolerance
  ): Either[FitError, CompiledTContrast] =
    for
      _ <- validateTolerance(hypothesis.id, rankTolerance)
      resolved <- resolveAll(hypothesis.id, hypothesis.terms, hypothesis.responseTerms, axis, hypothesis.source)
      (weights, selected) = tWeights(axis, resolved)
      evidence <- estimability(design, weights, hypothesis.id, rankTolerance)
      metadata = HypothesisMetadata(
        id = hypothesis.id,
        description = hypothesis.description,
        designFingerprint = axis.designFingerprint,
        selectedColumnIds = selected,
        weights = weights,
        source = hypothesis.source,
        estimability = evidence,
        responseFunctionals = hypothesis.responseTerms.map(receipt)
      )
    yield CompiledTContrast(
      metadata,
      axis,
      AlignedTContrast.unsafe(hypothesis.id.value, axis.columnNames, weights, Some(axis))
    )

  def compile(
      schema: DesignSchema,
      hypothesis: StructuralFContrast,
      rankTolerance: OlsRankTolerance
  ): Either[FitError, CompiledFContrast] =
    for
      _ <- validateSchema(schema, hypothesis.id)
      compiled <- compile(schema.coefficientAxis, schema.matrix, hypothesis, rankTolerance)
    yield compiled

  def compile(
      slice: RunwiseDesignSlice,
      hypothesis: StructuralFContrast,
      rankTolerance: OlsRankTolerance
  ): Either[FitError, CompiledFContrast] =
    for
      _ <- validateAxisDesign(slice.axis, slice.matrix, hypothesis.id)
      compiled <- compile(slice.axis, slice.matrix, hypothesis, rankTolerance)
    yield compiled

  private def compile(
      axis: CoefficientAxis,
      design: Mat,
      hypothesis: StructuralFContrast,
      rankTolerance: OlsRankTolerance
  ): Either[FitError, CompiledFContrast] =
    for
      _ <- validateTolerance(hypothesis.id, rankTolerance)
      resolvedRows <- resolveRows(hypothesis.id, hypothesis.rows, hypothesis.responseRows, axis, hypothesis.source)
      rawWeights = fWeights(axis, resolvedRows)
      independent = independentColumns(rawWeights, rankTolerance)
      _ <- if independent.nonEmpty then Right(()) else failure(hypothesis.id, StructuralHypothesisErrorKind.EmptySelection, "F contrast has no independent non-zero rows")
      weights = selectColumns(rawWeights, independent)
      evidence <- estimability(design, weights, hypothesis.id, rankTolerance)
      metadata = HypothesisMetadata(
        id = hypothesis.id,
        description = hypothesis.description,
        designFingerprint = axis.designFingerprint,
        selectedColumnIds = selectedIds(axis, weights),
        weights = weights,
        source = hypothesis.source,
        estimability = evidence,
        responseFunctionals = hypothesis.responseRows.flatten.map(receipt)
      )
    yield CompiledFContrast(
      metadata,
      axis,
      AlignedFContrast.unsafe(hypothesis.id.value, axis.columnNames, weights, Some(axis), evidence.contrastRank)
    )

  private def validateSchema(schema: DesignSchema, id: ContrastId): Either[FitError, Unit] =
    schema.validate.left.map(error =>
      FitError.StructuralHypothesisFailure(id.value, StructuralHypothesisErrorKind.IncompatibleDesign, error.message)
    )

  private def validateAxisDesign(axis: CoefficientAxis, design: Mat, id: ContrastId): Either[FitError, Unit] =
    if design.rows <= 0 then
      failure(id, StructuralHypothesisErrorKind.IncompatibleDesign, "runwise design slice must contain at least one row")
    else if design.cols != axis.predictors then
      failure(id, StructuralHypothesisErrorKind.IncompatibleDesign, s"design has ${design.cols} columns but axis has ${axis.predictors}")
    else if !design.data.forall(_.isFinite) then
      failure(id, StructuralHypothesisErrorKind.IncompatibleDesign, "runwise design slice contains non-finite values")
    else Right(())

  private def validateTolerance(id: ContrastId, tolerance: OlsRankTolerance): Either[FitError, Unit] =
    if tolerance.valid then Right(())
    else failure(id, StructuralHypothesisErrorKind.InvalidWeight, "rank tolerance must be finite and non-negative")

  private def failure[A](id: ContrastId, kind: StructuralHypothesisErrorKind, detail: String): Either[FitError, A] =
    Left(FitError.StructuralHypothesisFailure(id.value, kind, detail))

  private def resolveTerms(
      id: ContrastId,
      terms: Vector[StructuralWeight],
      axis: CoefficientAxis,
      source: HypothesisSource
  ): Either[FitError, Vector[(StructuralColumn, Double)]] =
    val out = Vector.newBuilder[(StructuralColumn, Double)]
    val seen = scala.collection.mutable.HashSet.empty[String]
    var termIndex = 0
    while termIndex < terms.length do
      val term = terms(termIndex)
      if !term.value.isFinite || term.value == 0.0 then
        return failure(id, StructuralHypothesisErrorKind.InvalidWeight, s"${term.selector.describe} has invalid weight ${term.value}")
      else if isRenderedLabel(term.selector) && source != HypothesisSource.CompatibilityNameKeyed then
        return failure(id, StructuralHypothesisErrorKind.IncompatibleDesign, "rendered-label selectors are permitted only by the explicit compatibility adapter")
      else
        val matches = resolveSelector(term.selector, axis)
        if matches.isEmpty then
          return selectorFailure(id, term.selector, axis)
        var matchIndex = 0
        while matchIndex < matches.length do
          val column = matches(matchIndex)
          if !seen.add(column.id.value) then
            return failure(id, StructuralHypothesisErrorKind.DuplicateIdentity, s"column '${column.id.value}' was selected more than once")
          out += column -> term.value
          matchIndex += 1
      termIndex += 1
    Right(out.result())

  private def resolveAll(
      id: ContrastId,
      terms: Vector[StructuralWeight],
      responseTerms: Vector[StructuralResponseWeight],
      axis: CoefficientAxis,
      source: HypothesisSource
  ): Either[FitError, Vector[(StructuralColumn, Double)]] =
    for
      ordinary <- resolveTerms(id, terms, axis, source)
      response <- resolveResponseTerms(id, responseTerms, axis, source)
      combined = ordinary ++ response
      _ <-
        if combined.map(_._1.id.value).distinct.length == combined.length then Right(())
        else failure(id, StructuralHypothesisErrorKind.DuplicateIdentity, "a hypothesis selected one structural column more than once")
    yield combined

  private def resolveResponseTerms(
      id: ContrastId,
      terms: Vector[StructuralResponseWeight],
      axis: CoefficientAxis,
      source: HypothesisSource
  ): Either[FitError, Vector[(StructuralColumn, Double)]] =
    val out = Vector.newBuilder[(StructuralColumn, Double)]
    var termIndex = 0
    while termIndex < terms.length do
      val term = terms(termIndex)
      if term.basisWeights.map(_.basis).distinct.length != term.basisWeights.length then
        return failure(id, StructuralHypothesisErrorKind.DuplicateIdentity, s"response functional '${term.functional}' repeats a basis element")
      val nonZero = term.basisWeights.filter(_.value != 0.0)
      if nonZero.exists(weight => !weight.value.isFinite) then
        return failure(id, StructuralHypothesisErrorKind.InvalidWeight, s"response functional '${term.functional}' contains a non-finite basis weight")
      if nonZero.isEmpty then
        return failure(id, StructuralHypothesisErrorKind.EmptySelection, s"response functional '${term.functional}' has no non-zero basis weights")
      if isRenderedLabel(term.selector) && source != HypothesisSource.CompatibilityNameKeyed then
        return failure(id, StructuralHypothesisErrorKind.IncompatibleDesign, "rendered-label selectors are permitted only by the explicit compatibility adapter")
      var basisIndex = 0
      while basisIndex < nonZero.length do
        val basisWeight = nonZero(basisIndex)
        withBasis(term.selector, basisWeight.basis) match
          case Left(error) => return Left(error)
          case Right(selector) =>
            val matches = resolveSelector(selector, axis)
            if matches.isEmpty then return selectorFailure(id, selector, axis)
            matches.foreach { column =>
              out += column -> column.hrfScale.transportLinearWeight(basisWeight.value)
            }
        basisIndex += 1
      termIndex += 1
    Right(out.result())

  private def withBasis(
      selector: StructuralColumnSelector,
      basis: BasisElementRef
  ): Either[FitError, StructuralColumnSelector] =
    selector match
      case StructuralColumnSelector.Event(term, phase, cell, modulator, _, role, runScope) =>
        Right(StructuralColumnSelector.Event(term, phase, cell, modulator, Some(basis), role, runScope))
      case StructuralColumnSelector.Drift(term, _, runScope) =>
        Right(StructuralColumnSelector.Drift(term, Some(basis), runScope))
      case StructuralColumnSelector.Baseline(term, role, _, runScope) =>
        Right(StructuralColumnSelector.Baseline(term, role, Some(basis), runScope))
      case _ =>
        Left(FitError.StructuralHypothesisFailure(
          "<uncompiled>",
          StructuralHypothesisErrorKind.IncompatibleDesign,
          s"response functional selector ${selector.describe} cannot be expanded over basis elements"
        ))

  private def receipt(term: StructuralResponseWeight): StructuralResponseReceipt =
    StructuralResponseReceipt(term.functional, term.units, term.basisWeights.map(_.basis))

  private def resolveRows(
      id: ContrastId,
      rows: Vector[Vector[StructuralWeight]],
      responseRows: Vector[Vector[StructuralResponseWeight]],
      axis: CoefficientAxis,
      source: HypothesisSource
  ): Either[FitError, Vector[Vector[(StructuralColumn, Double)]]] =
    if rows.nonEmpty && responseRows.nonEmpty && rows.length != responseRows.length then
      return failure(id, StructuralHypothesisErrorKind.IncompatibleDesign, "ordinary and response-functional F rows must have equal lengths")
    val rowCount = math.max(rows.length, responseRows.length)
    val out = Vector.newBuilder[Vector[(StructuralColumn, Double)]]
    var rowIndex = 0
    while rowIndex < rowCount do
      val ordinary = if rowIndex < rows.length then rows(rowIndex) else Vector.empty
      val response = if rowIndex < responseRows.length then responseRows(rowIndex) else Vector.empty
      resolveAll(id, ordinary, response, axis, source) match
        case Left(error) => return Left(error)
        case Right(row)  => out += row
      rowIndex += 1
    Right(out.result())

  private def selectorFailure(
      id: ContrastId,
      selector: StructuralColumnSelector,
      axis: CoefficientAxis
  ): Either[FitError, Nothing] =
    selector match
      case StructuralColumnSelector.Id(column) =>
        failure(id, StructuralHypothesisErrorKind.UnknownColumn, s"column '${column.value}' is absent from the compiled axis")
      case StructuralColumnSelector.Event(term, _, cell, _, basis, _, _) =>
        val origins = axis.columns.collect {
          case StructuralColumn(_, _, origin: StructuralColumnOrigin.Event, _, _, _) => origin
        }
        term match
          case Some(value) if !origins.exists(_.term == value) =>
            failure(id, StructuralHypothesisErrorKind.UnknownTerm, s"term '${value.value}' is absent from the compiled axis")
          case _ =>
            val termOrigins = term.fold(origins)(value => origins.filter(_.term == value))
            cell match
              case Some(key) if key.assignments.exists(assignment => !termOrigins.exists(_.cell.get(assignment.factor).nonEmpty)) =>
                failure(id, StructuralHypothesisErrorKind.UnknownFactor, s"factor in ${key.canonical} is absent from the compiled axis")
              case Some(key) if axis.audit.emptyCells.contains(key) && !termOrigins.exists(_.cell == key) =>
                failure(id, StructuralHypothesisErrorKind.DeclaredEmpty, s"cell ${key.canonical} was declared but has no realized design column under the compilation policy")
              case Some(key) if key.assignments.exists { assignment =>
                  termOrigins.exists(_.cell.get(assignment.factor).nonEmpty) && !termOrigins.exists(_.cell.get(assignment.factor).contains(assignment.level))
                } =>
                failure(id, StructuralHypothesisErrorKind.UnknownLevel, s"level in ${key.canonical} is absent from the compiled axis")
              case _ =>
                val cellOrigins = cell.fold(termOrigins)(key => termOrigins.filter(_.cell == key))
                basis match
                  case Some(value) if !cellOrigins.exists(_.basis.contains(value)) =>
                    val scope = cell.fold("compiled axis")(key => s"cell ${key.canonical}")
                    failure(id, StructuralHypothesisErrorKind.UnknownBasis, s"basis '${value.id}' is absent from $scope")
                  case _ =>
                    failure(id, StructuralHypothesisErrorKind.EmptySelection, s"selector ${selector.describe} matched no structural columns")
      case _ =>
        failure(id, StructuralHypothesisErrorKind.EmptySelection, s"selector ${selector.describe} matched no structural columns")

  private def resolveSelector(selector: StructuralColumnSelector, axis: CoefficientAxis): Vector[StructuralColumn] =
    axis.columns.filter(column => selectorMatches(selector, column))

  private def isRenderedLabel(selector: StructuralColumnSelector): Boolean =
    selector match
      case StructuralColumnSelector.RenderedLabel(_) => true
      case _                                          => false

  private def selectorMatches(selector: StructuralColumnSelector, column: StructuralColumn): Boolean =
    selector match
      case StructuralColumnSelector.Id(id) => column.id == id
      case StructuralColumnSelector.RenderedLabel(label) => column.renderedLabel == label
      case StructuralColumnSelector.Event(term, phase, cell, modulator, basis, role, runScope) =>
        column.origin match
          case StructuralColumnOrigin.Event(columnTerm, columnPhase, columnCell, columnModulator, columnBasis, columnRole, columnRunScope) =>
            term.forall(_ == columnTerm) &&
              phase.forall(value => columnPhase.contains(value)) &&
              cell.forall(_ == columnCell) &&
              modulator.matches(columnModulator) &&
              basis.forall(value => columnBasis.contains(value)) &&
              role.forall(_ == columnRole) &&
              runScope.forall(_ == columnRunScope)
          case _ => false
      case StructuralColumnSelector.Sampled(regressor, role, runScope) =>
        column.origin match
          case StructuralColumnOrigin.Sampled(columnRegressor, columnRole, columnRunScope) =>
            regressor.forall(_ == columnRegressor) && role.forall(_ == columnRole) && runScope.forall(_ == columnRunScope)
          case _ => false
      case StructuralColumnSelector.Intercept(runScope) =>
        column.origin match
          case StructuralColumnOrigin.Intercept(columnRunScope) => runScope.forall(_ == columnRunScope)
          case _ => false
      case StructuralColumnSelector.Drift(term, basis, runScope) =>
        column.origin match
          case StructuralColumnOrigin.Drift(columnTerm, columnBasis, columnRunScope) =>
            term.forall(_ == columnTerm) && basis.forall(value => columnBasis.contains(value)) && runScope.forall(_ == columnRunScope)
          case _ => false
      case StructuralColumnSelector.Nuisance(term, regressor, runScope) =>
        column.origin match
          case StructuralColumnOrigin.Nuisance(columnTerm, columnRegressor, columnRunScope) =>
            term.forall(_ == columnTerm) && regressor.forall(_ == columnRegressor) && runScope.forall(_ == columnRunScope)
          case _ => false
      case StructuralColumnSelector.Baseline(term, role, component, runScope) =>
        column.origin match
          case StructuralColumnOrigin.Baseline(columnTerm, columnRole, columnComponent, columnRunScope) =>
            term.forall(_ == columnTerm) && role.forall(_ == columnRole) && component.forall(value => columnComponent.contains(value)) && runScope.forall(_ == columnRunScope)
          case _ => false

  private def tWeights(
      axis: CoefficientAxis,
      resolved: Vector[(StructuralColumn, Double)]
  ): (DMat, Vector[ColumnId]) =
    val values = Array.fill(axis.predictors)(0.0)
    resolved.foreach { case (column, value) =>
      values(column.ordinal.zeroBased) = value
    }
    val out = Matrix.newBuilder(1, axis.predictors)
    var index = 0
    while index < values.length do
      out(0, index) = values(index)
      index += 1
    val matrix = out.result()
    matrix -> selectedIdsT(axis, matrix)

  private def fWeights(
      axis: CoefficientAxis,
      rows: Vector[Vector[(StructuralColumn, Double)]]
  ): DMat =
    val out = Matrix.newBuilder(axis.predictors, rows.length)
    var rowIndex = 0
    while rowIndex < rows.length do
      rows(rowIndex).foreach { case (column, value) =>
        out(column.ordinal.zeroBased, rowIndex) = value
      }
      rowIndex += 1
    out.result()

  private def selectedIds(axis: CoefficientAxis, weights: DMat): Vector[ColumnId] =
    axis.columns.zipWithIndex.collect { case (column, index) if hasNonZero(weights, index) => column.id }

  private def selectedIdsT(axis: CoefficientAxis, weights: DMat): Vector[ColumnId] =
    axis.columns.zipWithIndex.collect { case (column, index) if weights(0, index) != 0.0 => column.id }

  private def hasNonZero(weights: DMat, row: Int): Boolean =
    var col = 0
    while col < weights.cols do
      if weights(row, col) != 0.0 then return true
      col += 1
    false

  private def independentColumns(matrix: DMat, tolerance: OlsRankTolerance): Vector[Int] =
    val selected = Vector.newBuilder[Int]
    var indices = Vector.empty[Int]
    var rank = 0
    var col = 0
    while col < matrix.cols do
      val candidate = indices :+ col
      val candidateRank = rankOf(selectColumns(matrix, candidate), tolerance).rank
      if candidateRank > rank then
        indices = candidate
        rank = candidateRank
        selected += col
      col += 1
    selected.result()

  private def selectColumns(matrix: DMat, indices: Vector[Int]): DMat =
    val out = Matrix.newBuilder(matrix.rows, indices.length)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < indices.length do
        out(row, col) = matrix(row, indices(col))
        col += 1
      row += 1
    out.result()

  private def estimability(
      design: Mat,
      weights: DMat,
      id: ContrastId,
      tolerance: OlsRankTolerance
  ): Either[FitError, StructuralEstimabilityEvidence] =
    val designMatrix = MatrixAdapters.fromHrfMatrix(design)
    val designEvidence = rankOf(designMatrix, tolerance)
    val contrastEvidence = rankOf(weights, tolerance)
    // T weights are already one contrast row (1 x p); F weights are stored
    // as coefficient-by-row matrices (p x q), so their row-space generators
    // are the transpose (q x p).  Appending the wrong orientation would
    // falsely reject estimable T functions in rank-deficient designs.
    val contrastRows = if weights.rows == 1 then weights else weights.t
    val augmented = appendRows(designMatrix, contrastRows)
    val augmentedEvidence = rankOf(augmented, tolerance)
    val decision =
      if augmentedEvidence.rank == designEvidence.rank then StructuralEstimabilityDecision.Estimable
      else StructuralEstimabilityDecision.NonEstimable
    val evidence = StructuralEstimabilityEvidence(
      designRank = designEvidence.rank,
      contrastRank = contrastEvidence.rank,
      augmentedRank = augmentedEvidence.rank,
      toleranceConvention = tolerance.convention,
      designTolerance = designEvidence.tolerance,
      contrastTolerance = contrastEvidence.tolerance,
      augmentedTolerance = augmentedEvidence.tolerance,
      decision = decision,
      detail =
        s"design rank=${designEvidence.rank} (tol=${designEvidence.tolerance}), " +
          s"contrast rank=${contrastEvidence.rank} (tol=${contrastEvidence.tolerance}), " +
          s"augmented rank=${augmentedEvidence.rank} (tol=${augmentedEvidence.tolerance}), " +
          s"convention=${tolerance.convention}"
    )
    decision match
      case StructuralEstimabilityDecision.Estimable => Right(evidence)
      case StructuralEstimabilityDecision.NonEstimable =>
        Left(FitError.StructuralHypothesisFailure(id.value, StructuralHypothesisErrorKind.NonEstimable, evidence.detail))

  private def appendRows(base: DMat, extra: DMat): DMat =
    val out = Matrix.newBuilder(base.rows + extra.rows, base.cols)
    var row = 0
    while row < base.rows do
      var col = 0
      while col < base.cols do
        out(row, col) = base(row, col)
        col += 1
      row += 1
    var extraRow = 0
    while extraRow < extra.rows do
      var col = 0
      while col < extra.cols do
        out(base.rows + extraRow, col) = extra(extraRow, col)
        col += 1
      extraRow += 1
    out.result()

  private final case class NumericalRank(rank: Int, tolerance: Double)

  private def rankOf(matrix: DMat, tolerance: OlsRankTolerance): NumericalRank =
    if matrix.rows == 0 || matrix.cols == 0 then NumericalRank(0, 0.0)
    else
      val qr = matrix.qr(QROptions(QRPivoting.Column, tolerance.qrValue))
      NumericalRank(
        rank = qr.diagnostics.rank.getOrElse(math.min(matrix.rows, matrix.cols)),
        tolerance = qr.diagnostics.rankTolerance.getOrElse(0.0)
      )

  private[fit] def validateResultAxis(
      id: ContrastId,
      expected: CoefficientAxis,
      actual: Option[CoefficientAxis]
  ): Either[FitError, Unit] =
    actual match
      case None => Left(FitError.MissingStructuralIdentity(s"compiled hypothesis '${id.value}'"))
      case Some(axis) if expected.structurallyCompatible(axis) => Right(())
      case Some(axis)
          if expected.designFingerprint == axis.designFingerprint && expected.columnIds != axis.columnIds =>
        Left(
          FitError.HypothesisCoefficientAxisMismatch(
            id.value,
            expected.designFingerprint.value,
            axis.designFingerprint.value,
            expected.columnIds.map(_.value),
            axis.columnIds.map(_.value)
          )
        )
      case Some(axis) =>
        Left(FitError.HypothesisDesignMismatch(id.value, expected.designFingerprint.value, axis.designFingerprint.value))

  private[fit] def alignTForResult(
      id: ContrastId,
      expected: CoefficientAxis,
      actual: Option[CoefficientAxis],
      weights: DMat
  ): Either[FitError, AlignedTContrast] =
    alignColumns(id, expected, actual).flatMap { case (axis, indices) =>
      requireSelectedTColumns(id, expected, axis, weights).map { _ =>
        val projected = Matrix.newBuilder(weights.rows, indices.length)
        var row = 0
        while row < weights.rows do
          var col = 0
          while col < indices.length do
            projected(row, col) = weights(row, indices(col))
            col += 1
          row += 1
        AlignedTContrast.unsafe(id.value, axis.columnNames, projected.result(), Some(axis))
      }
    }

  private[fit] def alignFForResult(
      id: ContrastId,
      expected: CoefficientAxis,
      actual: Option[CoefficientAxis],
      weights: DMat,
      contrastRank: Int
  ): Either[FitError, AlignedFContrast] =
    alignColumns(id, expected, actual).flatMap { case (axis, indices) =>
      requireSelectedFColumns(id, expected, axis, weights).map { _ =>
        val projected = Matrix.newBuilder(indices.length, weights.cols)
        var row = 0
        while row < indices.length do
          var col = 0
          while col < weights.cols do
            projected(row, col) = weights(indices(row), col)
            col += 1
          row += 1
        AlignedFContrast.unsafe(id.value, axis.columnNames, projected.result(), Some(axis), contrastRank)
      }
    }

  private def requireSelectedTColumns(
      id: ContrastId,
      expected: CoefficientAxis,
      actual: CoefficientAxis,
      weights: DMat
  ): Either[FitError, Unit] =
    val available = actual.columnIds.toSet
    val missing = expected.columnIds.zipWithIndex.collect {
      case (column, index) if !available.contains(column) && tColumnSelected(weights, index) => column
    }
    requireNoMissingSelectedColumns(id, missing)

  private def requireSelectedFColumns(
      id: ContrastId,
      expected: CoefficientAxis,
      actual: CoefficientAxis,
      weights: DMat
  ): Either[FitError, Unit] =
    val available = actual.columnIds.toSet
    val missing = expected.columnIds.zipWithIndex.collect {
      case (column, index) if !available.contains(column) && fColumnSelected(weights, index) => column
    }
    requireNoMissingSelectedColumns(id, missing)

  private def requireNoMissingSelectedColumns(
      id: ContrastId,
      missing: Vector[ColumnId]
  ): Either[FitError, Unit] =
    if missing.isEmpty then Right(())
    else
      failure(
        id,
        StructuralHypothesisErrorKind.NonEstimable,
        s"result axis omits selected structural columns: ${missing.map(_.value).mkString(", ")}"
      )

  private def tColumnSelected(weights: DMat, column: Int): Boolean =
    var row = 0
    while row < weights.rows do
      if weights(row, column) != 0.0 then return true
      row += 1
    false

  private def fColumnSelected(weights: DMat, column: Int): Boolean =
    var contrast = 0
    while contrast < weights.cols do
      if weights(column, contrast) != 0.0 then return true
      contrast += 1
    false

  private def alignColumns(
      id: ContrastId,
      expected: CoefficientAxis,
      actual: Option[CoefficientAxis]
  ): Either[FitError, (CoefficientAxis, Vector[Int])] =
    actual match
      case None => Left(FitError.MissingStructuralIdentity(s"compiled hypothesis '${id.value}'"))
      case Some(axis) if expected.designFingerprint != axis.designFingerprint =>
        Left(FitError.HypothesisDesignMismatch(id.value, expected.designFingerprint.value, axis.designFingerprint.value))
      case Some(axis) if expected.structurallyCompatible(axis) =>
        Right(axis -> axis.columnIds.indices.toVector)
      case Some(axis) =>
        val indices = axis.columnIds.map(expected.columnIds.indexOf)
        if indices.exists(_ < 0) then
          Left(
            FitError.HypothesisCoefficientAxisMismatch(
              id.value,
              expected.designFingerprint.value,
              axis.designFingerprint.value,
              expected.columnIds.map(_.value),
              axis.columnIds.map(_.value)
            )
          )
        else Right(axis -> indices)
