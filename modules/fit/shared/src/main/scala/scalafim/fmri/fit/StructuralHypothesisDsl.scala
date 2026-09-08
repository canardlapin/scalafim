package scalafim.fmri.fit

import scalafim.fmri.design.*
import scalafim.fmri.design.contrast.LevelId
import scalafim.fmri.hrf.*

import scala.annotation.targetName

/** A small consumer-facing vocabulary for hypotheses over a compiled design.
  *
  * The lower-level [[StructuralTContrast]] and [[StructuralFContrast]] types
  * remain the canonical interchange format.  This DSL is deliberately only a
  * construction layer: it keeps term, phase, cell, modulator, and basis
  * identity structural until the hypothesis is compiled against a
  * [[DesignSchema]].  In particular, it never asks callers to maintain a
  * coefficient ordinal or parse a rendered column label.
  *
  * {{
  * import StructuralHypothesisDsl.*
  *
  * val probe = term("probe")
  * val mismatch = probe.cell(matchStatus === "mismatch", load === "high")
  * val matchHigh = probe.cell(matchStatus === "match", load === "high")
  * val effect =
  *   (mismatch.response(Hrfs.SPMG2, ResponseFunctional.At(6.seconds)) -
  *     matchHigh.response(Hrfs.SPMG2, ResponseFunctional.At(6.seconds)))
  *     .named("probe-mismatch", "probe mismatch minus match at six seconds")
  * val compiled = effect.compile(schema)
  * }}
  */
object StructuralHypothesisDsl:

  /** A factor that can be used to build a structural cell assignment. */
  final case class FactorRef private[fit] (id: FactorId):
    infix def ===(level: String): CellAssignment =
      CellAssignment(
        id,
        LevelId(level).fold(error => throw new IllegalArgumentException(error.message), identity)
      )

    @targetName("assignLevelId")
    infix def ===(level: LevelId): CellAssignment =
      CellAssignment(id, level)

  /** A sampled/covariate regressor identified by structural provenance. */
  final case class SampledRef private[fit] (
      regressor: ModulatorId,
      role: Option[ColumnRole] = None,
      runScope: Option[RunScope] = None
  ):
    def withRole(value: ColumnRole): SampledRef =
      copy(role = Some(value))

    def inRun(run: RunIndex): SampledRef =
      copy(runScope = Some(RunScope.Run(run)))

    def coefficient: SemanticT =
      SemanticT.sampled(this)

  /** A reference to one event term, optionally restricted to one phase. */
  final case class TermRef private[fit] (id: TermId, phaseId: Option[PhaseId] = None):
    def inPhase(phase: String): TermRef =
      copy(
        phaseId = Some(
          PhaseId(phase).fold(error => throw new IllegalArgumentException(error.message), identity)
        )
      )

    @targetName("inPhaseId")
    def inPhase(phase: PhaseId): TermRef =
      copy(phaseId = Some(phase))

    def cell(assignments: CellAssignment*): CellRef =
      new CellRef(this, CellKey.unsafe(assignments))

  /** A structural cell reference. */
  final case class CellRef private[fit] (
      term: TermRef,
      cell: CellKey,
      modulator: ModulatorSelection = ModulatorSelection.Unmodulated
  ):
    def modulatedBy(modulator: String): CellRef =
      copy(
        modulator = ModulatorSelection.ById(
          ModulatorId(modulator).fold(error => throw new IllegalArgumentException(error.message), identity)
        )
      )

    @targetName("modulatedById")
    def modulatedBy(modulator: ModulatorId): CellRef =
      copy(modulator = ModulatorSelection.ById(modulator))

    /** Select one coefficient by semantic basis role. */
    def coefficient(basis: Hrf, role: BasisRole): SemanticT =
      SemanticT.coefficient(this, basis, role)

    /** Select the complete reconstructed response at one point or window. */
    def response(basis: Hrf, functional: ResponseFunctional): SemanticT =
      SemanticT.response(this, basis, functional)

    /** Select all basis coefficients, or only the shape dimensions. */
    def omnibus(basis: Hrf, scope: BasisScope = BasisScope.All): SemanticF =
      SemanticF.omnibus(this, basis, scope)

  /** Which basis coordinates belong to a joint F hypothesis. */
  enum BasisScope:
    case All
    case NonCanonical

  /** An unnamed structural T expression assembled from semantic effects.
    * Naming produces the only value that can be compiled.
    */
  final case class SemanticT private[fit] (
      private val selectors: Vector[SelectorEffect],
      private val coefficients: Vector[CoefficientEffect],
      private val responses: Vector[ResponseEffect]
  ):
    def +(other: SemanticT): SemanticT =
      combine(other)

    def -(other: SemanticT): SemanticT =
      combine(other.negated)

    def unary_- : SemanticT =
      negated

    /** Scale a semantic coefficient expression without exposing its lowered
      * column weights. Invalid zero or non-finite rows remain typed compile
      * failures at the structural hypothesis boundary.
      */
    def *(value: Double): SemanticT =
      scaled(value)

    /** Promote this expression to one independently meaningful F row. */
    def asOmnibusRow: SemanticF =
      SemanticF(Vector(this))

    def named(id: String, description: String): NamedT =
      val parsed = ContrastId(id).fold(error => throw new IllegalArgumentException(error.message), identity)
      named(parsed, description)

    @targetName("namedById")
    def named(id: ContrastId, description: String): NamedT =
      NamedT(id, requireDescription(description), this)

    private[fit] def lower(id: ContrastId, description: String): Either[FitError, StructuralTContrast] =
      for
        coefficientWeights <- traverse(coefficients)(_.lower(id))
        response <- traverse(responses)(_.lower(id))
        ordinary = selectors.map(_.lower) ++ coefficientWeights
        _ <-
          if ordinary.nonEmpty || response.nonEmpty then Right(())
          else
            Left(
              FitError.StructuralHypothesisFailure(
                id.value,
                StructuralHypothesisErrorKind.EmptySelection,
                "semantic T hypothesis has no selected columns"
              )
            )
      yield StructuralTContrast(
          id = id,
          description = description,
          terms = ordinary,
          source = HypothesisSource.Structural,
          responseTerms = response
        )

    private def combine(other: SemanticT): SemanticT =
      SemanticT(
        selectors ++ other.selectors,
        coefficients ++ other.coefficients,
        responses ++ other.responses
      )

    private def negated: SemanticT =
      scaled(-1.0)

    private def scaled(value: Double): SemanticT =
      SemanticT(
        selectors.map(_.scaled(value)),
        coefficients.map(_.scaled(value)),
        responses.map(_.scaled(value))
      )

  /** A named T hypothesis.  The type state makes forgetting `.named(...)` a
    * compile-time error instead of a failed runtime compilation.
    */
  final case class NamedT private[fit] (
      id: ContrastId,
      description: String,
      expression: SemanticT
  ):
    /** Materialize the canonical hypothesis without binding a design axis.
      * This preserves semantic selectors, response units and typed lowering
      * failures for APIs that accept structural hypotheses directly.
      */
    def toStructural: Either[FitError, StructuralTContrast] =
      expression.lower(id, description)

    def compile(
        schema: DesignSchema,
        rankTolerance: OlsRankTolerance = StructuralHypothesis.DefaultRankTolerance
    ): Either[FitError, CompiledTContrast] =
      toStructural.flatMap(_.compile(schema, rankTolerance))

  /** An unnamed structural F expression assembled from semantic basis effects. */
  final case class SemanticF private[fit] (
      private val rows: Vector[SemanticT]
  ):
    /** Form one joint F hypothesis from independently meaningful row blocks. */
    def +(other: SemanticF): SemanticF =
      SemanticF(rows ++ other.rows)

    def named(id: String, description: String): NamedF =
      val parsed = ContrastId(id).fold(error => throw new IllegalArgumentException(error.message), identity)
      named(parsed, description)

    @targetName("namedById")
    def named(id: ContrastId, description: String): NamedF =
      NamedF(id, requireDescription(description), this)

    private[fit] def lower(id: ContrastId, description: String): Either[FitError, StructuralFContrast] =
      if rows.isEmpty then
        Left(
          FitError.StructuralHypothesisFailure(
            id.value,
            StructuralHypothesisErrorKind.EmptySelection,
            "semantic F hypothesis has no selected basis elements"
          )
        )
      else
        val lowered = rows.map(_.lower(id, description))
        sequence(lowered).map { values =>
          val responseRows =
            if values.exists(_.responseTerms.nonEmpty) then values.map(_.responseTerms)
            else Vector.empty
          StructuralFContrast(
            id = id,
            description = description,
            rows = values.map(_.terms),
            source = HypothesisSource.Structural,
            responseRows = responseRows
          )
        }

  /** A named F hypothesis. */
  final case class NamedF private[fit] (
      id: ContrastId,
      description: String,
      expression: SemanticF
  ):
    /** Materialize the canonical hypothesis without binding a design axis.
      * This preserves semantic selectors, response units and typed lowering
      * failures for APIs that accept structural hypotheses directly.
      */
    def toStructural: Either[FitError, StructuralFContrast] =
      expression.lower(id, description)

    def compile(
        schema: DesignSchema,
        rankTolerance: OlsRankTolerance = StructuralHypothesis.DefaultRankTolerance
    ): Either[FitError, CompiledFContrast] =
      toStructural.flatMap(_.compile(schema, rankTolerance))

  private[fit] final case class CoefficientEffect(
      cell: CellRef,
      basis: Hrf,
      role: BasisRole,
      value: Double
  ):
    def scaled(scale: Double): CoefficientEffect = copy(value = value * scale)

    def lower(id: ContrastId): Either[FitError, StructuralWeight] =
      basisRef(basis, role, id).map { ref =>
        StructuralWeight(selector(cell, Some(ref)), value)
      }

  private[fit] final case class SelectorEffect(
      selector: StructuralColumnSelector,
      value: Double
  ):
    def scaled(scale: Double): SelectorEffect = copy(value = value * scale)
    def lower: StructuralWeight = StructuralWeight(selector, value)

  private[fit] final case class ResponseEffect(
      cell: CellRef,
      basis: Hrf,
      functional: ResponseFunctional,
      value: Double
  ):
    def scaled(scale: Double): ResponseEffect = copy(value = value * scale)

    def lower(id: ContrastId): Either[FitError, StructuralResponseWeight] =
      for
        refs <- basisRefs(basis, id)
        functionalWeights <- responseWeights(basis, functional, id)
      yield StructuralResponseWeight(
        selector = selector(cell, None),
        functional = functionalWeights.functional,
        units = functionalWeights.units,
        basisWeights = refs.zip(functionalWeights.values).map { case (ref, weight) =>
          BasisFunctionalWeight(ref, value * weight)
        }
      )

  private final case class FunctionalValues(
      functional: ResponseFunctional,
      units: ResponseUnits,
      values: Vector[Double]
  )

  private object SemanticT:
    def coefficient(cell: CellRef, basis: Hrf, role: BasisRole): SemanticT =
      SemanticT(Vector.empty, Vector(CoefficientEffect(cell, basis, role, 1.0)), Vector.empty)

    def response(cell: CellRef, basis: Hrf, functional: ResponseFunctional): SemanticT =
      SemanticT(Vector.empty, Vector.empty, Vector(ResponseEffect(cell, basis, functional, 1.0)))

    def sampled(ref: SampledRef): SemanticT =
      SemanticT(
        Vector(SelectorEffect(
          StructuralColumnSelector.sampled(
            regressor = Some(ref.regressor),
            role = ref.role,
            runScope = ref.runScope
          ),
          1.0
        )),
        Vector.empty,
        Vector.empty
      )

  private object SemanticF:
    def omnibus(cell: CellRef, basis: Hrf, scope: BasisScope): SemanticF =
      val elements = basis.basisElements.filter { element =>
        scope match
          case BasisScope.All          => true
          case BasisScope.NonCanonical => element.role != BasisRole.Canonical
      }
      SemanticF(
        elements.map(element =>
          SemanticT.coefficient(cell, basis, element.role)
        )
      )

  def term(id: String): TermRef =
    TermId(id).fold(error => throw new IllegalArgumentException(error.message), value => new TermRef(value))

  @targetName("termFromId")
  def term(id: TermId): TermRef =
    new TermRef(id)

  def factor(id: String): FactorRef =
    FactorId(id).fold(error => throw new IllegalArgumentException(error.message), value => new FactorRef(value))

  @targetName("factorFromId")
  def factor(id: FactorId): FactorRef =
    new FactorRef(id)

  def sampled(regressor: String): SampledRef =
    ModulatorId(regressor).fold(
      error => throw new IllegalArgumentException(error.message),
      value => new SampledRef(value)
    )

  @targetName("sampledFromId")
  def sampled(regressor: ModulatorId): SampledRef =
    new SampledRef(regressor)

  private def selector(cell: CellRef, basis: Option[BasisElementRef]): StructuralColumnSelector =
    StructuralColumnSelector.event(
      term = Some(cell.term.id),
      phase = cell.term.phaseId,
      cell = Some(cell.cell),
      modulator = cell.modulator,
      basis = basis
    )

  private def basisRefs(basis: Hrf, id: ContrastId): Either[FitError, Vector[BasisElementRef]] =
    basis.basisElementsValidated.left.map { error =>
      FitError.StructuralHypothesisFailure(
        id.value,
        StructuralHypothesisErrorKind.IncompatibleDesign,
        s"basis '${basis.name}' has invalid semantic elements: ${error.message}"
      )
    }.map { elements =>
      elements.map { element =>
        BasisElementRef(
          basisId = basis.name,
          index = BasisIndex.unsafeOneBased(element.index),
          role = Some(element.role),
          elementId = Some(element.id)
        )
      }
    }

  private def basisRef(basis: Hrf, role: BasisRole, id: ContrastId): Either[FitError, BasisElementRef] =
    basisRefs(basis, id).flatMap { refs =>
      refs.find(_.role.contains(role)).toRight(
        FitError.StructuralHypothesisFailure(
          id.value,
          StructuralHypothesisErrorKind.UnknownBasis,
          s"basis '${basis.name}' has no element with role '${role.stableLabel}'"
        )
      )
    }

  private def responseWeights(
      basis: Hrf,
      functional: ResponseFunctional,
      id: ContrastId
  ): Either[FitError, FunctionalValues] =
    ResponseBasis.of(basis).responseFunctional(functional).left.map { error =>
      FitError.StructuralHypothesisFailure(
        id.value,
        StructuralHypothesisErrorKind.IncompatibleDesign,
        s"response functional '$functional' cannot be evaluated on basis '${basis.name}': ${error.message}"
      )
    }.map { values =>
      FunctionalValues(values.functional, values.units, values.values)
    }

  private def requireDescription(description: String): String =
    require(description.trim.nonEmpty, "semantic hypothesis description must be non-empty")
    description

  private def traverse[A, B](values: Vector[A])(f: A => Either[FitError, B]): Either[FitError, Vector[B]] =
    val out = Vector.newBuilder[B]
    var i = 0
    while i < values.length do
      f(values(i)) match
        case Left(error) => return Left(error)
        case Right(value) => out += value
      i += 1
    Right(out.result())

  private def sequence[A](values: Vector[Either[FitError, A]]): Either[FitError, Vector[A]] =
    traverse(values)(identity)
