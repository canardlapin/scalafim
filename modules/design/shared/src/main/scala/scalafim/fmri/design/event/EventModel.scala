package scalafim.fmri.design.event

import scalafim.fmri.design.*
import scalafim.fmri.design.contrast.ContrastSpec
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

import scala.collection.immutable.VectorMap
import scala.util.control.NonFatal

enum EventModelDiagnosticKind:
  case BasisDegeneracy, DegenerateModulator, NonFiniteModulator, OnsetOutOfBounds

final case class EventModelDiagnostic(
    kind: EventModelDiagnosticKind,
    term: String,
    message: String,
    eventIndex: Option[Int] = None,
    column: Option[String] = None
)

final case class EventModel(
    terms: Vector[(String, EventModelTerm)],
    samplingFrame: SamplingFrame,
    designMatrix: Mat,
    columnNames: Vector[String],
    termSpans: Vector[(Int, Int)],
    colIndices: Map[String, Vector[Int]],
    contrastSetsByTerm: VectorMap[String, ContrastSpec.ContrastSet] = VectorMap.empty,
    diagnostics: Vector[EventModelDiagnostic] = Vector.empty,
    compiledSchema: Option[DesignSchema] = None,
    missingValueResolutions: Vector[MissingValueResolution] = Vector.empty,
    policyReceipts: Vector[PolicyReceipt] = Vector.empty,
    centeringReceipts: Vector[CenteringReceipt] = Vector.empty,
    degenerateModulatorReceipts: Vector[DegenerateModulatorReceipt] = Vector.empty,
    orthogonalizationReceipts: Vector[OrthogonalizationReceipt] = Vector.empty
)
:
  def termKeys: Vector[String] = terms.map(_._1)

  /** Canonical matrix/schema view.  Hand-built legacy values receive an
    * explicit `Legacy` structural origin until they are rebuilt through the
    * checked constructors.
    */
  def designSchema: DesignSchema =
    compiledSchema.getOrElse(
      DesignSchema.legacy(
        matrix = designMatrix,
        samplingFrame = samplingFrame,
        columnNames = columnNames,
        source = ModelSource.Event
      )
    )

  def designSchemaValidation: Either[DesignError, Unit] =
    compiledSchema match
      case Some(schema) => schema.validate
      case None         => Right(())

  /** Compatibility wrapper over [[withDiagnosticsEither]]. Compiler paths use
    * the total method so schema-validation failures retain their typed cause.
    */
  def withDiagnostics(values: Vector[EventModelDiagnostic]): EventModel =
    withDiagnosticsEither(values).fold(error => throw new IllegalArgumentException(error.message), identity)

  /** Total compiler boundary for attaching diagnostics and refreshing identity. */
  def withDiagnosticsEither(values: Vector[EventModelDiagnostic]): Either[DesignError, EventModel] =
    val schema0 = designSchema
    val audit0 = schema0.audit.copy(diagnostics = values.map(EventModel.toDesignDiagnostic))
    DesignSchema
      .validated(schema0.matrix, schema0.rows, schema0.columns, audit0)
      .map(schema1 => copy(diagnostics = values, compiledSchema = compiledSchema.map(_ => schema1)))

  /** Compatibility wrapper over [[withPolicyEvidenceEither]].
    *
    * Policy decisions are part of scientific identity: two numerically equal
    * matrices produced by different missing-value repairs must not silently
    * share a design fingerprint.
    */
  def withPolicyEvidence(
      missing: Vector[MissingValueResolution],
      policies: Vector[PolicyReceipt],
      factorLevels: Vector[FactorLevelAudit] = Vector.empty,
      emptyCells: Vector[CellKey] = Vector.empty,
      emptyCellAudits: Vector[EmptyCellAudit] = Vector.empty,
      centering: Vector[CenteringReceipt] = Vector.empty,
      degenerateModulators: Vector[DegenerateModulatorReceipt] = Vector.empty,
      orthogonalization: Vector[OrthogonalizationReceipt] = Vector.empty
  ): EventModel =
    withPolicyEvidenceEither(
      missing = missing,
      policies = policies,
      factorLevels = factorLevels,
      emptyCells = emptyCells,
      emptyCellAudits = emptyCellAudits,
      centering = centering,
      degenerateModulators = degenerateModulators,
      orthogonalization = orthogonalization
    ).fold(error => throw new IllegalArgumentException(error.message), identity)

  /** Total compiler boundary for attaching auditable scientific policies. */
  def withPolicyEvidenceEither(
      missing: Vector[MissingValueResolution],
      policies: Vector[PolicyReceipt],
      factorLevels: Vector[FactorLevelAudit] = Vector.empty,
      emptyCells: Vector[CellKey] = Vector.empty,
      emptyCellAudits: Vector[EmptyCellAudit] = Vector.empty,
      centering: Vector[CenteringReceipt] = Vector.empty,
      degenerateModulators: Vector[DegenerateModulatorReceipt] = Vector.empty,
      orthogonalization: Vector[OrthogonalizationReceipt] = Vector.empty
  ): Either[DesignError, EventModel] =
    val schema0 = designSchema
    val audit0 = schema0.audit.copy(
      factorLevels = schema0.audit.factorLevels ++ factorLevels,
      emptyCells = (schema0.audit.emptyCells ++ emptyCells).distinct,
      emptyCellAudits = (schema0.audit.emptyCellAudits ++ emptyCellAudits).distinct,
      missingValues = schema0.audit.missingValues ++ missing,
      centeringReceipts = schema0.audit.centeringReceipts ++ centering,
      degenerateModulatorReceipts = schema0.audit.degenerateModulatorReceipts ++ degenerateModulators,
      orthogonalizationReceipts = schema0.audit.orthogonalizationReceipts ++ orthogonalization,
      policyReceipts = schema0.audit.policyReceipts ++ policies
    )
    DesignSchema
      .validated(schema0.matrix, schema0.rows, schema0.columns, audit0)
      .map { schema1 =>
        copy(
          compiledSchema = compiledSchema.map(_ => schema1),
          missingValueResolutions = missingValueResolutions ++ missing,
          policyReceipts = policyReceipts ++ policies,
          centeringReceipts = centeringReceipts ++ centering,
          degenerateModulatorReceipts = degenerateModulatorReceipts ++ degenerateModulators,
          orthogonalizationReceipts = orthogonalizationReceipts ++ orthogonalization
        )
      }

object EventModel:

  def build(terms: Seq[ConvolvedTerm], samplingFrame: SamplingFrame): EventModel =
    buildTerms(terms.toVector, samplingFrame)

  def buildTermsEither(terms: Seq[EventModelTerm], samplingFrame: SamplingFrame): Either[DesignError, EventModel] =
    try Right(buildTerms(terms, samplingFrame))
    catch
      case NonFatal(t) => Left(DesignError.fromThrowable(t))

  def buildTerms(terms: Seq[EventModelTerm], samplingFrame: SamplingFrame): EventModel =
    val ts0 = terms.toVector
    val totalRows = samplingFrame.blockLens.sum
    ts0.foreach { t =>
      require(t.data.rows == totalRows, "term matrix row mismatch with samplingFrame")
      t.requireColumnMetadata()
    }

    val rawKeys =
      ts0.zipWithIndex.map { case (t, i) =>
        t.keyHint.getOrElse(s"term_${i + 1}")
      }
    val keys = Names.makeUniqueTags(rawKeys)

    val totalCols = ts0.map(_.data.cols).sum
    val out = new Array[Double](totalRows * totalCols)
    val colNames = Vector.newBuilder[String]

    val spans = Vector.newBuilder[(Int, Int)]
    val indices = scala.collection.mutable.LinkedHashMap.empty[String, Vector[Int]]
    val outTerms = Vector.newBuilder[(String, EventModelTerm)]

    var colOffset = 0
    var i = 0
    while i < ts0.length do
      val term = ts0(i)
      val cols = term.data.cols
      val key = keys(i)

      // Copy into the combined matrix (row-major cbind).
      var r = 0
      while r < totalRows do
        System.arraycopy(term.data.data, r * cols, out, r * totalCols + colOffset, cols)
        r += 1

      val term0 =
        (term, term.keyHint) match
          case (ct: ConvolvedTerm, Some(old)) if old != key =>
            val ren = ct.columnNames.map { cn =>
              if cn.startsWith(old + "_") then key + cn.drop(old.length) else cn
            }
            ct.copy(term = ct.term.copy(termTag = Some(key)), columnNames = ren)
          case (cv: CovariateConvolvedTerm, Some(old)) if old != key =>
            cv.copy(id = key, spec = cv.spec.copy(id = Some(key)))
          case _ => term

      outTerms += (key -> term0)
      colNames ++= term0.columnNames

      val start = colOffset
      val endExcl = colOffset + cols
      spans += ((start, endExcl))

      indices.update(key, (start until endExcl).toVector)

      colOffset = endExcl
      i += 1

    val termVector = outTerms.result()
    val matrix = Mat.unsafe(totalRows, totalCols, out)
    val schemaColumns = structuralColumns(termVector, totalCols)
    val audit = auditFor(termVector, diagnostics = Vector.empty)
    val schema =
      DesignSchema.validated(
        matrix = matrix,
        rows = RowLayout.fromSamplingFrame(samplingFrame),
        columns = schemaColumns,
        audit = audit
      ).fold(error => throw new IllegalArgumentException(error.message), identity)

    EventModel(
      terms = termVector,
      samplingFrame = samplingFrame,
      designMatrix = matrix,
      columnNames = colNames.result(),
      termSpans = spans.result(),
      colIndices = indices.toMap,
      compiledSchema = Some(schema)
    )

  private def structuralColumns(
      terms: Vector[(String, EventModelTerm)],
      totalCols: Int
  ): Vector[StructuralColumn] =
    val out = Vector.newBuilder[StructuralColumn]
    out.sizeHint(totalCols)
    var ordinal = 1
    terms.foreach { case (key, term) =>
      val roles = term.resolvedColumnRoles
      var local = 0
      while local < term.data.cols do
        val origin = originFor(key, term, local, roles(local))
        val label = term.columnNames(local)
        val hrfScale = term match
          case convolved: ConvolvedTerm => convolved.scaleForColumn(local)
          case _                        => HrfColumnScale.identity
        val column =
          StructuralColumn.fromOrigin(ordinal, origin, label, prettyLabel = label, hrfScale = hrfScale)
            .fold(error => throw new IllegalArgumentException(error.message), identity)
        out += column
        ordinal += 1
        local += 1
    }
    out.result()

  private def originFor(
      key: String,
      term: EventModelTerm,
      local: Int,
      role: EventTermColumnRole
  ): StructuralColumnOrigin =
    term match
      case ct: ConvolvedTerm =>
        val columnHrf = ct.hrfForColumn(local)
        val basis =
          if ct.columnBasisIx.nonEmpty && local < ct.columnBasisIx.length then
            ct.columnBasisIx(local).map { ix =>
              val element = columnHrf.basisElementsValidated.toOption.flatMap(_.lift(ix - 1))
              BasisElementRef(
                basisId = columnHrf.name,
                index = BasisIndex.unsafeOneBased(ix),
                role = element.map(_.role),
                elementId = element.map(_.id)
              )
            }
          else None
        val cell =
          if ct.columnCells.nonEmpty && local < ct.columnCells.length then ct.columnCells(local).getOrElse(CellKey.empty)
          else CellKey.empty
        val modulator =
          if ct.columnModulators.nonEmpty && local < ct.columnModulators.length then ct.columnModulators(local)
          else None
        StructuralColumnOrigin.Event(
          term = TermId.unsafe(key),
          phase = ct.term.phaseId,
          cell = cell,
          modulator = modulator,
          basis = basis,
          role = eventColumnRole(role),
          runScope = eventRunScope(ct.term)
        )
      case cv: CovariateConvolvedTerm =>
        val id = cv.id
        StructuralColumnOrigin.Sampled(
          regressor = ModulatorId.unsafe(id),
          role = ColumnRole.Covariate,
          runScope = RunScope.Global
        )
      case _ =>
        StructuralColumnOrigin.Legacy(ModelSource.Event, term.columnNames(local), local + 1)

  private def eventColumnRole(role: EventTermColumnRole): ColumnRole =
    role match
      case EventTermColumnRole.Task          => ColumnRole.Task
      case EventTermColumnRole.Trial         => ColumnRole.Trial
      case EventTermColumnRole.TrialAggregate => ColumnRole.TrialAggregate
      case EventTermColumnRole.Covariate     => ColumnRole.Covariate

  private def eventRunScope(term: EventTerm): RunScope =
    term.blockIds0.distinct match
      case Vector(run) => RunScope.Run(RunIndex.unsafeOneBased(run + 1))
      case _            => RunScope.Global

  private def auditFor(
      terms: Vector[(String, EventModelTerm)],
      diagnostics: Vector[EventModelDiagnostic]
  ): DesignAudit =
    val convolved = terms.collect { case (_, ct: ConvolvedTerm) => ct }
    val seen = convolved.map(_.term.onsets.length).sum
    val used = convolved.map(_.term.onsets.count(onset => onset.value.isFinite && onset.value >= 0.0)).sum
    val excluded =
      convolved.flatMap { ct =>
        ct.term.onsets.zipWithIndex.collect {
          case (onset, i) if !onset.value.isFinite || onset.value < 0.0 =>
            EventExclusion(i, "negative-or-non-finite-onset", ct.term.termTag.map(TermId.unsafe))
        }
      }
    val policies = convolved.map { ct =>
      val sources =
        if ct.columnHrfs.isEmpty then Vector(ct.hrf)
        else ct.columnHrfs.distinct
      PolicyReceipt(
        "hrf",
        s"sources=${sources.map(hrf => s"${hrf.name};nbasis=${hrf.nbasis}").mkString(",")}"
      )
    }
    DesignAudit(
      eventsSeen = seen,
      eventsUsed = used,
      excludedEvents = excluded,
      eventProvenance = convolved.flatMap(_.term.eventProvenance),
      policyReceipts = policies,
      diagnostics = diagnostics.map(toDesignDiagnostic)
    )

  private[design] def toDesignDiagnostic(diagnostic: EventModelDiagnostic): DesignDiagnostic =
    val kind =
      diagnostic.kind match
        case EventModelDiagnosticKind.BasisDegeneracy    => DesignDiagnosticKind.BasisDegeneracy
        case EventModelDiagnosticKind.DegenerateModulator => DesignDiagnosticKind.DegenerateModulator
        case EventModelDiagnosticKind.NonFiniteModulator  => DesignDiagnosticKind.NonFiniteModulator
        case EventModelDiagnosticKind.OnsetOutOfBounds    => DesignDiagnosticKind.OnsetOutOfBounds
    DesignDiagnostic(kind, TermId(diagnostic.term).toOption, diagnostic.message)
