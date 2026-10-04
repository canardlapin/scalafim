package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.design.{ColumnId, TrialId, RunIndex as DesignRunIndex}

/** Identity of one trial within a trialwise term. `trialwise(id = ...)` ids are
  * unique only within a run, so the run is part of the key. */
final case class LssTrialKey(run: DesignRunIndex, trial: TrialId):
  def label: String = s"run ${run.oneBased} trial ${trial.value}"

/** Stable identity for one trial column in a caller-selected LSS term. The
  * trial design column at the same position must be named `sourceColumnId`. */
final case class LssTrialIdentity(run: DesignRunIndex, trialId: TrialId, sourceColumnId: ColumnId):
  def key: LssTrialKey = LssTrialKey(run, trialId)

enum LssTargetDesignError:
  case IdentityCount(expected: Int, actual: Int)
  case IdentityBinding(position: Int, expected: ColumnId, actual: String)
  case DuplicateTrialIds(keys: Vector[LssTrialKey])
  case DuplicateSourceColumns(ids: Vector[ColumnId])
  case TrialColumnInFixed(id: ColumnId)
  case NonFiniteTrialValue(column: ColumnId, row: Int)
  case NonFiniteFixedValue(column: String, row: Int)
  case RowProvenanceCount(expected: Int, actual: Int)
  case DuplicateRowProvenance(rows: Vector[Int])
  case NegativeRowProvenance(row: Int)
  case MissingTarget(key: LssTrialKey, known: Vector[LssTrialKey])
  case FixedRowMismatch(expected: Int, actual: Int)
  case NonFiniteOtherTrials(row: Int)

  def message: String = this match
    case IdentityCount(expected, actual) => s"LSS trial identities must match $expected trial columns, got $actual"
    case IdentityBinding(position, expected, actual) =>
      s"LSS trial identity ${position + 1} names column '${expected.value}' but the trial design column there is '$actual'"
    case DuplicateTrialIds(keys) => s"duplicate LSS trial ids: ${keys.map(_.label).mkString(", ")}"
    case DuplicateSourceColumns(ids) => s"duplicate LSS source columns: ${ids.map(_.value).mkString(", ")}"
    case TrialColumnInFixed(id) => s"LSS trial column '${id.value}' also appears in the fixed design; a term's trials belong only in the trial design"
    case NonFiniteTrialValue(column, row) => s"LSS trial column '${column.value}' is non-finite at row $row"
    case NonFiniteFixedValue(column, row) => s"LSS fixed column '$column' is non-finite at row $row"
    case RowProvenanceCount(expected, actual) => s"LSS row provenance must contain $expected rows, got $actual"
    case DuplicateRowProvenance(rows) => s"duplicate LSS row provenance: ${rows.mkString(", ")}"
    case NegativeRowProvenance(row) => s"negative LSS row provenance: $row"
    case MissingTarget(key, known) => s"LSS target ${key.label} is absent (known: ${known.map(_.label).mkString(", ")})"
    case FixedRowMismatch(expected, actual) => s"LSS fixed design has $actual rows, expected $expected"
    case NonFiniteOtherTrials(row) => s"LSS other-trials aggregate is non-finite at row $row"

/** The trials of one term plus the caller-owned fixed design, ready for LSS-1
  * target selection.
  *
  * LSS-1 contract: each target design is `[target, sum of this term's other
  * trials, fixed]`. Everything that is not a trial of this term (other
  * conditions' trials, other task terms, drift, intercepts, nuisance) belongs
  * in `fixed`; this term's trial columns must not. Identities are bound to
  * trial columns by name, not only by position.
  */
final case class LssTermDesign private (
    trials: LssTrialDesign,
    fixed: LssFixedDesign,
    identities: Vector[LssTrialIdentity],
    rowProvenance: Vector[Int]
)

object LssTermDesign:
  def make(trials: LssTrialDesign, fixed: LssFixedDesign, identities: Vector[LssTrialIdentity], rowProvenance: Vector[Int]): Either[LssTargetDesignError, LssTermDesign] =
    if identities.length != trials.trials then Left(LssTargetDesignError.IdentityCount(trials.trials, identities.length))
    else if fixed.timepoints != trials.timepoints then Left(LssTargetDesignError.FixedRowMismatch(trials.timepoints, fixed.timepoints))
    else if rowProvenance.length != trials.timepoints then Left(LssTargetDesignError.RowProvenanceCount(trials.timepoints, rowProvenance.length))
    else if rowProvenance.exists(_ < 0) then Left(LssTargetDesignError.NegativeRowProvenance(rowProvenance.find(_ < 0).get))
    else
      val duplicateTrials = identities.groupMapReduce(_.key)(_ => 1)(_ + _).collect { case (key, count) if count > 1 => key }.toVector
        .sortBy(key => (key.run.oneBased, key.trial.value))
      val duplicateColumns = identities.groupMapReduce(_.sourceColumnId)(_ => 1)(_ + _).collect { case (id, count) if count > 1 => id }.toVector.sortBy(_.value)
      val duplicateRows = rowProvenance.groupMapReduce(identity)(_ => 1)(_ + _).collect { case (row, count) if count > 1 => row }.toVector.sorted
      val misbound = identities.indices.find(index => identities(index).sourceColumnId.value != trials.trialNames(index))
      val fixedNames = fixed.columnNames.toSet
      if duplicateTrials.nonEmpty then Left(LssTargetDesignError.DuplicateTrialIds(duplicateTrials))
      else if duplicateColumns.nonEmpty then Left(LssTargetDesignError.DuplicateSourceColumns(duplicateColumns))
      else if misbound.nonEmpty then
        val index = misbound.get
        Left(LssTargetDesignError.IdentityBinding(index, identities(index).sourceColumnId, trials.trialNames(index)))
      else if duplicateRows.nonEmpty then Left(LssTargetDesignError.DuplicateRowProvenance(duplicateRows))
      else identities.find(identity => fixedNames.contains(identity.sourceColumnId.value)) match
        case Some(identity) => Left(LssTargetDesignError.TrialColumnInFixed(identity.sourceColumnId))
        case None =>
          for
            _ <- firstNonFinite(trials.value).toLeft(()).left.map((row, column) => LssTargetDesignError.NonFiniteTrialValue(identities(column).sourceColumnId, row))
            _ <- firstNonFinite(fixed.value).toLeft(()).left.map((row, column) => LssTargetDesignError.NonFiniteFixedValue(fixed.columnNames(column), row))
          yield LssTermDesign(trials, fixed, identities, rowProvenance)

  private def firstNonFinite(matrix: DMat): Option[(Int, Int)] =
    var row = 0
    while row < matrix.rows do
      var column = 0
      while column < matrix.cols do
        if !matrix(row, column).isFinite then return Some((row, column))
        column += 1
      row += 1
    None

/** One explicit LSS-1 design: target trial, the sum of its term peers, then
  * unchanged caller-owned fixed/nuisance columns. A term with a single trial
  * has no peer column (`otherTrialsRegressor = None`). */
final case class LssTargetDesign private[fit] (
    target: LssTrialIdentity,
    targetRegressor: DMat,
    otherTrialsRegressor: Option[DMat],
    fixed: LssFixedDesign,
    rowProvenance: Vector[Int],
    sourceColumnIds: Vector[ColumnId]
):
  def matrix: DMat =
    Matrix.tabulate(targetRegressor.rows, 1 + otherTrialsRegressor.fold(0)(_ => 1) + fixed.predictors) { (row, column) =>
      if column == 0 then targetRegressor(row, 0)
      else otherTrialsRegressor match
        case Some(other) if column == 1 => other(row, 0)
        case Some(_) => fixed.value(row, column - 2)
        case None => fixed.value(row, column - 1)
    }

object LssTargetDesign:
  def select(term: LssTermDesign, target: LssTrialKey): Either[LssTargetDesignError, LssTargetDesign] =
    term.identities.indexWhere(_.key == target) match
      case -1 => Left(LssTargetDesignError.MissingTarget(target, term.identities.map(_.key)))
      case targetIndex =>
        val target = Matrix.tabulate(term.trials.timepoints, 1)((row, _) => term.trials.value(row, targetIndex))
        val otherIndices = term.identities.indices.filter(_ != targetIndex).toVector
        val otherValues = new Array[Double](term.trials.timepoints)
        var row = 0
        while row < term.trials.timepoints do
          var sum = 0.0
          var offset = 0
          while offset < otherIndices.length do
            sum += term.trials.value(row, otherIndices(offset))
            offset += 1
          if !sum.isFinite then return Left(LssTargetDesignError.NonFiniteOtherTrials(row))
          otherValues(row) = sum
          row += 1
        val other = if otherIndices.isEmpty then None else Some(Matrix.tabulate(term.trials.timepoints, 1)((row, _) => otherValues(row)))
        Right(LssTargetDesign(term.identities(targetIndex), target, other, term.fixed, term.rowProvenance, term.identities.map(_.sourceColumnId)))
