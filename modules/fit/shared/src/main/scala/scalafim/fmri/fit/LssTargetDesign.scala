package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.design.ColumnId

/** Stable identity for one trial column in a caller-selected LSS term. */
final case class LssTrialIdentity(trialId: String, sourceColumnId: ColumnId):
  require(trialId.trim.nonEmpty, "LSS trial id must be non-empty")

enum LssTargetDesignError:
  case IdentityCount(expected: Int, actual: Int)
  case DuplicateTrialIds(ids: Vector[String])
  case DuplicateSourceColumns(ids: Vector[ColumnId])
  case RowProvenanceCount(expected: Int, actual: Int)
  case DuplicateRowProvenance(rows: Vector[Int])
  case NegativeRowProvenance(row: Int)
  case MissingTarget(id: String, known: Vector[String])
  case FixedRowMismatch(expected: Int, actual: Int)
  case NonFiniteOtherTrials(row: Int)

  def message: String = this match
    case IdentityCount(expected, actual) => s"LSS trial identities must match $expected trial columns, got $actual"
    case DuplicateTrialIds(ids) => s"duplicate LSS trial ids: ${ids.mkString(", ")}"
    case DuplicateSourceColumns(ids) => s"duplicate LSS source columns: ${ids.map(_.value).mkString(", ")}"
    case RowProvenanceCount(expected, actual) => s"LSS row provenance must contain $expected rows, got $actual"
    case DuplicateRowProvenance(rows) => s"duplicate LSS row provenance: ${rows.mkString(", ")}"
    case NegativeRowProvenance(row) => s"negative LSS row provenance: $row"
    case MissingTarget(id, known) => s"LSS target '$id' is absent (known: ${known.mkString(", ")})"
    case FixedRowMismatch(expected, actual) => s"LSS fixed design has $actual rows, expected $expected"
    case NonFiniteOtherTrials(row) => s"LSS other-trials aggregate is non-finite at row $row"

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
      val duplicateTrials = identities.groupMapReduce(_.trialId)(_ => 1)(_ + _).collect { case (id, count) if count > 1 => id }.toVector.sorted
      val duplicateColumns = identities.groupMapReduce(_.sourceColumnId)(_ => 1)(_ + _).collect { case (id, count) if count > 1 => id }.toVector.sortBy(_.value)
      val duplicateRows = rowProvenance.groupMapReduce(identity)(_ => 1)(_ + _).collect { case (row, count) if count > 1 => row }.toVector.sorted
      if duplicateTrials.nonEmpty then Left(LssTargetDesignError.DuplicateTrialIds(duplicateTrials))
      else if duplicateColumns.nonEmpty then Left(LssTargetDesignError.DuplicateSourceColumns(duplicateColumns))
      else if duplicateRows.nonEmpty then Left(LssTargetDesignError.DuplicateRowProvenance(duplicateRows))
      else Right(LssTermDesign(trials, fixed, identities, rowProvenance))

/** One explicit LSS-1 design: target trial, the sum of its term peers, then
  * unchanged caller-owned fixed/nuisance columns. */
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
  def select(term: LssTermDesign, targetId: String): Either[LssTargetDesignError, LssTargetDesign] =
    term.identities.indexWhere(_.trialId == targetId) match
      case -1 => Left(LssTargetDesignError.MissingTarget(targetId, term.identities.map(_.trialId)))
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
