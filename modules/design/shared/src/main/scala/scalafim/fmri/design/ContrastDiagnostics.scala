package scalafim.fmri.design

import gale.linalg.DMat
import gale.spectral.SpectralBackend.given
import scalafim.fmri.hrf.linalg.Mat

final case class ColumnContrast(weights: Vector[(ColumnId, Double)]):
  require(weights.nonEmpty, "a contrast must contain at least one weight")
  require(weights.map(_._1).distinct.length == weights.length, "contrast column ids must be unique")

final case class FColumnContrast(rows: Vector[ColumnContrast]):
  require(rows.nonEmpty, "an F contrast must contain at least one row")

enum LostEstimability:
  case DroppedColumns(columns: Vector[ColumnId])
  case Aliased(residual: Double)
  case EmptyHypothesis

enum ContrastEstimability:
  case Estimable
  case Lost(reason: LostEstimability)

final case class TContrastDiagnostic(contrast: ColumnContrast, estimability: ContrastEstimability, varianceOverSigmaSquared: Option[Double])
final case class FContrastDiagnostic(contrast: FColumnContrast, estimability: ContrastEstimability, effectiveDf: Int, worstDirectionSeOverSigma: Option[Double])
final case class AliasEquation(coefficients: Vector[(ColumnId, Double)], residual: Double)

private[design] final case class ContrastRunGeometry(ids: Vector[ColumnId], matrix: Mat, right: Vector[Array[Double]], singular: Vector[Double], cutoff: Double)

final case class ContrastDiagnosticsResult(
    fingerprint: DesignFingerprint, selectedRows: Vector[ScanIndex], rankPolicy: RankPolicy,
    rank: Int, cutoff: Double, aliases: Vector[AliasEquation], t: Vector[TContrastDiagnostic],
    f: Vector[FContrastDiagnostic],
    commonSigmaAssumption: String = "Unwhitened OLS; reported variances are divided by one common residual sigma squared.",
    private[design] val geometry: ContrastRunGeometry
)

final case class FixedEffectsTDiagnostic(contrast: ColumnContrast, varianceOverSigmaSquared: Option[Double], contributingRuns: Vector[Int], excludedRuns: Vector[(Int, ContrastEstimability)], assumption: String = "Fixed effects assumes one common residual sigma squared across contributing runs.")
final case class FixedEffectsFDiagnostic(contrast: FColumnContrast, worstDirectionSeOverSigma: Option[Double], effectiveDf: Int, contributingRuns: Vector[Int], excludedRuns: Vector[(Int, ContrastEstimability)], assumption: String = "Fixed effects assumes one common residual sigma squared across contributing runs.")
final case class RunNuisanceDiagnostic(run: Int, nuisanceRank: Int, residualDegreesOfFreedom: Int)
final case class ConcatenatedTaskDiagnostic(taskColumns: Vector[ColumnId], contrast: ColumnContrast, varianceOverSigmaSquared: Option[Double], contributingRuns: Vector[Int], excludedRuns: Vector[Int], runNuisance: Vector[RunNuisanceDiagnostic])
final case class ConcatenatedTaskFDiagnostic(taskColumns: Vector[ColumnId], contrast: FColumnContrast, worstDirectionSeOverSigma: Option[Double], effectiveDf: Int, contributingRuns: Vector[Int], excludedRuns: Vector[Int], runNuisance: Vector[RunNuisanceDiagnostic])
final case class HighPassVarianceCost(selectedCosineColumns: Vector[ColumnId], withCosinesVarianceOverSigmaSquared: Double, withoutCosinesVarianceOverSigmaSquared: Double, varianceRatio: Double, excessVarianceRatio: Double)

/** General schema-bound, common-sigma OLS contrast diagnostics. The declared
  * RankPolicy controls every rank, covariance, projection, and alias decision. */
object ContrastDiagnostics:
  /** Reject only a rank-saturated selected design.  Raw column count is not a
    * degrees-of-freedom calculation: duplicate columns can leave residual
    * degrees of freedom even when p >= n. */
  def preflight(schema: DesignSchema, selectedRows: Vector[ScanIndex] = Vector.empty, rankPolicy: RankPolicy = RankPolicy.Default): Either[DesignDiagnosticsError, Unit] =
    val rows = if selectedRows.isEmpty then schema.rows.selectedRows else selectedRows
    if rows.isEmpty then Left(DesignDiagnosticsError.EmptyRows)
    else if rows.distinct.length != rows.length then Left(DesignDiagnosticsError.DuplicateRows)
    else rows.find(row => row.oneBased > schema.matrixValues.rows) match
      case Some(row) => Left(DesignDiagnosticsError.RowOutOfBounds(row, schema.matrixValues.rows))
      case None =>
        val selected = Mat.unsafe(rows.length, schema.matrixValues.cols, Array.tabulate(rows.length * schema.matrixValues.cols)(i => schema.matrixValues(rows(i / schema.matrixValues.cols).oneBased - 1, i % schema.matrixValues.cols)))
        val bad = selected.data.indexWhere(value => !value.isFinite)
        if bad >= 0 then Left(DesignDiagnosticsError.NonFiniteValue(rows(bad / selected.cols), schema.columnIds(bad % selected.cols), selected.data(bad)))
        else spectrum(DMat.tabulate(selected.rows, selected.cols)(selected.apply)).flatMap { case (singular, _) =>
          val maximum = singular.headOption.getOrElse(0.0)
          val rank = singular.count(_ > rankPolicy.cutoff(selected.rows, selected.cols, maximum))
          if rank >= rows.length then Left(DesignDiagnosticsError.InsufficientResidualDof(rows.length, rank)) else Right(())
        }

  /** Separate construction budget for callers that need to stop an
    * accidentally expansive model before allocation. */
  def checkColumnBudget(schema: DesignSchema, maximumColumns: Int): Either[DesignDiagnosticsError, Unit] =
    if maximumColumns < 0 then Left(DesignDiagnosticsError.NumericalFailure(s"column budget must be non-negative, got $maximumColumns"))
    else if schema.matrixValues.cols > maximumColumns then Left(DesignDiagnosticsError.NumericalFailure(s"design has ${schema.matrixValues.cols} columns and exceeds the configured budget $maximumColumns"))
    else Right(())

  def fixedEffectsT(perRun: Vector[ContrastDiagnosticsResult], contrast: ColumnContrast): FixedEffectsTDiagnostic =
    val seen = perRun.zipWithIndex.map { case (r, i) => i -> r.t.find(_.contrast == contrast) }
    val accepted = seen.collect { case (i, Some(TContrastDiagnostic(_, ContrastEstimability.Estimable, Some(v)))) => i -> v }
    val excluded = seen.collect {
      case (i, Some(d)) if d.estimability != ContrastEstimability.Estimable => i -> d.estimability
      case (i, None) => i -> ContrastEstimability.Lost(LostEstimability.EmptyHypothesis)
    }
    FixedEffectsTDiagnostic(contrast, if accepted.isEmpty then None else Some(1.0 / accepted.map((_, v) => 1.0 / v).sum), accepted.map(_._1), excluded)

  def fixedEffectsF(perRun: Vector[ContrastDiagnosticsResult], contrast: FColumnContrast): Either[DesignDiagnosticsError, FixedEffectsFDiagnostic] =
    sequence(perRun.zipWithIndex.map { case (run, index) => fCovariance(run, contrast).map(index -> _) }).flatMap { evaluated =>
      val accepted = evaluated.collect { case (index, Some(covariance)) => index -> covariance }
      val excluded = perRun.zipWithIndex.collect {
        case (run, index) if !accepted.exists(_._1 == index) =>
          index -> run.f.find(_.contrast == contrast).map(_.estimability).getOrElse(ContrastEstimability.Lost(LostEstimability.EmptyHypothesis))
      }
      if accepted.isEmpty then Right(FixedEffectsFDiagnostic(contrast, None, 0, Vector.empty, excluded))
      else combineCovariances(accepted.map(_._2)).flatMap { combined =>
        worstSe(combined).map(value => FixedEffectsFDiagnostic(contrast, Some(value), combined.rows, accepted.map(_._1), excluded))
      }
    }

  /** Uses sum_r T_r'(I - N_r N_r+)T_r. This deliberately excludes a run whose
    * task is wholly nuisance-confounded, rather than inverting a generalized
    * inverse task covariance block and inventing information. */
  def concatenatedTaskT(runs: Vector[ContrastDiagnosticsResult], taskColumns: Vector[ColumnId], contrast: ColumnContrast): Either[DesignDiagnosticsError, ConcatenatedTaskDiagnostic] =
    if taskColumns.isEmpty then Left(DesignDiagnosticsError.NumericalFailure("concatenated task diagnostics require task columns"))
    else if taskColumns.distinct.length != taskColumns.length then Left(DesignDiagnosticsError.NumericalFailure("concatenated task columns must be unique"))
    else if contrast.weights.exists((id, _) => !taskColumns.contains(id)) then Left(DesignDiagnosticsError.UnknownColumn(contrast.weights.find((id, _) => !taskColumns.contains(id)).get._1))
    else if runs.isEmpty then Right(ConcatenatedTaskDiagnostic(taskColumns, contrast, None, Vector.empty, Vector.empty, Vector.empty))
    else
      val used = Vector.newBuilder[Int]; val excluded = Vector.newBuilder[Int]
      val nuisanceDiagnostics = Vector.newBuilder[RunNuisanceDiagnostic]
      val residualized = Vector.newBuilder[Mat]
      runs.zipWithIndex.foreach { case (run, index) =>
        indices(run.geometry.ids, taskColumns) match
          case None => excluded += index
          case Some(task) =>
            val nuisance = (0 until run.geometry.matrix.cols).filterNot(task.contains).toVector
            nuisanceDiagnostics += RunNuisanceDiagnostic(index, matrixRank(DMat.tabulate(run.geometry.matrix.rows, nuisance.length)((r, c) => run.geometry.matrix(r, nuisance(c))), run.geometry.cutoff), run.geometry.matrix.rows - run.rank)
            val residual = residualizedTaskDesign(run.geometry.matrix, task, run.rankPolicy)
            if !hasNumericalSignal(residual, run.geometry.matrix, task, run.rankPolicy) then excluded += index
            else { residualized += residual; used += index }
      }
      vector(contrast, taskColumns).flatMap { w =>
        val contributing = used.result()
        val rejected = excluded.result()
        val diagnostics = nuisanceDiagnostics.result()
        if contributing.isEmpty then Right(ConcatenatedTaskDiagnostic(taskColumns, contrast, None, Vector.empty, rejected, diagnostics))
        else
          val stacked = stack(residualized.result(), taskColumns.length)
          spectrum(DMat.tabulate(stacked.rows, stacked.cols)(stacked.apply)).flatMap { case (singular, right) =>
            singular.headOption match
              case None => Left(DesignDiagnosticsError.NumericalFailure("contributing task design has no singular spectrum"))
              case Some(maximum) =>
                val cutoff = runs.head.rankPolicy.cutoff(stacked.rows, stacked.cols, maximum)
                val keep = singular.indices.filter(i => singular(i) > cutoff).toVector
                val variance = if residual(w, right, keep) > scale(w) then None else Some(quadratic(w, w, singular, right, keep))
                Right(ConcatenatedTaskDiagnostic(taskColumns, contrast, variance, contributing, rejected, diagnostics))
          }
      }

  def concatenatedTaskF(runs: Vector[ContrastDiagnosticsResult], taskColumns: Vector[ColumnId], contrast: FColumnContrast): Either[DesignDiagnosticsError, ConcatenatedTaskFDiagnostic] =
    if taskColumns.isEmpty then Left(DesignDiagnosticsError.NumericalFailure("concatenated task diagnostics require task columns"))
    else if taskColumns.distinct.length != taskColumns.length then Left(DesignDiagnosticsError.NumericalFailure("concatenated task columns must be unique"))
    else if contrast.rows.flatMap(_.weights).exists((id, _) => !taskColumns.contains(id)) then Left(DesignDiagnosticsError.UnknownColumn(contrast.rows.flatMap(_.weights).find((id, _) => !taskColumns.contains(id)).get._1))
    else if runs.isEmpty then Right(ConcatenatedTaskFDiagnostic(taskColumns, contrast, None, 0, Vector.empty, Vector.empty, Vector.empty))
    else
      val raw = contrast.rows.map(vector(_, taskColumns)).foldLeft[Either[DesignDiagnosticsError, Vector[Array[Double]]]](Right(Vector.empty))((a, x) => a.flatMap(v => x.map(v :+ _)))
      raw.flatMap { rows =>
        val q = orthonormal(rows)
        if q.isEmpty then Right(ConcatenatedTaskFDiagnostic(taskColumns, contrast, None, 0, Vector.empty, runs.indices.toVector, Vector.empty))
        else
          val residualized = Vector.newBuilder[Mat]; val used = Vector.newBuilder[Int]; val excluded = Vector.newBuilder[Int]; val diagnostics = Vector.newBuilder[RunNuisanceDiagnostic]
          runs.zipWithIndex.foreach { case (run, index) => indices(run.geometry.ids, taskColumns) match
            case None => excluded += index
            case Some(task) =>
              val nuisance = (0 until run.geometry.matrix.cols).filterNot(task.contains).toVector
              diagnostics += RunNuisanceDiagnostic(index, matrixRank(DMat.tabulate(run.geometry.matrix.rows, nuisance.length)((r, c) => run.geometry.matrix(r, nuisance(c))), run.geometry.cutoff), run.geometry.matrix.rows - run.rank)
              val residual = residualizedTaskDesign(run.geometry.matrix, task, run.rankPolicy)
              if !hasNumericalSignal(residual, run.geometry.matrix, task, run.rankPolicy) then excluded += index else { residualized += residual; used += index }
          }
          val residualRows = residualized.result()
          val contributing = used.result()
          val rejected = excluded.result()
          val runNuisance = diagnostics.result()
          if contributing.isEmpty then Right(ConcatenatedTaskFDiagnostic(taskColumns, contrast, None, 0, Vector.empty, rejected, runNuisance))
          else
            val stacked = stack(residualRows, taskColumns.length)
            spectrum(DMat.tabulate(stacked.rows, stacked.cols)(stacked.apply)).flatMap { case (singular, right) =>
              singular.headOption match
                case None => Left(DesignDiagnosticsError.NumericalFailure("contributing task design has no singular spectrum"))
                case Some(maximum) =>
                  val cutoff = runs.head.rankPolicy.cutoff(stacked.rows, stacked.cols, maximum)
                  val keep = singular.indices.filter(i => singular(i) > cutoff).toVector
                  val lost = q.map(residual(_, right, keep)).max > q.map(scale(_)).max
                  val covariance = DMat.tabulate(q.length, q.length)((i, j) => quadratic(q(i), q(j), singular, right, keep))
                  val se = if lost then Right(None) else worstSe(covariance).map(Some(_))
                  se.map(value => ConcatenatedTaskFDiagnostic(taskColumns, contrast, value, if value.isDefined then q.length else 0, contributing, rejected, runNuisance))
            }
      }

  def highPassVarianceRatio(withCosines: ContrastDiagnosticsResult, withoutCosines: ContrastDiagnosticsResult, contrast: ColumnContrast, selectedCosineColumns: Vector[ColumnId]): Either[DesignDiagnosticsError, HighPassVarianceCost] =
    highPassPair(withCosines, withoutCosines, selectedCosineColumns).flatMap { _ => (withCosines.t.find(_.contrast == contrast).flatMap(_.varianceOverSigmaSquared), withoutCosines.t.find(_.contrast == contrast).flatMap(_.varianceOverSigmaSquared)) match
      case (Some(withValue), Some(withoutValue)) if withoutValue > 0.0 => val ratio = withValue / withoutValue; Right(HighPassVarianceCost(selectedCosineColumns, withValue, withoutValue, ratio, ratio - 1.0))
      case _ => Left(DesignDiagnosticsError.NumericalFailure("high-pass contrast must be estimable with positive variance both with and without the selected cosine columns"))
    }

  def highPassFVarianceRatio(withCosines: ContrastDiagnosticsResult, withoutCosines: ContrastDiagnosticsResult, contrast: FColumnContrast, selectedCosineColumns: Vector[ColumnId]): Either[DesignDiagnosticsError, HighPassVarianceCost] =
    highPassPair(withCosines, withoutCosines, selectedCosineColumns).flatMap { _ => (withCosines.f.find(_.contrast == contrast).flatMap(_.worstDirectionSeOverSigma), withoutCosines.f.find(_.contrast == contrast).flatMap(_.worstDirectionSeOverSigma)) match
      case (Some(withSe), Some(withoutSe)) if withoutSe > 0.0 => val ratio = withSe * withSe / (withoutSe * withoutSe); Right(HighPassVarianceCost(selectedCosineColumns, withSe * withSe, withoutSe * withoutSe, ratio, ratio - 1.0))
      case _ => Left(DesignDiagnosticsError.NumericalFailure("high-pass F contrast must be estimable with positive variance both with and without the selected cosine columns"))
    }

  def analyze(schema: DesignSchema, tContrasts: Vector[ColumnContrast], fContrasts: Vector[FColumnContrast], selectedRows: Vector[ScanIndex] = Vector.empty, rankPolicy: RankPolicy = RankPolicy.Default): Either[DesignDiagnosticsError, ContrastDiagnosticsResult] =
    val rows = if selectedRows.isEmpty then schema.rows.selectedRows else selectedRows
    if rows.isEmpty then Left(DesignDiagnosticsError.EmptyRows)
    else if rows.distinct.length != rows.length then Left(DesignDiagnosticsError.DuplicateRows)
    else rows.find(r => r.oneBased < 1 || r.oneBased > schema.matrixValues.rows).map(r => Left(DesignDiagnosticsError.RowOutOfBounds(r, schema.matrixValues.rows))).getOrElse {
        val m = Mat.unsafe(rows.length, schema.matrixValues.cols, Array.tabulate(rows.length * schema.matrixValues.cols)(i => schema.matrixValues(rows(i / schema.matrixValues.cols).oneBased - 1, i % schema.matrixValues.cols)))
        val bad = m.data.indexWhere(v => !v.isFinite)
        if bad >= 0 then Left(DesignDiagnosticsError.NonFiniteValue(rows(bad / m.cols), schema.columnIds(bad % m.cols), m.data(bad)))
        else spectrum(DMat.tabulate(m.rows, m.cols)(m.apply)).flatMap { case (singular, right) =>
          val cutoff = rankPolicy.cutoff(m.rows, m.cols, singular.headOption.getOrElse(0.0))
          val kept = singular.indices.filter(i => singular(i) > cutoff).toVector
          val geometry = ContrastRunGeometry(schema.columnIds, m, right, singular, cutoff)
          for
            t <- sequence(tContrasts.map(tDiagnostic(_, schema.columnIds, m, singular, right, kept)))
            f <- sequence(fContrasts.map(fDiagnostic(_, schema.columnIds, singular, right, kept)))
          yield ContrastDiagnosticsResult(schema.fingerprint, rows, rankPolicy, kept.length, cutoff, aliases(schema.columnIds, m, geometry), t, f, geometry = geometry)
        }
    }

  private def tDiagnostic(c: ColumnContrast, ids: Vector[ColumnId], matrix: Mat, singular: Vector[Double], right: Vector[Array[Double]], kept: Vector[Int]): Either[DesignDiagnosticsError, TContrastDiagnostic] = vector(c, ids).map { w =>
    val dropped = c.weights.collect { case (id, weight) if weight != 0.0 && norm(Array.tabulate(matrix.rows)(row => matrix(row, ids.indexOf(id)))) == 0.0 => id }
    val r = residual(w, right, kept)
    if r > scale(w) then TContrastDiagnostic(c, ContrastEstimability.Lost(LostEstimability.Aliased(r)), None)
    else if dropped.nonEmpty then TContrastDiagnostic(c, ContrastEstimability.Lost(LostEstimability.DroppedColumns(dropped)), None)
    else TContrastDiagnostic(c, ContrastEstimability.Estimable, Some(quadratic(w, w, singular, right, kept)))
  }

  private def fDiagnostic(c: FColumnContrast, ids: Vector[ColumnId], singular: Vector[Double], right: Vector[Array[Double]], kept: Vector[Int]): Either[DesignDiagnosticsError, FContrastDiagnostic] = sequence(c.rows.map(vector(_, ids))).flatMap { raw =>
    val q = orthonormal(raw)
    if q.isEmpty then Right(FContrastDiagnostic(c, ContrastEstimability.Lost(LostEstimability.EmptyHypothesis), 0, None))
    else { val r = q.map(residual(_, right, kept)).max
      if r > q.map(scale(_)).max then Right(FContrastDiagnostic(c, ContrastEstimability.Lost(LostEstimability.Aliased(r)), 0, None))
      else spectrum(DMat.tabulate(q.length, q.length)((i, j) => quadratic(q(i), q(j), singular, right, kept))).map { case (s, _) => FContrastDiagnostic(c, ContrastEstimability.Estimable, q.length, Some(math.sqrt(math.max(0.0, s.headOption.getOrElse(0.0))))) }
    }
  }

  /** Economy SVD omits wide-null directions. The row-space projector recovers
    * them, then RREF produces stable sparse equations instead of raw rotations. */
  private def aliases(ids: Vector[ColumnId], x: Mat, g: ContrastRunGeometry): Vector[AliasEquation] =
    val p = ids.length
    val projector = (0 until p).map(r => Array.tabulate(p)(c => (if r == c then 1.0 else 0.0) - g.right.zipWithIndex.filter((_, i) => g.singular(i) > g.cutoff).map((v, _) => v(r) * v(c)).sum)).toVector
    rref(projector, 1e-10).flatMap { row =>
      val cs = row.indices.filter(i => math.abs(row(i)) > 1e-10).map(i => ids(i) -> row(i)).toVector
      if cs.isEmpty then None else Some(AliasEquation(cs, norm(Array.tabulate(x.rows)(r => row.indices.map(c => x(r, c) * row(c)).sum)) / math.max(1.0, frobenius(x))))
    }.sortBy(_.coefficients.length)

  private def residualizedTaskDesign(x: Mat, task: Vector[Int], policy: RankPolicy): Mat =
    val nuisance = (0 until x.cols).filterNot(task.contains).toVector; val n = x.rows
    val residualizer = Array.tabulate(n * n)(i => if i / n == i % n then 1.0 else 0.0)
    if nuisance.nonEmpty then spectrum(DMat.tabulate(n, nuisance.length)((r, c) => x(r, nuisance(c)))).foreach { case (s, right) =>
      // U = X V / s avoids needing an economy-SVD U accessor and remains portable.
      val max = s.headOption.getOrElse(0.0)
      val cutoff = policy.cutoff(n, nuisance.length, max)
      val keep = s.indices.filter(i => s(i) > cutoff)
      keep.foreach { k => val u = Array.tabulate(n)(r => nuisance.indices.map(c => x(r, nuisance(c)) * right(k)(c)).sum / s(k)); for r <- 0 until n; c <- 0 until n do residualizer(r * n + c) -= u(r) * u(c) }
    }
    Mat.unsafe(n, task.length, Array.tabulate(n * task.length) { z =>
      val r = z / task.length; val taskColumn = task(z % task.length)
      (0 until n).map(c => residualizer(r * n + c) * x(c, taskColumn)).sum
    })

  private def stack(matrices: Vector[Mat], cols: Int): Mat =
    val rows = matrices.map(_.rows).sum
    Mat.unsafe(rows, cols, matrices.flatMap(_.data).toArray)

  private def fCovariance(run: ContrastDiagnosticsResult, contrast: FColumnContrast): Either[DesignDiagnosticsError, Option[DMat]] =
    sequence(contrast.rows.map(vector(_, run.geometry.ids))).map { rows =>
      val q = orthonormal(rows)
      val kept = run.geometry.singular.indices.filter(i => run.geometry.singular(i) > run.geometry.cutoff).toVector
      if q.isEmpty || q.map(residual(_, run.geometry.right, kept)).max > q.map(scale(_)).max then None
      else Some(DMat.tabulate(q.length, q.length)((i, j) => quadratic(q(i), q(j), run.geometry.singular, run.geometry.right, kept)))
    }

  private def combineCovariances(values: Vector[DMat]): Either[DesignDiagnosticsError, DMat] =
    val n = values.head.rows
    values.foldLeft[Either[DesignDiagnosticsError, Array[Double]]](Right(Array.fill(n * n)(0.0))) { (acc, value) =>
      for
        information <- acc
        inverseValue <- inverse(value)
      yield
        var i = 0
        while i < information.length do { information(i) += inverseValue(i / n, i % n); i += 1 }
        information
    }.flatMap(information => inverse(DMat.tabulate(n, n)((r, c) => information(r * n + c))))

  private def inverse(x: DMat): Either[DesignDiagnosticsError, DMat] = spectrum(x).flatMap { case (s, right) =>
    if s.isEmpty || s.exists(_ == 0.0) then Left(DesignDiagnosticsError.NumericalFailure("F hypothesis covariance is singular"))
    else Right(DMat.tabulate(x.rows, x.cols)((r, c) => s.indices.map(k => right(k)(r) * right(k)(c) / s(k)).sum))
  }

  private def worstSe(covariance: DMat): Either[DesignDiagnosticsError, Double] =
    spectrum(covariance).flatMap { case (singular, _) =>
      singular.headOption match
        case Some(value) if value.isFinite && value >= 0.0 => Right(math.sqrt(value))
        case _ => Left(DesignDiagnosticsError.NumericalFailure("F hypothesis covariance has no finite non-negative spectrum"))
    }

  private def highPassPair(withCosines: ContrastDiagnosticsResult, withoutCosines: ContrastDiagnosticsResult, selected: Vector[ColumnId]): Either[DesignDiagnosticsError, Unit] =
    if selected.isEmpty || selected.distinct.length != selected.length then Left(DesignDiagnosticsError.NumericalFailure("high-pass comparison requires unique explicitly selected cosine columns"))
    else if withCosines.selectedRows != withoutCosines.selectedRows || withCosines.rankPolicy.label != withoutCosines.rankPolicy.label then Left(DesignDiagnosticsError.NumericalFailure("high-pass comparison requires the same rows and rank policy"))
    else if withCosines.geometry.ids.filterNot(selected.contains) != withoutCosines.geometry.ids then Left(DesignDiagnosticsError.NumericalFailure("without-cosines columns must equal with-cosines columns minus the selected cosine columns"))
    else if selected.exists(id => !withCosines.geometry.ids.contains(id) || withoutCosines.geometry.ids.contains(id)) then Left(DesignDiagnosticsError.NumericalFailure("selected cosine columns must be present only in the with-cosines design"))
    else
      val equal = withoutCosines.geometry.ids.indices.forall { j =>
        val i = withCosines.geometry.ids.indexOf(withoutCosines.geometry.ids(j))
        (0 until withoutCosines.geometry.matrix.rows).forall(r => withCosines.geometry.matrix(r, i) == withoutCosines.geometry.matrix(r, j))
      }
      if equal then Right(()) else Left(DesignDiagnosticsError.NumericalFailure("common high-pass design columns differ"))

  private def spectrum(x: DMat): Either[DesignDiagnosticsError, (Vector[Double], Vector[Array[Double]])] = x.svd.left.map(e => DesignDiagnosticsError.NumericalFailure(e.toString)).map(s => Vector.tabulate(s.size)(s.singularValues(_)) -> (0 until s.size).map(k => Array.tabulate(s.vt.cols)(i => s.vt(k, i))).toVector)
  private def indices(ids: Vector[ColumnId], task: Vector[ColumnId]): Option[Vector[Int]] = { val out = task.map(ids.indexOf); if out.exists(_ < 0) then None else Some(out) }
  private def vector(c: ColumnContrast, ids: Vector[ColumnId]): Either[DesignDiagnosticsError, Array[Double]] = c.weights.foldLeft[Either[DesignDiagnosticsError, Array[Double]]](Right(Array.fill(ids.length)(0.0))) { case (a, (id, w)) => a.flatMap { out => val i = ids.indexOf(id); if i < 0 then Left(DesignDiagnosticsError.UnknownColumn(id)) else if !w.isFinite then Left(DesignDiagnosticsError.NumericalFailure(s"non-finite contrast weight for ${id.value}")) else { out(i) = w; Right(out) } } }
  private def residual(w: Array[Double], right: Vector[Array[Double]], kept: Vector[Int]): Double = { val out = w.clone(); kept.foreach { k => val d = out.indices.map(i => out(i) * right(k)(i)).sum; out.indices.foreach(i => out(i) -= d * right(k)(i)) }; norm(out) }
  private def quadratic(a: Array[Double], b: Array[Double], s: Vector[Double], right: Vector[Array[Double]], kept: Vector[Int]): Double = kept.map { k => val av = a.indices.map(i => a(i) * right(k)(i)).sum; val bv = b.indices.map(i => b(i) * right(k)(i)).sum; av * bv / (s(k) * s(k)) }.sum
  private def orthonormal(rows: Vector[Array[Double]]): Vector[Array[Double]] = rows.foldLeft(Vector.empty[Array[Double]]) { (basis, row) =>
    val inputNorm = norm(row)
    if inputNorm == 0.0 then basis
    else
      val out = row.map(_ / inputNorm)
      basis.foreach { q => val d = out.indices.map(i => out(i) * q(i)).sum; out.indices.foreach(i => out(i) -= d * q(i)) }
      val n = norm(out)
      if n > 1e-12 then basis :+ out.map(_ / n) else basis
  }
  private def rref(in: Vector[Array[Double]], tolerance: Double): Vector[Array[Double]] = { val a = in.map(_.clone()).toArray; var pr = 0; var c = 0; while pr < a.length && c < (if a.isEmpty then 0 else a(0).length) do { val k = (pr until a.length).maxBy(i => math.abs(a(i)(c))); if math.abs(a(k)(c)) <= tolerance then c += 1 else { val tmp = a(pr); a(pr) = a(k); a(k) = tmp; val p = a(pr)(c); a(pr).indices.foreach(i => a(pr)(i) /= p); a.indices.filter(_ != pr).foreach { r => val q = a(r)(c); if math.abs(q) > tolerance then a(r).indices.foreach(i => a(r)(i) -= q * a(pr)(i)) }; pr += 1; c += 1 } }; a.take(pr).map { row => row.indices.foreach(i => if math.abs(row(i)) <= tolerance then row(i) = 0.0); row }.toVector }
  private def matrixRank(a: DMat, cutoff: Double): Int = spectrum(a).map { case (s, _) => val threshold = cutoff.max(math.max(a.rows, a.cols) * DesignDiagnostics.MachineEpsilon * s.headOption.getOrElse(0.0)); s.count(_ > threshold) }.getOrElse(0)
  /** Contrast-space membership is dimensionless. The SVD cutoff selects the
    * row space; it must not be multiplied into a coefficient-space residual. */
  private def scale(w: Array[Double]): Double = 1e-10 * norm(w)
  /** Scaled sum of squares keeps contrast-row normalization well-defined for
    * very large and very small, but finite, row scalings. */
  private def norm(x: Array[Double]): Double =
    var scale = 0.0
    var sum = 1.0
    var i = 0
    while i < x.length do
      val value = math.abs(x(i))
      if value != 0.0 then
        if scale < value then
          val ratio = if scale == 0.0 then 0.0 else scale / value
          sum = 1.0 + sum * ratio * ratio
          scale = value
        else
          val ratio = value / scale
          sum += ratio * ratio
      i += 1
    if scale == 0.0 then 0.0 else scale * math.sqrt(sum)
  private def hasNumericalSignal(residual: Mat, original: Mat, task: Vector[Int], policy: RankPolicy): Boolean =
    val originalNorm = norm(Array.tabulate(original.rows * task.length)(i => original(i / task.length, task(i % task.length))))
    norm(residual.data) > policy.cutoff(original.rows, task.length, originalNorm)
  private def frobenius(x: Mat): Double = norm(x.data)
  private def sequence[A](xs: Vector[Either[DesignDiagnosticsError, A]]): Either[DesignDiagnosticsError, Vector[A]] = xs.foldLeft[Either[DesignDiagnosticsError, Vector[A]]](Right(Vector.empty))((a, x) => a.flatMap(v => x.map(v :+ _)))
