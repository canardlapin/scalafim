package scalafim.fmri.mvpa.group

import gale.linalg.DMat
import multivar.core.SemanticSpace
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.analysis.{EvidenceExposure, ExposureControl, ExposureScope}
import scalafim.fmri.mvpa.pattern.*

enum SubjectPredictionError:
  case Binding(detail: String)
  case Leakage(detail: String)
  case Invalid(detail: String)
  case Component(cause: ComponentConfirmationError)
  case Budget(cells: BigInt, allowed: Long)

  def message: String = this match
    case Binding(detail) => s"subject prediction binding: $detail"
    case Leakage(detail) => s"subject prediction evidence separation: $detail"
    case Invalid(detail) => s"invalid subject prediction: $detail"
    case Component(cause) => cause.toString
    case Budget(cells, allowed) => s"subject prediction summary requires $cells cells, allowed $allowed"

enum SubjectHeadTraining:
  /** Heads are trained only on subjects in the shared learning cohort. */
  case SharedTraining
  /** Only OLS heads adapt, on separate rows/units of this new subject.
    * Shared projections, metric, spatial support and preprocessing stay fixed. */
  case SubjectAdaptation(subject: SubjectCoordinateKey, protocol: String)

final case class SubjectPredictionContract(population: String, sampling: String, generalization: String):
  require(Vector(population, sampling, generalization).forall(_.trim.nonEmpty))

/** A subject boundary around the existing M4.06 frozen head procedure. No new
  * trainer, tuning loop, CV engine, loss definition or inferential test lives
  * here. Columns declare physical subject identity; renamed/omitted foreign
  * exposure cannot be authenticated by these immutable accounts. */
final class HeldOutSubjectPrediction private (
    val shared: SharedTaskCoordinates, val heads: ComponentPredictionHeads,
    val learningSubjects: Column[?, SubjectCoordinateKey], val headSubjects: Column[?, SubjectCoordinateKey],
    val assessmentSubjects: Column[?, SubjectCoordinateKey], val training: SubjectHeadTraining,
    val contract: SubjectPredictionContract, val headExposure: EvidenceExposure,
    val assessmentExposure: EvidenceExposure, val identity: String
):
  val subjectKeys: Vector[SubjectCoordinateKey] = assessmentSubjects.values.distinct

  /** Current exposure must still match the frozen untouched account. Checks
    * and reducer admission precede the existing procedure's operator reads. */
  def evaluate[S <: SemanticSpace, N <: SemanticSpace, Q <: SemanticSpace](
      observations: Observations[S, N], targets: MultiResponse[S, Q], currentExposure: EvidenceExposure,
      budget: ComponentConfirmationBudget = ComponentConfirmationBudget(), maximumSummaryCells: Long = 1_000_000L
  ): Either[SubjectPredictionError, HeldOutSubjectResult] =
    val r = heads.plan.incrementalMembers.size
    // Two subject-by-component matrices, one loss column and integer count/
    // ownership arrays. Borrowed native results and M4.06 workspace are separate.
    val cells = BigInt(subjectKeys.size) * (2 * BigInt(r) + 3) + 2 * BigInt(heads.plan.design.confirmation.samples.units.size)
    for
      _ <- if currentExposure.identity == assessmentExposure.identity &&
          ExposureControl.untouchedConfirmation(currentExposure, ExposureScope.Holdout).isRight then Right(())
        else Left(SubjectPredictionError.Leakage("assessment exposure changed after the subject prediction plan was frozen"))
      _ <- if cells <= maximumSummaryCells && cells <= Int.MaxValue then Right(())
        else Left(SubjectPredictionError.Budget(cells, maximumSummaryCells))
      result <- ComponentConfirmation.incremental(heads, observations, targets, budget).left.map(SubjectPredictionError.Component.apply)
    yield
      val mapping = result.rowUnitOrdinals
      val unitCount = result.independentUnits.size
      val subjectIndex = subjectKeys.zipWithIndex.toMap
      val unitRows = Array.fill(unitCount)(0)
      val unitSubjects = Array.fill(unitCount)(0)
      val subjectRows = Array.fill(subjectKeys.size)(0)
      mapping.indices.foreach: row =>
        val s = subjectIndex(assessmentSubjects.values(row))
        unitRows(mapping(row)) += 1
        unitSubjects(mapping(row)) = s
        subjectRows(s) += 1
      // M4.06 averages rows within each unit. Reweight those unit means by
      // row count to recover equal rows within a subject, then equal subjects.
      def reduce(at: (Int, Int) => Double, columns: Int): DMat =
        val out = DMat.newBuilder(subjectKeys.size, columns)
        var u = 0
        while u < unitCount do
          val s = unitSubjects(u)
          val weight = unitRows(u).toDouble / subjectRows(s)
          var k = 0
          while k < columns do
            out(s, k) = out(s, k) + weight * at(u, k)
            k += 1
          u += 1
        out.result()
      val full = reduce((u, _) => result.fullUnitLoss(u), 1)
      val reduced = reduce(result.reducedUnitLoss.apply, r)
      val improvement = reduce(result.unitImprovements.apply, r)
      new HeldOutSubjectResult(this, result, subjectRows.toVector, full, reduced, improvement)

object HeldOutSubjectPrediction:
  def freeze(shared: SharedTaskCoordinates, heads: ComponentPredictionHeads,
      learningSubjects: Column[?, SubjectCoordinateKey], headSubjects: Column[?, SubjectCoordinateKey],
      assessmentSubjects: Column[?, SubjectCoordinateKey], training: SubjectHeadTraining,
      contract: SubjectPredictionContract, headExposure: EvidenceExposure, assessmentExposure: EvidenceExposure
  ): Either[SubjectPredictionError, HeldOutSubjectPrediction] =
    val discovery = shared.discovery; val local = heads.plan.design.discovery
    val assessment = heads.plan.design.confirmation
    val held = assessmentSubjects.values.toSet
    val learned = learningSubjects.values.toSet
    val trained = headSubjects.values.toSet
    for
      _ <- if learningSubjects.rowAxis == discovery.samples.rows.descriptor && headSubjects.rowAxis == local.samples.rows.descriptor &&
          assessmentSubjects.rowAxis == assessment.samples.rows.descriptor && held.nonEmpty && learned.nonEmpty && trained.nonEmpty then Right(())
        else Left(SubjectPredictionError.Binding("row-bound learning, head-training and assessment subject columns"))
      _ <- if held.intersect(learned).isEmpty && discovery.samples.rows.descriptor != assessment.samples.rows.descriptor &&
          discovery.samples.unitKeys.toSet.intersect(assessment.samples.unitKeys.toSet).isEmpty then Right(())
        else Left(SubjectPredictionError.Leakage("assessment subjects/units contributed to shared-coordinate learning"))
      _ <- training match
        case SubjectHeadTraining.SharedTraining if trained.subsetOf(learned) && trained.intersect(held).isEmpty => Right(())
        case SubjectHeadTraining.SubjectAdaptation(subject, protocol) if protocol.trim.nonEmpty &&
            trained == Set(subject) && held == Set(subject) && !learned.contains(subject) &&
            local.samples.rows.descriptor != discovery.samples.rows.descriptor &&
            local.samples.unitKeys.toSet.intersect(discovery.samples.unitKeys.toSet).isEmpty => Right(())
        case _ => Left(SubjectPredictionError.Leakage("head-training subjects do not satisfy the declared shared-training or single-subject adaptation policy"))
      _ <- if local.brainProjection.identity == discovery.brainProjection.identity && local.targetProjection.identity == discovery.targetProjection.identity &&
          local.spatialSupportIdentity == discovery.spatialSupportIdentity && local.rotationIdentity == discovery.rotationIdentity &&
          local.preprocessingReceipt == discovery.preprocessingReceipt && shared.featureAxis == discovery.brainProjection.input.descriptor then Right(())
        else Left(SubjectPredictionError.Binding("prediction requires the exact shared projections, support, rotation, preprocessing and common feature axis"))
      metricMatches = discovery.artifact.target match
        case TargetGeometry.Continuous(target) => target.metricDiagonal.values == heads.plan.targetMetric
        case _ => false
      _ <- if metricMatches then Right(()) else Left(SubjectPredictionError.Binding("shared discovery target loss metric"))
      _ <- if headExposure.reference.evidenceIdentity == local.identity && headExposure.reference.result.text == heads.identity &&
          headExposure.initialScope == ExposureScope.Training && !headExposure.events.exists(_.request.scope.canOverlap(ExposureScope.Holdout)) then Right(())
        else Left(SubjectPredictionError.Leakage("head account must bind actual fitted heads and contain only training exposure"))
      _ <- if assessmentExposure.reference.evidenceIdentity == assessment.identity &&
          !assessmentExposure.events.exists(_.request.scope.canOverlap(ExposureScope.Holdout)) &&
          ExposureControl.untouchedConfirmation(assessmentExposure, ExposureScope.Holdout).isRight then Right(())
        else Left(SubjectPredictionError.Leakage("assessment account is foreign, unknown or already exposed"))
      _ <- unitSubjects(discovery.samples, learningSubjects)
      _ <- unitSubjects(local.samples, headSubjects)
      _ <- unitSubjects(assessment.samples, assessmentSubjects)
      _ <- if discovery.samples.rows.descriptor != local.samples.rows.descriptor || learningSubjects.values == headSubjects.values then Right(())
        else Left(SubjectPredictionError.Binding("the same shared-learning/head-training rows have different subject labels"))
    yield
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.held-out-subject-prediction.v1"); writer.string(shared.identity); writer.string(heads.identity)
        Vector(learningSubjects, headSubjects, assessmentSubjects).foreach: column =>
          writer.string(column.identity.toString)
          writer.intLE(column.values.size); column.values.foreach(subject => writer.string(subject.value))
        writer.string(training.toString); writer.string(headExposure.identity.text); writer.string(assessmentExposure.identity.text)
        Vector(contract.population, contract.sampling, contract.generalization).foreach(writer.string)
      new HeldOutSubjectPrediction(shared, heads, learningSubjects, headSubjects, assessmentSubjects, training, contract,
        headExposure, assessmentExposure, identity)

  private def unitSubjects(samples: ConfirmationUnits[?, ?], column: Column[?, SubjectCoordinateKey]): Either[SubjectPredictionError, Unit] =
    val groups = samples.rowUnitOrdinals.zip(column.values).groupMap(_._1)(_._2)
    if groups.size != samples.units.size || groups.values.exists(_.distinct.size != 1) then
      Left(SubjectPredictionError.Binding("every independent unit must be represented and belong to exactly one subject"))
    else Right(())

/** Per-subject full/reduced losses and signed improvements from actual held-out
  * predictions. These descriptive products do not acquire a p-value or SE. */
final class HeldOutSubjectResult private[group] (
    val plan: HeldOutSubjectPrediction, val native: ComponentIncrementalResult,
    val rowCounts: Vector[Int], val fullLoss: DMat, val reducedLoss: DMat, val improvements: DMat
):
  def subjects: Vector[SubjectCoordinateKey] = plan.subjectKeys

final class SubjectPredictiveSummary private (
    val results: Vector[HeldOutSubjectResult], val subjects: Vector[SubjectCoordinateKey],
    val meanFullLoss: Double, val meanReducedLoss: Vector[Double], val meanImprovements: Vector[Double],
    val empiricalImprovementVariance: Vector[Double], val identity: String
):
  val scope: String = "descriptive new-subject performance conditional on frozen shared learning and the declared head adaptation protocol; equal subjects, equal rows within subject"
  def prevalence: Either[SubjectGroupError, Nothing] = SubjectGroupSummary.prevalence
  def populationInference: Either[SubjectGroupError, Nothing] =
    Left(SubjectGroupError.Unavailable("predictive loss summaries supply no calibrated population test, interval or fold-as-subject standard error"))

object SubjectPredictiveSummary:
  /** Complete fixed external assessment cohort. This is not a cross-validation
    * reducer: all results must share the same frozen learning source. */
  def combine(expectedSubjects: Vector[SubjectCoordinateKey], results: Vector[HeldOutSubjectResult],
      maximumSummaryCells: Long = 1_000_000L): Either[SubjectPredictionError, SubjectPredictiveSummary] =
    val subjects = results.flatMap(_.subjects)
    if results.isEmpty || expectedSubjects.size < 2 || expectedSubjects.distinct.size != expectedSubjects.size || subjects != expectedSubjects then
      Left(SubjectPredictionError.Binding("complete ordered cohort of at least two unique subjects; no omissions, repeats or folds as subjects"))
    else
      val first = results.head.plan
      val cells = 4 * BigInt(subjects.size) + 4 * BigInt(first.heads.plan.incrementalMembers.size)
      def samePolicy(value: SubjectHeadTraining): Boolean = (first.training, value) match
        case (SubjectHeadTraining.SharedTraining, SubjectHeadTraining.SharedTraining) => true
        case (SubjectHeadTraining.SubjectAdaptation(_, a), SubjectHeadTraining.SubjectAdaptation(_, b)) => a == b
        case _ => false
      val compatible = results.forall: result =>
        val plan = result.plan
        plan.shared.identity == first.shared.identity && plan.learningSubjects.identity == first.learningSubjects.identity &&
          plan.learningSubjects.values == first.learningSubjects.values &&
          plan.contract == first.contract && samePolicy(plan.training) && plan.heads.plan.incrementalMembers == first.heads.plan.incrementalMembers &&
          plan.heads.plan.nuisanceColumns == first.heads.plan.nuisanceColumns &&
          (plan.training != SubjectHeadTraining.SharedTraining || plan.heads.identity == first.heads.identity)
      if results.map(_.native.brainEvidenceIdentity).distinct.size != results.size then
        Left(SubjectPredictionError.Binding("the same actual assessment brain evidence cannot be relabelled as independent subjects"))
      else if !compatible then Left(SubjectPredictionError.Binding("shared learning, loss family, nuisance semantics, contract or adaptation protocol differ"))
      else if cells > maximumSummaryCells || cells > Int.MaxValue then Left(SubjectPredictionError.Budget(cells, maximumSummaryCells))
      else
        val entries = results.flatMap(result => result.subjects.indices.map(i => (result, i)))
        val n = entries.size; val r = first.heads.plan.incrementalMembers.size
        val full = entries.map((result, i) => result.fullLoss(i, 0) / n).sum
        val reduced = Vector.tabulate(r)(k => entries.map((result, i) => result.reducedLoss(i, k) / n).sum)
        val means = Vector.tabulate(r)(k => entries.map((result, i) => result.improvements(i, k) / n).sum)
        val variances = Vector.tabulate(r): k =>
          entries.map: (result, i) =>
            val delta = result.improvements(i, k) - means(k)
            delta * delta / (n - 1)
          .sum
        if (Vector(full) ++ reduced ++ means ++ variances).exists(!_.isFinite) then
          Left(SubjectPredictionError.Invalid("nonfinite subject loss summary; no subject was dropped"))
        else
          val identity = AxisDigest.sha256Hex: writer =>
            writer.string("scalafim.subject-predictive-summary.v1")
            subjects.foreach(subject => writer.string(subject.value))
            results.foreach: result =>
              writer.string(result.plan.identity)
              result.native.brainEvidenceIdentity.writeFramed(writer); result.native.targetEvidenceIdentity.writeFramed(writer)
              Vector(result.fullLoss, result.reducedLoss, result.improvements).foreach(SubjectGroupBridge.writeMatrix(writer, _))
          Right(new SubjectPredictiveSummary(results, subjects, full, reduced, means, variances, identity))
