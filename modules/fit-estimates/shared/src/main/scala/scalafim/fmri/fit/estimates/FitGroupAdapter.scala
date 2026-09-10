package scalafim.fmri.fit.estimates

import scalafim.fmri.group.*

import gale.linalg.Matrix
import scalafim.dataset.SubjectId
import scalafim.fmri.fit.TContrastResult

/** Bridge from first-level results to a group cube. Each subject contributes,
  * per first-level contrast, an effect map (the contrast estimate) and a variance
  * map (its squared standard error) — exactly the inputs the meta-analytic
  * estimators need. This eager convenience lives outside the group core; durable
  * inputs use its bounded EstimateGroup reader.
  */
object FitGroupAdapter:

  /** Assemble a `GroupData` from per-subject, per-contrast t-contrast results.
    *
    * Every subject must provide each contrast, and every result must span the
    * sample space. Variances are `se²`, so the group cube is meta-analysis-ready.
    */
  def groupData(
      space: GroupSpace,
      subjects: Vector[SubjectId],
      contrasts: Vector[String],
      results: Map[(SubjectId, String), TContrastResult]
  ): Either[GroupError, GroupData[VarianceCapability.WithVariances]] =
    parseContrasts(contrasts)
      .flatMap(_.foldLeft[Either[GroupError, Vector[(String, GroupResponse[VarianceCapability.WithVariances])]]](Right(Vector.empty)) {
        case (Left(err), _) => Left(err)
        case (Right(acc), contrast) =>
          buildResponse(space, subjects, contrast, results).map(response => acc :+ (contrast.value -> response))
      })
      .flatMap(responses => GroupData.build(subjects, space, responses))

  private def buildResponse(
      space: GroupSpace,
      subjects: Vector[SubjectId],
      contrast: FirstLevelContrastName,
      results: Map[(SubjectId, String), TContrastResult]
  ): Either[GroupError, GroupResponse.WithVariances] =
    collectCells(space.nSamples, subjects, contrast, results).flatMap { cells =>
      val nSubjects = subjects.length
      val nSamples = space.nSamples
      val effects = Matrix.newBuilder(nSubjects, nSamples)
      val variances = Matrix.newBuilder(nSubjects, nSamples)
      var subjectIdx = 0
      while subjectIdx < nSubjects do
        val result = cells(subjectIdx)
        var sample = 0
        while sample < nSamples do
          val se = result.standardErrors(sample)
          effects(subjectIdx, sample) = result.estimates(sample)
          variances(subjectIdx, sample) = se * se
          sample += 1
        subjectIdx += 1
      GroupResponse.weighted(
        effects.result(),
        variances.result()
      )
    }

  /** Gather the per-subject results for one contrast, in subject order,
    * short-circuiting on the first missing or mis-sized cell.
    */
  private def collectCells(
      nSamples: Int,
      subjects: Vector[SubjectId],
      contrast: FirstLevelContrastName,
      results: Map[(SubjectId, String), TContrastResult]
  ): Either[GroupError, Vector[TContrastResult]] =
    subjects.foldLeft[Either[GroupError, Vector[TContrastResult]]](Right(Vector.empty)) {
      case (Left(err), _) => Left(err)
      case (Right(acc), subject) =>
        results.get((subject, contrast.value)) match
          case None =>
            Left(GroupError.MissingSubjectContrast(subject.value, contrast.value))
          case Some(result) if result.estimates.length != nSamples =>
            Left(GroupError.sampleMismatch(nSamples, result.estimates.length))
          case Some(result) =>
            Right(acc :+ result)
    }

  private def parseContrasts(contrasts: Vector[String]): Either[GroupError, Vector[FirstLevelContrastName]] =
    contrasts.foldLeft[Either[GroupError, Vector[FirstLevelContrastName]]](Right(Vector.empty)) {
      case (Left(err), _) => Left(err)
      case (Right(acc), contrast) => FirstLevelContrastName(contrast).map(acc :+ _)
    }
