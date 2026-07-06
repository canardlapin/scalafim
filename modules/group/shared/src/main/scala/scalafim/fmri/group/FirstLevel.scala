package scalafim.fmri.group

import scalafim.dataset.SubjectId
import scalafim.fmri.fit.TContrastResult
import scalafim.linalg.DoubleMatrix

/** Bridge from first-level results to a group cube. Each subject contributes,
  * per first-level contrast, an effect map (the contrast estimate) and a variance
  * map (its squared standard error) — exactly the inputs the meta-analytic
  * estimators need. This is why the module depends on `scalafim-fmri-fit`.
  */
object FirstLevel:

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
  ): Either[GroupError, GroupData] =
    contrasts
      .foldLeft[Either[GroupError, Vector[(String, GroupResponse)]]](Right(Vector.empty)) {
        case (Left(err), _) => Left(err)
        case (Right(acc), contrast) =>
          buildResponse(space, subjects, contrast, results).map(response => acc :+ (contrast -> response))
      }
      .flatMap(responses => GroupData(subjects, space, responses))

  private def buildResponse(
      space: GroupSpace,
      subjects: Vector[SubjectId],
      contrast: String,
      results: Map[(SubjectId, String), TContrastResult]
  ): Either[GroupError, GroupResponse] =
    collectCells(space.nSamples, subjects, contrast, results).flatMap { cells =>
      val nSubjects = subjects.length
      val nSamples = space.nSamples
      val effects = new Array[Double](nSubjects * nSamples)
      val variances = new Array[Double](nSubjects * nSamples)
      var subjectIdx = 0
      while subjectIdx < nSubjects do
        val result = cells(subjectIdx)
        var sample = 0
        while sample < nSamples do
          val se = result.standardErrors(sample)
          effects(subjectIdx * nSamples + sample) = result.estimates(sample)
          variances(subjectIdx * nSamples + sample) = se * se
          sample += 1
        subjectIdx += 1
      GroupResponse.weighted(
        DoubleMatrix.unsafe(nSubjects, nSamples, effects),
        DoubleMatrix.unsafe(nSubjects, nSamples, variances)
      ).map(r => r: GroupResponse)
    }

  /** Gather the per-subject results for one contrast, in subject order,
    * short-circuiting on the first missing or mis-sized cell.
    */
  private def collectCells(
      nSamples: Int,
      subjects: Vector[SubjectId],
      contrast: String,
      results: Map[(SubjectId, String), TContrastResult]
  ): Either[GroupError, Vector[TContrastResult]] =
    subjects.foldLeft[Either[GroupError, Vector[TContrastResult]]](Right(Vector.empty)) {
      case (Left(err), _) => Left(err)
      case (Right(acc), subject) =>
        results.get((subject, contrast)) match
          case None =>
            Left(GroupError.MissingSubjectContrast(subject.value, contrast))
          case Some(result) if result.estimates.length != nSamples =>
            Left(GroupError.SampleMismatch(nSamples, result.estimates.length))
          case Some(result) =>
            Right(acc :+ result)
    }
