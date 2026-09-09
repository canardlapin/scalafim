package scalafim.fmri.group

import gale.linalg.Matrix
import scalafim.dataset.SubjectId
import scalafim.fmri.fit.TContrastResult

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
    collectCells(space, subjects, contrast, results).flatMap { cells =>
      val nSubjects = subjects.length
      val nSamples = space.nSamples
      val effects = Matrix.newBuilder(nSubjects, nSamples)
      val variances = Matrix.newBuilder(nSubjects, nSamples)
      var subjectIdx = 0
      while subjectIdx < nSubjects do
        val (result, order) = cells(subjectIdx)
        var sample = 0
        while sample < nSamples do
          val se = result.standardErrors(order(sample))
          effects(subjectIdx, sample) = result.estimates(order(sample))
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
      space: GroupSpace,
      subjects: Vector[SubjectId],
      contrast: FirstLevelContrastName,
      results: Map[(SubjectId, String), TContrastResult]
  ): Either[GroupError, Vector[(TContrastResult, Vector[Int])]] =
    subjects.foldLeft[Either[GroupError, Vector[(TContrastResult, Vector[Int])]]](Right(Vector.empty)) {
      case (Left(err), _) => Left(err)
      case (Right(acc), subject) =>
        results.get((subject, contrast.value)) match
          case None =>
            Left(GroupError.MissingSubjectContrast(subject.value, contrast.value))
          case Some(result) if result.estimates.length != space.nSamples =>
            Left(GroupError.sampleMismatch(space.nSamples, result.estimates.length))
          case Some(result) =>
            alignment(space, subject, contrast, result).map(order => acc :+ (result -> order))
    }

  /** VoxelAxis is authoritative. A geometry-free SampleAxis accepts only
    * canonical zero-based indices; parcel identity cannot be inferred from voxels.
    */
  private def alignment(
      space: GroupSpace,
      subject: SubjectId,
      contrast: FirstLevelContrastName,
      result: TContrastResult
  ): Either[GroupError, Vector[Int]] =
    def mismatch(detail: String) = GroupError.SpatialIdentityMismatch(subject.value, contrast.value, detail)
    val expected = space match
      case GroupSpace.VoxelAxis(_, indices) => Right(indices)
      case GroupSpace.SampleAxis(n, _) => Right(Vector.tabulate(n)(identity))
      case GroupSpace.ParcelAxis(_) => Left(mismatch("voxel results require an explicit voxel-to-parcel reduction"))
    if result.name != contrast.value then
      Left(GroupError.ContrastIdentityMismatch(subject.value, contrast.value, result.name))
    else expected.flatMap { indices =>
      val actual = result.voxelIndices
      if indices.distinct.length != indices.length || actual.distinct.length != actual.length then
        Left(mismatch("duplicate voxel indices"))
      else if indices.toSet != actual.toSet then Left(mismatch("voxel index sets differ"))
      else
        val invalid = result.standardErrors.toSeq.indexWhere(se => !se.isFinite || se <= 0.0 || !(se * se).isFinite || se * se == 0.0)
        if invalid >= 0 then
          Left(GroupError.InvalidStandardError(subject.value, contrast.value, invalid, result.standardErrors(invalid)))
        else
          val positions = actual.zipWithIndex.toMap
          Right(indices.map(positions))
    }

  private def parseContrasts(contrasts: Vector[String]): Either[GroupError, Vector[FirstLevelContrastName]] =
    contrasts.foldLeft[Either[GroupError, Vector[FirstLevelContrastName]]](Right(Vector.empty)) {
      case (Left(err), _) => Left(err)
      case (Right(acc), contrast) => FirstLevelContrastName(contrast).map(acc :+ _)
    }
