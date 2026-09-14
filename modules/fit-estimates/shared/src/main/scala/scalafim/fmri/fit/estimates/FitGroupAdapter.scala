package scalafim.fmri.fit.estimates

import scalafim.fmri.group.*

import gale.linalg.Matrix
import scalafim.dataset.SubjectId
import scalafim.estimates.{DfRole, ScientificFact}
import scalafim.fmri.fit.TContrastResult

/** Bridge from first-level results to a group cube. Each subject contributes,
  * per first-level contrast, an effect map and a variance map formed by squaring
  * its standard error exactly once. The aligned receipt keeps nominal residual
  * df and any native fit provenance carried by the contrast result. Provenance
  * does not itself qualify calibration. This eager convenience lives outside
  * the group core; durable inputs use its bounded EstimateGroup reader.
  */
object FitGroupAdapter:

  /** Assemble a `GroupData` from per-subject, per-contrast t-contrast results.
    *
    * Every subject must provide each contrast, and every result must span the
    * sample space. Variances are `se²`; invalid SEs and mismatched contrast or
    * voxel identities are refused before constructing the group cube.
    */
  def groupData(
      space: GroupSpace,
      subjects: Vector[SubjectId],
      contrasts: Vector[String],
      results: Map[(SubjectId, String), TContrastResult]
  ): Either[GroupError, GroupData[VarianceCapability.WithVariances]] =
    for
      parsed <- parseContrasts(contrasts)
      built <- parsed.foldLeft[Either[GroupError, (Vector[(String, GroupResponse[VarianceCapability.WithVariances])], Vector[GroupUncertaintySource])]](
        Right(Vector.empty -> Vector.empty)):
        case (Left(error), _) => Left(error)
        case (Right((responses, sources)), contrast) =>
          buildResponse(space, subjects, contrast, results).map: (response, nextSources) =>
            (responses :+ (contrast.value -> response), sources ++ nextSources)
      sampleIds = space match
        case GroupSpace.VoxelAxis(_, indices) => indices
        case _ => Vector.tabulate(space.nSamples)(identity)
      sources = built._2.sortBy(source =>
        subjects.indexOf(source.subject) -> parsed.indexWhere(_.value == source.contrast))
      receipt <- GroupUncertaintyReceipt.make(subjects, parsed.map(_.value), sampleIds, sources,
        GroupGeometryEvidence.Unknown("TContrastResult voxel identities do not prove cross-subject registration"))
      data <- GroupData.build(subjects, space, built._1, Some(receipt))
    yield data

  private def buildResponse(
      space: GroupSpace,
      subjects: Vector[SubjectId],
      contrast: FirstLevelContrastName,
      results: Map[(SubjectId, String), TContrastResult]
  ): Either[GroupError, (GroupResponse.WithVariances, Vector[GroupUncertaintySource])] =
    collectCells(space, subjects, contrast, results).flatMap { cells =>
      val nSubjects = subjects.length
      val nSamples = space.nSamples
      val effects = Matrix.newBuilder(nSubjects, nSamples)
      val variances = Matrix.newBuilder(nSubjects, nSamples)
      var subjectIdx = 0
      while subjectIdx < nSubjects do
        val (result, positions) = cells(subjectIdx)
        var sample = 0
        while sample < nSamples do
          val position = positions(sample)
          val se = result.standardErrors(position)
          effects(subjectIdx, sample) = result.estimates(position)
          variances(subjectIdx, sample) = se * se
          sample += 1
        subjectIdx += 1
      GroupResponse.weighted(
        effects.result(),
        variances.result()
      ).map: response =>
        val samples = space match
          case GroupSpace.VoxelAxis(_, indices) => indices
          case _ => Vector.tabulate(space.nSamples)(identity)
        val sources = subjects.zip(cells).map: (subject, cell) =>
          val fit = groupFitProvenance(cell._1)
          val df = GroupDegreesOfFreedom(DfRole.Residual,
            GroupDfValues.Scalar(cell._1.residualDegreesOfFreedom.value.toDouble),
            "native TContrastResult residual degrees of freedom", false)
          GroupUncertaintySource(subject, contrast.value, samples, None,
            GroupVarianceOrigin.Estimated(df), fit, None)
        response -> sources
    }

  private def groupFitProvenance(result: TContrastResult): GroupFitProvenance =
    result.fitProvenance match
      case None =>
        val unknown = ScientificFact.Unknown("TContrastResult does not retain the selected fit policy")
        GroupFitProvenance(unknown, unknown, unknown, unknown)
      case Some(provenance) =>
        val estimator = ScientificFact.Known(
          s"native ${provenance.engine} fit; coefficient scope ${provenance.summary.coefficientScope.label}")
        val serialCorrelation = provenance.autocorrelation match
          case Some(diagnostics) =>
            ScientificFact.Known(
              s"AR(${diagnostics.order}) whitening; method ${diagnostics.whitening.method}; pooling ${diagnostics.whitening.pooling}; " +
                s"initial condition ${diagnostics.whitening.initialCondition}; segments ${diagnostics.whitening.segments.size}; " +
                s"censor gaps ${diagnostics.whitening.censorGaps.size}")
          case None if provenance.summary.autocorrelated =>
            ScientificFact.Unknown("fit summary reports autocorrelation but the contrast result lacks whitening diagnostics")
          case None => ScientificFact.Known("no autocorrelation whitening reported by the fitted model")
        val nuisance = provenance.responsePreparation match
          case Some(preparation) => ScientificFact.Known(s"native response-preparation receipt retained with ${preparation.records.size} records")
          case None => ScientificFact.Unknown("response-preparation provenance was not retained")
        val runCombination = ScientificFact.Known(
          s"native coefficient scope ${provenance.summary.coefficientScope.label}")
        GroupFitProvenance(estimator, serialCorrelation, nuisance, runCombination)

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
          case Some(result) =>
            alignResult(space, subject, contrast, result).map(aligned => acc :+ (result -> aligned))
    }

  private def alignResult(
      space: GroupSpace,
      subject: SubjectId,
      contrast: FirstLevelContrastName,
      result: TContrastResult
  ): Either[GroupError, Vector[Int]] =
    if result.name != contrast.value then
      Left(GroupError.ContrastIdentityMismatch(subject.value, contrast.value, result.name))
    else
      val aligned = space match
        case GroupSpace.VoxelAxis(_, requested) =>
          val positions = requested.map(result.voxelIndices.indexOf)
          if result.voxelIndices.distinct.size != result.voxelIndices.size || positions.exists(_ < 0) ||
              result.estimates.length != requested.size || result.voxelIndices.size != requested.size then
            Left(GroupError.SpatialIdentityMismatch(subject.value, contrast.value,
              "retained voxel identities must exactly cover the requested group sample axis"))
          else Right(positions)
        case _ =>
          if result.estimates.length != space.nSamples then Left(GroupError.sampleMismatch(space.nSamples, result.estimates.length))
          else Right(Vector.tabulate(space.nSamples)(identity))
      aligned.flatMap: positions =>
        positions.zipWithIndex.collectFirst:
          case (position, sample) if !result.standardErrors(position).isFinite || result.standardErrors(position) <= 0.0 =>
            GroupError.InvalidStandardError(subject.value, contrast.value, sample, result.standardErrors(position))
        match
          case Some(error) => Left(error)
          case None if positions.exists(position => !result.estimates(position).isFinite) =>
            Left(GroupError.NonFiniteData(s"subject '${subject.value}' contrast '${contrast.value}' effect estimates"))
          case None => Right(positions)

  private def parseContrasts(contrasts: Vector[String]): Either[GroupError, Vector[FirstLevelContrastName]] =
    contrasts.foldLeft[Either[GroupError, Vector[FirstLevelContrastName]]](Right(Vector.empty)) {
      case (Left(err), _) => Left(err)
      case (Right(acc), contrast) => FirstLevelContrastName(contrast).map(acc :+ _)
    }
