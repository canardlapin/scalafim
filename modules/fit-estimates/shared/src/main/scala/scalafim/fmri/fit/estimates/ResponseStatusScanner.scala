package scalafim.fmri.fit.estimates

import scalafim.estimates.*

/** Bounded scan of the existing inference-status capability for one binding.
  *
  * It looks up exactly the coefficient evidence of the bound observation and
  * the `Fit(observation)` plane, never falling back to an all-Estimable
  * default. It reads only the bound in-support features, in chunks of at most
  * `chunk` samples, and verifies every receipt. IO failure, cancellation or an
  * unverifiable receipt returns `Left` and no summary exists.
  */
object ResponseStatusScanner:
  def scan(
      src: InferenceEvidenceSource,
      b: ResponseSourceBinding,
      chunk: Int,
      cancelled: () => Boolean
  ): Either[EstimateError, StatusEvidence] =
    val unit = src.unit
    if chunk < 1 then Left(EstimateError.Invalid("status scan chunk must be positive"))
    else if unit.revision != b.unit then
      Left(EstimateError.Conflict(s"source unit revision ${unit.revision.value} differs from bound ${b.unit.value}"))
    else
      evidence(unit, b) match
        case Left(reason) => Right(StatusEvidence.Absent(reason))
        case Right(coefficients) =>
          val plane = new InferenceStatusScope.Fit(b.observation)
          val features = ResponseDigests.features(unit.domain)
          // A different feature identity is reported by evaluation as a source
          // mismatch; nothing is read from features the binding does not name.
          if features != b.features then
            Right(StatusEvidence.Scanned(StatusSummary(unit.revision, plane, features, Map.empty, coefficients.conditioning)))
          else
            count(src, plane, unit.domain.support, math.min(chunk, src.limits.maximumCells), cancelled).map: counts =>
              StatusEvidence.Scanned(StatusSummary(unit.revision, plane, features, counts, coefficients.conditioning))

  /** Exact lookups in declared order; any absence is typed, never Estimable. */
  private def evidence(unit: EstimateUnit, b: ResponseSourceBinding): Either[StatusAbsence, CoefficientInferenceEvidence] =
    for
      declared <- unit.inferenceEvidence.toRight(StatusAbsence.UnitHasNoEvidence)
      coefficients <- declared.coefficients.find(_.observation == b.observation).toRight(StatusAbsence.NoCoefficientEvidence)
      _ <- if coefficients.columns == b.columns then Right(()) else Left(StatusAbsence.ColumnsDisagree)
      notInferable = b.selected.filterNot(coefficients.inferableColumns.contains)
      _ <- if notInferable.isEmpty then Right(()) else Left(StatusAbsence.SelectedNotInferable(notInferable))
      _ <- if declared.planes.contains(InferenceStatusScope.Fit(b.observation)) then Right(()) else Left(StatusAbsence.FitPlaneAbsent)
    yield coefficients

  private def count(
      src: InferenceEvidenceSource,
      plane: InferenceStatusScope.Fit,
      samples: Vector[Int],
      chunk: Int,
      cancelled: () => Boolean
  ): Either[EstimateError, Map[InferenceStatusCode, Long]] =
    val codes = new Array[Byte](chunk)
    val totals = new Array[Long](InferenceStatusCode.values.length)
    var start = 0
    var failure: Option[EstimateError] = None
    while failure.isEmpty && start < samples.size do
      if cancelled() then failure = Some(EstimateError.Cancelled)
      else
        val selection = InferenceStatusSelection(Vector(plane), samples.slice(start, start + chunk))
        src.readInferenceStatus(selection, codes, cancelled) match
          case Left(error) => failure = Some(error)
          case Right(receipt) if receipt.selection != selection || receipt.cells.toLong != selection.cells =>
            failure = Some(EstimateError.Integrity("inference status receipt differs from the requested selection"))
          case Right(_) =>
            var i = 0
            while failure.isEmpty && i < selection.samples.size do
              InferenceStatusCode.fromCode(codes(i)) match
                case Left(error) => failure = Some(error)
                case Right(code) => totals(code.ordinal) += 1L
              i += 1
            start += selection.samples.size
    failure match
      case Some(error) => Left(error)
      case None if cancelled() => Left(EstimateError.Cancelled)
      case None =>
        Right(InferenceStatusCode.values.iterator.filter(code => totals(code.ordinal) > 0L)
          .map(code => code -> totals(code.ordinal)).toMap)
