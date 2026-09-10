package scalafim.examples

// docs:selected-imports:start
import gale.backend.Backend
import scalafim.dataset.DatasetSeriesReader
import scalafim.fmri.design.ColumnId
import scalafim.fmri.fit.*
import scalafim.fmri.hrf.{Hrf, ResponseFunctional}
import scalafim.fmri.model.FitPlan
// docs:selected-imports:end

object SelectedEstimatesExample:
  // docs:selected-condition:start
  def conditionRequest(a: ColumnId, b: ColumnId): Either[FitError, FirstLevelEstimateRequest] =
    FirstLevelEstimateRequest.make(Vector(
      EstimateOutput.Coefficient(a),
      EstimateOutput.Coefficient(b),
      EstimateOutput.Contrast(StructuralTContrast.fromIds(
        ContrastId.unsafe("a-minus-b"), "A minus B", Map(a -> 1.0, b -> -1.0)))
    ))
  // docs:selected-condition:end

  // docs:selected-fir:start
  def firRequest(firColumns: Vector[ColumnId]): Either[FitError, FirstLevelEstimateRequest] =
    FirstLevelEstimateRequest.make(firColumns.map(EstimateOutput.Coefficient.apply))
  // docs:selected-fir:end

  // docs:selected-response:start
  def responseRequest(
      cell: StructuralHypothesisDsl.CellRef,
      basis: Hrf,
      functional: ResponseFunctional
  ): Either[FitError, FirstLevelEstimateRequest] =
    for
      contrast <- cell.response(basis, functional)
        .named("response", "Reconstructed response for the selected cell")
        .toStructural
      request <- FirstLevelEstimateRequest.make(Vector(EstimateOutput.Contrast(contrast)))
    yield request
  // docs:selected-response:end

  // docs:selected-uncertainty:start
  def withUncertainty(
      request: FirstLevelEstimateRequest,
      uncertainty: EstimateUncertaintyRequest
  ): Either[FitError, FirstLevelEstimateRequest] =
    FirstLevelEstimateRequest.make(request.outputs, uncertainty, request.retainRunCoefficients)
  // docs:selected-uncertainty:end

  // docs:selected-nuisance:start
  def withNuisance(
      request: FirstLevelEstimateRequest,
      nuisanceColumns: Vector[ColumnId]
  ): Either[FitError, FirstLevelEstimateRequest] =
    FirstLevelEstimateRequest.make(
      request.outputs ++ nuisanceColumns.map(EstimateOutput.Coefficient.apply),
      request.uncertainty, request.retainRunCoefficients)
  // docs:selected-nuisance:end

  // docs:selected-run-coefficients:start
  def withRunCoefficients(
      request: FirstLevelEstimateRequest,
      runColumns: Vector[ColumnId]
  ): Either[FitError, FirstLevelEstimateRequest] =
    FirstLevelEstimateRequest.make(request.outputs, request.uncertainty,
      retainRunCoefficients = runColumns)
  // docs:selected-run-coefficients:end

  // docs:selected-prepare:start
  def prepareSelected(
      model: FitPlan,
      request: FirstLevelEstimateRequest,
      maximumVoxelsPerRead: Int
  ): Either[FitError, PreparedSelectedEstimates] =
    for
      size <- ChunkSize(maximumVoxelsPerRead)
      prepared <- SelectedEstimates.prepare(model, request, size)
    yield prepared
  // docs:selected-prepare:end

  def estimateSelected(
      model: FitPlan,
      reader: DatasetSeriesReader,
      columns: Vector[ColumnId],
      write: FirstLevelEstimateBlock => Either[FitError, Unit]
  )(using Backend): Either[FitError, EstimateExecutionOutcome] =
    for
      request <- FirstLevelEstimateRequest.make(columns.map(EstimateOutput.Coefficient.apply))
      size <- ChunkSize(1024)
      prepared <- FirstLevelEstimates.prepare(model, request, size)
      outcome <- prepared.foreachBlock(reader, write)
    yield outcome

  def estimatePooled(
      model: FitPlan,
      reader: DatasetSeriesReader,
      request: FirstLevelEstimateRequest,
      write: FirstLevelFixedEffectsEstimateBlock => Either[FitError, Unit]
  )(using Backend): Either[FitError, EstimateExecutionOutcome] =
    for
      size <- ChunkSize(1024)
      prepared <- FirstLevelFixedEffectsEstimates.prepare(model, request, size)
      outcome <- prepared.foreachBlock(reader, write)
    yield outcome

  // docs:selected-execute:start
  def estimateWithDeclaredEngine(
      model: FitPlan,
      reader: DatasetSeriesReader,
      request: FirstLevelEstimateRequest,
      write: SelectedEstimateBlock => Either[FitError, Unit]
  )(using Backend): Either[FitError, EstimateExecutionOutcome] =
    for
      size <- ChunkSize(1024)
      prepared <- SelectedEstimates.prepare(model, request, size)
      outcome <- prepared.foreachBlock(reader, write)
    yield outcome
  // docs:selected-execute:end
