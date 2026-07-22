package scalafim.multivar
package capability

import scalafim.multivar.core.*

import gale.linalg.DMat
import gale.linalg.DVec

final case class TransformDiagnostics(
    method: String,
    requestedComponents: ComponentCount,
    effectiveComponents: Int,
    spectrum: Option[DVec] = None
):
  require(method.nonEmpty, "transform method must be non-empty")
  require(effectiveComponents > 0, "a fitted transform must retain at least one component")
  require(spectrum.forall(_.length == effectiveComponents), "transform spectrum must match retained components")

/** Lifecycle-aware transform backed by one typed functional frame.
  *
  * Preprocessing remains fitted lifecycle state. The mathematical action is the
  * `FunctionalFrame`; applying it to new data constructs a fresh typed table and
  * derives scores through ordinary operator composition.
  */
final class FittedFrameTransform private (
    val trainingRowSpace: SpaceRef,
    val trainingRowSchema: RowSchema,
    val featureSpace: SpaceRef,
    val componentSpace: SpaceRef,
    val preprocessor: FittedPreprocessor,
    val featureSchema: FeatureSchema,
    val diagnostics: TransformDiagnostics,
    val provenance: SemanticProvenance
)(
    val frame: FunctionalFrame[featureSpace.Id, componentSpace.Id, UncheckedEvidence],
    val trainingScores: Op[Primal[componentSpace.Id], Primal[trainingRowSpace.Id], ScoreOperatorRole, UncheckedEvidence],
    val trainingValues: DMat
):
  def requireSynthesis: Either[MultivarError, FittedBidirectionalTransform] =
    Left(
      MultivarError.DecoderUnavailable(
        s"fitted frame '${diagnostics.method}' has analysis capability only; attach an explicit synthesis policy"
      )
    )

  def withExplicitSynthesis(
      decoder: DMat,
      identity: ValueIdentity
  ): Either[MultivarError, FittedBidirectionalTransform] =
    FittedBidirectionalTransform.explicit(this, decoder, identity)

  def withEuclideanSynthesis(
      ridge: Ridge
  ): Either[MultivarError, FittedBidirectionalTransform] =
    FittedBidirectionalTransform.euclideanLeastSquares(this, ridge)

  def withOrthonormalSynthesis(
      tolerance: SynthesisTolerance = SynthesisTolerance.default
  ): Either[MultivarError, FittedBidirectionalTransform] =
    FittedBidirectionalTransform.orthonormalTranspose(this, tolerance)

  def supplementary: SupplementaryProjector[trainingRowSpace.Id, componentSpace.Id] =
    SupplementaryProjector.from(this)

  def restrictFeatures(
      columns: IndexSet
  ): Either[MultivarError, RestrictedFrameTransform[featureSpace.Id, componentSpace.Id]] =
    RestrictedFrameTransform.from(this, columns)

  def project(input: MatrixView): Either[MultivarError, DMat] =
    if input.cols != featureSpace.descriptor.size then
      Left(
        MultivarError.MatrixShapeMismatch(
          s"fitted frame expected ${featureSpace.descriptor.size} columns, got ${input.cols}"
        )
      )
    else
      for
        processed <- preprocessor.transform(input)
        rows <- SpaceRef.of(
          s"${featureSpace.descriptor.id.value}.projection-rows",
          SpaceRole.Samples,
          input.rows
        )
        table <- transformSemantic(
          Op.fromMatrixView(
            processed,
            CoordinateEvidence.dual(featureSpace.evidence),
            CoordinateEvidence.primal(rows.evidence),
            OperatorRoleWitness.table,
            ValueIdentity.derived("fitted-transform-input", frame.weights.valueIdentity),
            provenance.append(
              SemanticProvenanceEvent.Derived("transform-new-data", Vector(frame.weights.valueIdentity))
            )
          )
        )
        values <- transformSemantic(frame.scores(table).toDense)
      yield values

object FittedFrameTransform:
  def fromTraining(
      input: MatrixView,
      weights: DMat,
      preprocessor: FittedPreprocessor,
      method: String,
      requested: ComponentCount,
      spectrum: Option[DVec] = None,
      featureIds: Option[Vector[FeatureId]] = None,
      rowIds: Option[Vector[RowId]] = None
  ): Either[MultivarError, FittedFrameTransform] =
    if weights.rows != input.cols then
      Left(
        MultivarError.MatrixShapeMismatch(
          s"$method frame has ${weights.rows} feature rows, expected ${input.cols}"
        )
      )
    else if weights.cols <= 0 then Left(MultivarError.InvalidDimension(s"$method retained components", weights.cols))
    else
      val cleanMethod = method.trim
      if cleanMethod.isEmpty then Left(MultivarError.InvalidId("transform method", method, "must be non-empty"))
      else
        for
          processed <- preprocessor.transform(input)
          rows <- SpaceRef.of(s"$cleanMethod.training-rows", SpaceRole.Samples, input.rows)
          features <- SpaceRef.of(s"$cleanMethod.features", SpaceRole.Observed, input.cols)
          components <- SpaceRef.of(s"$cleanMethod.components", SpaceRole.Latent, weights.cols)
          provenance = SemanticProvenance.source(s"$cleanMethod-fitted-frame")
          frameIdentity = ValueIdentity.source(ValueId.unsafe(s"$cleanMethod.functional-frame"))
          rowSchema <- rowIds match
            case Some(ids) => RowSchema.from(rows.descriptor, ids, ValueIdentity.derived("training-row-schema", frameIdentity))
            case None      => RowSchema.positional(rows.descriptor, ValueIdentity.derived("training-row-schema", frameIdentity))
          schema <- featureIds match
            case Some(ids) => FeatureSchema.from(features.descriptor, ids, ValueIdentity.derived("feature-schema", frameIdentity))
            case None      => FeatureSchema.positional(features.descriptor, ValueIdentity.derived("feature-schema", frameIdentity))
          table <- transformSemantic(
            Op.fromMatrixView(
              processed,
              CoordinateEvidence.dual(features.evidence),
              CoordinateEvidence.primal(rows.evidence),
              OperatorRoleWitness.table,
              ValueIdentity.source(ValueId.unsafe(s"$cleanMethod.training-table")),
              provenance
            )
          )
          frameOperator <- transformSemantic(
            Op.fromDense(
              weights,
              CoordinateEvidence.primal(components.evidence),
              CoordinateEvidence.dual(features.evidence),
              OperatorRoleWitness.frame,
              frameIdentity,
              provenance
            )
          )
          frame = FunctionalFrame(frameOperator)
          scoreOperator = frame.scores(table)
          scoreValues <- transformSemantic(scoreOperator.toDense)
        yield
          new FittedFrameTransform(
            rows,
            rowSchema,
            features,
            components,
            preprocessor,
            schema,
            TransformDiagnostics(cleanMethod, requested, weights.cols, spectrum),
            provenance
          )(
            frame,
            scoreOperator,
            scoreValues
          )

/** Directed prediction transform backed by a typed coefficient operator. */
final class FittedCoefficientTransform private (
    val sourceFeatureSpace: SpaceRef,
    val targetFeatureSpace: SpaceRef,
    val predictorPreprocessor: FittedPreprocessor,
    val responsePreprocessor: FittedPreprocessor,
    val provenance: SemanticProvenance
)(
    val coefficient: OpCoefficient[sourceFeatureSpace.Id, targetFeatureSpace.Id, UncheckedEvidence]
):
  def predictWorking(input: MatrixView): Either[MultivarError, DMat] =
    if input.cols != sourceFeatureSpace.descriptor.size then
      Left(
        MultivarError.MatrixShapeMismatch(
          s"coefficient transform expected ${sourceFeatureSpace.descriptor.size} columns, got ${input.cols}"
        )
      )
    else
      for
        processed <- predictorPreprocessor.transform(input)
        rows <- SpaceRef.of("coefficient.prediction-rows", SpaceRole.Samples, input.rows)
        table <- transformSemantic(
          Op.fromMatrixView(
            processed,
            CoordinateEvidence.dual(sourceFeatureSpace.evidence),
            CoordinateEvidence.primal(rows.evidence),
            OperatorRoleWitness.table,
            ValueIdentity.derived("coefficient-predictor-table", coefficient.valueIdentity),
            provenance
          )
        )
        predicted <- transformSemantic(coefficient.andThen(table).toDense)
      yield predicted

  def predict(input: MatrixView): Either[MultivarError, DMat] =
    for
      working <- predictWorking(input)
      restored <- responsePreprocessor.inverseTransform(
        MatrixView.dense(working),
        policy = StoragePolicy.AllowDense
      )
      dense <- restored.toDense(StoragePolicy.AllowDense)
    yield dense

object FittedCoefficientTransform:
  def from(
      coefficient: DMat,
      predictorPreprocessor: FittedPreprocessor,
      responsePreprocessor: FittedPreprocessor,
      method: String
  ): Either[MultivarError, FittedCoefficientTransform] =
    val cleanMethod = method.trim
    if cleanMethod.isEmpty then Left(MultivarError.InvalidId("coefficient method", method, "must be non-empty"))
    else if coefficient.rows != predictorPreprocessor.inputCols then
      Left(MultivarError.MatrixShapeMismatch("coefficient rows must match predictor feature count"))
    else if coefficient.cols != responsePreprocessor.inputCols then
      Left(MultivarError.MatrixShapeMismatch("coefficient columns must match response feature count"))
    else
      for
        source <- SpaceRef.of(s"$cleanMethod.source-features", SpaceRole.Observed, coefficient.rows)
        target <- SpaceRef.of(s"$cleanMethod.target-features", SpaceRole.Observed, coefficient.cols)
        provenance = SemanticProvenance.source(s"$cleanMethod-fitted-coefficient")
        operator <- transformSemantic(
          Op.fromDense(
            coefficient,
            CoordinateEvidence.dual(target.evidence),
            CoordinateEvidence.dual(source.evidence),
            OperatorRoleWitness.coefficient,
            ValueIdentity.source(ValueId.unsafe(s"$cleanMethod.coefficient")),
            provenance
          )
        )
      yield
        new FittedCoefficientTransform(
          source,
          target,
          predictorPreprocessor,
          responsePreprocessor,
          provenance
        )(operator)

private def transformSemantic[A](value: Either[SemanticError, A]): Either[MultivarError, A] =
  value.left.map:
    case SemanticError.MultivarFailure(error)  => error
    case SemanticError.LinearMapFailure(error) => LinalgErrorAdapter.toMultivarError(error)
    case error                                 => MultivarError.InvalidMap(error.message)
