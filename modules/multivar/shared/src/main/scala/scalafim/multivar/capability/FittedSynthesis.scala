package scalafim.multivar
package capability

import scalafim.multivar.core.*

import gale.linalg.CholeskyOptions
import gale.linalg.DMat

opaque type SynthesisTolerance = Double

object SynthesisTolerance:
  def apply(value: Double): Either[MultivarError, SynthesisTolerance] =
    if value.isFinite && value >= 0.0 then Right(value)
    else Left(MultivarError.InvalidTolerance("synthesis tolerance", value))

  val default: SynthesisTolerance =
    1e-10

  extension (tolerance: SynthesisTolerance)
    inline def value: Double = tolerance

enum SynthesisPolicy:
  case Explicit(decoder: ValueIdentity)
  case OrthonormalTranspose(tolerance: SynthesisTolerance)
  case EuclideanLeastSquares(ridge: Ridge)

enum ReconstructionCoordinate:
  case Working
  case Original

enum ReconstructionSource:
  case SuppliedScores
  case FullProjection
  case PartialProjection(policy: PartialScorePolicy)

final case class ReconstructionProvenance(
    analysisFrame: ValueIdentity,
    decoder: ValueIdentity,
    policy: SynthesisPolicy,
    source: ReconstructionSource,
    components: Vector[Int],
    targetFeatures: Vector[FeatureId],
    coordinate: ReconstructionCoordinate,
    semantic: SemanticProvenance
)

final case class ReconstructionResult private[multivar] (
    values: DMat,
    provenance: ReconstructionProvenance
)

/** Analysis plus an explicitly constructed feature synthesis operator. */
final class FittedBidirectionalTransform private (
    val analysis: FittedFrameTransform,
    val policy: SynthesisPolicy,
    val decoderValues: DMat,
    val provenance: SemanticProvenance
)(
    val decoder: Op[
      Dual[analysis.featureSpace.Id],
      Dual[analysis.componentSpace.Id],
      SynthesisOperatorRole,
      UncheckedEvidence
    ]
):
  def synthesizeWorking(
      scores: DMat,
      components: Option[IndexSet] = None,
      targetFeatures: Option[IndexSet] = None
  ): Either[MultivarError, ReconstructionResult] =
    synthesize(
      scores,
      components,
      targetFeatures,
      ReconstructionCoordinate.Working,
      ReconstructionSource.SuppliedScores
    )

  def synthesizeOriginal(
      scores: DMat,
      components: Option[IndexSet] = None,
      targetFeatures: Option[IndexSet] = None
  ): Either[MultivarError, ReconstructionResult] =
    synthesize(
      scores,
      components,
      targetFeatures,
      ReconstructionCoordinate.Original,
      ReconstructionSource.SuppliedScores
    )

  def reconstruct(
      input: MatrixView,
      coordinate: ReconstructionCoordinate = ReconstructionCoordinate.Original,
      components: Option[IndexSet] = None,
      targetFeatures: Option[IndexSet] = None
  ): Either[MultivarError, ReconstructionResult] =
    for
      fullScores <- analysis.project(input)
      selected <- selectScoreColumns(fullScores, components)
      result <- synthesize(
        selected,
        components,
        targetFeatures,
        coordinate,
        ReconstructionSource.FullProjection
      )
    yield result

  def reconstructPartial(
      partial: RestrictedFrameTransform[?, ?],
      input: IdentifiedFeatureMatrix,
      scorePolicy: PartialScorePolicy,
      coordinate: ReconstructionCoordinate = ReconstructionCoordinate.Original,
      targetFeatures: Option[IndexSet] = None
  ): Either[MultivarError, ReconstructionResult] =
    if partial.restriction.sourceFrameIdentity != analysis.frame.weights.valueIdentity then
      Left(MultivarError.DecoderUnavailable("partial projection and decoder belong to different fitted frames"))
    else
      for
        scores <- partial.score(input, scorePolicy)
        result <- synthesize(
          scores.values,
          None,
          targetFeatures,
          coordinate,
          ReconstructionSource.PartialProjection(scorePolicy)
        )
      yield result

  private[multivar] def synthesizeTransfer(
      scores: DMat,
      coordinate: ReconstructionCoordinate,
      targetFeatures: Option[IndexSet]
  ): Either[MultivarError, ReconstructionResult] =
    synthesize(
      scores,
      None,
      targetFeatures,
      coordinate,
      ReconstructionSource.SuppliedScores
    )

  private def synthesize(
      scores: DMat,
      components: Option[IndexSet],
      targetFeatures: Option[IndexSet],
      coordinate: ReconstructionCoordinate,
      source: ReconstructionSource
  ): Either[MultivarError, ReconstructionResult] =
    for
      selectedComponents <- componentSelection(components)
      selectedFeatures <- featureSelection(targetFeatures)
      _ <-
        if scores.cols == selectedComponents.length then Right(())
        else
          Left(
            MultivarError.MatrixShapeMismatch(
              s"synthesis received ${scores.cols} score columns but component selection has ${selectedComponents.length}"
            )
          )
      componentDecoder = GaleNumerics.selectRows(decoderValues, selectedComponents.indices)
      selectedDecoder = GaleNumerics.selectColumns(componentDecoder, selectedFeatures.indices)
      working = GaleNumerics.multiply(scores, selectedDecoder)
      values <- coordinate match
        case ReconstructionCoordinate.Working => Right(working)
        case ReconstructionCoordinate.Original =>
          val columns =
            if selectedFeatures.length == analysis.featureSpace.descriptor.size then None
            else Some(selectedFeatures)
          for
            restored <- analysis.preprocessor.inverseTransform(
              MatrixView.dense(working),
              columns,
              StoragePolicy.AllowDense
            )
            dense <- restored.toDense(StoragePolicy.AllowDense)
          yield dense
    yield
      ReconstructionResult(
        values,
        ReconstructionProvenance(
          analysis.frame.weights.valueIdentity,
          decoder.valueIdentity,
          policy,
          source,
          selectedComponents.indices,
          selectedFeatures.indices.map(analysis.featureSchema.identities),
          coordinate,
          provenance
        )
      )

  private def selectScoreColumns(
      scores: DMat,
      selection: Option[IndexSet]
  ): Either[MultivarError, DMat] =
    componentSelection(selection).map: checked =>
      if checked.indices == (0 until scores.cols).toVector then scores
      else GaleNumerics.selectColumns(scores, checked.indices)

  private def componentSelection(selection: Option[IndexSet]): Either[MultivarError, IndexSet] =
    selection match
      case None => IndexSet.contiguous(analysis.componentSpace.descriptor.size, IndexAxis.Component)
      case Some(indices) =>
        if indices.axis != IndexAxis.Component then
          Left(MultivarError.InvalidMap(s"component selection must use the component axis, got ${indices.axis.label}"))
        else indices.requireWithin(analysis.componentSpace.descriptor.size)

  private def featureSelection(selection: Option[IndexSet]): Either[MultivarError, IndexSet] =
    selection match
      case None => IndexSet.contiguous(analysis.featureSpace.descriptor.size, IndexAxis.Feature)
      case Some(indices) => MatrixView.requireColumnIndexSet(indices, analysis.featureSpace.descriptor.size)

object FittedBidirectionalTransform:
  def explicit(
      analysis: FittedFrameTransform,
      decoder: DMat,
      identity: ValueIdentity
  ): Either[MultivarError, FittedBidirectionalTransform] =
    fromDecoder(analysis, decoder, SynthesisPolicy.Explicit(identity), identity)

  def orthonormalTranspose(
      analysis: FittedFrameTransform,
      tolerance: SynthesisTolerance = SynthesisTolerance.default
  ): Either[MultivarError, FittedBidirectionalTransform] =
    for
      weights <- synthesisSemantic(analysis.frame.weights.toDense)
      gram = GaleNumerics.crossProduct(weights)
      _ <- validateIdentityGram(gram, tolerance)
      identity = ValueIdentity.derived("orthonormal-transpose-synthesis", analysis.frame.weights.valueIdentity)
      result <- fromDecoder(
        analysis,
        weights.transpose,
        SynthesisPolicy.OrthonormalTranspose(tolerance),
        identity
      )
    yield result

  def euclideanLeastSquares(
      analysis: FittedFrameTransform,
      ridge: Ridge
  ): Either[MultivarError, FittedBidirectionalTransform] =
    for
      weights <- synthesisSemantic(analysis.frame.weights.toDense)
      gram = MatrixOps.addRidge(GaleNumerics.crossProduct(weights), ridge.value)
      factor <- LinalgErrorAdapter.adapt(gram.cholesky(CholeskyOptions()))
      decoder <- LinalgErrorAdapter.adapt(factor.solve(weights.transpose))
      identity = ValueIdentity.derived("euclidean-ls-synthesis", analysis.frame.weights.valueIdentity)
      result <- fromDecoder(
        analysis,
        decoder,
        SynthesisPolicy.EuclideanLeastSquares(ridge),
        identity
      )
    yield result

  private def fromDecoder(
      analysis: FittedFrameTransform,
      decoderValues: DMat,
      policy: SynthesisPolicy,
      identity: ValueIdentity
  ): Either[MultivarError, FittedBidirectionalTransform] =
    if decoderValues.rows != analysis.componentSpace.descriptor.size ||
        decoderValues.cols != analysis.featureSpace.descriptor.size
    then
      Left(
        MultivarError.MatrixShapeMismatch(
          s"decoder is ${decoderValues.rows}x${decoderValues.cols}, expected " +
            s"${analysis.componentSpace.descriptor.size}x${analysis.featureSpace.descriptor.size}"
        )
      )
    else
      for
        _ <- MatrixOps.checkFinite("synthesis decoder", decoderValues)
        operator <- synthesisSemantic(
          Op.fromDense(
            decoderValues,
            CoordinateEvidence.dual(analysis.featureSpace.evidence),
            CoordinateEvidence.dual(analysis.componentSpace.evidence),
            OperatorRoleWitness.synthesis,
            identity,
            analysis.provenance.append(
              SemanticProvenanceEvent.Derived("attach-synthesis", Vector(analysis.frame.weights.valueIdentity, identity))
            )
          )
        )
      yield
        new FittedBidirectionalTransform(
          analysis,
          policy,
          decoderValues,
          operator.provenance
        )(operator)

  private def validateIdentityGram(
      gram: DMat,
      tolerance: SynthesisTolerance
  ): Either[MultivarError, Unit] =
    var row = 0
    var error = Option.empty[MultivarError]
    while row < gram.rows && error.isEmpty do
      var col = 0
      while col < gram.cols && error.isEmpty do
        val expected = if row == col then 1.0 else 0.0
        if !gram(row, col).isFinite || Math.abs(gram(row, col) - expected) > tolerance.value then
          error = Some(
            MultivarError.NonOrthonormalBasis("synthesis frame", row, col, gram(row, col))
          )
        col += 1
      row += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(())

private def synthesisSemantic[A](value: Either[SemanticError, A]): Either[MultivarError, A] =
  value.left.map:
    case SemanticError.MultivarFailure(error)  => error
    case SemanticError.LinearMapFailure(error) => LinalgErrorAdapter.toMultivarError(error)
    case error                                 => MultivarError.InvalidMap(error.message)
