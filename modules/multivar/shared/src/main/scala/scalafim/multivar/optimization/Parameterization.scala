package scalafim.multivar
package optimization

import scalafim.multivar.core.*

import gale.linalg.DMat

enum InjectivityClaim:
  case VerifiedInjective
  case NonInjective
  case Unknown

enum ImageClaim:
  case ExactFeasibleImage
  case RestrictedImage
  case Unknown

enum RedundancyClaim:
  case NoRedundancy
  case Redundant
  case Unknown

enum DifferentialKind:
  case Linear
  case Bilinear

final case class ParameterizationProperties(
    injectivity: InjectivityClaim,
    image: ImageClaim,
    redundancy: RedundancyClaim,
    gauge: ParameterizationGauge,
    differential: DifferentialKind,
    invertibleOnImage: Boolean
):
  def redundantCoordinates: Boolean = redundancy == RedundancyClaim.Redundant

enum ParameterizationError:
  case InvalidDefinition(reason: String)
  case NullSpaceResidual(residual: Double, threshold: Double)
  case InverseUnavailable
  case SolverBoundary(error: scalafim.linalg.FirstOrderError)
  case Program(error: ProgramError)
  case Semantic(error: SemanticError)

  def message: String =
    this match
      case InvalidDefinition(reason) => reason
      case NullSpaceResidual(residual, threshold) =>
        s"declared null-space basis has constraint residual $residual above threshold $threshold"
      case InverseUnavailable => "parameterization has no unique semantic inverse"
      case SolverBoundary(error) => error.message
      case Program(error) => error.message
      case Semantic(error) => error.message

final case class NullSpaceProof(
    constraintIdentity: ValueIdentity,
    basisIdentity: ValueIdentity,
    rankTolerance: CertificateTolerance,
    residual: Double,
    numericalCertificate: scalafim.linalg.LinearReductionCertificate
)

/** Executable exact linear realization `W = E Z`. The descriptor used by
  * `OperatorProgram` and the actual realization/differential are kept together.
  */
final class LinearFrameParameterization[
    Feature <: SemanticSpace,
    FreeFeature <: SemanticSpace,
    Component <: SemanticSpace
] private (
    val descriptor: FrameParameterization[Feature, Component],
    val embedding: Op[Dual[FreeFeature], Dual[Feature], ? <: OperatorRoleTag, ? <: OperatorEvidence],
    val inverseOnImage: Option[
      Op[Dual[Feature], Dual[FreeFeature], ? <: OperatorRoleTag, ? <: OperatorEvidence]
    ],
    val properties: ParameterizationProperties,
    val nullSpaceProof: Option[NullSpaceProof],
    val provenance: SemanticProvenance
):
  def realize(free: DMat): Either[ParameterizationError, DMat] =
    embedding(free).left.map(ParameterizationError.Semantic.apply)

  def jvp(tangent: DMat): Either[ParameterizationError, DMat] =
    realize(tangent)

  def vjp(cotangent: DMat): Either[ParameterizationError, DMat] =
    embedding.dual(cotangent).left.map(ParameterizationError.Semantic.apply)

  def invert(semantic: DMat): Either[ParameterizationError, DMat] =
    inverseOnImage match
      case Some(inverse) => inverse(semantic).left.map(ParameterizationError.Semantic.apply)
      case None => Left(ParameterizationError.InverseUnavailable)

object LinearFrameParameterization:
  def knownSupport[
      Feature <: SemanticSpace,
      FreeFeature <: SemanticSpace,
      Component <: SemanticSpace
  ](
      variable: FrameVariable[Feature, Component],
      freeFeatureSpace: SpaceEvidence[FreeFeature],
      support: IndexSet,
      embeddingIdentity: ValueIdentity
  ): Either[ParameterizationError, LinearFrameParameterization[Feature, FreeFeature, Component]] =
    if support.length != freeFeatureSpace.dimension then
      Left(ParameterizationError.InvalidDefinition("support size must equal the free feature dimension"))
    else if support.indices.exists(index => index < 0 || index >= variable.featureSpace.dimension) then
      Left(ParameterizationError.InvalidDefinition("support index lies outside the semantic feature space"))
    else
      val forward = new Array[Double](variable.featureSpace.dimension * freeFeatureSpace.dimension)
      val inverse = new Array[Double](freeFeatureSpace.dimension * variable.featureSpace.dimension)
      var free = 0
      while free < support.length do
        val semantic = support.indices(free)
        forward(semantic * freeFeatureSpace.dimension + free) = 1.0
        inverse(free * variable.featureSpace.dimension + semantic) = 1.0
        free += 1
      for
        embedding <- coefficient(
          GaleNumerics.matrixFromRowMajor(variable.featureSpace.dimension, freeFeatureSpace.dimension, forward),
          CoordinateEvidence.dual(freeFeatureSpace),
          CoordinateEvidence.dual(variable.featureSpace),
          embeddingIdentity
        )
        inverseMap <- coefficient(
          GaleNumerics.matrixFromRowMajor(freeFeatureSpace.dimension, variable.featureSpace.dimension, inverse),
          CoordinateEvidence.dual(variable.featureSpace),
          CoordinateEvidence.dual(freeFeatureSpace),
          ValueIdentity.derived("support-left-inverse", embeddingIdentity)
        )
      yield
        new LinearFrameParameterization(
          FrameParameterization.knownSupport(variable, freeFeatureSpace, embedding, injective = true),
          embedding,
          Some(inverseMap),
          ParameterizationProperties(
            InjectivityClaim.VerifiedInjective,
            ImageClaim.ExactFeasibleImage,
            RedundancyClaim.NoRedundancy,
            ParameterizationGauge.Unique,
            DifferentialKind.Linear,
            invertibleOnImage = true
          ),
          None,
          SemanticProvenance.source("known-support-parameterization")
        )

  def nullSpace[
      Feature <: SemanticSpace,
      FreeFeature <: SemanticSpace,
      Component <: SemanticSpace,
      ConstraintSpace <: SemanticSpace,
      RB <: OperatorRoleTag,
      EB <: OperatorEvidence,
      RC <: OperatorRoleTag,
      EC <: OperatorEvidence
  ](
      variable: FrameVariable[Feature, Component],
      freeFeatureSpace: SpaceEvidence[FreeFeature],
      basis: Op[Dual[FreeFeature], Dual[Feature], RB, EB],
      constraint: Op[Dual[Feature], Primal[ConstraintSpace], RC, EC],
      rankTolerance: CertificateTolerance,
      inverseOnImage: Option[
        Op[Dual[Feature], Dual[FreeFeature], ? <: OperatorRoleTag, ? <: OperatorEvidence]
      ] = None
  ): Either[ParameterizationError, LinearFrameParameterization[Feature, FreeFeature, Component]] =
    for
      tolerance <- scalafim.linalg.FirstOrderTolerance
        .from(rankTolerance.absoluteValue, rankTolerance.relativeValue)
        .left
        .map(ParameterizationError.SolverBoundary.apply)
      numerical <- scalafim.linalg.ExactLinearReduction
        .verify(
          new OperatorLinearMap(basis),
          new OperatorLinearMap(constraint),
          tolerance
        )
        .left
        .map(ParameterizationError.SolverBoundary.apply)
    yield
      new LinearFrameParameterization(
        FrameParameterization.nullSpace(variable, freeFeatureSpace, basis, rankTolerance),
        basis,
        inverseOnImage,
        ParameterizationProperties(
          if inverseOnImage.isDefined then InjectivityClaim.VerifiedInjective else InjectivityClaim.Unknown,
          ImageClaim.ExactFeasibleImage,
          RedundancyClaim.NoRedundancy,
          ParameterizationGauge.Unique,
          DifferentialKind.Linear,
          invertibleOnImage = inverseOnImage.isDefined
        ),
        Some(
          NullSpaceProof(
            constraint.valueIdentity,
            basis.valueIdentity,
            rankTolerance,
            numerical.residual,
            numerical
          )
        ),
        SemanticProvenance
          .source("null-space-parameterization")
          .append(SemanticProvenanceEvent.Derived("verify-null-space", Vector(constraint.valueIdentity, basis.valueIdentity)))
      )

  def sharedBasis[
      Feature <: SemanticSpace,
      FreeFeature <: SemanticSpace,
      Component <: SemanticSpace,
      R <: OperatorRoleTag,
      E <: OperatorEvidence
  ](
      variable: FrameVariable[Feature, Component],
      freeFeatureSpace: SpaceEvidence[FreeFeature],
      basis: Op[Dual[FreeFeature], Dual[Feature], R, E],
      inverseOnImage: Option[
        Op[Dual[Feature], Dual[FreeFeature], ? <: OperatorRoleTag, ? <: OperatorEvidence]
      ] = None
  ): LinearFrameParameterization[Feature, FreeFeature, Component] =
    new LinearFrameParameterization(
      FrameParameterization.sharedBasis(variable, freeFeatureSpace, basis, injective = inverseOnImage.isDefined),
      basis,
      inverseOnImage,
      ParameterizationProperties(
        if inverseOnImage.isDefined then InjectivityClaim.VerifiedInjective else InjectivityClaim.Unknown,
        ImageClaim.RestrictedImage,
        if inverseOnImage.isDefined then RedundancyClaim.NoRedundancy else RedundancyClaim.Unknown,
        ParameterizationGauge.Unique,
        DifferentialKind.Linear,
        invertibleOnImage = inverseOnImage.isDefined
      ),
      None,
      SemanticProvenance.source("shared-basis-parameterization")
    )

  def blockBasis[
      Feature <: SemanticSpace,
      FreeFeature <: SemanticSpace,
      Component <: SemanticSpace,
      R <: OperatorRoleTag,
      E <: OperatorEvidence
  ](
      variable: FrameVariable[Feature, Component],
      freeFeatureSpace: SpaceEvidence[FreeFeature],
      embedding: Op[Dual[FreeFeature], Dual[Feature], R, E],
      blocks: Vector[ParameterId]
  ): Either[ParameterizationError, LinearFrameParameterization[Feature, FreeFeature, Component]] =
    FrameParameterization.blockDiagonal(variable, blocks).left.map(ParameterizationError.Program.apply).map: descriptor =>
      new LinearFrameParameterization(
        descriptor,
        embedding,
        None,
        ParameterizationProperties(
          InjectivityClaim.Unknown,
          ImageClaim.RestrictedImage,
          RedundancyClaim.Unknown,
          ParameterizationGauge.Unique,
          DifferentialKind.Linear,
          invertibleOnImage = false
        ),
        None,
        SemanticProvenance.source("block-basis-parameterization")
      )

  private def coefficient[From <: Coordinate, To <: Coordinate](
      value: DMat,
      domain: CoordinateEvidence[From],
      codomain: CoordinateEvidence[To],
      identity: ValueIdentity
  ): Either[ParameterizationError, Op[From, To, CoefficientOperatorRole, UncheckedEvidence]] =
    Op.fromDense(value, domain, codomain, OperatorRoleWitness.coefficient, identity)
      .left
      .map(ParameterizationError.Semantic.apply)

final case class FactorCoordinates private (left: DMat, right: DMat)

object FactorCoordinates:
  def from(left: DMat, right: DMat): Either[ParameterizationError, FactorCoordinates] =
    if left.cols <= 0 || right.cols != left.cols then
      Left(ParameterizationError.InvalidDefinition("fixed-rank factors require equal positive inner rank"))
    else Right(FactorCoordinates(left, right))

final class FixedRankParameterization private (
    val rows: Int,
    val columns: Int,
    val rank: ComponentCount,
    val semanticIdentity: ValueIdentity,
    val properties: ParameterizationProperties,
    val provenance: SemanticProvenance
):
  def realize(free: FactorCoordinates): Either[ParameterizationError, DMat] =
    validate(free).map(_ => free.left * free.right.t)

  def lift(free: FactorCoordinates): Either[ParameterizationError, DMat] = realize(free)

  def invert(semantic: DMat): Either[ParameterizationError, FactorCoordinates] =
    Left(ParameterizationError.InverseUnavailable)

  def jvp(at: FactorCoordinates, tangent: FactorCoordinates): Either[ParameterizationError, DMat] =
    for
      _ <- validate(at)
      _ <- validate(tangent)
    yield at.left * tangent.right.t + tangent.left * at.right.t

  def vjp(at: FactorCoordinates, cotangent: DMat): Either[ParameterizationError, FactorCoordinates] =
    if cotangent.rows != rows || cotangent.cols != columns then
      Left(ParameterizationError.InvalidDefinition("factorization cotangent shape does not match semantic map"))
    else validate(at).flatMap(_ => FactorCoordinates.from(cotangent * at.right, cotangent.t * at.left))

  def gaugeTransform(
      free: FactorCoordinates,
      transform: DMat,
      inverseTranspose: DMat
  ): Either[ParameterizationError, FactorCoordinates] =
    if transform.rows != rank.value || transform.cols != rank.value ||
        inverseTranspose.rows != rank.value || inverseTranspose.cols != rank.value then
      Left(ParameterizationError.InvalidDefinition("gauge transforms must be square on the factor rank"))
    else validate(free).flatMap(_ => FactorCoordinates.from(free.left * transform, free.right * inverseTranspose))

  private def validate(value: FactorCoordinates): Either[ParameterizationError, Unit] =
    if value.left.rows != rows || value.right.rows != columns ||
        value.left.cols != rank.value || value.right.cols != rank.value then
      Left(ParameterizationError.InvalidDefinition("factor coordinates do not match fixed-rank parameterization"))
    else Right(())

object FixedRankParameterization:
  def from(
      rows: Int,
      columns: Int,
      rank: ComponentCount,
      semanticIdentity: ValueIdentity
  ): Either[ParameterizationError, FixedRankParameterization] =
    if rows <= 0 || columns <= 0 || rank.value > Math.min(rows, columns) then
      Left(ParameterizationError.InvalidDefinition("fixed rank must fit positive semantic map dimensions"))
    else
      Right(
        new FixedRankParameterization(
          rows,
          columns,
          rank,
          semanticIdentity,
          ParameterizationProperties(
            InjectivityClaim.NonInjective,
            ImageClaim.ExactFeasibleImage,
            RedundancyClaim.Redundant,
            ParameterizationGauge.GeneralLinear,
            DifferentialKind.Bilinear,
            invertibleOnImage = false
          ),
          SemanticProvenance.source("fixed-rank-factorization")
        )
      )
