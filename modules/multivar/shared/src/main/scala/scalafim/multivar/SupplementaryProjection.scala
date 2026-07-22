package scalafim.multivar

import gale.linalg.CholeskyOptions
import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.MutableDVec

opaque type RowId = String

object RowId:
  def apply(value: String): Either[MultivarError, RowId] =
    Identifier.validate("row id", value)

  def unsafe(value: String): RowId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: RowId)
    inline def value: String = id

enum RowIdentityMode:
  case Named
  case Positional

/** Ordered identities for the exact fitted training-row space. */
final class RowSchema private (
    val space: MvSpace,
    val identities: Vector[RowId],
    val valueIdentity: ValueIdentity,
    val mode: RowIdentityMode
):
  require(identities.length == space.size, "row schema size must match its semantic space")
  require(identities.distinct.length == identities.length, "row identities must be unique")

object RowSchema:
  def from(
      space: MvSpace,
      identities: Vector[RowId],
      valueIdentity: ValueIdentity
  ): Either[MultivarError, RowSchema] =
    validate(space, identities).map(_ => new RowSchema(space, identities, valueIdentity, RowIdentityMode.Named))

  def positional(space: MvSpace, valueIdentity: ValueIdentity): Either[MultivarError, RowSchema] =
    val identities = Vector.tabulate(space.size)(index => RowId.unsafe(s"${space.id.value}.row-$index"))
    Right(new RowSchema(space, identities, valueIdentity, RowIdentityMode.Positional))

  def reordered(source: RowSchema, identities: Vector[RowId]): Either[MultivarError, RowSchema] =
    validate(source.space, identities).flatMap: _ =>
      if identities.toSet != source.identities.toSet then
        Left(MultivarError.RowIdentityMismatch("reordered row schema must contain exactly the fitted row identities"))
      else Right(new RowSchema(source.space, identities, source.valueIdentity, source.mode))

  private def validate(space: MvSpace, identities: Vector[RowId]): Either[MultivarError, Unit] =
    if identities.length != space.size then
      Left(
        MultivarError.MatrixShapeMismatch(
          s"row schema for '${space.id.value}' has ${identities.length} identities, expected ${space.size}"
        )
      )
    else
      val seen = scala.collection.mutable.HashSet.empty[String]
      var index = 0
      var duplicate = Option.empty[RowId]
      while index < identities.length && duplicate.isEmpty do
        val id = identities(index)
        if seen.contains(id.value) then duplicate = Some(id)
        else seen += id.value
        index += 1
      duplicate match
        case Some(id) => Left(MultivarError.RowIdentityMismatch(s"duplicate row identity '${id.value}'"))
        case None     => Right(())

opaque type ComponentTolerance = Double

object ComponentTolerance:
  def apply(value: Double): Either[MultivarError, ComponentTolerance] =
    if value.isFinite && value >= 0.0 then Right(value)
    else Left(MultivarError.InvalidTolerance("component-null tolerance", value))

  val default: ComponentTolerance =
    1e-12

  extension (tolerance: ComponentTolerance)
    inline def value: Double = tolerance

enum NullComponentPolicy:
  case Reject(tolerance: ComponentTolerance)
  case Drop(tolerance: ComponentTolerance)
  case Regularize(ridge: Ridge)

object NullComponentPolicy:
  def reject(tolerance: Double = 1e-12): Either[MultivarError, NullComponentPolicy] =
    ComponentTolerance(tolerance).map(NullComponentPolicy.Reject(_))

  def drop(tolerance: Double = 1e-12): Either[MultivarError, NullComponentPolicy] =
    ComponentTolerance(tolerance).map(NullComponentPolicy.Drop(_))

  def regularize(ridge: Double): Either[MultivarError, NullComponentPolicy] =
    Ridge(ridge).flatMap: value =>
      if value.value > 0.0 then Right(NullComponentPolicy.Regularize(value))
      else Left(MultivarError.InvalidRegularization("component ridge", ridge, "must be strictly positive"))

enum SupplementaryCentering:
  case ArithmeticMean
  case RowMeasureMean

enum SupplementaryConvention:
  case MultivariousCovarianceScaled(nullPolicy: NullComponentPolicy)
  case MetricLeastSquares(
      measure: ValueIdentity,
      centering: SupplementaryCentering,
      nullPolicy: NullComponentPolicy
  )

final case class SupplementaryProjectionProvenance(
    supplementaryTable: ValueIdentity,
    fittedScores: ValueIdentity,
    fittedRows: ValueIdentity,
    sourceComponents: Vector[Int],
    convention: SupplementaryConvention,
    semantic: SemanticProvenance
)

/** Supplementary variables aligned into the exact fitted training-row order. */
final class SupplementaryTable[
    Rows <: SemanticSpace,
    Variables <: SemanticSpace
] private[multivar] (
    val rowSpace: SpaceEvidence[Rows],
    val rowSchema: RowSchema,
    val featureSpace: SpaceEvidence[Variables],
    val featureSchema: FeatureSchema,
    val alignment: Vector[Int],
    val values: MatrixView,
    val valueIdentity: ValueIdentity,
    val provenance: SemanticProvenance
)(
    val table: OpTable[Rows, Variables, UncheckedEvidence]
)

/** A typed variable-to-component frame, never a bare loading matrix. */
final class SupplementaryFrame[
    Rows <: SemanticSpace,
    Variables <: SemanticSpace,
    Components <: SemanticSpace
] private[multivar] (
    val table: SupplementaryTable[Rows, Variables],
    val componentSpace: SpaceEvidence[Components],
    val sourceComponents: IndexSet,
    val convention: SupplementaryConvention,
    val projectionProvenance: SupplementaryProjectionProvenance,
    val coefficients: DMat
)(
    val frame: FunctionalFrame[Variables, Components, UncheckedEvidence]
)

/** Supplementary projection capability bound to one fitted score operator. */
final class SupplementaryProjector[
    Rows <: SemanticSpace,
    Components <: SemanticSpace
] private (
    val trainingRows: SpaceEvidence[Rows],
    val rowSchema: RowSchema,
    val components: SpaceEvidence[Components],
    val diagnostics: TransformDiagnostics,
    val provenance: SemanticProvenance
)(
    val trainingScores: Op[Primal[Components], Primal[Rows], ScoreOperatorRole, UncheckedEvidence]
):
  def bind(
      values: MatrixView,
      variableIds: Vector[FeatureId],
      tableId: ValueId
  ): Either[MultivarError, SupplementaryTable[Rows, ? <: SemanticSpace]] =
    rowSchema.mode match
      case RowIdentityMode.Positional => bind(values, rowSchema, variableIds, tableId)
      case RowIdentityMode.Named =>
        Left(
          MultivarError.RowIdentityMismatch(
            "named supplementary rows must supply their row schema explicitly"
          )
        )

  def bind(
      values: MatrixView,
      suppliedRows: RowSchema,
      variableIds: Vector[FeatureId],
      tableId: ValueId
  ): Either[MultivarError, SupplementaryTable[Rows, ? <: SemanticSpace]] =
    for
      permutation <- alignmentPermutation(suppliedRows)
      _ <-
        if values.rows == suppliedRows.identities.length then Right(())
        else
          Left(
            MultivarError.MatrixShapeMismatch(
              s"supplementary table has ${values.rows} rows but supplied row schema has ${suppliedRows.identities.length}"
            )
          )
      aligned <-
        if permutation.indices.forall(index => permutation(index) == index) then Right(values)
        else
          IndexSet
            .from(permutation, IndexAxis.Row, Some(values.rows))
            .flatMap(values.selectRows)
      variableSpace <- SpaceRef.of(s"${tableId.value}.features", SpaceRole.Observed, values.cols)
      schema <- FeatureSchema.from(
        variableSpace.descriptor,
        variableIds,
        ValueIdentity.derived("supplementary-feature-schema", ValueIdentity.source(tableId))
      )
      tableIdentity = ValueIdentity.source(tableId)
      tableProvenance = provenance.append(
        SemanticProvenanceEvent.Derived(
          "align-supplementary-table",
          Vector(tableIdentity, trainingScores.valueIdentity)
        )
      )
      operator <- supplementarySemantic(
        Op.fromMatrixView(
          aligned,
          CoordinateEvidence.dual(variableSpace.evidence),
          CoordinateEvidence.primal(trainingRows),
          OperatorRoleWitness.table,
          tableIdentity,
          tableProvenance
        )
      )
    yield
      new SupplementaryTable(
        trainingRows,
        rowSchema,
        variableSpace.evidence,
        schema,
        permutation,
        aligned,
        tableIdentity,
        tableProvenance
      )(operator)

  def multivarious[Variables <: SemanticSpace](
      table: SupplementaryTable[Rows, Variables],
      nullPolicy: NullComponentPolicy = NullComponentPolicy.Reject(ComponentTolerance.default)
  ): Either[MultivarError, SupplementaryFrame[Rows, Variables, ? <: SemanticSpace]] =
    if table.values.rows <= 1 then
      Left(MultivarError.InsufficientRows("supplementary-variable projection", 2, table.values.rows))
    else diagnostics.spectrum match
      case None => Left(MultivarError.SolverFailed("multivarious supplementary projection requires fitted component scales"))
      case Some(scales) =>
        for
          scores <- supplementarySemantic(trainingScores.toDense)
          strengths = DVec.fromSeq(Vector.tabulate(scales.length)(index => scales(index) * scales(index)))
          selection <- selectComponents(strengths, nullPolicy)
          selectedScores = GaleNumerics.selectColumns(scores, selection.indices)
          centered <- center(table.values, SupplementaryCentering.ArithmeticMean, None)
          cross <- centered.transposeMultiply(MatrixView.dense(selectedScores))
          coefficients = scaleCompatibility(cross, scales, selection, table.values.rows, nullPolicy)
          convention = SupplementaryConvention.MultivariousCovarianceScaled(nullPolicy)
          result <- buildFrame(table, coefficients, selection, convention)
        yield result

  def metricLeastSquares[Variables <: SemanticSpace](
      table: SupplementaryTable[Rows, Variables],
      measure: RowMeasure[Rows],
      centering: SupplementaryCentering,
      nullPolicy: NullComponentPolicy
  ): Either[MultivarError, SupplementaryFrame[Rows, Variables, ? <: SemanticSpace]] =
    if table.values.rows <= 1 then
      Left(MultivarError.InsufficientRows("supplementary-variable projection", 2, table.values.rows))
    else if measure.space.descriptor != trainingRows.descriptor then
      Left(MultivarError.RowIdentityMismatch("supplementary row measure belongs to a foreign training-row space"))
    else
      for
        scores <- supplementarySemantic(trainingScores.toDense)
        weightedScores = MatrixView.scaleRows(scores, measure.weights)
        strengths = diagonalOf(GaleNumerics.transposeMultiply(scores, weightedScores))
        selection <- selectComponents(strengths, nullPolicy)
        selectedScores = GaleNumerics.selectColumns(scores, selection.indices)
        selectedWeightedScores = GaleNumerics.selectColumns(weightedScores, selection.indices)
        centered <- center(table.values, centering, Some(measure))
        cross <- centered.transposeMultiply(MatrixView.dense(selectedWeightedScores))
        gram = GaleNumerics.transposeMultiply(selectedScores, selectedWeightedScores)
        regularized = MatrixOps.addRidge(gram, ridgeOf(nullPolicy))
        factor <- LinalgErrorAdapter.adapt(regularized.cholesky(CholeskyOptions()))
        transposed <- LinalgErrorAdapter.adapt(factor.solve(cross.transpose))
        coefficients = transposed.transpose
        convention = SupplementaryConvention.MetricLeastSquares(
          measure.descriptor.valueIdentity,
          centering,
          nullPolicy
        )
        result <- buildFrame(table, coefficients, selection, convention)
      yield result

  private def alignmentPermutation(supplied: RowSchema): Either[MultivarError, Vector[Int]] =
    if supplied.space != rowSchema.space then
      Left(
        MultivarError.RowIdentityMismatch(
          s"supplementary rows belong to '${supplied.space.id.value}', expected '${rowSchema.space.id.value}'"
        )
      )
    else if supplied.valueIdentity != rowSchema.valueIdentity then
      Left(MultivarError.RowIdentityMismatch("supplementary rows belong to a foreign fitted row schema"))
    else if supplied.identities.toSet != rowSchema.identities.toSet then
      Left(MultivarError.RowIdentityMismatch("supplementary row identities do not match the fitted training rows"))
    else if rowSchema.mode == RowIdentityMode.Positional && supplied.identities != rowSchema.identities then
      Left(MultivarError.RowIdentityMismatch("positional training rows cannot be permuted"))
    else Right(rowSchema.identities.map(supplied.identities.indexOf))

  private def center(
      values: MatrixView,
      policy: SupplementaryCentering,
      measure: Option[RowMeasure[Rows]]
  ): Either[MultivarError, MatrixView] =
    val means =
      policy match
        case SupplementaryCentering.ArithmeticMean => values.columnStats.flatMap(_.means)
        case SupplementaryCentering.RowMeasureMean =>
          measure match
            case None => Left(MultivarError.InvalidMap("row-measure centering requires a row measure"))
            case Some(value) => weightedMeans(values, value.weights)
    means.flatMap: fittedMeans =>
      MatrixView.affine(
        values,
        MatrixView.ones(values.cols),
        MatrixView.negate(fittedMeans),
        StoragePolicy.Operator,
        "supplementary-variable centering"
      )

  private def weightedMeans(values: MatrixView, weights: DVec): Either[MultivarError, DVec] =
    val output = MutableDVec.zeros(values.cols)
    values.transposeMultiplyVector(weights, output).map: _ =>
      val result = Array.ofDim[Double](values.cols)
      var index = 0
      while index < values.cols do
        result(index) = output(index)
        index += 1
      GaleNumerics.vectorFromArray(result)

  private def selectComponents(
      strengths: DVec,
      policy: NullComponentPolicy
  ): Either[MultivarError, IndexSet] =
    policy match
      case NullComponentPolicy.Regularize(ridge) =>
        val invalid = firstNonFinite(strengths)
        if invalid.nonEmpty then
          val index = invalid.get
          Left(MultivarError.NonFiniteValue("supplementary component strength", index, strengths(index)))
        else if ridge.value <= 0.0 then
          Left(MultivarError.InvalidRegularization("component ridge", ridge.value, "must be strictly positive"))
        else IndexSet.contiguous(strengths.length, IndexAxis.Component)
      case NullComponentPolicy.Reject(tolerance) =>
        val invalid = firstNull(strengths, tolerance)
        invalid match
          case Some(index) =>
            Left(
              MultivarError.NonInvertibleValue(
                "supplementary component strength",
                index,
                strengths(index)
              )
            )
          case None => IndexSet.contiguous(strengths.length, IndexAxis.Component)
      case NullComponentPolicy.Drop(tolerance) =>
        val kept = Vector.tabulate(strengths.length)(identity).filter(index => strengths(index) > tolerance.value)
        if kept.isEmpty then Left(MultivarError.SolverFailed("supplementary projection dropped every null component"))
        else IndexSet.from(kept, IndexAxis.Component, Some(strengths.length))

  private def firstNull(strengths: DVec, tolerance: ComponentTolerance): Option[Int] =
    var index = 0
    var invalid = Option.empty[Int]
    while index < strengths.length && invalid.isEmpty do
      if !strengths(index).isFinite || strengths(index) <= tolerance.value then invalid = Some(index)
      index += 1
    invalid

  private def firstNonFinite(strengths: DVec): Option[Int] =
    var index = 0
    var invalid = Option.empty[Int]
    while index < strengths.length && invalid.isEmpty do
      if !strengths(index).isFinite then invalid = Some(index)
      index += 1
    invalid

  private def scaleCompatibility(
      cross: DMat,
      scales: DVec,
      selection: IndexSet,
      rows: Int,
      policy: NullComponentPolicy
  ): DMat =
    val ridge = ridgeOf(policy)
    val values = cross.copyData
    var row = 0
    while row < cross.rows do
      var local = 0
      while local < cross.cols do
        val source = selection.indices(local)
        val denominator = (rows - 1).toDouble * (scales(source) * scales(source) + ridge)
        values(row * cross.cols + local) /= denominator
        local += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(cross.rows, cross.cols, values)

  private def ridgeOf(policy: NullComponentPolicy): Double =
    policy match
      case NullComponentPolicy.Regularize(ridge) => ridge.value
      case _                                     => 0.0

  private def diagonalOf(matrix: DMat): DVec =
    DVec.fromSeq(Vector.tabulate(matrix.rows)(index => matrix(index, index)))

  private def buildFrame[Variables <: SemanticSpace](
      table: SupplementaryTable[Rows, Variables],
      coefficients: DMat,
      selection: IndexSet,
      convention: SupplementaryConvention
  ): Either[MultivarError, SupplementaryFrame[Rows, Variables, ? <: SemanticSpace]] =
    for
      effective <- SpaceRef.of(
        s"${components.id.value}.supplementary-${selection.indices.mkString("-")}",
        SpaceRole.Latent,
        selection.length
      )
      frameIdentity = ValueIdentity.derived(
        "supplementary-variable-frame",
        table.valueIdentity,
        trainingScores.valueIdentity
      )
      frameProvenance = (provenance ++ table.provenance).append(
        SemanticProvenanceEvent.Derived(
          "supplementary-variable-frame",
          Vector(table.valueIdentity, trainingScores.valueIdentity)
        )
      )
      operator <- supplementarySemantic(
        Op.fromDense(
          coefficients,
          CoordinateEvidence.primal(effective.evidence),
          CoordinateEvidence.dual(table.featureSpace),
          OperatorRoleWitness.frame,
          frameIdentity,
          frameProvenance
        )
      )
      frame = FunctionalFrame(operator)
    yield
      new SupplementaryFrame(
        table,
        effective.evidence,
        selection,
        convention,
        SupplementaryProjectionProvenance(
          table.valueIdentity,
          trainingScores.valueIdentity,
          rowSchema.valueIdentity,
          selection.indices,
          convention,
          frameProvenance
        ),
        coefficients
      )(frame)

object SupplementaryProjector:
  def from(
      transform: FittedFrameTransform
  ): SupplementaryProjector[transform.trainingRowSpace.Id, transform.componentSpace.Id] =
    new SupplementaryProjector(
      transform.trainingRowSpace.evidence,
      transform.trainingRowSchema,
      transform.componentSpace.evidence,
      transform.diagnostics,
      transform.provenance
    )(transform.trainingScores)

private def supplementarySemantic[A](value: Either[SemanticError, A]): Either[MultivarError, A] =
  value.left.map:
    case SemanticError.MultivarFailure(error)  => error
    case SemanticError.LinearMapFailure(error) => LinalgErrorAdapter.toMultivarError(error)
    case error                                 => MultivarError.InvalidMap(error.message)
