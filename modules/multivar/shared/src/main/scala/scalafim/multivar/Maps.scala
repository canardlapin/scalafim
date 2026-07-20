package scalafim.multivar

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

trait PseudoInverseSolver:
  def rightPseudoInverse(weights: DoubleMatrix): Either[MultivarError, DoubleMatrix]

object PseudoInverseSolver:
  def orthonormalColumns(tolerance: Double = 1e-10): PseudoInverseSolver =
    new PseudoInverseSolver:
      override def rightPseudoInverse(weights: DoubleMatrix): Either[MultivarError, DoubleMatrix] =
        val gram = DoubleMatrix.crossProduct(weights)
        var row = 0
        var error = Option.empty[MultivarError]
        while row < gram.rows && error.isEmpty do
          var col = 0
          while col < gram.cols && error.isEmpty do
            val expected = if row == col then 1.0 else 0.0
            if !(Math.abs(gram(row, col) - expected) <= tolerance) then
              error = Some(MultivarError.DecoderUnavailable("weights do not have orthonormal columns"))
            col += 1
          row += 1
        error match
          case Some(value) => Left(value)
          case None        => Right(weights.transpose)

trait MvMap:
  def domain: MvSpace
  def codomain: MvSpace

  def forward(input: MatrixView): Either[MultivarError, DoubleMatrix]

  def restrictInput(columns: IndexSet): Either[MultivarError, MvMap]

  def decoder(using solver: PseudoInverseSolver): Either[MultivarError, Decoder]

  def andThen(next: MvMap): Either[MultivarError, MvMap] =
    ComposedMap.from(this, next)

object MvMap:
  private[multivar] def requireFiniteWeights(role: String, weights: DoubleMatrix): Either[MultivarError, Unit] =
    val data = weights.dataArray
    var i = 0
    var error = Option.empty[MultivarError]
    while i < data.length && error.isEmpty do
      val value = data(i)
      if !value.isFinite then error = Some(MultivarError.NonFiniteValue(role, i, value))
      i += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(())

/** Linear decode step plus an optional inverse of the fitted preprocessing, so decoding
  * always lands in the raw codomain space rather than in preprocessed coordinates.
  */
final case class Decoder(
    domain: MvSpace,
    codomain: MvSpace,
    weights: DoubleMatrix,
    inversePreprocessor: Option[FittedPreprocessor] = None
):
  require(weights.rows == domain.size, "decoder weight rows must match decoder domain")
  require(weights.cols == codomain.size, "decoder weight columns must match decoder codomain")
  inversePreprocessor.foreach { preprocessor =>
    require(preprocessor.inputCols == codomain.size, "decoder inverse preprocessor must match decoder codomain")
  }

  def forward(input: MatrixView): Either[MultivarError, DoubleMatrix] =
    if input.cols != domain.size then
      Left(MultivarError.MatrixShapeMismatch(s"decoder expected ${domain.size} input columns, got ${input.cols}"))
    else
      inversePreprocessor match
        case None => input.rightMultiply(weights)
        case Some(preprocessor) =>
          for
            projected <- input.rightMultiply(weights)
            raw <- preprocessor.inverseTransform(MatrixView.dense(projected))
            dense <- raw.toDense(StoragePolicy.AllowDense)
          yield dense

object Decoder:
  /** Drops preprocessing steps that are exact identities, so identity-preprocessed maps
    * keep plain linear decoders (and stay foldable under composition).
    */
  private[multivar] def inverseStep(preprocessor: FittedPreprocessor): Option[FittedPreprocessor] =
    preprocessor match
      case affine: FittedColumnAffine if isIdentity(affine) => None
      case other                                            => Some(other)

  private def isIdentity(affine: FittedColumnAffine): Boolean =
    var col = 0
    var identity = true
    while identity && col < affine.inputCols do
      identity = affine.scale(col) == 1.0 && affine.shift(col) == 0.0
      col += 1
    identity

final case class IdentityMap(space: MvSpace) extends MvMap:
  override def domain: MvSpace =
    space

  override def codomain: MvSpace =
    space

  override def forward(input: MatrixView): Either[MultivarError, DoubleMatrix] =
    if input.cols != domain.size then
      Left(MultivarError.MatrixShapeMismatch(s"identity map expected ${domain.size} columns, got ${input.cols}"))
    else input.toDense(StoragePolicy.AllowDense)

  override def restrictInput(columns: IndexSet): Either[MultivarError, MvMap] =
    MatrixView.requireColumnIndexSet(columns, domain.size).map { checked =>
      val restrictedSpace = MvSpace(
        SpaceId.unsafe(s"${space.id.value}.restricted"),
        space.role,
        Dimension.unsafe(checked.length)
      )
      IdentityMap(restrictedSpace)
    }

  override def decoder(using solver: PseudoInverseSolver): Either[MultivarError, Decoder] =
    Right(Decoder(space, space, DoubleMatrix.eye(space.size)))

final case class MatrixMap private (
    domain: MvSpace,
    codomain: MvSpace,
    weights: DoubleMatrix,
    preprocessor: FittedPreprocessor
) extends MvMap:
  override def forward(input: MatrixView): Either[MultivarError, DoubleMatrix] =
    for
      transformed <- preprocessor.transform(input)
      projected <- transformed.rightMultiply(weights)
    yield projected

  override def restrictInput(columns: IndexSet): Either[MultivarError, MvMap] =
    for
      checked <- MatrixView.requireColumnIndexSet(columns, domain.size)
      restrictedPreprocessor <- preprocessor.restrict(checked)
      restrictedDomain = MvSpace(
        SpaceId.unsafe(s"${domain.id.value}.restricted"),
        domain.role,
        Dimension.unsafe(checked.length)
      )
      restrictedWeights = weights.selectRows(checked.indices)
      restricted <- MatrixMap.from(restrictedDomain, codomain, restrictedWeights, restrictedPreprocessor)
    yield restricted

  override def decoder(using solver: PseudoInverseSolver): Either[MultivarError, Decoder] =
    solver.rightPseudoInverse(weights).map(Decoder(codomain, domain, _, Decoder.inverseStep(preprocessor)))

object MatrixMap:
  def from(
      domain: MvSpace,
      codomain: MvSpace,
      weights: DoubleMatrix,
      preprocessor: FittedPreprocessor
  ): Either[MultivarError, MatrixMap] =
    if weights.rows != domain.size || weights.cols != codomain.size then
      Left(
        MultivarError.InvalidMap(
          s"matrix map weights ${weights.rows}x${weights.cols} do not match ${domain.size}x${codomain.size}"
        )
      )
    else if preprocessor.inputCols != domain.size then
      Left(
        MultivarError.InvalidMap(
          s"preprocessor expects ${preprocessor.inputCols} columns but domain has ${domain.size}"
        )
      )
    else
      MvMap
        .requireFiniteWeights("matrix map weight", weights)
        .map(_ => MatrixMap(domain, codomain, weights, preprocessor))

final case class LinearMvMap private (
    domain: MvSpace,
    codomain: MvSpace,
    weights: DoubleMatrix,
    decoderWeights: Option[DoubleMatrix]
) extends MvMap:
  require(weights.rows == domain.size, "linear map weight rows must match domain")
  require(weights.cols == codomain.size, "linear map weight columns must match codomain")
  decoderWeights.foreach { value =>
    require(value.rows == codomain.size, "decoder weight rows must match codomain")
    require(value.cols == domain.size, "decoder weight columns must match domain")
  }

  override def forward(input: MatrixView): Either[MultivarError, DoubleMatrix] =
    if input.cols != domain.size then
      Left(MultivarError.MatrixShapeMismatch(s"linear map expected ${domain.size} input columns, got ${input.cols}"))
    else input.rightMultiply(weights)

  /** Restriction drops any explicit decoder: the pseudo-inverse of a row-restricted weight
    * matrix is not the column restriction of the original decoder, so the restricted map
    * recomputes its decoder through the solver, same as a map built without one.
    */
  override def restrictInput(columns: IndexSet): Either[MultivarError, MvMap] =
    for
      checked <- MatrixView.requireColumnIndexSet(columns, domain.size)
      restrictedDomain = MvSpace(
        SpaceId.unsafe(s"${domain.id.value}.restricted"),
        domain.role,
        Dimension.unsafe(checked.length)
      )
      restrictedWeights = weights.selectRows(checked.indices)
      restricted <- LinearMvMap.from(restrictedDomain, codomain, restrictedWeights)
    yield restricted

  override def decoder(using solver: PseudoInverseSolver): Either[MultivarError, Decoder] =
    decoderWeights match
      case Some(value) => Right(Decoder(codomain, domain, value))
      case None        => solver.rightPseudoInverse(weights).map(Decoder(codomain, domain, _))

object LinearMvMap:
  def from(
      domain: MvSpace,
      codomain: MvSpace,
      weights: DoubleMatrix,
      decoderWeights: Option[DoubleMatrix] = None
  ): Either[MultivarError, LinearMvMap] =
    if weights.rows != domain.size || weights.cols != codomain.size then
      Left(
        MultivarError.InvalidMap(
          s"linear map weights ${weights.rows}x${weights.cols} do not match ${domain.size}x${codomain.size}"
        )
      )
    else
      decoderWeights match
        case Some(value) if value.rows != codomain.size || value.cols != domain.size =>
          Left(
            MultivarError.InvalidMap(
              s"linear decoder weights ${value.rows}x${value.cols} do not match ${codomain.size}x${domain.size}"
            )
          )
        case _ =>
          for
            _ <- MvMap.requireFiniteWeights("linear map weight", weights)
            _ <- decoderWeights match
              case Some(value) => MvMap.requireFiniteWeights("linear decoder weight", value)
              case None        => Right(())
          yield LinearMvMap(domain, codomain, weights, decoderWeights)

  private[multivar] def unsafe(
      domain: MvSpace,
      codomain: MvSpace,
      weights: DoubleMatrix,
      decoderWeights: Option[DoubleMatrix] = None
  ): LinearMvMap =
    from(domain, codomain, weights, decoderWeights).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class ComposedMap private (first: MvMap, second: MvMap) extends MvMap:
  override def domain: MvSpace =
    first.domain

  override def codomain: MvSpace =
    second.codomain

  override def forward(input: MatrixView): Either[MultivarError, DoubleMatrix] =
    for
      middle <- first.forward(input)
      out <- second.forward(MatrixView.dense(middle))
    yield out

  override def restrictInput(columns: IndexSet): Either[MultivarError, MvMap] =
    first.restrictInput(columns).flatMap(ComposedMap.from(_, second))

  override def decoder(using solver: PseudoInverseSolver): Either[MultivarError, Decoder] =
    for
      secondDecoder <- second.decoder
      firstDecoder <- first.decoder
      _ <-
        if secondDecoder.inversePreprocessor.isEmpty then Right(())
        else
          Left(
            MultivarError.DecoderUnavailable(
              "composed decoder cannot fold an inverse-preprocessing step in the intermediate space"
            )
          )
      composedWeights = DoubleMatrix.multiply(secondDecoder.weights, firstDecoder.weights)
    yield Decoder(codomain, domain, composedWeights, firstDecoder.inversePreprocessor)

object ComposedMap:
  def from(first: MvMap, second: MvMap): Either[MultivarError, ComposedMap] =
    if first.codomain.size != second.domain.size then Left(MultivarError.NonComposableMaps(first.codomain, second.domain))
    else Right(ComposedMap(first, second))

final case class RestrictedMap private (
    base: MvMap,
    columns: IndexSet,
    restricted: MvMap
) extends MvMap:
  override def domain: MvSpace =
    restricted.domain

  override def codomain: MvSpace =
    restricted.codomain

  override def forward(input: MatrixView): Either[MultivarError, DoubleMatrix] =
    restricted.forward(input)

  override def restrictInput(nextColumns: IndexSet): Either[MultivarError, MvMap] =
    restricted.restrictInput(nextColumns)

  override def decoder(using solver: PseudoInverseSolver): Either[MultivarError, Decoder] =
    restricted.decoder

object RestrictedMap:
  def from(base: MvMap, columns: IndexSet): Either[MultivarError, RestrictedMap] =
    base.restrictInput(columns).map(restricted => RestrictedMap(base, columns, restricted))

final case class Projector(map: MvMap):
  def project(input: MatrixView): Either[MultivarError, DoubleMatrix] =
    map.forward(input)

  def decoder(using solver: PseudoInverseSolver): Either[MultivarError, Decoder] =
    map.decoder

final case class ComponentScale(values: DoubleVector):
  require(values.length > 0, "component scale must be non-empty")

final case class ProjectionDiagnostics(
    method: String,
    components: ComponentCount,
    effectiveComponents: Int,
    singularValues: Option[DoubleVector] = None,
    eigenValues: Option[DoubleVector] = None,
    backend: Option[String] = None,
    storagePolicy: Option[StoragePolicy] = None,
    tolerance: Option[Double] = None
)

final case class BiProjection(
    map: MvMap,
    scores: DoubleMatrix,
    scale: Option[ComponentScale] = None,
    diagnostics: Option[ProjectionDiagnostics] = None
):
  require(scores.cols == map.codomain.size, "score columns must match latent codomain")

  def project(input: MatrixView): Either[MultivarError, DoubleMatrix] =
    map.forward(input)

final case class CrossProjection(
    x: MvMap,
    y: MvMap,
    latent: MvSpace,
    xScores: DoubleMatrix,
    yScores: DoubleMatrix,
    scale: Option[ComponentScale] = None,
    diagnostics: Option[ProjectionDiagnostics] = None
):
  require(x.codomain == latent, "x map codomain must match latent space")
  require(y.codomain == latent, "y map codomain must match latent space")
  require(xScores.cols == latent.size, "x score columns must match latent space")
  require(yScores.cols == latent.size, "y score columns must match latent space")

  def projectX(input: MatrixView): Either[MultivarError, DoubleMatrix] =
    x.forward(input)

  def projectY(input: MatrixView): Either[MultivarError, DoubleMatrix] =
    y.forward(input)

  def transfer(
      source: DomainSide,
      target: DomainSide,
      input: MatrixView
  )(using solver: PseudoInverseSolver): Either[MultivarError, DoubleMatrix] =
    val sourceMap = mapFor(source)
    val targetMap = mapFor(target)
    for
      scores <- sourceMap.forward(input)
      targetDecoder <- targetMap.decoder
      transferred <- targetDecoder.forward(MatrixView.dense(scores))
    yield transferred

  private def mapFor(side: DomainSide): MvMap =
    side match
      case DomainSide.X => x
      case DomainSide.Y => y

enum DomainSide:
  case X
  case Y
