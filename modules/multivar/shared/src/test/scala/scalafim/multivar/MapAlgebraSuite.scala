package scalafim.multivar

import scalafim.linalg.DoubleMatrix

class MapAlgebraSuite extends munit.FunSuite:

  private val observed: MvSpace =
    MvSpace.of("observed", SpaceRole.Observed, 3).toOption.get

  private val latent: MvSpace =
    MvSpace.of("latent", SpaceRole.Latent, 2).toOption.get

  private val oneDim: MvSpace =
    MvSpace.of("one", SpaceRole.Latent, 1).toOption.get

  private val twoDim: MvSpace =
    MvSpace.of("two", SpaceRole.Observed, 2).toOption.get

  /** Exact right pseudo-inverse for single-column weights: pinv(w) = wᵀ / ‖w‖². */
  private val singleColumnSolver: PseudoInverseSolver =
    new PseudoInverseSolver:
      override def rightPseudoInverse(weights: DoubleMatrix): Either[MultivarError, DoubleMatrix] =
        if weights.cols != 1 then Left(MultivarError.SolverFailed("test solver supports single-column weights only"))
        else
          var normSq = 0.0
          var row = 0
          while row < weights.rows do
            normSq += weights(row, 0) * weights(row, 0)
            row += 1
          if normSq <= 0.0 then Left(MultivarError.SolverFailed("zero weight column has no pseudo-inverse"))
          else
            val out = new Array[Double](weights.rows)
            var i = 0
            while i < weights.rows do
              out(i) = weights(i, 0) / normSq
              i += 1
            Right(DoubleMatrix.unsafe(1, weights.rows, out))

  private def data: MatrixView =
    MatrixView.dense(
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 2.0, 3.0),
          Vector(4.0, 5.0, 6.0),
          Vector(7.0, 8.0, 9.0)
        )
      )
    )

  private def pass(cols: Int): FittedPreprocessor =
    FittedColumnAffine(cols, MatrixView.ones(cols), MatrixView.zeros(cols))

  private def assertMatrixClose(actual: DoubleMatrix, expected: Vector[Vector[Double]], tol: Double): Unit =
    assertEquals(actual.rows, expected.length)
    assertEquals(actual.cols, expected.headOption.map(_.length).getOrElse(0))
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row)(col), tol)
        col += 1
      row += 1

  test("IdentityMap preserves input values and can be restricted") {
    val identity = IdentityMap(observed)
    val out = identity.forward(data).toOption.get

    assertMatrixClose(out, data.toDense().toOption.get.toRows, 1e-12)

    val restricted = identity.restrictInput(IndexSet.from(Vector(2, 0), IndexAxis.Feature).toOption.get).toOption.get
    assertEquals(restricted.domain.size, 2)
    assertEquals(restricted.codomain.size, 2)
  }

  test("MatrixMap projection applies fitted preprocessing then weights") {
    val weights = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(1.0, 1.0)
      )
    )
    val preprocessor = PreprocessSpec.Center.fit(data).toOption.get
    val map = MatrixMap.from(observed, latent, weights, preprocessor).toOption.get

    val projected = map.forward(data).toOption.get

    assertMatrixClose(
      projected,
      Vector(
        Vector(-6.0, -6.0),
        Vector(0.0, 0.0),
        Vector(6.0, 6.0)
      ),
      1e-12
    )
  }

  test("restricted MatrixMap commutes with explicit column selection") {
    val weights = DoubleMatrix.fromRows(Vector(Vector(2.0), Vector(3.0), Vector(5.0)))
    val map = MatrixMap.from(observed, oneDim, weights, pass(3)).toOption.get
    val columns = IndexSet.from(Vector(2, 0), IndexAxis.Feature).toOption.get
    val selectedInput = data.selectColumns(columns).toOption.get
    val restricted = map.restrictInput(columns).toOption.get

    assertMatrixClose(
      restricted.forward(selectedInput).toOption.get,
      Vector(Vector(17.0), Vector(38.0), Vector(59.0)),
      1e-12
    )

    val wrapper = RestrictedMap.from(map, columns).toOption.get
    assertEquals(wrapper.domain.size, 2)
    assertMatrixClose(wrapper.forward(selectedInput).toOption.get, restricted.forward(selectedInput).toOption.get.toRows, 1e-12)
  }

  test("composition applies maps in order") {
    val firstWeights = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(1.0, 1.0)
      )
    )
    val secondWeights = DoubleMatrix.fromRows(Vector(Vector(2.0), Vector(-1.0)))
    val first = MatrixMap.from(observed, latent, firstWeights, pass(3)).toOption.get
    val second = MatrixMap.from(latent, oneDim, secondWeights, pass(2)).toOption.get
    val composed = first.andThen(second).toOption.get

    assertMatrixClose(
      composed.forward(data).toOption.get,
      Vector(Vector(3.0), Vector(9.0), Vector(15.0)),
      1e-12
    )
  }

  test("decoder requires an explicit pseudo-inverse capability") {
    given PseudoInverseSolver = PseudoInverseSolver.orthonormalColumns()

    val weights = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 0.0)
      )
    )
    val map = MatrixMap.from(observed, latent, weights, pass(3)).toOption.get
    val decoder = map.decoder.toOption.get
    val scores = MatrixView.dense(DoubleMatrix.fromRows(Vector(Vector(2.0, 3.0))))

    assertMatrixClose(decoder.forward(scores).toOption.get, Vector(Vector(2.0, 3.0, 0.0)), 1e-12)

    val badWeights = DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(1.0), Vector(0.0)))
    val badMap = MatrixMap.from(observed, oneDim, badWeights, pass(3)).toOption.get
    assert(badMap.decoder.swap.toOption.exists(_.message.contains("orthonormal")))
  }

  test("scaled block maps without a finite reciprocal weight have no decoder") {
    given PseudoInverseSolver = singleColumnSolver

    val weights = DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(0.0), Vector(0.0)))
    val map = MatrixMap.from(observed, oneDim, weights, pass(3)).toOption.get

    assert(BlockMap.ScaledMap(map, 0.0).decoder.isLeft)
    BlockMap.ScaledMap(map, Double.MinPositiveValue).decoder match
      case Left(MultivarError.DecoderUnavailable(_)) => ()
      case other => fail(s"expected a subnormal weight to have no decoder, got $other")
  }

  test("non-finite weights are rejected by map constructors and the pseudo-inverse gate") {
    val nanWeights = DoubleMatrix.fromRows(
      Vector(
        Vector(Double.NaN, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 0.0)
      )
    )

    def isNonFinite(result: Either[MultivarError, ?]): Boolean =
      result.swap.toOption.exists {
        case MultivarError.NonFiniteValue(_, _, _) => true
        case _                                     => false
      }

    assert(isNonFinite(MatrixMap.from(observed, latent, nanWeights, pass(3))))
    assert(isNonFinite(LinearMvMap.from(observed, latent, nanWeights)))

    val goodWeights = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 0.0)
      )
    )
    val nanDecoder = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0, Double.NaN),
        Vector(0.0, 1.0, 0.0)
      )
    )
    assert(isNonFinite(LinearMvMap.from(observed, latent, goodWeights, Some(nanDecoder))))

    val gate = PseudoInverseSolver.orthonormalColumns()
    assert(gate.rightPseudoInverse(nanWeights).isLeft, "NaN weights must not be certified orthonormal")
  }

  test("MatrixMap decoder returns raw-domain data when preprocessing centers the input") {
    given PseudoInverseSolver = PseudoInverseSolver.orthonormalColumns()

    val raw = MatrixView.dense(
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 2.0, 5.0),
          Vector(4.0, 3.0, 5.0),
          Vector(7.0, 10.0, 5.0)
        )
      )
    )
    val weights = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 0.0)
      )
    )
    val preprocessor = PreprocessSpec.Center.fit(raw).toOption.get
    val map = MatrixMap.from(observed, latent, weights, preprocessor).toOption.get

    val scores = map.forward(raw).toOption.get
    val decoder = map.decoder.toOption.get
    val reconstructed = decoder.forward(MatrixView.dense(scores)).toOption.get

    assertMatrixClose(reconstructed, raw.toDense().toOption.get.toRows, 1e-12)
  }

  test("LinearMvMap.restrictInput recomputes the decoder instead of column-selecting an explicit one") {
    given PseudoInverseSolver = singleColumnSolver

    val weights = DoubleMatrix.fromRows(Vector(Vector(0.6), Vector(0.8)))
    val explicitDecoder = DoubleMatrix.fromRows(Vector(Vector(0.6, 0.8)))
    val map = LinearMvMap.from(twoDim, oneDim, weights, Some(explicitDecoder)).toOption.get

    val columns = IndexSet.from(Vector(0), IndexAxis.Feature).toOption.get
    val restricted = map.restrictInput(columns).toOption.get
    val input = MatrixView.dense(DoubleMatrix.fromRows(Vector(Vector(2.0), Vector(-3.0))))

    val scores = restricted.forward(input).toOption.get
    val decoder = restricted.decoder.toOption.get
    val roundTrip = decoder.forward(MatrixView.dense(scores)).toOption.get

    assertMatrixClose(roundTrip, Vector(Vector(2.0), Vector(-3.0)), 1e-12)
  }

  test("ComposedMap.decoder inverts the composition for orthonormal weights") {
    given PseudoInverseSolver = PseudoInverseSolver.orthonormalColumns()

    val latentOut = MvSpace.of("latentOut", SpaceRole.Latent, 2).toOption.get
    val firstWeights = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 0.0)
      )
    )
    val secondWeights = DoubleMatrix.fromRows(
      Vector(
        Vector(0.0, 1.0),
        Vector(-1.0, 0.0)
      )
    )
    val first = MatrixMap.from(observed, latent, firstWeights, pass(3)).toOption.get
    val second = MatrixMap.from(latent, latentOut, secondWeights, pass(2)).toOption.get
    val composed = first.andThen(second).toOption.get

    val input = MatrixView.dense(
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 2.0, 0.0),
          Vector(-3.0, 4.0, 0.0)
        )
      )
    )
    val scores = composed.forward(input).toOption.get
    val decoder = composed.decoder.toOption.get
    assertEquals(decoder.domain, composed.codomain)
    assertEquals(decoder.codomain, composed.domain)

    val roundTrip = decoder.forward(MatrixView.dense(scores)).toOption.get
    assertMatrixClose(roundTrip, Vector(Vector(1.0, 2.0, 0.0), Vector(-3.0, 4.0, 0.0)), 1e-12)
  }

  test("ComposedMap.from rejects maps with non-composable spaces") {
    val firstWeights = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 0.0)
      )
    )
    val first = MatrixMap.from(observed, latent, firstWeights, pass(3)).toOption.get
    val second = MatrixMap.from(oneDim, oneDim, DoubleMatrix.fromRows(Vector(Vector(1.0))), pass(1)).toOption.get

    first.andThen(second) match
      case Left(MultivarError.NonComposableMaps(left, right)) =>
        assertEquals(left, latent)
        assertEquals(right, oneDim)
      case other =>
        fail(s"expected NonComposableMaps, got $other")
  }

  test("LinearMvMap projects with raw weights and round-trips through its decoder") {
    given PseudoInverseSolver = PseudoInverseSolver.orthonormalColumns()

    val weights = DoubleMatrix.fromRows(Vector(Vector(0.6), Vector(0.8)))
    val map = LinearMvMap.from(twoDim, oneDim, weights).toOption.get
    val input = MatrixView.dense(
      DoubleMatrix.fromRows(
        Vector(
          Vector(0.6, 0.8),
          Vector(-1.2, -1.6)
        )
      )
    )

    val scores = map.forward(input).toOption.get
    assertEquals(scores.rows, 2)
    assertEquals(scores.cols, 1)
    assertMatrixClose(scores, Vector(Vector(1.0), Vector(-2.0)), 1e-12)

    val decoder = map.decoder.toOption.get
    assertMatrixClose(
      decoder.forward(MatrixView.dense(scores)).toOption.get,
      Vector(Vector(0.6, 0.8), Vector(-1.2, -1.6)),
      1e-12
    )

    val explicitDecoder = DoubleMatrix.fromRows(Vector(Vector(1.0, 0.0)))
    val explicitMap = LinearMvMap.from(twoDim, oneDim, weights, Some(explicitDecoder)).toOption.get
    assertMatrixClose(
      explicitMap.decoder.toOption.get.forward(MatrixView.dense(scores)).toOption.get,
      Vector(Vector(1.0, 0.0), Vector(-2.0, 0.0)),
      1e-12
    )
  }

  test("Projector and BiProjection preserve the map projection contract") {
    val weights = DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(0.0), Vector(-1.0)))
    val map = MatrixMap.from(observed, oneDim, weights, pass(3)).toOption.get
    val projector = Projector(map)
    val scores = projector.project(data).toOption.get
    val fit = BiProjection(map, scores)

    assertMatrixClose(scores, Vector(Vector(-2.0), Vector(-2.0), Vector(-2.0)), 1e-12)
    assertMatrixClose(fit.project(data).toOption.get, scores.toRows, 1e-12)
  }

