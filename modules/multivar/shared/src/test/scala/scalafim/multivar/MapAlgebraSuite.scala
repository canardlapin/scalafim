package scalafim.multivar

import scalafim.linalg.DoubleMatrix

class MapAlgebraSuite extends munit.FunSuite:

  private val observed: MvSpace =
    MvSpace.of("observed", SpaceRole.Observed, 3).toOption.get

  private val latent: MvSpace =
    MvSpace.of("latent", SpaceRole.Latent, 2).toOption.get

  private val oneDim: MvSpace =
    MvSpace.of("one", SpaceRole.Latent, 1).toOption.get

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

  test("composition applies maps in order and preserves decoder order") {
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

  test("Projector and BiProjection preserve the map projection contract") {
    val weights = DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(0.0), Vector(-1.0)))
    val map = MatrixMap.from(observed, oneDim, weights, pass(3)).toOption.get
    val projector = Projector(map)
    val scores = projector.project(data).toOption.get
    val fit = BiProjection(map, scores)

    assertMatrixClose(scores, Vector(Vector(-2.0), Vector(-2.0), Vector(-2.0)), 1e-12)
    assertMatrixClose(fit.project(data).toOption.get, scores.toRows, 1e-12)
  }

