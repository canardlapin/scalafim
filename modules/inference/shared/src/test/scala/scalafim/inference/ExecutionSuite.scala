package scalafim.inference

import gale.linalg.DMat

class ExecutionSuite extends munit.FunSuite:

  private def accepted[A](value: Either[InferenceError, A]): A =
    value.fold(error => fail(error.message), identity)

  test("compiled programs lower to deterministic scheduler-neutral replicate jobs") {
    val rows = accepted(RowCount(12))
    val capacity = accepted(MonteCarloDraws(10))
    val draws = accepted(MonteCarloDraws(4))
    val step = accepted(ComponentIx(2))
    val spec = InferenceSpec.of(
      FitDescriptor.Pca,
      TargetSpec.VarianceRoots,
      NullSpec.PermuteRows,
      ResamplingDesign.exchangeableRows(rows),
      UnitPolicy.SingleAxes,
      MonteCarloPolicy.Fixed(capacity),
      RequestedEvidence.SignificanceOnly,
      RootSeed(991L)
    )
    val program = InferenceCompiler.compile(spec)
    val first = accepted(ProgramLowering.lower(program, step, capacity, draws))
    val second = accepted(ProgramLowering.lower(program, step, capacity, draws))

    assertEquals(first, second)
    assertEquals(first.jobs.map(_.replicate.value), Vector(20, 21, 22, 23))
    assertEquals(first.jobs.map(_.withinStep), Vector(0, 1, 2, 3))
    assertEquals(first.randomAlgorithm, RandomSource.Algorithm)

    def containsRuntimeHandle(value: Any): Boolean =
      value match
        case iterable: Iterable[?] => iterable.exists(containsRuntimeHandle)
        case _: Function0[?]    => true
        case _: Function1[?, ?] => true
        case product: Product   => product.productIterator.exists(containsRuntimeHandle)
        case _ => false

    assert(!containsRuntimeHandle(first))
  }

  test("thin-SVD exact updates match full paired refits at numerical precision") {
    import CrossCovarianceRefit.given

    val data = accepted(PairedMatrixData.from(
      InferenceNumerics.matrixFromRows(Vector(
        Vector(1.0, 0.0, 2.0),
        Vector(0.0, 1.0, 1.0),
        Vector(2.0, 1.0, 0.0),
        Vector(1.0, 3.0, 1.0),
        Vector(3.0, 2.0, 2.0),
        Vector(2.0, 4.0, 3.0)
      )),
      InferenceNumerics.matrixFromRows(Vector(
        Vector(0.0, 2.0),
        Vector(1.0, 0.0),
        Vector(2.0, 1.0),
        Vector(3.0, 2.0),
        Vector(1.0, 4.0),
        Vector(4.0, 3.0)
      ))
    ))
    val observed = accepted(CrossCovarianceRefit.fit(data))
    val permutations = Vector(
      Vector(5, 4, 3, 2, 1, 0),
      Vector(1, 3, 5, 0, 2, 4),
      Vector(2, 0, 4, 1, 5, 3)
    ).map(rows => accepted(RowPermutation.from(rows)))

    permutations.foreach { permutation =>
      val full = accepted(RefitExecutor.full(data, permutation, RootSeed(71L)))
      val exact = accepted(RefitExecutor.exact(data, observed, permutation, RootSeed(71L)))

      assertEquals(full.fit.roots.length, exact.fit.roots.length)
      var i = 0
      while i < full.fit.roots.length do
        assertEqualsDouble(exact.fit.roots(i), full.fit.roots(i), 1e-10)
        i += 1
      assertEquals(full.provenance.path, RefitPath.FullRefit)
      exact.provenance.path match
        case RefitPath.ExactReduced("paired-thin-svd-cross-v1") => ()
        case other => fail(s"unexpected exact path $other")
    }
  }

  test("exact paired core reduces feature dimensions before replicate updates") {
    val data = accepted(PairedMatrixData.from(
      InferenceNumerics.matrixFromRows(Vector.tabulate(20)(row =>
        Vector.tabulate(8)(col => Math.sin((row + 1.0) * (col + 2.0)))
      )),
      InferenceNumerics.matrixFromRows(Vector.tabulate(20)(row =>
        Vector.tabulate(6)(col => Math.cos((row + 3.0) * (col + 1.0)))
      ))
    ))
    val observed = accepted(CrossCovarianceRefit.fit(data))
    val reduction = ExactCrossCovarianceReduction()
    val core = accepted(reduction.core(data, observed))

    assert(core.xRows.cols <= data.x.cols)
    assert(core.yRows.cols <= data.y.cols)
    assertEquals(core.xRows.rows, data.x.rows)
    assertEquals(core.yRows.rows, data.y.rows)
  }

  test("associative reducers tolerate reordered and chunked floating reduction") {
    val values = Vector.tabulate(1001) { index =>
      val large = if index % 2 == 0 then 1e8 else -1e8
      large + index.toDouble / 1000.0
    }
    val sequential = AssociativeReducer.reduce(values, MomentReducer)
    val reversed = AssociativeReducer.reduce(values.reverse, MomentReducer)
    val chunked = AssociativeReducer.reduceChunks(values.grouped(37).toVector, MomentReducer)
    val tolerance = 1e-7

    assertEquals(sequential.count, values.length.toLong)
    assert(Math.abs(sequential.mean - reversed.mean) <= tolerance)
    assert(Math.abs(sequential.mean - chunked.mean) <= tolerance)
    val varianceTolerance = 1e-12 * Math.max(1.0, Math.abs(sequential.variance))
    assert(Math.abs(sequential.variance - reversed.variance) <= varianceTolerance)
    assert(Math.abs(sequential.variance - chunked.variance) <= varianceTolerance)

    val exactSequential = AssociativeReducer.reduce(
      values,
      ExceedanceReducer(0.0, Alternative.Greater)
    )
    val exactChunked = AssociativeReducer.reduceChunks(
      values.grouped(29).toVector.reverse,
      ExceedanceReducer(0.0, Alternative.Greater)
    )
    assertEquals(exactSequential, exactChunked)
  }
