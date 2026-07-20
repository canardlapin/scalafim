package scalafim.spatial

private[spatial] enum LazyContractError:
  case UnsupportedRoute(source: String, target: String)
  case InverseQualityRejected(target: String)
  case DataUnavailable(root: String)
  case InvalidDemand(indices: Vector[Int])

private[spatial] final case class LazyContractMetrics(
  sourceReads: Int,
  compilations: Int,
  spatialResamplings: Int,
  resultCacheWrites: Int
)

private[spatial] final case class LazyContractProvenance(
  root: String,
  requestedSteps: Vector[String],
  route: Vector[String],
  demand: Option[Vector[Int]],
  compiler: String,
  fusedSpatially: Boolean
)

private[spatial] final case class LazyContractEvaluation(
  values: Vector[Double],
  sourceRows: Vector[Int],
  cacheKey: String,
  provenance: LazyContractProvenance
)

private[spatial] trait SpatialLazyContractAdapter:
  type View

  def root: View
  def unavailableRoot: View
  def rootId(view: View): String
  def domain(view: View): String
  def requestedSteps(view: View): Vector[String]
  def demand(view: View): Option[Vector[Int]]
  def to(view: View, target: String): Either[LazyContractError, View]
  def rows(view: View, indices: Vector[Int]): Either[LazyContractError, View]
  def withSampling(view: View, sampling: String): View
  def withInversePolicy(view: View, policy: String): View
  def executionKey(view: View): Either[LazyContractError, String]
  def value(view: View): Either[LazyContractError, LazyContractEvaluation]
  def materialize(view: View): Either[LazyContractError, View]
  def metrics: LazyContractMetrics

private[spatial] trait SpatialLazyContractLaws:
  self: munit.FunSuite =>

  protected def contractAdapter(): SpatialLazyContractAdapter

  protected final def registerSpatialLazyContractLaws(): Unit =
    test("lazy contract: descriptive operations preserve one root and perform no work"):
      val adapter = contractAdapter()
      val root = adapter.root
      val mid = contractValue(adapter.to(root, "mid"))
      val target = contractValue(adapter.to(mid, "target"))
      val selected = contractValue(adapter.rows(target, Vector(2, 0)))

      assertEquals(adapter.rootId(mid), adapter.rootId(root))
      assertEquals(adapter.rootId(target), adapter.rootId(root))
      assertEquals(adapter.rootId(selected), adapter.rootId(root))
      assertEquals(adapter.domain(selected), "target")
      assertEquals(
        adapter.requestedSteps(selected),
        Vector("to:mid", "to:target", "rows:2,0")
      )
      assertEquals(adapter.demand(selected), Some(Vector(2, 0)))
      assertEquals(adapter.metrics, LazyContractMetrics(0, 0, 0, 0))

    test("lazy contract: chained and direct views share one fused execution"):
      val adapter = contractAdapter()
      val root = adapter.root
      val chained = contractValue(adapter.to(contractValue(adapter.to(root, "mid")), "target"))
      val direct = contractValue(adapter.to(root, "target"))

      assertEquals(contractValue(adapter.executionKey(chained)), contractValue(adapter.executionKey(direct)))

      val chainedEvaluation = contractValue(adapter.value(chained))
      val directEvaluation = contractValue(adapter.value(direct))

      assertDoubleVector(chainedEvaluation.values, Vector(8.0, 0.0, 4.0, 0.0))
      assertDoubleVector(directEvaluation.values, chainedEvaluation.values)
      assertEquals(adapter.metrics, LazyContractMetrics(1, 1, 1, 1))
      assert(chainedEvaluation.provenance.fusedSpatially)
      assertEquals(chainedEvaluation.provenance.route, Vector("root-to-mid", "mid-to-target"))

    test("lazy contract: demanded values equal selecting a full evaluation"):
      val adapter = contractAdapter()
      val target = contractValue(adapter.to(adapter.root, "target"))
      val selected = contractValue(adapter.rows(target, Vector(2, 0)))

      val fullEvaluation = contractValue(adapter.value(target))
      val selectedEvaluation = contractValue(adapter.value(selected))

      assertDoubleVector(
        selectedEvaluation.values,
        Vector(fullEvaluation.values(2), fullEvaluation.values(0))
      )
      assertEquals(selectedEvaluation.sourceRows, Vector(1, 3))
      assert(selectedEvaluation.sourceRows.length < fullEvaluation.sourceRows.length)

    test("lazy contract: cache identity includes every result-changing policy"):
      val adapter = contractAdapter()
      val root = adapter.root
      val chained = contractValue(adapter.to(contractValue(adapter.to(root, "mid")), "target"))
      val direct = contractValue(adapter.to(root, "target"))
      val selected = contractValue(adapter.rows(direct, Vector(0, 2)))
      val nearest = adapter.withSampling(direct, "nearest")
      val approximate = adapter.withInversePolicy(direct, "allow-approximate")

      val directKey = contractValue(adapter.executionKey(direct))
      assertEquals(contractValue(adapter.executionKey(chained)), directKey)
      assertNotEquals(contractValue(adapter.executionKey(selected)), directKey)
      assertNotEquals(contractValue(adapter.executionKey(nearest)), directKey)
      assertNotEquals(contractValue(adapter.executionKey(approximate)), directKey)

    test("lazy contract: materialization establishes a new root and clears the plan"):
      val adapter = contractAdapter()
      val original = adapter.root
      val target = contractValue(adapter.to(original, "target"))
      val materialized = contractValue(adapter.materialize(target))
      val descendant = contractValue(adapter.to(materialized, "mid"))

      assertNotEquals(adapter.rootId(materialized), adapter.rootId(original))
      assertEquals(adapter.domain(materialized), "target")
      assertEquals(adapter.requestedSteps(materialized), Vector.empty)
      assertEquals(adapter.demand(materialized), None)
      assertEquals(adapter.rootId(descendant), adapter.rootId(materialized))
      assertEquals(adapter.rootId(original), "root-data")

    test("lazy contract: route, inverse, data, and demand failures are typed"):
      val adapter = contractAdapter()
      val unsupported = adapter.to(adapter.root, "unsupported")
      val approximate = contractValue(adapter.to(adapter.root, "approx-target"))
      val invalidDemand = adapter.rows(adapter.root, Vector(9))

      assertEquals(
        unsupported.left.toOption,
        Some(LazyContractError.UnsupportedRoute("root", "unsupported"))
      )
      assertEquals(
        adapter.value(approximate).left.toOption,
        Some(LazyContractError.InverseQualityRejected("approx-target"))
      )
      assertEquals(
        adapter.value(adapter.unavailableRoot).left.toOption,
        Some(LazyContractError.DataUnavailable("external-root"))
      )
      assertEquals(
        invalidDemand.left.toOption,
        Some(LazyContractError.InvalidDemand(Vector(9)))
      )
      assertEquals(adapter.metrics, LazyContractMetrics(0, 0, 0, 0))

    test("lazy contract: fusion preserves requested-step provenance"):
      val adapter = contractAdapter()
      val target = contractValue(adapter.to(contractValue(adapter.to(adapter.root, "mid")), "target"))
      val selected = contractValue(adapter.rows(target, Vector(2, 0)))
      val evaluation = contractValue(adapter.value(selected))

      assertEquals(evaluation.provenance.root, "root-data")
      assertEquals(
        evaluation.provenance.requestedSteps,
        Vector("to:mid", "to:target", "rows:2,0")
      )
      assertEquals(evaluation.provenance.route, Vector("root-to-mid", "mid-to-target"))
      assertEquals(evaluation.provenance.demand, Some(Vector(2, 0)))
      assertEquals(evaluation.provenance.compiler, "reference-pullback-v1")
      assert(evaluation.provenance.fusedSpatially)

  private def contractValue[A](result: Either[LazyContractError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(s"unexpected contract error: $error")

  private def assertDoubleVector(actual: Vector[Double], expected: Vector[Double]): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      assertEqualsDouble(actual(i), expected(i), 1e-12)
      i += 1
