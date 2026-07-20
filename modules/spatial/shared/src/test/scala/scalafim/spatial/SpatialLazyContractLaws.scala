package scalafim.spatial

final case class SpatialLazyWork(
  sourceReads: Int,
  compilations: Int,
  resamplings: Int,
  cacheWrites: Int
)

final case class SpatialLazyExplanation(
  rootId: String,
  rootDomain: String,
  targetDomain: String,
  requestedRows: Vector[Int],
  sourceSupport: Vector[Int],
  resamplingPasses: Int
)

trait SpatialLazyContractAdapter:
  type Root
  type View
  type Failure

  def freshRoot(): Root
  def rootId(root: Root): String
  def rootView(root: Root): View
  def halfShift(view: View): Either[Failure, View]
  def directFullShift(view: View): Either[Failure, View]
  def selectRows(view: View, rows: Vector[Int]): Either[Failure, View]
  def invalidSelection(view: View): Either[Failure, View]
  def materialize(view: View): Either[Failure, Vector[Double]]
  def explain(view: View): SpatialLazyExplanation
  def cacheKey(view: View): String
  def work: SpatialLazyWork
  def resetWork(): Unit

trait SpatialLazyContractLaws:
  self: munit.FunSuite =>

  def spatialLazyAdapter: SpatialLazyContractAdapter

  final def registerSpatialLazyContractLaws(): Unit =
    test("lazy contract: view construction preserves root identity and performs no work"):
      val adapter = spatialLazyAdapter
      adapter.resetWork()
      val root = adapter.freshRoot()
      val rootIdentity = adapter.rootId(root)
      val view = success(adapter.halfShift(adapter.rootView(root)))
      val chained = success(adapter.halfShift(view))

      assertEquals(adapter.explain(chained).rootId, rootIdentity)
      assertEquals(adapter.work, SpatialLazyWork(0, 0, 0, 0))

    test("lazy contract: chained transforms equal a direct root-first fused view"):
      val adapter = spatialLazyAdapter
      val root = adapter.freshRoot()
      val rootView = adapter.rootView(root)
      val chained = success(adapter.halfShift(success(adapter.halfShift(rootView))))
      val direct = success(adapter.directFullShift(rootView))

      adapter.resetWork()
      val chainedValues = success(adapter.materialize(chained))
      assertEquals(chainedValues, Vector(8.0, 0.0, 4.0, 0.0))
      assertEquals(adapter.work.resamplings, 1)

      adapter.resetWork()
      val directValues = success(adapter.materialize(direct))
      assertEquals(chainedValues, directValues)
      assertEquals(adapter.work.resamplings, 1)

    test("lazy contract: partial demand equals full selection and restricts source support"):
      val adapter = spatialLazyAdapter
      val root = adapter.freshRoot()
      val fullView = success(adapter.directFullShift(adapter.rootView(root)))

      adapter.resetWork()
      val full = success(adapter.materialize(fullView))

      val partialView = success(adapter.selectRows(fullView, Vector(0, 2)))
      adapter.resetWork()
      val partial = success(adapter.materialize(partialView))
      val explanation = adapter.explain(partialView)

      assertEquals(partial, Vector(full(0), full(2)))
      assert(explanation.sourceSupport.nonEmpty)
      assert(explanation.sourceSupport.length < 4)
      assertEquals(adapter.work.sourceReads, explanation.sourceSupport.length)
      assertEquals(adapter.work.resamplings, 1)

    test("lazy contract: cache identity includes result-affecting semantics"):
      val adapter = spatialLazyAdapter
      val firstRoot = adapter.freshRoot()
      val secondRoot = adapter.freshRoot()
      val first = success(adapter.directFullShift(adapter.rootView(firstRoot)))
      val second = success(adapter.directFullShift(adapter.rootView(secondRoot)))
      val selected = success(adapter.selectRows(first, Vector(0, 2)))

      assertNotEquals(adapter.cacheKey(first), adapter.cacheKey(second))
      assertNotEquals(adapter.cacheKey(first), adapter.cacheKey(selected))
      assertNotEquals(adapter.cacheKey(first), adapter.cacheKey(success(adapter.halfShift(adapter.rootView(firstRoot)))))

    test("lazy contract: only terminal materialization performs work"):
      val adapter = spatialLazyAdapter
      adapter.resetWork()
      val root = adapter.freshRoot()
      val view = success(adapter.directFullShift(adapter.rootView(root)))
      adapter.explain(view)
      adapter.cacheKey(view)
      assertEquals(adapter.work, SpatialLazyWork(0, 0, 0, 0))

      success(adapter.materialize(view))
      assertEquals(adapter.work.sourceReads, 3)
      assertEquals(adapter.work.compilations, 1)
      assertEquals(adapter.work.resamplings, 1)
      assertEquals(adapter.work.cacheWrites, 1)

    test("lazy contract: invalid demand is typed and performs no partial work"):
      val adapter = spatialLazyAdapter
      adapter.resetWork()
      val result = adapter.invalidSelection(adapter.rootView(adapter.freshRoot()))

      assert(result.isLeft)
      assertEquals(adapter.work, SpatialLazyWork(0, 0, 0, 0))

    test("lazy contract: explanations retain root, intent, demand, and execution facts"):
      val adapter = spatialLazyAdapter
      val root = adapter.freshRoot()
      val selected = success(
        adapter.selectRows(
          success(adapter.halfShift(success(adapter.halfShift(adapter.rootView(root))))),
          Vector(0, 2)
        )
      )

      val before = adapter.explain(selected)
      assertEquals(before.rootId, adapter.rootId(root))
      assertEquals(before.rootDomain, "root")
      assertEquals(before.targetDomain, "shifted")
      assertEquals(before.requestedRows, Vector(0, 2))
      assertEquals(before.resamplingPasses, 0)

      success(adapter.materialize(selected))
      val after = adapter.explain(selected)
      assertEquals(after.sourceSupport, Vector(1, 3))
      assertEquals(after.resamplingPasses, 1)

  private def success[F, A](result: Either[F, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(s"unexpected lazy-contract failure: $error")
