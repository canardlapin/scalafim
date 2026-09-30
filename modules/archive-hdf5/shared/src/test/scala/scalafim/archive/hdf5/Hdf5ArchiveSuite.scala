package scalafim.archive.hdf5

class Hdf5ArchiveSuite extends munit.FunSuite:
  private def value[A](e: Either[Hdf5Error, A]): A = e.fold(err => fail(err.message), identity)
  private val limits = Hdf5Limits.bounded
  private def info(shape: Vector[Long] = Vector(7, 11, 5), chunk: Vector[Long] = Vector(3, 4, 2)) =
    Hdf5DatasetInfo(value(Hdf5DatasetName("data")), Hdf5DType.Float64, value(Hdf5Extent(shape)), value(Hdf5Extent(chunk)), Hdf5Filter.None)

  test("names have a finite flat ASCII grammar"):
    List(null, "", "/data", "x/y", ".", "1data", "é", "a" * 129, "bad\u0000").foreach(n => assert(Hdf5DatasetName(n).isLeft))
    assertEquals(value(Hdf5DatasetName("A_9")).value, "A_9")

  test("extent rejects rank, sign and product overflow without Double conversion"):
    List(Vector.empty, Vector(1L, 1L, 1L, 1L), Vector(0L), Vector(-1L)).foreach(d => assert(Hdf5Extent(d).isLeft))
    assert(Hdf5Extent(Vector(Long.MaxValue, 2)).left.toOption.exists(_.isInstanceOf[Hdf5Error.Overflow]))
    assertEquals(value(Hdf5Extent(Vector(9007199254740993L))).elements, 9007199254740993L)

  test("slab rejects negative bounds, ranks, sum and selected-product overflow"):
    assert(Hdf5Slab(Vector(0), Vector(1, 1)).isLeft)
    assert(Hdf5Slab(Vector(-1), Vector(1)).isLeft)
    assert(Hdf5Slab(Vector(0), Vector(-1)).isLeft)
    assert(Hdf5Slab(Vector(Long.MaxValue), Vector(1)).isLeft)
    assert(Hdf5Slab(Vector(0, 0), Vector(Long.MaxValue, 2)).isLeft)
    val end = value(Hdf5Slab(Vector(7, 0, 0), Vector(0, 1, 1)))
    assertEquals(end.elements, 0L)
    assertEquals(value(Hdf5Plan.slab(info(), end, Hdf5Block.Float64(Array.emptyDoubleArray), limits)), 0L)
    assert(Hdf5Slab.within(value(Hdf5Slab(Vector(7, 0, 0), Vector(1, 1, 1))), info().extent).isLeft)
    assert(Hdf5Slab.within(value(Hdf5Slab(Vector(0), Vector(1))), info().extent).isLeft)

  test("zero product does not multiply unrelated huge counts"):
    val zero = value(Hdf5Slab(Vector(0, 0, 0), Vector(Long.MaxValue, Long.MaxValue, 0)))
    assertEquals(zero.elements, 0L)

  test("dtype capacity and null blocks fail before planning payload"):
    val slab = value(Hdf5Slab(Vector(1, 2, 3), Vector(2, 3, 2)))
    assertEquals(value(Hdf5Plan.slab(info(), slab, Hdf5Block.Float64(new Array[Double](12)), limits)), 96L)
    assertEquals(Hdf5Plan.slab(info(), slab, Hdf5Block.Float64(new Array[Double](11)), limits), Left(Hdf5Error.Capacity(12, 11)))
    assert(Hdf5Plan.slab(info(), slab, Hdf5Block.Float32(new Array[Float](12)), limits).isLeft)
    assert(Hdf5Plan.slab(info(), slab, Hdf5Block.Float64(null), limits).isLeft)
    assert(Hdf5Plan.slab(info(), slab, null, limits).isLeft)

  test("chunk and selected block caps and byte overflow are independent"):
    assert(Hdf5Plan.dataset(info(Vector(7), Vector(8)), limits).isLeft)
    assert(Hdf5Plan.dataset(info(Vector(7, 11), Vector(3)), limits).isLeft)
    assert(Hdf5Plan.dataset(info(Vector(200000), Vector(200000)), limits).isLeft)
    assert(Hdf5Plan.dataset(info(Vector(Long.MaxValue), Vector(1)), limits).isLeft)
    val large = info(Vector(100000), Vector(1000))
    assert(Hdf5Plan.slab(large, value(Hdf5Slab(Vector(0), Vector(65537))), Hdf5Block.Float64(new Array[Double](65537)), limits).isLeft)
    assert(info().copy(filter = Hdf5Filter.Deflate(10)).filter.validate.isLeft)

  test("limits never admit unlimited block, cache, file, dataset or ID ceilings"):
    intercept[IllegalArgumentException](limits.copy(maxBlockElements = 0))
    intercept[IllegalArgumentException](limits.copy(maxBlockElements = 65537))
    intercept[IllegalArgumentException](limits.copy(rawCacheBytes = 1048577))
    intercept[IllegalArgumentException](limits.copy(rawCacheSlots = 522))
    intercept[IllegalArgumentException](limits.copy(metadataCacheBytes = 4194305))
    intercept[IllegalArgumentException](limits.copy(maxFiles = 3))
    intercept[IllegalArgumentException](limits.copy(maxDatasetsPerFile = 3))
    intercept[IllegalArgumentException](limits.copy(maxNativeIds = 25))

  test("scoped callback retains primary and cleanup error"):
    val closeError = Hdf5Error.NativeFailure("close", "injected")
    var closed = 0
    val file = new Hdf5File:
      def create(i: Hdf5DatasetInfo) = Left(Hdf5Error.Closed("fixture"))
      def inspect(n: Hdf5DatasetName) = Left(Hdf5Error.Closed("fixture"))
      def close() =
        closed += 1
        Left(closeError)
    val result = Hdf5Scope.file(Right(file))(_ => throw new IllegalStateException("primary callback"))
    result match
      case Left(Hdf5Error.ScopeFailure(primary, cleanup)) =>
        assert(primary.message.contains("primary callback"))
        assertEquals(cleanup, Vector(closeError))
      case other => fail(other.toString)
    assertEquals(closed, 1)

  test("scope retains exact typed primary when cleanup also fails"):
    val primary = Hdf5Error.Cancelled
    val cleanup = Hdf5Error.NativeFailure("close", "fixture")
    val file = new Hdf5File:
      def create(i: Hdf5DatasetInfo) = Left(Hdf5Error.Closed("fixture"))
      def inspect(n: Hdf5DatasetName) = Left(Hdf5Error.Closed("fixture"))
      def close() = Left(cleanup)
    assertEquals(Hdf5Scope.file(Right(file))(_ => Left(primary)), Left(Hdf5Error.ScopeFailure(primary, Vector(cleanup))))
