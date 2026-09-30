package scalafim.archive.hdf5

import java.nio.file.{Files, Path}
import java.lang.management.ManagementFactory
import java.security.MessageDigest

private[hdf5] object AdapterChecks:
  def get[A](result: Either[Hdf5Error, A]): A = result.fold(e => throw new AssertionError(e.message), identity)
  def name(n: String): Hdf5DatasetName = get(Hdf5DatasetName(n))
  def slab(offset: Long*)(count: Long*): Hdf5Slab = get(Hdf5Slab(offset.toVector, count.toVector))
  def info(n: String, dtype: Hdf5DType, shape: Vector[Long], chunks: Vector[Long], filter: Hdf5Filter = Hdf5Filter.None): Hdf5DatasetInfo =
    Hdf5DatasetInfo(name(n), dtype, get(Hdf5Extent(shape)), get(Hdf5Extent(chunks)), filter)
  def small(archive: Hdf5Archive, path: Path): Unit =
    get(Hdf5Scope.file(archive.createExclusive(path.toString)): file =>
      for filter <- Vector(Hdf5Filter.None, Hdf5Filter.Deflate(4)) do
        val suffix = if filter == Hdf5Filter.None then "_none" else "_deflate"
        for kind <- Vector("f32", "f64", "u8", "valid") do
          val dtype = kind match
            case "f32" => Hdf5DType.Float32
            case "f64" => Hdf5DType.Float64
            case _ => Hdf5DType.UInt8
          get(Hdf5Scope.dataset(file.create(info(kind + suffix, dtype, Vector(7, 11, 5), Vector(3, 4, 2), filter))): d =>
            // Descending, nonzero, deliberately misaligned offsets and partial edge slabs.
            for i <- 6 to 0 by -2; j <- 9 to 0 by -3; k <- 4 to 0 by -2 do
              val count = Vector(math.min(2, 7 - i).toLong, math.min(3, 11 - j).toLong, math.min(2, 5 - k).toLong)
              val n = count.product.toInt
              val values = new Array[Double](n)
              val codes = new Array[Byte](n)
              var a = 0
              for x <- 0 until count(0).toInt; y <- 0 until count(1).toInt; z <- 0 until count(2).toInt do
                values(a) = (i + x) * 1000 + (j + y) * 10 + (k + z) + 0.25
                codes(a) = (if kind == "u8" then ((i+x)*47 + (j+y)*19 + (k+z)*5) % 256
                  else ((i+x) + 2*(j+y) + 3*(k+z)) % 4).toByte
                a += 1
              val block = dtype match
                case Hdf5DType.Float32 => Hdf5Block.Float32(values.map(_.toFloat))
                case Hdf5DType.Float64 => Hdf5Block.Float64(values)
                case Hdf5DType.UInt8 => Hdf5Block.UInt8(codes)
              get(d.write(get(Hdf5Slab(Vector(i.toLong, j.toLong, k.toLong), count)), block))
            Right(())
          )
      Right(())
    )

  def big(archive: Hdf5Archive, path: Path, write: Boolean): Unit =
    get(Hdf5Scope.file(if write then archive.createExclusive(path.toString) else archive.openReadOnly(path.toString)): file =>
      val shape = Vector(128L, 131072L)
      val chunks = Vector(1L, 32768L)
      val openedValues = if write then file.create(info("values", Hdf5DType.Float64, shape, chunks)) else file.inspect(name("values"))
      get(Hdf5Scope.dataset(openedValues): values =>
        val openedValidity = if write then file.create(info("validity", Hdf5DType.UInt8, shape, chunks)) else file.inspect(name("validity"))
        get(Hdf5Scope.dataset(openedValidity): validity =>
          require(values.info.extent.dimensions == shape && validity.info.extent.dimensions == shape)
          require(values.info.chunks.dimensions == chunks && validity.info.chunks.dimensions == chunks)
          val v = new Array[Double](65536)
          val codes = new Array[Byte](65536)
          var checked = 0L
          for row <- 0 until 128; col <- 0 until 131072 by 65536 do
            val selection = slab(row, col)(1, 65536)
            if write then
              var i = 0
              while i < v.length do
                v(i) = row * 1048576.0 + (col + i) * 0.125 - 17.25
                codes(i) = ((row + col + i) % 4).toByte
                i += 1
              get(values.write(selection, Hdf5Block.Float64(v)))
              get(validity.write(selection, Hdf5Block.UInt8(codes)))
              java.util.Arrays.fill(v, Double.NaN)
              java.util.Arrays.fill(codes, 99.toByte)
            get(values.readInto(selection, Hdf5Block.Float64(v)))
            get(validity.readInto(selection, Hdf5Block.UInt8(codes)))
            var i = 0
            while i < v.length do
              val expected = row * 1048576.0 + (col + i) * 0.125 - 17.25
              require(java.lang.Double.doubleToRawLongBits(v(i)) == java.lang.Double.doubleToRawLongBits(expected), s"value $row/${col+i}")
              require((codes(i) & 255) == (row + col + i) % 4, s"validity $row/${col+i}")
              i += 1
            checked += v.length
          println(s"ADAPTER_BIG_${if write then "WRITE" else "READ"}_PASS cells=$checked valueBytes=${checked*8} validityBytes=$checked blockElements=65536")
          Right(())
        )
        Right(())
      )
      Right(())
    )

/** A fresh-process bounded-heap lane invoking the adapter, not the old standalone provider probe. */
object Hdf5AdapterCheck:
  def main(args: Array[String]): Unit =
    println(s"OWNED_PID ${ProcessHandle.current().pid()} java=${System.getProperty("java.version")} arch=${System.getProperty("os.arch")}")
    if args(0) == "missing" then
      JvmHdf5JniAdapter.open() match
        case Left(e: Hdf5Error.MissingCapability) => println(s"MISSING_CAPABILITY_PASS ${e.message}")
        case other => throw new AssertionError(s"expected absent property refusal: $other")
    else
      val archive = AdapterChecks.get(JvmHdf5JniAdapter.open())
      args(0) match
        case "write" => AdapterChecks.big(archive, Path.of(args(1)), true)
        case "read" => AdapterChecks.big(archive, Path.of(args(1)), false)
        case other => throw new IllegalArgumentException(other)
      val r = archive.receipt
      println(s"ADAPTER_RECEIPT $r heapMaxBytes=${Runtime.getRuntime.maxMemory()} physicalDiskBytes=unavailable nativeAllocatedBytes=unavailable")
      require(r.openFiles == 0 && r.openDatasets == 0 && r.ownedIds == 0 && r.providerOpenIds.contains(0))

class JvmHdf5JniAdapterSuite extends munit.FunSuite:
  import AdapterChecks.*
  private val root = Option(System.getProperty("scalafim.hdf5.test.dir")).map(Path.of(_))
    .getOrElse(Files.createTempDirectory("scalafim-hdf5-adapter-"))
  private val archive = get(JvmHdf5JniAdapter.open())
  private val smallPath = root.resolve("small.h5")
  private def noPayload[A](result: => Either[Hdf5Error, A]): Hdf5Error =
    val before = archive.receipt
    val e = result.swap.fold(_ => fail("expected refusal"), identity)
    val after = archive.receipt
    assertEquals(after.payloadCalls, before.payloadCalls)
    assertEquals(after.ownedIds, before.ownedIds)
    e
  private def clean(): Unit =
    val r = archive.receipt
    assertEquals(r.ownedIds, 0)
    assertEquals(r.providerOpenIds, Some(0))
    assertEquals(r.openFiles, 0)
    assertEquals(r.openDatasets, 0)
  private def fdCount: Long = ManagementFactory.getOperatingSystemMXBean
    .asInstanceOf[com.sun.management.UnixOperatingSystemMXBean].getOpenFileDescriptorCount

  test("locked loader admits only the explicit macOS ARM64 JDK25 lane"):
    assert(JvmHdf5JniAdapter.validateProvider().isRight)
    println(s"ADAPTER_LOADER_PASS java=${System.getProperty("java.version")} arch=${System.getProperty("os.arch")} cache=${System.getProperty("scalafim.hdf5.provider.dir")}")
    clean()

  test("absent, incomplete, and mismatched provider properties refuse before payload"):
    val key = "scalafim.hdf5.provider.dir"
    val original = System.getProperty(key)
    try
      System.clearProperty(key)
      assert(JvmHdf5JniAdapter.open().left.toOption.exists(_.isInstanceOf[Hdf5Error.MissingCapability]))
      val incomplete = Files.createTempDirectory(root, "bad-provider-")
      System.setProperty(key, incomplete.toString)
      assert(JvmHdf5JniAdapter.open().left.toOption.exists(_.isInstanceOf[Hdf5Error.MissingCapability]))
      Files.createDirectories(incomplete.resolve("provider/HDF5-2.2.0-Darwin/HDF_Group/HDF5/2.2.0/lib"))
      Files.write(incomplete.resolve("hdf5-2.2.0-macos15_clang.tar.gz"), Array[Byte](1, 2, 3))
      assert(JvmHdf5JniAdapter.open().left.toOption.exists(_.isInstanceOf[Hdf5Error.MissingCapability]))
    finally System.setProperty(key, original)
    clean()

  test("write eight asymmetric fixed chunked datasets in reversed edge slabs"):
    small(archive, smallPath)
    clean()
    println(s"ADAPTER_SMALL_PASS path=$smallPath datasets=8 cells=3080")

  test("Python-authored literal LE datasets and nonzero-offset edges"):
    get(Hdf5Scope.file(archive.openReadOnly(root.resolve("literal.h5").toString)): file =>
      for (n, dtype) <- Vector("f32" -> Hdf5DType.Float32, "f64" -> Hdf5DType.Float64, "u8" -> Hdf5DType.UInt8) do
        get(Hdf5Scope.dataset(file.inspect(name(n))): d =>
          assertEquals(d.info.dtype, dtype)
          assertEquals(d.info.extent.dimensions, Vector(2L, 3L))
          assertEquals(d.info.chunks.dimensions, Vector(1L, 2L))
          assertEquals(d.info.filter, Hdf5Filter.Deflate(4))
          val doubles = new Array[Double](6)
          val floats = new Array[Float](6)
          val bytes = new Array[Byte](6)
          val block = dtype match
            case Hdf5DType.Float32 => Hdf5Block.Float32(floats)
            case Hdf5DType.Float64 => Hdf5Block.Float64(doubles)
            case Hdf5DType.UInt8 => Hdf5Block.UInt8(bytes)
          get(d.readInto(slab(0, 0)(2, 3), block))
          val expected = Vector(-7.5, 0.0, 1.25, 99.5, -0.125, 8192.0)
          for i <- 0 until 6 do dtype match
            case Hdf5DType.Float32 => assertEqualsDouble(floats(i).toDouble, expected(i), 0.0)
            case Hdf5DType.Float64 => assertEqualsDouble(doubles(i), expected(i), 0.0)
            case Hdf5DType.UInt8 => assertEquals(bytes(i) & 255, Vector(0, 255, 128, 1, 2, 3)(i))
          get(d.readInto(slab(1, 1)(1, 2), block))
          for i <- 0 until 2 do dtype match
            case Hdf5DType.Float32 => assertEqualsDouble(floats(i).toDouble, expected(i+4), 0.0)
            case Hdf5DType.Float64 => assertEqualsDouble(doubles(i), expected(i+4), 0.0)
            case Hdf5DType.UInt8 => assertEquals(bytes(i) & 255, Vector(2, 3)(i))
          Right(())
        )
      Right(())
    )
    clean()

  test("stored dtype, layout, filter, shape and rank refusals precede payload"):
    get(Hdf5Scope.file(archive.openReadOnly(root.resolve("unsupported.h5").toString)): file =>
      for n <- Vector("contiguous", "bigendian", "integer", "shuffle", "multifilter", "extensible", "rank4") do
        assert(noPayload(file.inspect(name(n))).isInstanceOf[Hdf5Error.UnsupportedStored])
      Right(())
    )
    clean()

  test("exclusive creation preserves existing bytes and HDF SHA; duplicate names refuse"):
    val sentinel = root.resolve("sentinel.bin")
    val bytes = Array[Byte](72, 68, 70, 0, 17, 99)
    Files.write(sentinel, bytes)
    assertEquals(noPayload(archive.createExclusive(sentinel.toString)), Hdf5Error.AlreadyExists(sentinel.toString))
    assertEquals(Files.readAllBytes(sentinel).toVector, bytes.toVector)
    def digest = MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(smallPath)).toVector
    val before = digest
    assertEquals(noPayload(archive.createExclusive(smallPath.toString)), Hdf5Error.AlreadyExists(smallPath.toString))
    assertEquals(digest, before)
    get(Hdf5Scope.file(archive.createExclusive(root.resolve("duplicate.h5").toString)): file =>
      val plan = info("same", Hdf5DType.UInt8, Vector(1), Vector(1))
      get(Hdf5Scope.dataset(file.create(plan))(_ => Right(())))
      assertEquals(noPayload(file.create(plan)), Hdf5Error.DuplicateDataset("same"))
      Right(())
    )
    clean()

  test("pre-native slab, block and chunk validation; zero and cancellation"):
    get(Hdf5Scope.file(archive.createExclusive(root.resolve("validation.h5").toString)): file =>
      get(Hdf5Scope.dataset(file.create(info("data", Hdf5DType.Float64, Vector(7, 11, 5), Vector(3, 4, 2)))): d =>
        noPayload(d.write(slab(0)(1), Hdf5Block.Float64(new Array[Double](1))))
        noPayload(d.write(slab(7, 0, 0)(1, 1, 1), Hdf5Block.Float64(new Array[Double](1))))
        noPayload(d.write(slab(0, 0, 0)(1, 2, 2), Hdf5Block.Float64(new Array[Double](3))))
        noPayload(d.write(slab(0, 0, 0)(1, 1, 1), Hdf5Block.Float32(new Array[Float](1))))
        noPayload(d.write(slab(0, 0, 0)(1, 1, 1), Hdf5Block.Float64(null)))
        noPayload(d.write(slab(0, 0, 0)(1, 1, 1), null))
        assertEquals(noPayload(d.write(slab(0, 0, 0)(1, 1, 1), Hdf5Block.Float64(new Array[Double](1)), () => true)), Hdf5Error.Cancelled)
        val before = archive.receipt
        get(d.write(slab(7, 0, 0)(0, 1, 1), Hdf5Block.Float64(Array.emptyDoubleArray)))
        assertEquals(archive.receipt.payloadCalls, before.payloadCalls)
        assertEquals(archive.receipt.ownedIds, before.ownedIds)
        noPayload(file.create(info("badchunk", Hdf5DType.Float64, Vector(7), Vector(8))))
        noPayload(file.create(info("overflow", Hdf5DType.Float64, Vector(Long.MaxValue), Vector(1))))
        noPayload(file.create(info("rankchunk", Hdf5DType.Float64, Vector(7, 11), Vector(1))))
        noPayload(file.create(info("filter", Hdf5DType.Float64, Vector(7), Vector(1), Hdf5Filter.Deflate(10))))
        Right(())
      )
      get(Hdf5Scope.dataset(file.create(info("capacity", Hdf5DType.Float64, Vector(65537), Vector(32768)))): d =>
        noPayload(d.write(slab(0)(65537), Hdf5Block.Float64(new Array[Double](65537))))
        Right(())
      )
      Right(())
    )
    clean()

  test("two stores, file/dataset caps and repeated close preserve independent handles"):
    val a = get(archive.openReadOnly(smallPath.toString))
    val otherAdapter = get(JvmHdf5JniAdapter.open())
    val b = get(otherAdapter.openReadOnly(smallPath.toString))
    try
      assert(noPayload(otherAdapter.openReadOnly(smallPath.toString)).isInstanceOf[Hdf5Error.ResourceLimit])
      get(a.close())
      get(a.close())
      val d = get(b.inspect(name("f64_none")))
      val v = get(b.inspect(name("valid_none")))
      try
        assert(noPayload(b.inspect(name("f32_none"))).isInstanceOf[Hdf5Error.ResourceLimit])
        val buffer = new Array[Double](1)
        get(d.readInto(slab(6, 10, 4)(1, 1, 1), Hdf5Block.Float64(buffer)))
        assertEqualsDouble(buffer(0), 6104.25, 0.0)
      finally
        get(d.close())
        get(v.close())
      assert(noPayload(d.readInto(slab(0, 0, 0)(1, 1, 1), Hdf5Block.Float64(new Array[Double](1)))).isInstanceOf[Hdf5Error.Closed])
    finally
      get(a.close())
      get(b.close())
    clean()

  test("acquisition failures and thrown callback close owned resources"):
    for target <- 1 to 2 do
      JvmHdf5JniAdapter.failAcquisitionAt(target)
      try assert(archive.createExclusive(root.resolve(s"acquire-file-$target.h5").toString).isLeft)
      finally JvmHdf5JniAdapter.failAcquisitionAt(-1)
      clean()
    for target <- 1 to 4 do
      get(Hdf5Scope.file(archive.createExclusive(root.resolve(s"acquire-dataset-$target.h5").toString)): file =>
        JvmHdf5JniAdapter.failAcquisitionAt(target)
        try assert(file.create(info("data", Hdf5DType.Float64, Vector(2, 3), Vector(1, 2))).isLeft)
        finally JvmHdf5JniAdapter.failAcquisitionAt(-1)
        Right(())
      )
      clean()
    val callback = Hdf5Scope.file(archive.createExclusive(root.resolve("callback.h5").toString)): file =>
      Hdf5Scope.dataset(file.create(info("data", Hdf5DType.UInt8, Vector(1), Vector(1)))): d =>
        get(d.write(slab(0)(1), Hdf5Block.UInt8(Array[Byte](42))))
        throw new IllegalStateException("callback after write")
    assert(callback.left.toOption.exists(_.message.contains("callback after write")))
    clean()

  test("100 warm open/read/close cycles have bounded descriptors and zero final IDs"):
    def cycle(): Unit =
      get(Hdf5Scope.file(archive.openReadOnly(smallPath.toString)): file =>
        Hdf5Scope.dataset(file.inspect(name("f64_deflate"))): d =>
          d.readInto(slab(0, 0, 0)(1, 1, 1), Hdf5Block.Float64(new Array[Double](1)))
      )
    for _ <- 0 until 3 do cycle()
    val before = fdCount
    for _ <- 0 until 100 do
      cycle()
      noPayload(archive.createExclusive(smallPath.toString))
    val after = fdCount
    assert(after <= before + 1, s"descriptors $before -> $after")
    clean()
    println(s"ADAPTER_LIFETIME_PASS cycles=100 fdBefore=$before fdAfter=$after receipt=${archive.receipt}")

  test("concurrent callers across two adapters are serialized and resources return to zero"):
    val a = get(JvmHdf5JniAdapter.open())
    val b = get(JvmHdf5JniAdapter.open())
    val f = get(a.openReadOnly(smallPath.toString))
    val g = get(b.openReadOnly(smallPath.toString))
    val d = get(f.inspect(name("f64_none")))
    val e = get(g.inspect(name("f64_deflate")))
    val errors = new java.util.concurrent.ConcurrentLinkedQueue[Throwable]()
    def thread(ds: Hdf5Dataset) = new Thread(() =>
      try
        for _ <- 0 until 50 do
          val buffer = new Array[Double](1)
          get(ds.readInto(slab(6, 10, 4)(1, 1, 1), Hdf5Block.Float64(buffer)))
          if java.lang.Double.doubleToRawLongBits(buffer(0)) != java.lang.Double.doubleToRawLongBits(6104.25) then
            throw new AssertionError("concurrent value")
      catch case t: Throwable =>
        errors.add(t)
        ()
    )
    val t = thread(d)
    val u = thread(e)
    try
      t.start()
      u.start()
      t.join(10000)
      u.join(10000)
      assert(!t.isAlive && !u.isAlive, "bounded concurrent readers")
      assert(errors.isEmpty, errors.toString)
    finally
      t.join()
      u.join()
      get(f.close())
      get(g.close())
    clean()
