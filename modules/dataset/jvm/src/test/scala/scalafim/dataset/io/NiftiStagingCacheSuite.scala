package scalafim.dataset.io

import munit.FunSuite
import scalafim.image.{NeuroSpace, NeuroVol}
import scalafim.image.io.{Nifti, NiftiCoordinateSystem, NiftiSpatialUnits, NiftiWriteOptions}
import java.nio.file.{Files, Path}
import java.util.zip.{Deflater, GZIPOutputStream}
import scala.jdk.CollectionConverters.*
import scala.util.Using

class NiftiStagingCacheSuite extends FunSuite:
  private def fixture(body: Path => Unit): Unit =
    val root = Files.createTempDirectory("scalafim-staging-integrity-")
    try body(root)
    finally Using.resource(Files.walk(root))(_.iterator().asScala.toVector.reverse.foreach(Files.delete))

  private def compressed(root: Path, value: Double): (Path, Array[Byte]) =
    // Explicit crop from the ds000001 MNI2009c 2 mm parent grid.
    val space = NeuroSpace(Vector(2, 1, 1), spacing = Some(Vector(2.0, 2.0, 2.0)),
      origin = Some(Vector(-16.5, -12.5, 1.5)))
    val raw = Nifti.writeVol(root.resolve("source.nii"),
      NeuroVol.fromLinearChecked(Array(value, value + 1.0), space).toOption.get,
      NiftiWriteOptions(NiftiCoordinateSystem.Mni152, NiftiSpatialUnits.Millimeters))
    val bytes = Files.readAllBytes(raw)
    val gzip = root.resolve("source.nii.gz")
    Using.resource(new GZIPOutputStream(Files.newOutputStream(gzip)) {
      `def`.setLevel(Deflater.NO_COMPRESSION)
    })(_.write(bytes))
    (gzip, bytes)

  test("same path size and modification time cannot reuse different compressed content") {
    fixture { root =>
      val (source, _) = compressed(root, 1.0)
      val originalTime = Files.getLastModifiedTime(source)
      val originalSize = Files.size(source)
      val cache = NiftiStagingCache.unsafe(root.resolve("cache"))
      val _ = cache.stage(source).fold(e => fail(e.message), identity)
      val (_, expected) = compressed(root, 10.0)
      assertEquals(Files.size(source), originalSize)
      val _ = Files.setLastModifiedTime(source, originalTime)
      val staged = cache.stage(source).fold(e => fail(e.message), identity)
      assertEquals(Files.readAllBytes(staged).toVector, expected.toVector)
    }
  }

  test("corrupt staged payload is repaired even when its size and header still match") {
    fixture { root =>
      val (source, expected) = compressed(root, 2.0)
      val cache = NiftiStagingCache.unsafe(root.resolve("cache"))
      val staged = cache.stage(source).fold(e => fail(e.message), identity)
      val damaged = expected.clone()
      damaged(damaged.length - 1) = (damaged.last ^ 1).toByte
      val _ = Files.write(staged, damaged)
      val repaired = cache.stage(source).fold(e => fail(e.message), identity)
      assertEquals(Files.readAllBytes(repaired).toVector, expected.toVector)
    }
  }

  private def entries(root: Path): Vector[Path] =
    Using.resource(Files.list(root))(_.iterator().asScala.toVector)

  test("expansion and free-space limits refuse publication and remove owned partials") {
    fixture { root =>
      val (source, expected) = compressed(root, 3.0)
      Vector(NiftiStagingLimits(maxExpandedBytes = 352L),
        NiftiStagingLimits(minimumFreeBytes = Long.MaxValue)).zipWithIndex.foreach { (limits, i) =>
        val cache = NiftiStagingCache.unsafe(root.resolve(s"cache-$i"), limits)
        assert(cache.stage(source).isLeft)
        assert(entries(cache.root).forall(_.toString.endsWith(".lock")))
      }
      val cache = NiftiStagingCache.unsafe(root.resolve("full"))
      val staged = cache.stage(source).toOption.get
      assertEquals(Files.readAllBytes(staged).toVector, expected.toVector)
      val restricted = NiftiStagingCache.unsafe(cache.root, NiftiStagingLimits(maxExpandedBytes = 352L))
      assert(restricted.stage(source).isLeft)
      assertEquals(Files.readAllBytes(staged).toVector, expected.toVector)
    }
  }

  test("cancelled staging preserves interruption and publishes nothing") {
    fixture { root =>
      val (source, _) = compressed(root, 4.0)
      val cache = NiftiStagingCache.unsafe(root.resolve("cache"))
      try
        Thread.currentThread().interrupt()
        assert(cache.stage(source).left.toOption.get.message.contains("cancelled"))
        assert(Thread.currentThread().isInterrupted)
      finally Thread.interrupted()
      assertEquals(entries(cache.root), Vector.empty)
    }
  }

  test("truncated compressed input cannot publish an apparently valid header") {
    fixture { root =>
      val (source, _) = compressed(root, 5.0)
      val bytes = Files.readAllBytes(source)
      val _ = Files.write(source, bytes.dropRight(8))
      val cache = NiftiStagingCache.unsafe(root.resolve("cache"))
      assert(cache.stage(source).isLeft)
      assert(entries(cache.root).forall(_.toString.endsWith(".lock")))
    }
  }

  test("concurrent cache instances share one verified content revision") {
    fixture { root =>
      val (source, expected) = compressed(root, 6.0)
      val cacheRoot = root.resolve("cache")
      val start = new java.util.concurrent.CountDownLatch(1)
      val pool = java.util.concurrent.Executors.newFixedThreadPool(4)
      try
        val jobs = Vector.fill(4) {
          pool.submit(new java.util.concurrent.Callable[Path]:
            def call(): Path =
              val cache = NiftiStagingCache.unsafe(cacheRoot)
              start.await()
              cache.stage(source).fold(e => throw new IllegalStateException(e.message), identity)
          )
        }
        start.countDown()
        val paths = jobs.map(_.get(10L, java.util.concurrent.TimeUnit.SECONDS))
        assertEquals(paths.distinct.size, 1)
        assertEquals(Files.readAllBytes(paths.head).toVector, expected.toVector)
        assertEquals(entries(cacheRoot).count(_.toString.endsWith(".nii")), 1)
        assert(!entries(cacheRoot).exists(_.toString.endsWith(".partial")))
        val copy = Files.copy(source, root.resolve("identical.nii.gz"))
        assertEquals(NiftiStagingCache.unsafe(cacheRoot).stage(copy).toOption.get, paths.head)
      finally
        pool.shutdownNow()
        assert(pool.awaitTermination(10L, java.util.concurrent.TimeUnit.SECONDS))
    }
  }

  test("distinct concurrent images cannot exceed a shared aggregate quota") {
    fixture { root =>
      val (source, bytes) = compressed(root, 8.0)
      val first = Files.copy(source, root.resolve("first.nii.gz"))
      val (second, _) = compressed(root, 9.0)
      val budget = bytes.length.toLong + 65L
      val cacheRoot = root.resolve("quota")
      val pool = java.util.concurrent.Executors.newFixedThreadPool(2)
      try
        val jobs = Vector(first, second).map { path =>
          pool.submit(new java.util.concurrent.Callable[Boolean]:
            def call(): Boolean =
              NiftiStagingCache.unsafe(cacheRoot, NiftiStagingLimits(maxCacheBytes = budget)).stage(path).isRight
          )
        }
        assertEquals(jobs.count(_.get(10L, java.util.concurrent.TimeUnit.SECONDS)), 1)
        val cache = NiftiStagingCache.unsafe(cacheRoot, NiftiStagingLimits(maxCacheBytes = budget))
        assertEquals(cache.sizeBytes.toOption.get, budget)
        assertEquals(Vector(first, second).count(cache.stage(_).isRight), 1)
        assert(!entries(cacheRoot).exists(_.toString.endsWith(".partial")))
      finally
        pool.shutdownNow()
        assert(pool.awaitTermination(10L, java.util.concurrent.TimeUnit.SECONDS))
    }
  }


  test("multi-window staging preserves exact bytes and tight publication limits") {
    fixture { root =>
      val space = NeuroSpace(Vector(64, 64, 9), spacing = Some(Vector(2.0, 2.0, 2.0)),
        origin = Some(Vector(-16.5, -12.5, 1.5)))
      val raw = Nifti.writeVol(root.resolve("large.nii"),
        NeuroVol.fromLinearChecked(Array.tabulate(64 * 64 * 9)(index => math.sin(index * 0.71)), space).toOption.get,
        NiftiWriteOptions(NiftiCoordinateSystem.Mni152, NiftiSpatialUnits.Millimeters))
      val expected = Files.readAllBytes(raw)
      assert(expected.length > 2 * 64 * 1024)
      val source = root.resolve("large.nii.gz")
      Using.resource(new GZIPOutputStream(Files.newOutputStream(source)) {
        `def`.setLevel(Deflater.NO_COMPRESSION)
      })(_.write(expected))
      val exact = NiftiStagingLimits(maxExpandedBytes = expected.length.toLong,
        maxCacheBytes = expected.length.toLong + 65L)
      val cache = NiftiStagingCache.unsafe(root.resolve("exact"), exact)
      val first = cache.stage(source).fold(error => fail(error.message), identity)
      assertEquals(Files.readAllBytes(first).toVector, expected.toVector)
      assertEquals(cache.stage(source).toOption.get, first)
      assertEquals(cache.sizeBytes.toOption.get, expected.length.toLong + 65L)
      Vector(exact.copy(maxExpandedBytes = expected.length.toLong - 1L),
        exact.copy(maxCacheBytes = expected.length.toLong + 64L)).zipWithIndex.foreach { (limits, index) =>
        val restricted = NiftiStagingCache.unsafe(root.resolve(s"restricted-$index"), limits)
        assert(restricted.stage(source).isLeft)
        assert(entries(restricted.root).forall(_.toString.endsWith(".lock")))
      }
    }
  }
