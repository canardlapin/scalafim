package scalafim.image

import SampleSpaces.*

import java.lang.management.ManagementFactory

import com.sun.management.ThreadMXBean
import locus4s.DomainRegistry
import locus4s.Region
import locus4s.Relation
import locus4s.Selection
import ravel.NDArray
import spire.std.double.given
import Ops.*
import GridDomainOps.*

class NativeImageAllocationSuite extends munit.FunSuite:
  private var retained: AnyRef = null

  test("representative native kernels retain one Ravel value destination"):
    val edge = 40
    val volumeSpace = ProviderSpaces.volume(SampleSpaces(Vector(edge, edge, edge)))
    val data =
      NDArray.tabulate[Double](edge, edge, edge): (x, y, z) =>
        ((x * 17 + y * 5 + z) % 101).toDouble
    val nativeVolume: SomeScalarVolume[Double] =
      NeuroVolume
        .continuous(volumeSpace, data)
        .map(SomeNeuroVolume.eraseSpace)
        .fold(error => fail(error.message), identity)
    val volume: SomeScalarVolume[Double] = nativeVolume
    val other = volume.mapValues(_ * 0.25)

    val seriesData =
      NDArray.tabulate[Double](edge, edge, edge / 2, 5):
        (x, y, z, time) =>
          ((x * 11 + y * 3 + z + time) % 97).toDouble
    val seriesSpace =
      SampleSpaces(Vector(edge, edge, edge / 2, 5))
    val series =
      SomeScalarSeries.unsafeFromRavel(seriesData, seriesSpace, "allocation-series")

    val grid = GridSpec.fromSpace(volumeSpace)
    val resampling =
      ResamplingPlan
        .make(
          grid,
          grid,
          SpatialPullbacks.worldAligned(grid, grid),
          Resample.Method.Linear
        )
        .fold(error => fail(error.message), identity)

    val registered =
      GridDomain
        .register(
          volumeSpace.grid,
          "native image allocation voxels",
          DomainRegistry.empty
        )
        .fold(error => fail(error.message), identity)
    val domain = registered.value
    val selectedOrdinals =
      (0 until domain.space.size by 3).toVector
    val selection =
      Selection
        .fromOrdinals(domain.space, selectedOrdinals)
        .fold(error => fail(error.message), identity)
    val selected =
      SelectedVolume
        .gather(domain, nativeVolume, selection)
        .fold(error => fail(error.message), identity)

    val centerOrdinal = domain.space.size / 2
    val center = domain.space.indexAtValidatedOrdinal(centerOrdinal)
    val centers =
      Region
        .fromOrdinals(domain.space, Vector(centerOrdinal))
        .fold(error => fail(error.message), identity)
    val allOrdinals = Array.tabulate(domain.space.size)(identity)
    val relation =
      Relation
        .fromOrdinalRows(
          domain.space,
          domain.space,
          Iterator.tabulate(domain.space.size): ordinal =>
            if ordinal == centerOrdinal then allOrdinals
            else Array.emptyIntArray
        )
        .fold(error => fail(error.message), identity)
    val searchlight =
      ExactVolumeSearchlight
        .fromRelation(centers, relation)
        .fold(error => fail(error.message), identity)
    val preparedSearchlight =
      ExactVolumeSearchlight
        .prepare(searchlight, center)
        .fold(error => fail(error.message), identity)

    val cases =
      Vector(
        Receipt(
          "map-view-safe",
          volume.values.size,
          requiredBytesPerSample = 8L,
          fixedAllowance = 192L * 1024L,
          () => retained = (volume + 1.0).sampled.asInstanceOf[AnyRef]
        ),
        Receipt(
          "zip-view-safe",
          volume.values.size,
          requiredBytesPerSample = 8L,
          fixedAllowance = 192L * 1024L,
          () =>
            retained = (volume - other)
              .sampled
              .asInstanceOf[AnyRef]
        ),
        Receipt(
          "temporal-reduction",
          series.space.spatialDims.product,
          requiredBytesPerSample = 8L,
          fixedAllowance = 192L * 1024L,
          () =>
            retained = NeuroStats
              .temporalMean(series)
              .sampled
              .asInstanceOf[AnyRef]
        ),
        Receipt(
          "linear-resampling",
          volume.values.size,
          requiredBytesPerSample = 8L,
          fixedAllowance = 192L * 1024L,
          () =>
            retained = resampling(volume)
              .fold(error => fail(error.message), identity)
              .sampled
              .asInstanceOf[AnyRef]
        ),
        Receipt(
          "ravel-stencil",
          volume.values.size,
          // image4s-filter executes the Ravel stencil into one mutable
          // primitive workspace, then freezes one immutable destination.
          requiredBytesPerSample = 16L,
          fixedAllowance = 256L * 1024L,
          () =>
            retained = SpatialFilters
              .gaussianBlur(volume, sigma = 0.8, window = 1)
              .sampled
              .asInstanceOf[AnyRef]
        ),
        Receipt(
          "dense-to-selected",
          selection.size,
          // Generic selected values cross one type-erased image4s-locus
          // callback. The budget includes its bounded primitive boxing plus
          // the sole retained eight-byte Ravel value destination.
          requiredBytesPerSample = 32L,
          fixedAllowance = 192L * 1024L,
          () =>
            retained = SelectedVolume
              .gather(domain, nativeVolume, selection)
              .fold(error => fail(error.message), identity)
              .selected
              .asInstanceOf[AnyRef]
        ),
        Receipt(
          "selected-to-dense",
          domain.space.size,
          requiredBytesPerSample = 16L,
          fixedAllowance = 192L * 1024L,
          () =>
            retained = selected
              .toDense(-1.0)
              .fold(error => fail(error.message), identity)
              .asInstanceOf[AnyRef]
        ),
        Receipt(
          "prepared-exact-searchlight",
          domain.space.size,
          requiredBytesPerSample = 32L,
          fixedAllowance = 192L * 1024L,
          () =>
            retained = ExactVolumeSearchlight
              .materializePreparedVolume(
                domain,
                preparedSearchlight,
                nativeVolume
              )
              .fold(error => fail(error.message), identity)
              .asInstanceOf[AnyRef]
        )
      )

    val observations =
      cases.map: receipt =>
        Vector.fill(4)(receipt.run())
        val allocated =
          Vector.fill(7)(allocatedBytes(receipt.run())).sorted.apply(3)
        val limit =
          receipt.samples.toLong * receipt.requiredBytesPerSample +
            receipt.fixedAllowance
        println(
          s"SCALAFIM-IMAGE JVM allocation: case=${receipt.name}, " +
            s"samples=${receipt.samples}, allocated=$allocated B, limit=$limit B"
        )
        (receipt, allocated, limit)

    observations.foreach: (receipt, allocated, limit) =>
      assert(
        allocated <= limit,
        s"${receipt.name} allocated $allocated bytes; limit=$limit"
      )

    assert(retained != null)

  private final case class Receipt(
      name: String,
      samples: Int,
      requiredBytesPerSample: Long,
      fixedAllowance: Long,
      run: () => Unit
  )

  @scala.annotation.nowarn("cat=deprecation")
  private def allocatedBytes(body: => Unit): Long =
    val bean =
      ManagementFactory.getThreadMXBean match
        case value: ThreadMXBean if value.isThreadAllocatedMemorySupported =>
          if !value.isThreadAllocatedMemoryEnabled then
            value.setThreadAllocatedMemoryEnabled(true)
          value
        case _ =>
          fail("thread allocation accounting is unavailable")
    val thread = Thread.currentThread().getId()
    val before = bean.getThreadAllocatedBytes(thread)
    body
    bean.getThreadAllocatedBytes(thread) - before
