package scalafim.surface.reference

import scalafim.image.*
import scalafim.surface.*
import scalafim.surface.io.GiftiSurfaceReader

import java.lang.management.ManagementFactory
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** WS5 real-data qualification runner: maps every exported PLSNeuro volume
  * (MNI152NLin2009cAsym res-2) onto fsLR 32k L and R through the admitted
  * route (declared midthickness in MNI152NLin6Asym, inverse point-map bridge,
  * MidthicknessNearest, Continuous) and writes the results for
  * `tools/fslr-qualification/compare_fslr_qualification.py`.
  *
  * Run: `sbt "surfaceJVM/Test/runMain scalafim.surface.reference.FslrQualification <pls-volumes> <out> [<export-script>]"`
  * with the locked TemplateFlow cache under `$TEMPLATEFLOW_HOME` or `~/.cache/templateflow`.
  *
  * Output per dataset `<out>/<dataset>/` (NumPy `.npy`, little-endian):
  *  - `world_<h>.npy` float64 (n, 3): placed vertex in 2009c RAS mm (pre-bridge position when unavailable);
  *  - `residual_<h>.npy` float64 (n): inverse residual mm (NaN when not converged);
  *  - `voxel_<h>_<k>.npy` int32 (n, 3): the voxel the lookup selected, (-1, -1, -1) outside the grid or unavailable;
  *  - `receipt_<h>_<k>.npy` int8 (n): inspect() contribution kind:
  *    0 Included, 1 OutsideGrid, 2 OutsideSupport, 3 NonFinite, -1 no lookup (bridge unavailable);
  *  - `coverage_<h>_<k>.npy` int8 (n): 0 Mapped, 1 MedialWall, 2 NoSupport, 3 BridgeUnavailable;
  *  - `value_<h>_<k>.npy` float64 (n): mapped value, NaN unless Mapped;
  *  - `results.json`: inputs and digests, volume order, display identity, picks, timings and heap.
  */
object FslrQualification:

  def main(args: Array[String]): Unit =
    if args.headOption.contains("--probe-heap") then probeHeap(Path.of(args(1)), args(2).toInt, Path.of(args(3)))
    else runAll(args)

  /** Peak-live-heap probe for prepare+map. Run with `-XX:+UseSerialGC -Xmn16m` so young collections are frequent:
    * between collections live heap can grow by at most the eden capacity, so
    * `max(heap used after any collection during the phase) - baseline + eden` bounds the additional live heap
    * from above. The baseline is the live heap after setup (point map, surfaces, routes, one volume).
    */
  private def probeHeap(volumeDir: Path, repeats: Int, artifact: Path): Unit =
    val policy = InversePolicy.make(1e-6, 50).toOption.get
    val bridge = FrameBridge.displacement(RealAssets.pointMap, PointMapUse.Inverse(policy)).toOption.get
    val exported = ujson.read(Files.readString(volumeDir.resolve("export.json")))
    val name = exported("outputs").obj.keys.toVector.sorted.head
    val basis = FrameBasis.derived("heap probe", Vector(DataAsset.make(exported("bundle").str, exported("bundleSha256").str).toOption.get))
      .toOption.get
    val declared = DeclaredVolumeReader.readNifti(volumeDir.resolve(name), FrameDeclaration.make(RealAssets.nlin2009c, basis,
      DataAsset.make(name, exported("outputs")(name).str).toOption.get).toOption.get).fold(e => sys.error(e.message), identity)
    val source = VolumeReference.make(RealAssets.nlin2009c, declared.volume.space).toOption.get
    val routes = RealAssets.hemispheres.map: h =>
      SurfaceRoute.admit(RouteRequest(source, StandardCorticalMesh.FsLR32k, h.reference.hemisphere,
        MappingMethod.MidthicknessNearest, ValueSemantics.Continuous),
        SamplingAnatomy.make(h.reference, AnatomicalGeometry.Midthickness(h.surface)).toOption.get, Some(bridge))
        .fold(r => sys.error(r.message), identity)
    import com.sun.management.GarbageCollectionNotificationInfo
    import javax.management.{NotificationEmitter, NotificationListener}
    import javax.management.openmbean.CompositeData
    val heapPools = ManagementFactory.getMemoryPoolMXBeans.asScala.filter(_.getType == java.lang.management.MemoryType.HEAP)
      .map(_.getName).toSet
    @volatile var maxAfter = 0L
    @volatile var collections = 0
    val listener: NotificationListener = (notification, _) =>
      if notification.getType == GarbageCollectionNotificationInfo.GARBAGE_COLLECTION_NOTIFICATION then
        val info = GarbageCollectionNotificationInfo.from(notification.getUserData.asInstanceOf[CompositeData])
        val after = info.getGcInfo.getMemoryUsageAfterGc.asScala.collect { case (name, usage) if heapPools(name) => usage.getUsed }.sum
        collections += 1
        if after > maxAfter then maxAfter = after
    System.gc()
    System.gc()
    val runtime = Runtime.getRuntime
    val baseline = runtime.totalMemory - runtime.freeMemory
    val eden = ManagementFactory.getMemoryPoolMXBeans.asScala.find(_.getName.toLowerCase.contains("eden")).map(_.getUsage.getMax).getOrElse(-1L)
    val emitters = ManagementFactory.getGarbageCollectorMXBeans.asScala.map(_.asInstanceOf[NotificationEmitter])
    emitters.foreach(_.addNotificationListener(listener, null, null))
    for _ <- 0 until repeats; route <- routes do route.map(declared).fold(e => sys.error(e.message), identity)
    emitters.foreach(_.removeNotificationListener(listener))
    // Sampled live heap: a watchdog forces full collections at arbitrary points while prepare+map runs
    // continuously; each post-collection heap is the live heap at that instant.
    @volatile var running = true
    var samples = Vector.empty[Long]
    val watchdog = new Thread(() =>
      while running do
        System.gc()
        samples :+= runtime.totalMemory - runtime.freeMemory
        Thread.sleep(3)
    )
    watchdog.start()
    val deadline = System.nanoTime() + 20_000_000_000L
    var runs = 0
    while System.nanoTime() < deadline do
      routes.foreach(_.map(declared).fold(e => sys.error(e.message), identity))
      runs += 1
    running = false
    watchdog.join()
    def mib(bytes: Long) = bytes.toDouble / (1024 * 1024)
    val summary = ujson.Obj(
      "schema" -> "scalafim.fslr-heap-probe/1",
      "volume" -> name,
      "volumeSha256" -> declared.declaration.asset.sha256.value,
      "jvm" -> ujson.Obj("version" -> sys.props("java.version"),
        "inputArguments" -> ujson.Arr.from(ManagementFactory.getRuntimeMXBean.getInputArguments.asScala.map(ujson.Str(_)))),
      "baselineMiB" -> mib(baseline),
      "edenMiB" -> mib(eden),
      "youngCollectionPhase" -> ujson.Obj("runs" -> repeats * routes.size, "collections" -> collections,
        "maxAfterCollectionMiB" -> mib(maxAfter), "boundMiB" -> mib(maxAfter - baseline + eden),
        "note" -> "includes promoted garbage; not a live-heap measure"),
      "sampledPhase" -> ujson.Obj("runs" -> runs * routes.size, "fullCollectionSamples" -> samples.size,
        "maxAboveBaselineMiB" -> mib(samples.max - baseline),
        "medianAboveBaselineMiB" -> mib(samples.sorted.apply(samples.size / 2) - baseline),
        "note" -> "live heap at forced full collections at arbitrary points; sampled, not a bound"))
    Files.writeString(artifact, ujson.write(summary, indent = 2), StandardCharsets.UTF_8)
    println(ujson.write(summary))

  private def runAll(args: Array[String]): Unit =
    require(args.length >= 2, "usage: FslrQualification <pls-volumes> <out> [<export-script>]")
    val input = Path.of(args(0))
    val out = Path.of(args(1))
    val script = Path.of(if args.length > 2 then args(2) else "tools/fslr-qualification/pls_bundle_to_nifti.py")
    val scriptAsset = DataAsset.make("tools/fslr-qualification/pls_bundle_to_nifti.py",
      AssetSha256.of(Files.readAllBytes(script)).value).toOption.get
    val policy = InversePolicy.make(1e-6, 50).toOption.get
    val bridge = FrameBridge.displacement(RealAssets.pointMap, PointMapUse.Inverse(policy)).toOption.get
    for dataset <- Vector("beta", "fir") if Files.isDirectory(input.resolve(dataset)) do
      run(dataset, input.resolve(dataset), out.resolve(dataset), scriptAsset, bridge)

  private def run(dataset: String, dir: Path, out: Path, scriptAsset: DataAsset, bridge: FrameBridge): Unit =
    Files.createDirectories(out)
    val exported = ujson.read(Files.readString(dir.resolve("export.json")))
    val bundle = DataAsset.make(exported("bundle").str, exported("bundleSha256").str).toOption.get
    val basis = FrameBasis.derived(
      "plsneuro task-PLS brain direction export (tools/fslr-qualification/pls_bundle_to_nifti.py)",
      Vector(bundle, scriptAsset)).toOption.get
    val names = exported("outputs").obj.keys.toVector.sorted
    val volumes = names.map: name =>
      val declaration = FrameDeclaration.make(RealAssets.nlin2009c, basis,
        DataAsset.make(name, exported("outputs")(name).str).toOption.get).toOption.get
      DeclaredVolumeReader.readNifti(dir.resolve(name), declaration).fold(e => sys.error(e.message), identity)
    val affine = exported("affine").arr.map(_.num).toVector
    val space = volumes.head.volume.space
    require(space.grid.shape == exported("dimensions").arr.map(_.num.toInt).toVector, "grid dims differ from export.json")
    require(volumes.head.volume.grid.indexToFrame.rowMajor == affine, "grid affine differs from export.json")
    val source = VolumeReference.make(RealAssets.nlin2009c, space).toOption.get
    val volumeReport = ujson.Arr()
    val hemiReport = ujson.Obj()

    for h <- RealAssets.hemispheres do
      val n = h.surface.geometry.vertexCount
      val request = RouteRequest(source, StandardCorticalMesh.FsLR32k, h.reference.hemisphere,
        MappingMethod.MidthicknessNearest, ValueSemantics.Continuous)
      val anatomy = SamplingAnatomy.make(h.reference, AnatomicalGeometry.Midthickness(h.surface)).toOption.get
      def admit() = SurfaceRoute.admit(request, anatomy, Some(bridge)).fold(r => sys.error(r.message), identity)
      val admitCold = timed(admit())._2
      val admitWarm = Vector.fill(5)(timed(admit())._2)
      val route = admit()
      val placement = route.bridgePlacement.get

      // Placement and residual (volume-independent), then per-vertex receipts and chosen voxels for every volume.
      val prepared0 = route.prepare(volumes.head).fold(e => sys.error(e.message), identity)
      val world = new Array[Double](3 * n)
      val residual = Array.fill(n)(Double.NaN)
      for i <- 0 until n do
        placement.outcomesAt(i).head match
          case PointMapOutcome.Converged(p, r, _) =>
            residual(i) = r
            world(3 * i) = p.x; world(3 * i + 1) = p.y; world(3 * i + 2) = p.z
          case _ =>
            val p = h.surface.geometry.mesh.vertex(VertexId(i))
            world(3 * i) = p.x; world(3 * i + 1) = p.y; world(3 * i + 2) = p.z
      writeDoubles(out.resolve(s"world_${h.label}.npy"), world, Vector(n, 3))
      writeDoubles(out.resolve(s"residual_${h.label}.npy"), residual, Vector(n))
      for (declared, k) <- volumes.zipWithIndex do
        val prepared = route.prepare(declared).fold(e => sys.error(e.message), identity)
        val voxel = Array.fill(3 * n)(-1)
        val receipt = Array.fill[Byte](n)(-1)
        for i <- 0 until n do
          val evidence = route.inspect(prepared, VertexId(i)).fold(e => sys.error(e.message), identity)
          evidence.samples.headOption.foreach: sample =>
            val (kind, v) = sample.contribution match
              case Contribution.Included(v, _, _) => (0, Some(v))
              case Contribution.OutsideGrid => (1, None)
              case Contribution.OutsideSupport(v) => (2, Some(v))
              case Contribution.NonFinite(v, _) => (3, Some(v))
            receipt(i) = kind.toByte
            v.foreach: c =>
              voxel(3 * i) = c.x; voxel(3 * i + 1) = c.y; voxel(3 * i + 2) = c.z
        writeInts(out.resolve(s"voxel_${h.label}_$k.npy"), voxel, Vector(n, 3))
        writeBytes(out.resolve(s"receipt_${h.label}_$k.npy"), receipt, Vector(n))

      val timings = ujson.Arr()
      for (declared, k) <- volumes.zipWithIndex do
        val (mapped, _) = timed(route.map(declared).fold(e => sys.error(e.message), identity))
        writeDoubles(out.resolve(s"value_${h.label}_$k.npy"), Array.tabulate(n)(i => mapped.valueAt(VertexId(i)).getOrElse(Double.NaN)),
          Vector(n))
        writeBytes(out.resolve(s"coverage_${h.label}_$k.npy"), Array.tabulate(n) { i =>
          mapped.coverageAt(VertexId(i)).get match
            case VertexCoverage.Mapped => 0.toByte
            case VertexCoverage.MedialWall => 1.toByte
            case VertexCoverage.NoSupport => 2.toByte
            case VertexCoverage.BridgeUnavailable => 3.toByte
        }, Vector(n))
        // Warm timing: prepare (admission mask over the whole grid) and map (kernel) separately.
        for _ <- 0 until 3 do route.map(route.prepare(declared).toOption.get)
        val prepareMs = Vector.fill(7)(timed(route.prepare(declared).toOption.get)._2)
        val p = route.prepare(declared).toOption.get
        val mapMs = Vector.fill(7)(timed(route.map(p).toOption.get)._2)
        val totalMs = Vector.fill(7)(timed(route.map(declared).toOption.get)._2)
        timings.arr += ujson.Obj("volume" -> names(k), "prepareMedianMs" -> median(prepareMs), "mapMedianMs" -> median(mapMs),
          "prepareAndMapMedianMs" -> median(totalMs), "prepareAndMapMaxMs" -> totalMs.max)
      // Memory after all timing, so explicit collections do not perturb the timed runs.
      for (declared, k) <- volumes.zipWithIndex do
        val entry = timings.arr(k).obj
        entry("allocatedMiB") = allocatedMiB(route.map(declared).toOption.get)
        entry("retainedMiB") = retainedMiB(route.map(declared).toOption.get)
        entry("poolPeakAboveBaselineMiB") = peakHeapMiB(route.map(declared).toOption.get)

      // Display identity: inflated and very-inflated shapes of the same mesh reference carry the same object.
      val mapped0 = route.map(prepared0).toOption.get
      val displays = Vector("inflated" -> SurfaceKind.Inflated, "veryinflated" -> SurfaceKind.Custom("very_inflated")).map: (form, kind) =>
        val path = RealAssets.root.resolve(s"tpl-fsLR/tpl-fsLR_den-32k_hemi-${h.label}_$form.surf.gii")
        val geometry = GiftiSurfaceReader.readEither(path, h.surface.geometry.hemisphere, kind).fold(e => sys.error(e.message), identity)
        val display = DisplaySurface.make(h.reference, geometry).fold(e => sys.error(e.message), identity)
        val shown = mapped0.onDisplay(display).fold(e => sys.error(e.message), identity)
        ujson.Obj("form" -> form, "sha256" -> AssetSha256.of(Files.readAllBytes(path)).value,
          "sameObject" -> (shown.mapped eq mapped0), "displayForm" -> display.form.label)

      // Twenty deterministic cortical picks: evenly spaced ranks over cortical vertex ids.
      val cortical = (0 until n).filter(h.cortex).toVector
      val picks = ujson.Arr.from((0 until 20).map { j =>
        val v = cortical(j * cortical.size / 20)
        ujson.Obj("vertex" -> v, "volumes" -> ujson.Arr.from(volumes.zipWithIndex.map { (declared, k) =>
          val e = route.inspect(route.prepare(declared).toOption.get, VertexId(v)).toOption.get
          ujson.Obj("volume" -> k, "coverage" -> e.coverage.toString,
            "value" -> e.value.fold[ujson.Value](ujson.Null)(ujson.Num(_)),
            "bridge" -> e.bridge.map(_.toString).mkString,
            "samples" -> ujson.Arr.from(e.samples.map { s =>
              val (kind, voxel, value) = s.contribution match
                case Contribution.Included(c, x, _) => ("Included", Some(c), Some(x))
                case Contribution.OutsideGrid => ("OutsideGrid", None, None)
                case Contribution.OutsideSupport(c) => ("OutsideSupport", Some(c), None)
                case Contribution.NonFinite(c, x) => ("NonFinite", Some(c), Some(x))
              ujson.Obj("world" -> ujson.Arr(s.world.x, s.world.y, s.world.z), "kind" -> kind,
                "voxel" -> voxel.fold[ujson.Value](ujson.Null)(c => ujson.Arr(c.x, c.y, c.z)),
                "sourceValue" -> value.fold[ujson.Value](ujson.Null)(x => if x.isFinite then ujson.Num(x) else ujson.Str(x.toString)))
            }))
        }))
      })

      hemiReport(h.label) = ujson.Obj(
        "vertices" -> n,
        "cortex" -> cortical.size,
        "bridgeUnavailable" -> placement.unavailableCount,
        "admissionColdMs" -> admitCold,
        "admissionWarmMedianMs" -> median(admitWarm),
        "volumes" -> timings,
        "display" -> ujson.Arr.from(displays),
        "picks" -> picks)

    for (name, declared) <- names.zip(volumes) do
      volumeReport.arr += ujson.Obj("file" -> name, "sha256" -> declared.declaration.asset.sha256.value,
        "declaration" -> declared.declaration.display)
    val results = ujson.Obj(
      "schema" -> "scalafim.fslr-qualification/1",
      "dataset" -> dataset,
      "bundleSha256" -> exported("bundleSha256").str,
      "exportScriptSha256" -> scriptAsset.sha256.value,
      "bridge" -> bridge.display,
      "pointMapManifestSha256" -> RealAssets.pointMap.manifest.sha256.value,
      "volumes" -> volumeReport,
      "hemispheres" -> hemiReport,
      "jvm" -> ujson.Obj("version" -> sys.props("java.version"), "maxHeapMiB" -> Runtime.getRuntime.maxMemory / (1024 * 1024)))
    Files.writeString(out.resolve("results.json"), ujson.write(results, indent = 2), StandardCharsets.UTF_8)
    println(s"$dataset: wrote ${out.toAbsolutePath}")

  private def timed[A](body: => A): (A, Double) =
    val start = System.nanoTime()
    val result = body
    (result, (System.nanoTime() - start) / 1e6)

  private def median(values: Vector[Double]): Double = values.sorted.apply(values.size / 2)

  /** Bytes allocated by this thread while `body` runs (transient churn, including garbage). */
  private def allocatedMiB[A](body: => A): Double =
    val threads = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
    val id = Thread.currentThread().threadId()
    val before = threads.getThreadAllocatedBytes(id)
    body
    (threads.getThreadAllocatedBytes(id) - before).toDouble / (1024 * 1024)

  /** Live heap retained by the result of `body` after a full GC, relative to a post-GC baseline. */
  private def retainedMiB[A](body: => A): Double =
    def used() =
      System.gc()
      System.gc()
      Runtime.getRuntime.totalMemory - Runtime.getRuntime.freeMemory
    val baseline = used()
    val result = body
    val after = used()
    java.lang.ref.Reference.reachabilityFence(result)
    (after - baseline).toDouble / (1024 * 1024)

  /** Heap-pool peak usage (MiB) above the post-GC baseline while `body` runs. It includes garbage not yet
    * collected (young-generation fill), so it bounds peak live heap from above, loosely.
    */
  private def peakHeapMiB[A](body: => A): Double =
    val pools = ManagementFactory.getMemoryPoolMXBeans.asScala.filter(_.getType == java.lang.management.MemoryType.HEAP)
    System.gc()
    val baseline = pools.map(_.getUsage.getUsed).sum
    pools.foreach(_.resetPeakUsage())
    body
    val peak = pools.map(_.getPeakUsage.getUsed).sum
    (peak - baseline).toDouble / (1024 * 1024)

  private def npy(path: Path, descr: String, shape: Vector[Int], payload: ByteBuffer): Unit =
    val dims = if shape.size == 1 then s"(${shape.head},)" else shape.mkString("(", ", ", ")")
    val dict = s"{'descr': '$descr', 'fortran_order': False, 'shape': $dims, }"
    val unpadded = 10 + dict.length + 1
    val header = dict + " " * ((64 - unpadded % 64) % 64) + "\n"
    val prefix = ByteBuffer.allocate(10).order(ByteOrder.LITTLE_ENDIAN)
    prefix.put(0x93.toByte).put("NUMPY".getBytes(StandardCharsets.US_ASCII)).put(1.toByte).put(0.toByte).putShort(header.length.toShort)
    Files.write(path, prefix.array() ++ header.getBytes(StandardCharsets.US_ASCII) ++ payload.array())

  private def writeDoubles(path: Path, values: Array[Double], shape: Vector[Int]): Unit =
    val b = ByteBuffer.allocate(8 * values.length).order(ByteOrder.LITTLE_ENDIAN)
    values.foreach(b.putDouble)
    npy(path, "<f8", shape, b)

  private def writeInts(path: Path, values: Array[Int], shape: Vector[Int]): Unit =
    val b = ByteBuffer.allocate(4 * values.length).order(ByteOrder.LITTLE_ENDIAN)
    values.foreach(b.putInt)
    npy(path, "<i4", shape, b)

  private def writeBytes(path: Path, values: Array[Byte], shape: Vector[Int]): Unit =
    npy(path, "|i1", shape, ByteBuffer.wrap(values))
