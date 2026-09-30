package scalafim.archive.hdf5

import hdf.hdf5lib.H5
import hdf.hdf5lib.HDF5Constants.*
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scala.util.control.NonFatal

/** Explicit local macOS ARM64/JDK25 capability. No eager H5 or constant initialization. */
object JvmHdf5JniAdapter:
  private val provider = "HDFGroup official JNI 2.2.0"
  private def missing(detail: String): Hdf5Error =
    Hdf5Error.MissingCapability(provider, s"${System.getProperty("os.name")}/${System.getProperty("os.arch")}", System.getProperty("java.version"), detail)

  private def hash(path: Path): String =
    val digest = MessageDigest.getInstance("SHA-256")
    val stream = Files.newInputStream(path)
    try
      val buffer = new Array[Byte](1048576)
      var n = stream.read(buffer)
      while n >= 0 do
        if n > 0 then digest.update(buffer, 0, n)
        n = stream.read(buffer)
    finally stream.close()
    digest.digest().map(b => f"${b & 0xff}%02x").mkString

  /** These pins are the accepted provider-lock.json closure, including retained packaging bytes. */
  private[hdf5] def validateProvider(): Either[Hdf5Error, Path] =
    try
      val property = Option(System.getProperty("scalafim.hdf5.provider.dir")).filter(_.nonEmpty)
      if property.isEmpty then return Left(missing("explicit provider.dir property absent"))
      val root = Path.of(property.get).toRealPath()
      if System.getProperty("os.name") != "Mac OS X" || !Set("aarch64", "arm64").contains(System.getProperty("os.arch")) then
        return Left(missing("only macOS ARM64 admitted"))
      if Runtime.version().feature() != 25 then return Left(missing("only actual JDK25 runtime admitted; JDK21 unrun"))
      val lib = root.resolve("provider/HDF5-2.2.0-Darwin/HDF_Group/HDF5/2.2.0/lib").toRealPath()
      def verify(path: Path, bytes: Long, sha: String): Unit =
        if !Files.isRegularFile(path) || Files.size(path) != bytes || hash(path) != sha then
          throw new IllegalArgumentException(s"locked artifact mismatch: $path")
      val archive = "hdf5-2.2.0-macos15_clang.tar.gz"
      val sha = "7402939a854b643022e239dfa544a200c048b798bb0861890010db48a72efc73"
      verify(root.resolve(archive), 34803158, sha)
      if !Files.readString(root.resolve("hdf5-2.2.0.sha256sums.txt")).contains(s"$sha  $archive") ||
          !Files.readString(root.resolve("release.json")).contains(s"https://github.com/HDFGroup/hdf5/releases/download/2.2.0/$archive") then
        throw new IllegalArgumentException("official archive/publisher metadata mismatch")
      verify(root.resolve("outer/hdf5/HDF5-2.2.0-Darwin.tar.gz"), 36490631,
        "6df762cc48394584e86fade25e972189d3706a9370923f3ccc968de3b5ee5543")
      Vector(
        ("jarhdf5-2.2.0.jar", 86209L, "8a561afd611d95ef2167195eaf89b43f2f96f517690280094d7ffcde306a3895"),
        ("slf4j-api-2.0.16.jar", 69435L, "a12578dde1ba00bd9b816d388a0b879928d00bab3c83c240f7013bf4196c579a"),
        ("slf4j-nop-2.0.16.jar", 4982L, "deca6c04ed35515a0a911fa44c0e836bee92c0c59d2e8fa9bab8ffbc464a9ba7"),
        ("libhdf5_java.dylib", 1236544L, "0a67221b6e1617aef522dcf9fc3aad1c27f424fbc4844087efcab560adb3a93c"),
        ("libhdf5.320.2.0.dylib", 10592048L, "9c681586a5fee10ad7efaf49bab058c38dfc9119aa5d55a78c04e476962ebf8e"),
        ("libhdf5.320.dylib", 10592048L, "9c681586a5fee10ad7efaf49bab058c38dfc9119aa5d55a78c04e476962ebf8e"),
        ("libhdf5.settings", 4248L, "732d3e1857023a3d9e505966fd11192d49c2a7805250ae6d71a5d75a2292d642")
      ).foreach((n, size, digest) => verify(lib.resolve(n), size, digest))
      if System.getProperty("java.library.path") != lib.toString ||
          Option(System.getProperty("hdf.hdf5lib.H5.loadLibraryName")).nonEmpty ||
          Option(System.getProperty("hdf.hdf5lib.H5.hdf5lib")).nonEmpty ||
          Option(System.getenv("HDF5_PLUGIN_PRELOAD")) != Some("::") then
        return Left(missing("explicit locked java.library.path, no H5 loader overrides, and HDF5_PLUGIN_PRELOAD=:: required"))
      // Resolving a class literal does not initialize H5. Reject a substituted jar before H5 calls.
      val loadedJar = Path.of(classOf[H5].getProtectionDomain.getCodeSource.getLocation.toURI).toRealPath()
      if loadedJar != lib.resolve("jarhdf5-2.2.0.jar").toRealPath() then return Left(missing("H5 classpath differs from locked jar"))
      Right(lib)
    catch
      case e: LinkageError => Left(missing(e.toString))
      case NonFatal(e) => Left(missing(e.toString))

  def open(limits: Hdf5Limits = Hdf5Limits.bounded): Either[Hdf5Error, Hdf5Archive] = Owner.synchronized:
    if limits == null then Left(missing("limits absent"))
    else validateProvider().flatMap: lib =>
      try
        if Owner.loadedPath.exists(_ != lib) then Left(missing("another native closure already admitted in this process"))
        else
          val version = new Array[Int](3)
          H5.H5get_libversion(version)
          if !version.sameElements(Array(2, 2, 0)) || H5.H5Zfilter_avail(H5Z_FILTER_DEFLATE) != 1 ||
              (H5.H5Zget_filter_info(H5Z_FILTER_DEFLATE) & 3) != 3 then Left(missing("version or DEFLATE encode/decode differs"))
          else
            Owner.loadedPath = Some(lib)
            Right(new Adapter(limits))
      catch
        case e: LinkageError => Left(missing(e.toString))
        case NonFatal(e) => Left(missing(e.toString))

  private class Refusal(val error: Hdf5Error) extends RuntimeException(error.message)
  private def refuse(error: Hdf5Error): Nothing = throw new Refusal(error)
  private def get[A](result: Either[Hdf5Error, A]): A = result.fold(refuse, identity)
  private def error(operation: String, e: Throwable): Hdf5Error = e match
    case r: Refusal => r.error
    case _: hdf.hdf5lib.exceptions.HDF5LibraryException => Hdf5Error.NativeFailure(operation, s"$e; ${nativeDetails()}")
    case _ => Hdf5Error.NativeFailure(operation, e.toString)

  private def nativeDetails(): String =
    require(Thread.holdsLock(Owner))
    val text = new StringBuilder
    try
      H5.H5Ewalk2(H5E_DEFAULT, H5E_WALK_DOWNWARD, (_, entry, _) =>
        val remaining = 8192 - text.length
        if remaining > 0 then text.append(entry.desc.take(remaining)).append('\n')
        0
      , new hdf.hdf5lib.callbacks.H5E_walk_t {})
      text.toString.take(8192)
    catch case NonFatal(e) => s"error-stack capture failed: $e"

  private enum Kind:
    case File, Dataset, Space, Property, CopiedType

  /** One singleton covers every adapter, file, dataset, loader and close, never just one handle. */
  private object Owner:
    var loadedPath: Option[Path] = None
    var files = 0
    var datasets = 0
    var ids = 0
    var peakIds = 0
    var attempts = 0L
    var calls = 0L
    var successes = 0L
    var elements = 0L
    var bytes = 0L
    var failures = 0L
    var metadataPeak = 0L
    var heapPeak = 0L
    var acquisition = 0
    var failAt = -1

  // Injection is package-private test instrumentation; not a configuration or public capability.
  private[hdf5] def failAcquisitionAt(n: Int): Unit = Owner.synchronized:
    Owner.acquisition = 0
    Owner.failAt = n

  private final class Id(val value: Long, kind: Kind):
    var closed = false
    def close(): Unit =
      require(Thread.holdsLock(Owner))
      if !closed then
        kind match
          case Kind.File => H5.H5Fclose(value)
          case Kind.Dataset => H5.H5Dclose(value)
          case Kind.Space => H5.H5Sclose(value)
          case Kind.Property => H5.H5Pclose(value)
          case Kind.CopiedType => H5.H5Tclose(value)
        closed = true
        Owner.ids -= 1

  private final class Scope(limits: Hdf5Limits):
    private var stack = List.empty[Id]
    private var retained = Set.empty[Id]
    def own(kind: Kind)(acquire: => Long): Id =
      if Owner.ids >= limits.maxNativeIds then refuse(Hdf5Error.ResourceLimit("native IDs"))
      val id = new Id(acquire, kind)
      stack = id :: stack // Register before any injection or subsequent operation.
      Owner.ids += 1
      Owner.peakIds = math.max(Owner.peakIds, Owner.ids)
      Owner.acquisition += 1
      if Owner.acquisition == Owner.failAt then refuse(Hdf5Error.NativeFailure("acquire", s"injected acquisition ${Owner.acquisition}"))
      id
    def keep(id: Id): Unit = retained += id
    def close(all: Boolean): Vector[Hdf5Error] =
      stack.filter(id => all || !retained.contains(id)).flatMap: id =>
        try
          id.close()
          None
        catch case NonFatal(e) => Some(error("close", e))
      .toVector

  private def scoped[A](limits: Hdf5Limits)(body: Scope => A): A =
    val s = new Scope(limits)
    var primary: Option[Hdf5Error] = None
    val result = try Some(body(s)) catch
      case NonFatal(e) =>
        primary = Some(error("native", e))
        None
    val cleanup = s.close(all = primary.nonEmpty)
    if primary.nonEmpty || cleanup.nonEmpty then
      val retainedCleanup = if primary.isEmpty then s.close(all = true) else Vector.empty
      val p = primary.getOrElse(cleanup.head)
      val rest = if primary.nonEmpty then cleanup else cleanup.tail ++ retainedCleanup
      if rest.isEmpty then refuse(p)
      else refuse(Hdf5Error.ScopeFailure(p, rest))
    result.get

  private def disk(dtype: Hdf5DType): Long = dtype match
    case Hdf5DType.Float32 => H5T_IEEE_F32LE
    case Hdf5DType.Float64 => H5T_IEEE_F64LE
    case Hdf5DType.UInt8 => H5T_STD_U8LE
  private def memory(dtype: Hdf5DType): Long = dtype match
    case Hdf5DType.Float32 => H5T_NATIVE_FLOAT
    case Hdf5DType.Float64 => H5T_NATIVE_DOUBLE
    case Hdf5DType.UInt8 => H5T_NATIVE_UCHAR

  private final class Adapter(val limits: Hdf5Limits) extends Hdf5Archive:
    private def guarded[A](operation: String)(body: => A): Either[Hdf5Error, A] = Owner.synchronized:
      Owner.attempts = get(Checked.add(Owner.attempts, 1, "attempt counter"))
      try Right(body)
      catch
        case e: LinkageError =>
          Owner.failures += 1
          Left(missing(e.toString))
        case NonFatal(e) =>
          Owner.failures += 1
          Left(error(operation, e))

    def receipt: Hdf5Receipt = Owner.synchronized:
      Hdf5Receipt(Owner.attempts, Owner.calls, Owner.successes, Owner.elements, Owner.bytes,
        Owner.failures, Owner.files, Owner.datasets, Owner.ids, Owner.peakIds,
        Owner.loadedPath.map(_ => H5.getOpenIDCount()), Owner.metadataPeak, Owner.heapPeak, limits)

    private def fapl(scope: Scope): Id =
      val p = scope.own(Kind.Property)(H5.H5Pcreate(H5P_FILE_ACCESS))
      H5.H5Pset_cache(p.value, 0, limits.rawCacheSlots, limits.rawCacheBytes, 0.75)
      val cfg = H5.H5Pget_mdc_config(p.value)
      cfg.set_initial_size = true
      cfg.initial_size = 1048576
      cfg.min_size = 1048576
      cfg.max_size = limits.metadataCacheBytes
      cfg.incr_mode = 0
      cfg.flash_incr_mode = 0
      cfg.decr_mode = 0
      H5.H5Pset_mdc_config(p.value, cfg)
      if H5.H5Pget_mdc_config(p.value).max_size != limits.metadataCacheBytes then
        refuse(Hdf5Error.NativeFailure("cache", "metadata cap readback"))
      p

    private def dapl(scope: Scope): Id =
      val p = scope.own(Kind.Property)(H5.H5Pcreate(H5P_DATASET_ACCESS))
      H5.H5Pset_chunk_cache(p.value, limits.rawCacheSlots, limits.rawCacheBytes, 0.75)
      val slots = new Array[Long](1)
      val bytes = new Array[Long](1)
      val weight = new Array[Double](1)
      H5.H5Pget_chunk_cache(p.value, slots, bytes, weight)
      if slots(0) != limits.rawCacheSlots || bytes(0) != limits.rawCacheBytes then
        refuse(Hdf5Error.NativeFailure("cache", "raw cap readback"))
      p

    private def openFile(path: String, create: Boolean): Either[Hdf5Error, Hdf5File] = guarded("open file"):
      if path == null || path.isEmpty || path.indexOf('\u0000') >= 0 then refuse(Hdf5Error.InvalidName("file path"))
      val target = Path.of(path)
      if create && Files.exists(target) then refuse(Hdf5Error.AlreadyExists(path))
      if Owner.files >= limits.maxFiles then refuse(Hdf5Error.ResourceLimit("open files"))
      val file = scoped(limits): scope =>
        val access = fapl(scope)
        val id = scope.own(Kind.File):
          if create then
            try H5.H5Fcreate(path, H5F_ACC_EXCL, H5P_DEFAULT, access.value)
            catch case e: hdf.hdf5lib.exceptions.HDF5LibraryException =>
              val details = nativeDetails()
              if details.matches("(?s).*errno\\s*=\\s*17\\b.*") && details.contains("File exists") then
                refuse(Hdf5Error.AlreadyExists(path))
              else refuse(Hdf5Error.NativeFailure("exclusive create", s"$e; $details"))
          else H5.H5Fopen(path, H5F_ACC_RDONLY, access.value)
        scope.keep(id)
        id
      Owner.files += 1
      new NativeFile(file, create)

    def createExclusive(path: String): Either[Hdf5Error, Hdf5File] = openFile(path, true)
    def openReadOnly(path: String): Either[Hdf5Error, Hdf5File] = openFile(path, false)

    private final class NativeFile(file: Id, writable: Boolean) extends Hdf5File:
      private var children = List.empty[NativeDataset]
      private def live(): Unit = if file.closed then refuse(Hdf5Error.Closed("file"))
      private def cap(): Unit =
        if children.size >= limits.maxDatasetsPerFile then refuse(Hdf5Error.ResourceLimit("datasets per file"))
      private def attach(id: Id, info: Hdf5DatasetInfo): NativeDataset =
        val dataset = new NativeDataset(id, info)
        children = dataset :: children
        Owner.datasets += 1
        dataset

      def create(info: Hdf5DatasetInfo): Either[Hdf5Error, Hdf5Dataset] = guarded("create dataset"):
        live()
        if !writable then refuse(Hdf5Error.UnsupportedStored("read-only file"))
        get(Hdf5Plan.dataset(info, limits))
        cap()
        if H5.H5Lexists(file.value, info.name.value, H5P_DEFAULT) then refuse(Hdf5Error.DuplicateDataset(info.name.value))
        info.filter match
          case Hdf5Filter.Deflate(_) =>
            if H5.H5Zfilter_avail(H5Z_FILTER_DEFLATE) != 1 || (H5.H5Zget_filter_info(H5Z_FILTER_DEFLATE) & 3) != 3 then
              refuse(Hdf5Error.UnsupportedStored("DEFLATE capability"))
          case Hdf5Filter.None => ()
        val id = scoped(limits): scope =>
          val space = scope.own(Kind.Space)(H5.H5Screate_simple(info.extent.rank, info.extent.dimensions.toArray, null))
          val creation = scope.own(Kind.Property)(H5.H5Pcreate(H5P_DATASET_CREATE))
          H5.H5Pset_chunk(creation.value, info.chunks.rank, info.chunks.dimensions.toArray)
          info.filter match
            case Hdf5Filter.Deflate(level) => H5.H5Pset_deflate(creation.value, level)
            case Hdf5Filter.None => ()
          val access = dapl(scope)
          val d = scope.own(Kind.Dataset)(H5.H5Dcreate(file.value, info.name.value, disk(info.dtype), space.value, H5P_DEFAULT, creation.value, access.value))
          scope.keep(d)
          d
        attach(id, info)

      override def verifyFlatInventory(inventory: Hdf5FlatInventory): Either[Hdf5Error, Unit] = guarded("flat inventory"):
        live()
        if inventory == null then refuse(Hdf5Error.InvalidName("null flat inventory"))
        val root = H5.H5Gget_info_by_name(file.value, "/", H5P_DEFAULT)
        if root == null || root.mounted || root.nlinks != inventory.expected.size.toLong then
          refuse(Hdf5Error.UnsupportedStored("exact unmounted flat root link count required"))
        // Only validated expected names reach JNI. No enumeration, traversal or link following.
        for name <- inventory.expected do
          if !H5.H5Lexists(file.value, name.value, H5P_DEFAULT) then
            refuse(Hdf5Error.UnsupportedStored("expected root link absent"))
          val link = H5.H5Lget_info(file.value, name.value, H5P_DEFAULT)
          if link == null || link.`type` != H5L_TYPE_HARD then
            refuse(Hdf5Error.UnsupportedStored("hard root dataset link required"))
          val obj = H5.H5Oget_info_by_name(file.value, name.value, H5O_INFO_BASIC, H5P_DEFAULT)
          if obj == null || obj.`type` != H5O_TYPE_DATASET || obj.rc != 1 then
            refuse(Hdf5Error.UnsupportedStored("singly linked root dataset required"))

      def inspect(name: Hdf5DatasetName): Either[Hdf5Error, Hdf5Dataset] = guarded("inspect dataset"):
        live()
        get(Hdf5DatasetName(name.value))
        cap()
        val pair = scoped(limits): scope =>
          val access = dapl(scope)
          val d = scope.own(Kind.Dataset)(H5.H5Dopen(file.value, name.value, access.value))
          val t = scope.own(Kind.CopiedType)(H5.H5Dget_type(d.value))
          val dtype = Hdf5DType.values.find(dt => H5.H5Tequal(t.value, disk(dt)))
            .getOrElse(refuse(Hdf5Error.UnsupportedStored("exact LE F32/F64/U8 only")))
          val space = scope.own(Kind.Space)(H5.H5Dget_space(d.value))
          val rank = H5.H5Sget_simple_extent_ndims(space.value)
          if rank < 1 || rank > 3 then refuse(Hdf5Error.UnsupportedStored("stored rank 1..3"))
          val shape = new Array[Long](rank)
          val maximum = new Array[Long](rank)
          H5.H5Sget_simple_extent_dims(space.value, shape, maximum)
          if !shape.sameElements(maximum) then refuse(Hdf5Error.UnsupportedStored("fixed extent required"))
          val creation = scope.own(Kind.Property)(H5.H5Dget_create_plist(d.value))
          if H5.H5Pget_layout(creation.value) != H5D_CHUNKED then refuse(Hdf5Error.UnsupportedStored("chunked layout required"))
          val chunks = new Array[Long](rank)
          H5.H5Pget_chunk(creation.value, rank, chunks)
          val nf = H5.H5Pget_nfilters(creation.value)
          val filter = if nf == 0 then Hdf5Filter.None else if nf == 1 then
            val flags = new Array[Int](1)
            val config = new Array[Int](1)
            val params = new Array[Int](8)
            val n = Array(8L)
            val text = new Array[String](1)
            val id = H5.H5Pget_filter(creation.value, 0, flags, n, params, 128, text, config)
            if id != H5Z_FILTER_DEFLATE || n(0) != 1 || params(0) < 0 || params(0) > 9 ||
                (H5.H5Zget_filter_info(id) & 3) != 3 then refuse(Hdf5Error.UnsupportedStored("one standard DEFLATE filter only"))
            Hdf5Filter.Deflate(params(0))
          else refuse(Hdf5Error.UnsupportedStored("filter count"))
          val info = Hdf5DatasetInfo(name, dtype, get(Hdf5Extent(shape.toVector)), get(Hdf5Extent(chunks.toVector)), filter)
          get(Hdf5Plan.dataset(info, limits))
          scope.keep(d)
          (d, info)
        attach(pair._1, pair._2)

      def close(): Either[Hdf5Error, Unit] = guarded("close file"):
        if !file.closed then
          val errors = children.toVector.flatMap(d => d.close().left.toOption)
          var fileError = Option.empty[Hdf5Error]
          try
            file.close()
            Owner.files -= 1
          catch case NonFatal(e) => fileError = Some(error("close file", e))
          val all = errors ++ fileError
          if all.size == 1 then refuse(all.head)
          else if all.nonEmpty then refuse(Hdf5Error.ScopeFailure(all.head, all.tail))

      private final class NativeDataset(id: Id, val info: Hdf5DatasetInfo) extends Hdf5Dataset:
        private def io(write: Boolean, slab: Hdf5Slab, block: Hdf5Block, cancelled: () => Boolean): Either[Hdf5Error, Unit] = guarded("slab"):
          live()
          if id.closed then refuse(Hdf5Error.Closed("dataset"))
          if write && !writable then refuse(Hdf5Error.UnsupportedStored("read-only file"))
          val bytes = get(Hdf5Plan.slab(info, slab, block, limits))
          if cancelled == null then refuse(Hdf5Error.InvalidBlock("null cancellation callback"))
          if cancelled() then refuse(Hdf5Error.Cancelled)
          live()
          if id.closed then refuse(Hdf5Error.Closed("dataset closed by cancellation callback"))
          if slab.elements > 0 then
            val totalElements = get(Checked.add(Owner.elements, slab.elements, "receipt elements"))
            val totalBytes = get(Checked.add(Owner.bytes, bytes, "receipt bytes"))
            scoped(limits): scope =>
              val space = scope.own(Kind.Space)(H5.H5Dget_space(id.value))
              H5.H5Sselect_hyperslab(space.value, H5S_SELECT_SET, slab.offset.toArray, null, slab.count.toArray, null)
              val mem = scope.own(Kind.Space)(H5.H5Screate_simple(1, Array(slab.elements), null))
              if H5.H5Sget_select_npoints(space.value) != slab.elements then refuse(Hdf5Error.NativeFailure("selection", "point-count mismatch"))
              Owner.calls += 1 // Attempts remain visible even if H5Dread/write fails.
              Owner.elements = totalElements
              Owner.bytes = totalBytes
              block match
                case Hdf5Block.Float32(a) =>
                  if write then H5.H5Dwrite_float(id.value, memory(info.dtype), mem.value, space.value, H5P_DEFAULT, a)
                  else H5.H5Dread_float(id.value, memory(info.dtype), mem.value, space.value, H5P_DEFAULT, a)
                case Hdf5Block.Float64(a) =>
                  if write then H5.H5Dwrite_double(id.value, memory(info.dtype), mem.value, space.value, H5P_DEFAULT, a)
                  else H5.H5Dread_double(id.value, memory(info.dtype), mem.value, space.value, H5P_DEFAULT, a)
                case Hdf5Block.UInt8(a) =>
                  if write then H5.H5Dwrite(id.value, memory(info.dtype), mem.value, space.value, H5P_DEFAULT, a)
                  else H5.H5Dread(id.value, memory(info.dtype), mem.value, space.value, H5P_DEFAULT, a)
              Owner.successes += 1
            val sizes = new Array[Long](3)
            H5.H5Fget_mdc_size(file.value, sizes)
            Owner.metadataPeak = math.max(Owner.metadataPeak, sizes(2))
            val runtime = Runtime.getRuntime
            Owner.heapPeak = math.max(Owner.heapPeak, runtime.totalMemory() - runtime.freeMemory())

        def write(slab: Hdf5Slab, block: Hdf5Block, cancelled: () => Boolean): Either[Hdf5Error, Unit] = io(true, slab, block, cancelled)
        def readInto(slab: Hdf5Slab, block: Hdf5Block, cancelled: () => Boolean): Either[Hdf5Error, Unit] = io(false, slab, block, cancelled)
        def close(): Either[Hdf5Error, Unit] = guarded("close dataset"):
          if !id.closed then
            id.close()
            children = children.filterNot(_ eq this)
            Owner.datasets -= 1
