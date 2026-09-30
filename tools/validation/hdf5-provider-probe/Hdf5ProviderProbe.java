import hdf.hdf5lib.H5;
import hdf.hdf5lib.structs.H5AC_cache_config_t;
import static hdf.hdf5lib.HDF5Constants.*;
import java.lang.management.ManagementFactory;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.concurrent.CancellationException;
import java.util.function.BooleanSupplier;

/** Executable generic JNI experiment, deliberately outside production modules. */
public final class Hdf5ProviderProbe {
  // All provider calls, including initialization, inspection and closure, run under this owner.
  // This probe exposes no handles and starts no native worker threads.
  private static final Object OWNER = new Object();
  private static final int MAX_BLOCK = 65536, MAX_IDS = 24, MAX_FILES = 2, MAX_DATASETS = 2;
  private static final long RAW_CACHE = 1L << 20, META_CACHE = 4L << 20;
  private static int files, ids, peakIds, acquisition, failAt = -1;
  private static long nativeEntries, ioCalls, selectedCells, selectedBytes, chunkUpperBytes, heapPeak, metadataPeak;

  private enum Code { InvalidRank, InvalidExtent, InvalidChunk, InvalidBounds, Overflow,
    BufferCapacity, TypeMismatch, UnsupportedRepresentation, ResourceLimit, AlreadyExists,
    DuplicateDataset, Closed, InjectedFailure }
  private static final class Refusal extends RuntimeException {
    private static final long serialVersionUID = 1L;
    final Code code;
    Refusal(Code code, String detail) { super(code + ": " + detail); this.code = code; }
  }
  private enum Type {
    F32, F64, U8;
    long disk() { return switch (this) { case F32 -> H5T_IEEE_F32LE; case F64 -> H5T_IEEE_F64LE; case U8 -> H5T_STD_U8LE; }; }
    long memory() { return switch (this) { case F32 -> H5T_NATIVE_FLOAT; case F64 -> H5T_NATIVE_DOUBLE; case U8 -> H5T_NATIVE_UCHAR; }; }
    int bytes() { return switch (this) { case F32 -> 4; case F64 -> 8; case U8 -> 1; }; }
    boolean accepts(Object a) { return switch (this) { case F32 -> a instanceof float[]; case F64 -> a instanceof double[]; case U8 -> a instanceof byte[]; }; }
  }
  private enum Kind { FILE, DATASET, SPACE, PROPERTY, TYPE, ERROR_STACK }
  private static void owner() { if (!Thread.holdsLock(OWNER)) throw new IllegalStateException("native owner missing"); }
  private static void check(boolean ok, String detail) { if (!ok) throw new AssertionError(detail); }
  private static Refusal refuse(Code code, String detail) { return new Refusal(code, detail); }
  private static long multiply(long a, long b) {
    try { return Math.multiplyExact(a, b); } catch (ArithmeticException e) { throw refuse(Code.Overflow, "product"); }
  }
  private static long add(long a, long b) {
    try { return Math.addExact(a, b); } catch (ArithmeticException e) { throw refuse(Code.Overflow, "sum"); }
  }
  private static void extent(long[] shape, long[] chunks) {
    if (shape == null || shape.length < 1 || shape.length > 3) throw refuse(Code.InvalidRank, "1..3 required");
    if (chunks == null || chunks.length != shape.length) throw refuse(Code.InvalidChunk, "rank");
    long n = 1, chunkBytes = 1;
    for (int i = 0; i < shape.length; i++) {
      if (shape[i] <= 0) throw refuse(Code.InvalidExtent, "positive fixed extent");
      if (chunks[i] <= 0 || chunks[i] > shape[i]) throw refuse(Code.InvalidChunk, "positive and <= extent");
      n = multiply(n, shape[i]); chunkBytes = multiply(chunkBytes, chunks[i]);
    }
    // Maximum type size; reject oversized chunks before any JNI construction.
    multiply(n, 8);
    if (multiply(chunkBytes, 8) > RAW_CACHE) throw refuse(Code.InvalidChunk, "chunk exceeds raw cache budget");
  }
  private static final class Id implements AutoCloseable {
    final long value; final Kind kind; boolean closed;
    Id(long value, Kind kind) { this.value = value; this.kind = kind; }
    public void close() {
      owner(); if (closed) return;
      switch (kind) {
        case FILE -> H5.H5Fclose(value);
        case DATASET -> H5.H5Dclose(value);
        case SPACE -> H5.H5Sclose(value);
        case PROPERTY -> H5.H5Pclose(value);
        case ERROR_STACK -> H5.H5Eclose_stack(value);
        case TYPE -> H5.H5Tclose(value); // Only H5Dget_type copies, never predefined constants.
      }
      closed = true; ids--;
    }
  }
  private static final class Scope implements AutoCloseable {
    final ArrayDeque<Id> stack = new ArrayDeque<>();
    Id own(long value, Kind kind) {
      owner(); Id id = new Id(value, kind); stack.push(id); ids++; peakIds = Math.max(peakIds, ids);
      if (ids > MAX_IDS) throw refuse(Code.ResourceLimit, "native identifiers");
      if (++acquisition == failAt) throw refuse(Code.InjectedFailure, "after acquisition " + acquisition);
      return id;
    }
    Id transfer(Id id) { check(stack.remove(id), "ownership transfer"); return id; }
    public void close() {
      RuntimeException primary = null;
      while (!stack.isEmpty()) {
        try { stack.pop().close(); } catch (RuntimeException e) {
          if (primary == null) primary = e; else primary.addSuppressed(e);
        }
      }
      if (primary != null) throw primary;
    }
  }
  private static Id fapl(Scope scope) throws Exception {
    Id p = scope.own(H5.H5Pcreate(H5P_FILE_ACCESS), Kind.PROPERTY);
    H5.H5Pset_cache(p.value, 0, 521, RAW_CACHE, 0.75);
    H5AC_cache_config_t cfg = H5.H5Pget_mdc_config(p.value);
    cfg.set_initial_size = true; cfg.initial_size = 1L << 20;
    cfg.min_size = 1L << 20; cfg.max_size = META_CACHE;
    cfg.incr_mode = 0; cfg.flash_incr_mode = 0; cfg.decr_mode = 0;
    H5.H5Pset_mdc_config(p.value, cfg);
    check(H5.H5Pget_mdc_config(p.value).max_size == META_CACHE, "metadata cap");
    return p;
  }
  private static Id dapl(Scope scope) throws Exception {
    Id p = scope.own(H5.H5Pcreate(H5P_DATASET_ACCESS), Kind.PROPERTY);
    H5.H5Pset_chunk_cache(p.value, 521, RAW_CACHE, 0.75);
    long[] slots = new long[1], bytes = new long[1]; double[] weight = new double[1];
    H5.H5Pget_chunk_cache(p.value, slots, bytes, weight);
    check(bytes[0] == RAW_CACHE && slots[0] == 521, "chunk cache cap");
    return p;
  }
  private static long exclusive(Path path, long access) {
    try {return H5.H5Fcreate(path.toString(), H5F_ACC_EXCL, H5P_DEFAULT, access);}
    catch(hdf.hdf5lib.exceptions.HDF5LibraryException primary) {
      // Capture the native cause before property cleanup clears HDF5's current error stack.
      StringBuilder details = new StringBuilder();
      try(Scope s = new Scope()) {
        Id errors = s.own(H5.H5Eget_current_stack(), Kind.ERROR_STACK);
        H5.H5Ewalk2(errors.value, H5E_WALK_DOWNWARD, (n, error, context) -> {
          if(details.length()<8192) details.append(error.desc).append("\n");
          return 0;
        }, new hdf.hdf5lib.callbacks.H5E_walk_t() {});
      } catch(RuntimeException cleanup) {primary.addSuppressed(cleanup); throw primary;}
      String nativeCause = details.toString();
      System.err.println("NATIVE_EXCLUSIVE_CREATE_ERROR " + nativeCause);
      // On this admitted macOS lane, POSIX EEXIST is errno 17. No generic IO error is accepted.
      if(nativeCause.matches("(?s).*errno\\s*=\\s*17\\b.*") && nativeCause.contains("File exists")) {
        Refusal duplicate=refuse(Code.AlreadyExists,"native H5F_ACC_EXCL errno17/EEXIST " + path);
        duplicate.initCause(primary); throw duplicate;
      }
      throw primary;
    }
  }
  private static final class Store implements AutoCloseable {
    final Id file; final Scope datasets = new Scope(); int openDatasets; boolean closed;
    Store(Id file) { this.file = file; files++; }
    static Store open(Path path, boolean create) throws Exception { return open(path, create, () -> {}); }
    static Store open(Path path, boolean create, Checked beforeCreate) throws Exception {
      owner();
      if (files >= MAX_FILES) throw refuse(Code.ResourceLimit, "open files");
      if (create && Files.exists(path)) throw refuse(Code.AlreadyExists, path.toString());
      nativeEntries++;
      try (Scope s = new Scope()) {
        Id p = fapl(s);
        if (create) beforeCreate.run();
        Id f = s.own(create ? exclusive(path, p.value)
                           : H5.H5Fopen(path.toString(), H5F_ACC_RDONLY, p.value), Kind.FILE);
        return new Store(s.transfer(f));
      }
    }
    void live() { if (closed) throw refuse(Code.Closed, "store"); }
    Dataset create(String name, Type type, long[] shape, long[] chunks, boolean deflate) throws Exception {
      live(); if (type == null) throw refuse(Code.TypeMismatch, "null dtype"); extent(shape, chunks); datasetCap();
      if (name == null || !name.matches("[a-zA-Z][a-zA-Z0-9_]*")) throw refuse(Code.UnsupportedRepresentation, "flat dataset name");
      nativeEntries++;
      if (H5.H5Lexists(file.value, name, H5P_DEFAULT)) throw refuse(Code.DuplicateDataset, name);
      try (Scope s = new Scope()) {
        Id space = s.own(H5.H5Screate_simple(shape.length, shape, null), Kind.SPACE);
        Id dcpl = s.own(H5.H5Pcreate(H5P_DATASET_CREATE), Kind.PROPERTY);
        H5.H5Pset_chunk(dcpl.value, shape.length, chunks);
        if (deflate) H5.H5Pset_deflate(dcpl.value, 4);
        Id access = dapl(s);
        Id d = s.own(H5.H5Dcreate(file.value, name, type.disk(), space.value, H5P_DEFAULT, dcpl.value, access.value), Kind.DATASET);
        Id owned = s.transfer(d); datasets.stack.push(owned); openDatasets++;
        return new Dataset(this, owned, type, shape.clone(), chunks.clone(), deflate);
      }
    }
    void datasetCap() { if (openDatasets >= MAX_DATASETS) throw refuse(Code.ResourceLimit, "open datasets"); }
    Dataset read(String name) throws Exception {
      live(); datasetCap();
      try (Scope s = new Scope()) {
        Id access = dapl(s);
        Id d = s.own(H5.H5Dopen(file.value, name, access.value), Kind.DATASET);
        Id t = s.own(H5.H5Dget_type(d.value), Kind.TYPE);
        Type type = null;
        for (Type candidate : Type.values()) if (H5.H5Tequal(t.value, candidate.disk())) type = candidate;
        if (type == null) throw refuse(Code.UnsupportedRepresentation, "only exact LE F32/F64/U8");
        Id space = s.own(H5.H5Dget_space(d.value), Kind.SPACE);
        int rank = H5.H5Sget_simple_extent_ndims(space.value);
        if (rank < 1 || rank > 3) throw refuse(Code.InvalidRank, "stored rank");
        long[] shape = new long[rank], max = new long[rank], chunks = new long[rank];
        H5.H5Sget_simple_extent_dims(space.value, shape, max);
        if (!Arrays.equals(shape, max)) throw refuse(Code.UnsupportedRepresentation, "extensible dataset");
        Id dcpl = s.own(H5.H5Dget_create_plist(d.value), Kind.PROPERTY);
        if (H5.H5Pget_layout(dcpl.value) != H5D_CHUNKED) throw refuse(Code.UnsupportedRepresentation, "chunked only");
        H5.H5Pget_chunk(dcpl.value, rank, chunks); extent(shape, chunks);
        int filters = H5.H5Pget_nfilters(dcpl.value);
        if (filters > 1) throw refuse(Code.UnsupportedRepresentation, "filter count");
        if (filters == 1) {
          int[] flags = new int[1], config = new int[1], params = new int[8];
          long[] n = {8}; String[] filterName = new String[1];
          int filter = H5.H5Pget_filter(dcpl.value, 0, flags, n, params, 128, filterName, config);
          if (filter != H5Z_FILTER_DEFLATE) throw refuse(Code.UnsupportedRepresentation, "filter " + filter);
        }
        Id owned = s.transfer(d); datasets.stack.push(owned); openDatasets++;
        return new Dataset(this, owned, type, shape, chunks, filters == 1);
      }
    }
    void sampleMetadata() throws Exception {
      long[] sizes = new long[3]; H5.H5Fget_mdc_size(file.value, sizes);
      metadataPeak = Math.max(metadataPeak, sizes[2]);
    }
    public void close() {
      owner(); if (closed) return;
      try (file) { datasets.close(); } // TWR preserves primary and suppressed close errors.
      finally { files--; closed = true; }
    }
  }
  private static final class Dataset implements AutoCloseable {
    final Store store; final Id id; final Type type; final long[] shape, chunks; final boolean deflate;
    Dataset(Store store, Id id, Type type, long[] shape, long[] chunks, boolean deflate) {
      this.store = store; this.id = id; this.type = type; this.shape = shape; this.chunks = chunks; this.deflate = deflate;
    }
    long validate(long[] offset, long[] count, Object buffer) {
      store.live(); if (id.closed) throw refuse(Code.Closed, "dataset");
      if (!type.accepts(buffer)) throw refuse(Code.TypeMismatch, type.toString());
      if (offset == null || count == null || offset.length != shape.length || count.length != shape.length)
        throw refuse(Code.InvalidRank, "slab rank");
      boolean zero = false;
      for (int i = 0; i < shape.length; i++) {
        if (offset[i] < 0 || count[i] < 0) throw refuse(Code.InvalidBounds, "negative");
        if (add(offset[i], count[i]) > shape[i]) throw refuse(Code.InvalidBounds, "outside fixed extent");
        zero |= count[i] == 0;
      }
      long n = zero ? 0 : 1;
      if (!zero) for (long c : count) n = multiply(n, c);
      int capacity = switch (type) { case F32 -> ((float[]) buffer).length; case F64 -> ((double[]) buffer).length; case U8 -> ((byte[]) buffer).length; };
      if (n > MAX_BLOCK || n > capacity) throw refuse(Code.BufferCapacity, "<=65536 and destination capacity");
      return n;
    }
    void io(boolean write, long[] offset, long[] count, Object buffer, BooleanSupplier cancelled) throws Exception {
      owner(); long n = validate(offset, count, buffer);
      if (cancelled.getAsBoolean()) throw new CancellationException("between synchronous slabs");
      if (n == 0) return;
      nativeEntries++;
      try (Scope s = new Scope()) {
        Id fileSpace = s.own(H5.H5Dget_space(id.value), Kind.SPACE);
        H5.H5Sselect_hyperslab(fileSpace.value, H5S_SELECT_SET, offset, null, count, null);
        Id memory = s.own(H5.H5Screate_simple(1, new long[]{n}, null), Kind.SPACE);
        check(H5.H5Sget_select_npoints(fileSpace.value) == n, "selected points");
        ioCalls++;
        switch (type) {
          case F32 -> { if (write) H5.H5Dwrite_float(id.value, type.memory(), memory.value, fileSpace.value, H5P_DEFAULT, (float[]) buffer); else H5.H5Dread_float(id.value, type.memory(), memory.value, fileSpace.value, H5P_DEFAULT, (float[]) buffer); }
          case F64 -> { if (write) H5.H5Dwrite_double(id.value, type.memory(), memory.value, fileSpace.value, H5P_DEFAULT, (double[]) buffer); else H5.H5Dread_double(id.value, type.memory(), memory.value, fileSpace.value, H5P_DEFAULT, (double[]) buffer); }
          case U8 -> { if (write) H5.H5Dwrite(id.value, type.memory(), memory.value, fileSpace.value, H5P_DEFAULT, (byte[]) buffer); else H5.H5Dread(id.value, type.memory(), memory.value, fileSpace.value, H5P_DEFAULT, (byte[]) buffer); }
        }
        selectedCells += n; selectedBytes += n * type.bytes();
        long touched = 1, chunkCells = 1;
        for (int i = 0; i < count.length; i++) {
          touched = multiply(touched, (offset[i] + count[i] - 1) / chunks[i] - offset[i] / chunks[i] + 1);
          chunkCells = multiply(chunkCells, chunks[i]);
        }
        chunkUpperBytes += touched * chunkCells * type.bytes();
      }
      sampleHeap(); store.sampleMetadata();
    }
    public void close() {
      owner(); if (!id.closed) { id.close(); store.datasets.stack.remove(id); store.openDatasets--; }
    }
  }
  private static void sampleHeap() {
    Runtime rt = Runtime.getRuntime(); heapPeak = Math.max(heapPeak, rt.totalMemory() - rt.freeMemory());
  }
  private static void loader() throws Exception {
    owner(); int[] version = new int[3]; H5.H5get_libversion(version);
    if (!Arrays.equals(version, new int[]{2, 2, 0})) throw refuse(Code.UnsupportedRepresentation, "provider " + Arrays.toString(version));
    int flags = H5.H5Zget_filter_info(H5Z_FILTER_DEFLATE);
    if (H5.H5Zfilter_avail(H5Z_FILTER_DEFLATE) != 1 || (flags & 3) != 3)
      throw refuse(Code.UnsupportedRepresentation, "deflate encode/decode");
    System.out.println("LOADER_PASS version=" + Arrays.toString(version) + " deflateFlags=" + flags +
      " java=" + System.getProperty("java.version") + " javaHome=" + System.getProperty("java.home") +
      " arch=" + System.getProperty("os.arch") + " openIDs=" + H5.getOpenIDCount());
  }
  @FunctionalInterface private interface Checked { void run() throws Exception; }
  private static void rejected(Code code, Checked f) throws Exception {
    int before = H5.getOpenIDCount(); long calls = ioCalls, entries = nativeEntries;
    try { f.run(); throw new AssertionError("expected " + code); }
    catch (Refusal e) { check(e.code == code, "expected " + code + " got " + e.code); }
    check(H5.getOpenIDCount() == before && ioCalls == calls, "refusal changed IO/handles");
    if (code != Code.DuplicateDataset) check(entries == nativeEntries, "refusal entered native region");
  }
  private static double smallValue(long i, long j, long k) { return i * 1000 + j * 10 + k + 0.25; }
  private static int u8(long i, long j, long k) { return (int) ((i * 47 + j * 19 + k * 5) % 256); }
  private static int validity(long i, long j, long k) { return (int) ((i + 2*j + 3*k) % 4); }
  private static void small(Path path) throws Exception {
    long[] shape = {7, 11, 5}, chunks = {3, 4, 2};
    try (Store store = Store.open(path, true)) {
      for (boolean deflate : new boolean[]{false, true}) {
        for (Type type : Type.values()) {
          try (Dataset d = store.create(type.name().toLowerCase() + (deflate ? "_deflate" : "_none"), type, shape, chunks, deflate)) {
            for (int i = 6; i >= 0; i -= 2) for (int j = 9; j >= 0; j -= 3) for (int k = 4; k >= 0; k -= 2) {
              long[] count = {Math.min(2, 7-i), Math.min(3, 11-j), Math.min(2, 5-k)};
              int n = (int)(count[0]*count[1]*count[2]); Object buffer = switch (type) {case F32 -> new float[n]; case F64 -> new double[n]; case U8 -> new byte[n];};
              int a = 0;
              for (int x = 0; x < count[0]; x++) for (int y = 0; y < count[1]; y++) for (int z = 0; z < count[2]; z++) {
                switch (type) {case F32 -> ((float[])buffer)[a] = (float)smallValue(i+x,j+y,k+z); case F64 -> ((double[])buffer)[a] = smallValue(i+x,j+y,k+z); case U8 -> ((byte[])buffer)[a] = (byte)u8(i+x,j+y,k+z); } a++;
              }
              d.io(true, new long[]{i,j,k}, count, buffer, () -> false);
            }
          }
        }
        try (Dataset d = store.create("valid" + (deflate ? "_deflate" : "_none"), Type.U8, shape, chunks, deflate)) {
          for (int i = 0; i < 7; i++) {
            byte[] b = new byte[55]; int a=0;
            for (int j=0;j<11;j++) for(int k=0;k<5;k++) b[a++]=(byte)validity(i,j,k);
            d.io(true,new long[]{i,0,0},new long[]{1,11,5},b,()->false);
          }
        }
      }
    }
    System.out.println("SMALL_WRITE_PASS cellsPerDataset=385 datasets=8 reverseMisalignedEdgeSlabs=true");
  }
  private static double bigValue(long row, long col) { return row * 1048576.0 + col * 0.125 - 17.25; }
  private static void big(Path path, int rows, boolean write) throws Exception {
    long[] shape={rows,131072}, chunks={1,32768};
    try (Store s=Store.open(path,write);
         Dataset d=write?s.create("values",Type.F64,shape,chunks,false):s.read("values");
         Dataset v=write?s.create("validity",Type.U8,shape,chunks,false):s.read("validity")) {
      check(d.type==Type.F64 && v.type==Type.U8 && Arrays.equals(d.shape,shape) && Arrays.equals(v.shape,shape)
        && Arrays.equals(d.chunks,chunks) && Arrays.equals(v.chunks,chunks) && !d.deflate && !v.deflate,"big representation");
      double[] b=new double[MAX_BLOCK]; byte[] codes=new byte[MAX_BLOCK]; long verified=0;
      for(int r=0;r<rows;r++) for(int c=0;c<131072;c+=MAX_BLOCK) {
        long[] offset={r,c}, count={1,MAX_BLOCK};
        if(write) {
          for(int i=0;i<MAX_BLOCK;i++) {b[i]=bigValue(r,c+i); codes[i]=(byte)((r+c+i)%4);}
          d.io(true,offset,count,b,()->false); v.io(true,offset,count,codes,()->false);
          Arrays.fill(b,Double.NaN); Arrays.fill(codes,(byte)99);
        }
        d.io(false,offset,count,b,()->false); v.io(false,offset,count,codes,()->false);
        for(int i=0;i<MAX_BLOCK;i++) if (b[i]!=bigValue(r,c+i) || Byte.toUnsignedInt(codes[i])!=(r+c+i)%4) throw new AssertionError("big cell "+r+","+(c+i));
        verified+=MAX_BLOCK;
      }
      System.out.println("BIG_"+(write?"WRITE":"READ")+"_PASS rows="+rows+" cells="+verified+" valuesBytes="+(verified*8)+" validityBytes="+verified+" blockElements="+MAX_BLOCK);
    }
  }
  private static void literal(Path path) throws Exception {
    double[] expected={-7.5,0,1.25,99.5,-0.125,8192}; int[] bytes={0,255,128,1,2,3};
    try(Store s=Store.open(path,false)) {
      for(Type type:Type.values()) try(Dataset d=s.read(type.name().toLowerCase())) {
        check(d.type==type && Arrays.equals(d.shape,new long[]{2,3}) && Arrays.equals(d.chunks,new long[]{1,2}) && d.deflate,"literal representation");
        Object b=switch(type){case F32->new float[6];case F64->new double[6];case U8->new byte[6];};
        d.io(false,new long[]{0,0},new long[]{2,3},b,()->false);
        for(int i=0;i<6;i++) check(switch(type){case F32->((float[])b)[i]==expected[i];case F64->((double[])b)[i]==expected[i];case U8->Byte.toUnsignedInt(((byte[])b)[i])==bytes[i];},"literal cell "+i);
        Object edge=switch(type){case F32->new float[2];case F64->new double[2];case U8->new byte[2];};
        d.io(false,new long[]{1,1},new long[]{1,2},edge,()->false);
        for(int i=0;i<2;i++) check(switch(type){case F32->((float[])edge)[i]==expected[i+4];case F64->((double[])edge)[i]==expected[i+4];case U8->Byte.toUnsignedInt(((byte[])edge)[i])==bytes[i+4];},"literal nonzero edge "+i);
      }
    }
    System.out.println("PYTHON_LITERAL_READ_PASS literalCells=18 extraNonzeroEdgeCells=6");
  }
  private static long fdCount() { return ((com.sun.management.UnixOperatingSystemMXBean)ManagementFactory.getOperatingSystemMXBean()).getOpenFileDescriptorCount(); }
  private static void failures(Path dir, Path smallPath) throws Exception {
    Path sentinel=dir.resolve("sentinel.bin"); byte[] original={72,68,70,0,17,99}; Files.write(sentinel,original);
    rejected(Code.AlreadyExists,()->Store.open(sentinel,true)); check(Arrays.equals(original,Files.readAllBytes(sentinel)),"no clobber bytes");
    byte[] hashBefore=java.security.MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(smallPath));
    rejected(Code.AlreadyExists,()->Store.open(smallPath,true));
    check(Arrays.equals(hashBefore,java.security.MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(smallPath))),"no clobber hash");
    Path p=dir.resolve("validation.h5");
    try(Store s=Store.open(p,true);Dataset d=s.create("test",Type.F64,new long[]{7,11,5},new long[]{3,4,2},false)) {
      rejected(Code.DuplicateDataset,()->s.create("test",Type.F64,d.shape,d.chunks,false));
      rejected(Code.InvalidRank,()->d.io(true,new long[]{0},new long[]{1},new double[1],()->false));
      rejected(Code.InvalidBounds,()->d.io(true,new long[]{-1,0,0},new long[]{1,1,1},new double[1],()->false));
      rejected(Code.InvalidBounds,()->d.io(true,new long[]{7,0,0},new long[]{1,1,1},new double[1],()->false));
      rejected(Code.Overflow,()->d.io(true,new long[]{Long.MAX_VALUE,0,0},new long[]{1,1,1},new double[1],()->false));
      rejected(Code.BufferCapacity,()->d.io(true,new long[]{0,0,0},new long[]{1,2,2},new double[3],()->false));
      rejected(Code.TypeMismatch,()->d.io(true,new long[]{0,0,0},new long[]{1,1,1},new float[1],()->false));
      rejected(Code.Overflow,()->s.create("overflow",Type.F64,new long[]{Long.MAX_VALUE,2},new long[]{1,1},false));
      rejected(Code.TypeMismatch,()->s.create("nulltype",null,new long[]{1},new long[]{1},false));
      rejected(Code.InvalidRank,()->s.create("rank",Type.F64,new long[]{1,1,1,1},new long[]{1,1,1,1},false));
      rejected(Code.InvalidExtent,()->s.create("extent",Type.F64,new long[]{0},new long[]{1},false));
      rejected(Code.InvalidChunk,()->s.create("chunk",Type.F64,new long[]{7},new long[]{8},false));
      long calls=ioCalls, entries=nativeEntries; int handles=H5.getOpenIDCount();
      d.io(true,new long[]{7,0,0},new long[]{0,1,1},new double[0],()->false);
      check(calls==ioCalls && entries==nativeEntries && handles==H5.getOpenIDCount(),"zero slab native no-op");
      try {d.io(true,new long[]{0,0,0},new long[]{1,1,1},new double[1],()->true);throw new AssertionError("cancellation");}
      catch(CancellationException expected){check(calls==ioCalls,"cancel before IO");}
      try(Dataset second=s.create("second",Type.U8,new long[]{1},new long[]{1},false)) {
        check(second.type==Type.U8,"second dataset");
        rejected(Code.ResourceLimit,()->s.create("third",Type.U8,new long[]{1},new long[]{1},false));
      }
    }
    try(Store s=Store.open(dir.resolve("capacity.h5"),true);Dataset d=s.create("test",Type.F64,new long[]{1,65537},new long[]{1,32768},false)) {
      rejected(Code.BufferCapacity,()->d.io(true,new long[]{0,0},new long[]{1,65537},new double[65537],()->false));
    }
    // Two stores coexist. Closing the first must not invalidate the second (no global H5close).
    Store a=Store.open(smallPath,false);
    try(Store b=Store.open(smallPath,false)) {
      rejected(Code.ResourceLimit,()->Store.open(smallPath,false)); a.close();
      try(Dataset d=b.read("f64_none")) {double[] x=new double[1];d.io(false,new long[]{6,10,4},new long[]{1,1,1},x,()->false);check(x[0]==6104.25,"second store alive");}
    } finally {a.close();}
    // Acquire-fail injection covers property/file and each dataset acquisition; registered immediately.
    for(int target=1;target<=2;target++) {
      acquisition=0;failAt=target;
      try {try(Store unused=Store.open(dir.resolve("acquire_file_"+target+".h5"),true)){check(unused!=null,"unreachable");}throw new AssertionError("injection");}
      catch(Refusal e){check(e.code==Code.InjectedFailure,"acquire failure");}
      finally{failAt=-1;} check(ids==0 && H5.getOpenIDCount()==0,"file acquisition cleanup");
    }
    for(int target=1;target<=4;target++) {
      try(Store s=Store.open(dir.resolve("acquire_dataset_"+target+".h5"),true)) {
        acquisition=0;failAt=target;
        try{try(Dataset unused=s.create("test",Type.F64,new long[]{2,3},new long[]{1,2},false)){check(unused!=null,"unreachable");}throw new AssertionError("injection");}
        catch(Refusal e){check(e.code==Code.InjectedFailure,"dataset failure");}
        finally{failAt=-1;}
      }
      check(ids==0 && H5.getOpenIDCount()==0,"dataset acquisition cleanup");
    }
    Path staging=dir.resolve("callback_owned_staging.h5");
    try {
      try(Store s=Store.open(staging,true);Dataset d=s.create("test",Type.U8,new long[]{1},new long[]{1},false)) {
        d.io(true,new long[]{0},new long[]{1},new byte[]{42},()->false);
        throw refuse(Code.InjectedFailure,"managed callback after write");
      }
    } catch(Refusal e) {check(e.code==Code.InjectedFailure,"callback failure");}
    check(Files.exists(staging) && Arrays.equals(original,Files.readAllBytes(sentinel)) && ids==0,"staging/target cleanup");
    // Warm up inspection and bean before comparing descriptors.
    for(int i=0;i<3;i++) try(Store s=Store.open(smallPath,false);Dataset d=s.read("f64_deflate")){d.io(false,new long[]{0,0,0},new long[]{1,1,1},new double[1],()->false);}
    long beforeFD=fdCount(); int beforeIDs=H5.getOpenIDCount();
    for(int i=0;i<100;i++) {
      try(Store s=Store.open(smallPath,false);Dataset d=s.read("f64_deflate")){d.io(false,new long[]{0,0,0},new long[]{1,1,1},new double[1],()->false);}
      rejected(Code.AlreadyExists,()->Store.open(smallPath,true));
    }
    long afterFD=fdCount(); check(beforeIDs==H5.getOpenIDCount() && ids==0 && afterFD<=beforeFD+1,"FD/native ID leak");
    System.out.println("FAILURE_LAWS_PASS cycles=100 fdBefore="+beforeFD+" fdAfter="+afterFD+" openIDs="+H5.getOpenIDCount()+" acquisitionFailures=6 callbackFailures=1 prenativeRefusals=true noClobber=true zeroNoOp=true cancellationBetweenSlabs=true");
  }
  private static void race(Path path, Path barrier, Path ready) throws Exception {
    Checked rendezvous = () -> {
      // Both contenders already passed preflight and acquired fapl before either H5Fcreate.
      Files.writeString(ready,"ready-for-native-excl pid="+ProcessHandle.current().pid());
      long deadline=System.nanoTime()+15_000_000_000L;
      while(!Files.exists(barrier)){if(System.nanoTime()>deadline)throw new IllegalStateException("race barrier timeout");Thread.sleep(10);}
    };
    try(Store s=Store.open(path,true,rendezvous)){check(s.file.value>=0,"race file");System.out.println("RACE_CREATED");}
    catch(Refusal e) {
      if(e.code!=Code.AlreadyExists || !e.getMessage().contains("native H5F_ACC_EXCL errno17/EEXIST")) throw e;
      check(Files.exists(path),"exclusive race target");
      System.out.println("RACE_REFUSED_NATIVE_EXCL errno=17 cause=EEXIST");metrics();System.exit(4);
    }
  }
  private static void abort(Path path) throws Exception {
    try(Store s=Store.open(path,true);Dataset d=s.create("values",Type.F64,new long[]{128,131072},new long[]{1,32768},false)) {
      d.io(true,new long[]{0,0},new long[]{1,MAX_BLOCK},new double[MAX_BLOCK],()->false);
      System.out.println("ABORTING_OWNED_STAGING no_complete_unit_published=true");System.out.flush();Runtime.getRuntime().halt(17);
    }
  }
  private static void metrics() {
    sampleHeap();
    System.out.println("METRICS heapSamplePeakBytes="+heapPeak+" heapMaxBytes="+Runtime.getRuntime().maxMemory()+" metadataObservedPeakBytes="+metadataPeak+
      " metadataCapPerFileBytes="+META_CACHE+" rawCacheCapPerDatasetBytes="+RAW_CACHE+" maxFiles="+MAX_FILES+" maxDatasetsPerFile="+MAX_DATASETS+" peakOwnedIDs="+peakIds+
      " maxOwnedIDs="+MAX_IDS+" finalOwnedIDs="+ids+" providerOpenIDs="+H5.getOpenIDCount()+" nativeEntries="+nativeEntries+" nativeIOCalls="+ioCalls+" selectedCells="+selectedCells+" selectedBytes="+selectedBytes+
      " theoreticalChunkUpperBytes="+chunkUpperBytes+" physicalDiskReadBytes=unavailable nativeAllocatedBytes=unavailable");
    check(ids==0 && files==0 && H5.getOpenIDCount()==0,"final resource leak");
  }
  public static void main(String[] args) throws Exception {
    synchronized(OWNER) {
      System.out.println("OWNED_PID "+ProcessHandle.current().pid());
      try {loader();}
      catch(LinkageError e){System.err.println("CAPABILITY_BLOCKER MissingNative: "+e);System.exit(3);}
      switch(args[0]) {
        case "loader" -> { }
        case "small" -> small(Path.of(args[1]));
        case "literal" -> literal(Path.of(args[1]));
        case "write" -> big(Path.of(args[1]),Integer.parseInt(args[2]),true);
        case "read" -> big(Path.of(args[1]),Integer.parseInt(args[2]),false);
        case "failures" -> failures(Path.of(args[1]),Path.of(args[2]));
        case "race" -> race(Path.of(args[1]),Path.of(args[2]),Path.of(args[3]));
        case "abort" -> abort(Path.of(args[1]));
        default -> throw new IllegalArgumentException("unknown mode");
      }
      metrics();
    }
  }
}
