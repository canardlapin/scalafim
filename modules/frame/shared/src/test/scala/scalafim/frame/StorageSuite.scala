package scalafim.frame

class StorageSuite extends munit.FunSuite:
  private def value[A](result: Either[StorageError, A]): A =
    result.fold(error => fail(error.message), identity)

  test("Int32 uses Arrow little-endian values and LSB-first validity bits"):
    val array = value(ColumnArray.int32(
      Array(1, 0x02030405, -1, 4),
      Array(true, false, true, false)
    ))
    val buffers = value(array.copyPhysicalBuffers)

    assertEquals(array.layout.buffers.map(_.role), Vector(BufferRole.Validity, BufferRole.Values))
    assertEquals(buffers(0).toVector, Vector(0x05.toByte))
    assertEquals(
      buffers(1).take(8).toVector,
      Vector(1, 0, 0, 0, 5, 4, 3, 2).map(_.toByte)
    )
    assertEquals(array.scalar(0), Right(ScalarValue.Int32(1)))
    assertEquals(array.scalar(1), Right(ScalarValue.Null))
    assertEquals(array.value(1), Left(StorageError.NullValue(1)))
    array.close()

  test("booleans pack values and validity independently"):
    val array = value(ColumnArray.bool(
      Array(true, false, true, true),
      Array(true, true, false, true)
    ))
    val buffers = value(array.copyPhysicalBuffers)

    assertEquals(buffers(0).toVector, Vector(0x0b.toByte))
    assertEquals(buffers(1).toVector, Vector(0x0d.toByte))
    assertEquals(array.scalar(2), Right(ScalarValue.Null))
    assertEquals(array.scalar(3), Right(ScalarValue.Bool(true)))
    array.close()

  test("fixed-width arrays preserve exact cross-platform scalar semantics"):
    val int64 = value(ColumnArray.int64(Array(Long.MinValue, Long.MaxValue)))
    val float32 = value(ColumnArray.float32(Array(1.25f, -0.0f)))
    val float64 = value(ColumnArray.float64(Array(Math.PI, Double.NaN)))
    val timestamp = value(ColumnArray.timestamp(Array(10L, 20L), TimeUnit.Microsecond))

    assertEquals(int64.value(0), Right(Long.MinValue))
    assertEquals(int64.value(1), Right(Long.MaxValue))
    assertEquals(float32.value(0), Right(1.25f))
    assertEquals(java.lang.Float.floatToRawIntBits(value(float32.value(1))), 0x80000000)
    assertEqualsDouble(value(float64.value(0)), Math.PI, 0.0)
    assert(value(float64.value(1)).isNaN)
    assertEquals(timestamp.scalar(1), Right(ScalarValue.Timestamp(20L, TimeUnit.Microsecond)))

    int64.close()
    float32.close()
    float64.close()
    timestamp.close()

  test("UTF-8 offsets and dictionary encoding are inspectable and sliceable"):
    val utf8 = value(ColumnArray.utf8(Array("a", "beta", "γ")))
    val physical = value(utf8.copyPhysicalBuffers)
    assertEquals(utf8.layout.buffers.map(_.role), Vector(BufferRole.Offsets, BufferRole.Values))
    assertEquals(physical(0).length, 16)
    assertEquals(utf8.scalar(2), Right(ScalarValue.Utf8("γ")))

    val indices = value(ColumnArray.int32(Array(1, 0, 1)))
    val preciseSlice: Either[StorageError, Int32Array] = indices.slice(0, 1)
    value(preciseSlice).close()
    val dictionaryValues = value(ColumnArray.utf8(Array("red", "blue")))
    val dictionary = ColumnArray.dictionary(indices, dictionaryValues)
    assertEquals(
      dictionary.encoding,
      PhysicalEncoding.Dictionary(DataType.Int32, DataType.Utf8)
    )
    assertEquals(dictionary.scalar(0), Right(ScalarValue.Utf8("blue")))
    val sliced = value(dictionary.slice(1, 2))
    assertEquals(sliced.scalar(0), Right(ScalarValue.Utf8("red")))

    sliced.close()
    dictionary.close()
    utf8.close()

  test("retained slices keep buffers alive until the final deterministic close"):
    val tracker = new BufferTracker
    val array = value(ColumnArray.int32(
      Array(10, 20, 30, 40),
      Array(true, false, true, true),
      tracker
    ))
    val slice = value(array.slice(1, 2))

    assertEquals(tracker.snapshot, BufferSnapshot(activeOwners = 2, activeViews = 4, releasedOwners = 0))
    array.close()
    assertEquals(slice.scalar(0), Right(ScalarValue.Null))
    assertEquals(slice.scalar(1), Right(ScalarValue.Int32(30)))
    assertEquals(tracker.snapshot.activeOwners, 2)

    slice.close()
    assertEquals(tracker.snapshot, BufferSnapshot(activeOwners = 0, activeViews = 0, releasedOwners = 2))
    assertEquals(slice.scalar(1), Left(StorageError.BufferClosed))

  test("borrowed buffers release their external lease exactly once"):
    val tracker = new BufferTracker
    var releases = 0
    val root = Buffer.borrowed(Array[Byte](1, 2, 3), () => releases += 1, tracker)
    val retained = value(root.slice(1, 2))

    root.close()
    root.close()
    assertEquals(releases, 0)
    assertEquals(retained.copyBytes.map(_.toVector), Right(Vector[Byte](2, 3)))
    retained.close()
    retained.close()

    assertEquals(releases, 1)
    assertEquals(tracker.snapshot, BufferSnapshot(0, 0, 1))

  test("record batches validate schema, lengths, types, and nullability"):
    type S = (id: Int, label: String)
    val schema = summon[SchemaDescriptor[S]].schema
    val ids = value(ColumnArray.int32(Array(1, 2)))
    val labels = value(ColumnArray.utf8(Array("a", "b")))
    val batch = value(RecordBatch(schema, Vector(ids, labels)))

    assertEquals(batch.rowCount, 2)
    assertEquals(value(batch.column("label")).scalar(1), Right(ScalarValue.Utf8("b")))

    val bad = value(ColumnArray.int64(Array(1L, 2L)))
    assertEquals(
      RecordBatch(schema, Vector(bad, value(ColumnArray.utf8(Array("a", "b"))))),
      Left(StorageError.ColumnTypeMismatch(0, DataType.Int32, DataType.Int64))
    )
    bad.close()
    batch.close()

  test("typed tables preserve batch boundaries and visibly own materialized data"):
    type S = (id: Int)
    val schema = summon[SchemaDescriptor[S]].schema
    def batch(values: Array[Int]): RecordBatch =
      value(RecordBatch(schema, Vector(value(ColumnArray.int32(values)))))

    val table = value(Table[S](Vector(batch(Array(1, 2)), batch(Array(3)))))
    assertEquals(table.rowCount, 3L)
    assertEquals(table.batches.map(_.rowCount), Vector(2, 1))
    assert(!table.isClosed)
    table.close()
    table.close()
    assert(table.isClosed)
    assertEquals(table.batches.head.column("id"), Left(StorageError.BufferClosed))

  test("owned sources close unconsumed batches on early termination"):
    type S = (id: Int)
    val schema = summon[SchemaDescriptor[S]].schema
    def batch(number: Int): RecordBatch =
      value(RecordBatch(schema, Vector(value(ColumnArray.int32(Array(number))))))
    val first = batch(1)
    val second = batch(2)
    val source = value(OwnedBatchSource(schema, Vector(first, second)))

    val result = source.use(_.nextBatch())
    assert(result.exists(_.contains(first)))
    assert(!first.isClosed)
    assert(second.isClosed)
    assertEquals(source.open(), Left(StorageError.SourceAlreadyOpened))
    first.close()

  test("failed collection closes accumulated and unconsumed batches"):
    type S = (id: Int)
    val schema = summon[SchemaDescriptor[S]].schema
    val first = value(RecordBatch(schema, Vector(value(ColumnArray.int32(Array(1))))))
    val remaining = value(RecordBatch(schema, Vector(value(ColumnArray.int32(Array(2))))))
    val source = new BatchSource:
      val schema = summon[SchemaDescriptor[S]].schema

      def open(): Either[StorageError, BatchCursor] = Right:
        new BatchCursor:
          private var calls = 0
          private var closed = false

          def nextBatch(): Either[StorageError, Option[RecordBatch]] =
            calls += 1
            if calls == 1 then Right(Some(first))
            else Left(StorageError.Unexpected("injected failure"))

          def close(): Unit =
            if !closed then
              closed = true
              remaining.close()

    assertEquals(source.collect[S], Left(StorageError.Unexpected("injected failure")))
    assert(first.isClosed)
    assert(remaining.isClosed)

  test("batch source captures non-fatal failures with diagnostics and still closes"):
    type S = (id: Int)
    var closed = false
    val source = new BatchSource:
      val schema = summon[SchemaDescriptor[S]].schema

      def open(): Either[StorageError, BatchCursor] = Right:
        new BatchCursor:
          def nextBatch(): Either[StorageError, Option[RecordBatch]] = Right(None)
          def close(): Unit = closed = true

    val result = source.use(_ => throw new RuntimeException())
    result match
      case Left(StorageError.Unexpected(detail)) => assert(detail.nonEmpty)
      case other => fail(s"expected structured unexpected failure, found $other")
    assert(closed)
