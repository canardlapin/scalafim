package scalafim.frame

object StorageBench:
  private def nanos[A](operation: => A): (A, Long) =
    val started = System.nanoTime()
    val result = operation
    (result, System.nanoTime() - started)

  private def requireValue[A](result: Either[StorageError, A]): A =
    result.fold(error => throw new IllegalStateException(error.message), identity)

  def main(arguments: Array[String]): Unit =
    val length = 1000000
    val values = Array.tabulate(length)(identity)
    val valid = Array.tabulate(length)(index => index % 10 != 0)
    val tracker = new BufferTracker

    val (array, buildNanos) = nanos:
      requireValue(ColumnArray.int32(values, valid, tracker))

    val (checksum, scanNanos) = nanos:
      var total = 0L
      var index = 0
      while index < length do
        array.scalar(index) match
          case Right(ScalarValue.Int32(value)) => total += value.toLong
          case Right(ScalarValue.Null) => ()
          case other => throw new IllegalStateException(s"unexpected scalar $other")
        index += 1
      total

    val (_, sliceNanos) = nanos:
      var index = 0
      while index < 10000 do
        val slice = requireValue(array.slice(index % (length - 128), 128))
        requireValue(slice.scalar(127))
        slice.close()
        index += 1

    val beforeClose = tracker.snapshot
    array.close()
    val afterClose = tracker.snapshot

    println(s"rows=$length checksum=$checksum")
    println(f"build_ms=${buildNanos / 1000000.0}%.3f")
    println(f"validity_and_value_scan_ms=${scanNanos / 1000000.0}%.3f")
    println(f"retained_slice_10000_ms=${sliceNanos / 1000000.0}%.3f")
    println(s"before_close=$beforeClose")
    println(s"after_close=$afterClose")
