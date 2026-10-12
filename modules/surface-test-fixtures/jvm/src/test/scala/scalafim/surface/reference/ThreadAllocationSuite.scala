package scalafim.surface.reference

class ThreadAllocationSuite extends munit.FunSuite:
  test("supported runtime measures a retained allocation"):
    ThreadAllocation.measure(new Array[Byte](1024 * 1024)) match
      case AllocationMeasurement.Measured(bytes) => assert(bytes >= 1024 * 1024L)
      case AllocationMeasurement.Unavailable(reason) => fail(reason)

  test("unavailable counter executes the body once and retains the limitation"):
    var runs = 0
    val measured = ThreadAllocation.measureWith(Left("unsupported")) { runs += 1 }
    assertEquals(runs, 1)
    assertEquals(measured, AllocationMeasurement.Unavailable("unsupported"))

  test("negative or decreasing counters cannot become zero-allocation evidence"):
    assertEquals(ThreadAllocation.measureWith(Right(() => -1L))(()),
      AllocationMeasurement.Unavailable("allocation counter is unavailable (-1)"))
    var next = 10L
    val counter = () => { next -= 1; next }
    assertEquals(ThreadAllocation.measureWith(Right(counter))(()),
      AllocationMeasurement.Unavailable("allocation counter decreased"))
