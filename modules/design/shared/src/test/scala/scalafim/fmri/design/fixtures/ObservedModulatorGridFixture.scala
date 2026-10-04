package scalafim.fmri.design.fixtures

import scalafim.fmri.design.*
import scalafim.fmri.design.contrast.LevelId

/** Selected independently recomputed receipts from the frozen m7b modulator
  * grid. Source: audit/m7b-methods/dump1.json, SHA-256
  * d0f104de1991ac05cd19a970dd562517f853d7b855c7c3365a713381d260c818.
  * The fixture is self-contained; tests never load the external bundle.
  */
object ObservedModulatorGridFixture:
  final case class Receipt(events: Int, mean: Double, sampleSd: Double)

  val dropCellRawRun1 = Receipt(104, 0.0, 0.102)
  val dropCellZRun1 = Receipt(104, 0.0, 1.0)
  val zeroCellRawRun1 = Receipt(127, 0.0, 0.102)
  val zeroCellZRun1 = Receipt(127, 0.0, 1.0)
  val dropNoneStandardDeviationRun1 = Receipt(104, 5.0444, 1.0)

  final case class Partition(values: Vector[Double], cells: Vector[CellKey])

  private final class Mulberry(initial: Int):
    private var state = initial
    private def imul(left: Int, right: Int): Int = (left.toLong * right.toLong).toInt
    def next(): Double =
      state = state + 0x6D2B79F5
      var value = imul(state ^ (state >>> 15), 1 | state)
      value = (value + imul(value ^ (value >>> 7), 61 | value)) ^ value
      ((value ^ (value >>> 14)).toLong & 0xffffffffL).toDouble / 4294967296.0

  private def cell(outcome: String): CellKey =
    CellKey.unsafe(Vector(CellAssignment(FactorId.unsafe("outcome"), LevelId.unsafe(outcome))))

  /** Exact event-table portion used by the frozen stop-modulator grid. */
  def stopRuns: Vector[Partition] = Vector(11, 29).map { seed =>
    val random = new Mulberry(seed)
    val values = Vector.newBuilder[Double]
    val cells = Vector.newBuilder[CellKey]
    var time = 4.0
    while time < 380.0 do
      val draw = random.next()
      val outcome = if draw < .7 then "go" else if draw < .85 then "stop_success" else "stop_fail"
      val rt = if outcome == "stop_success" then Double.NaN else math.rint((.35 + random.next() * .35) * 1000.0) / 1000.0
      if outcome != "go" then
        val _ = random.next() // SSD, retained to preserve the source generator stream
      values += rt
      cells += cell(outcome)
      time += 1.5 + random.next() * 3.0
    Partition(values.result(), cells.result())
  }
