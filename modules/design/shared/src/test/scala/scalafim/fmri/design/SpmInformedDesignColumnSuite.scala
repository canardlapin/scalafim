package scalafim.fmri.design

import scalafim.fmri.design.data.*
import scalafim.fmri.design.fixtures.SpmInformedKernelFixture as Spm
import scalafim.fmri.design.formula.*
import scalafim.fmri.design.formula.EventModelBuilder.*
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

/** `temporal_derivative = "spm-1s"` realized as design columns through the
  * formula path, against SPM12's `spm_get_bf` kernel (0 .. 32 s). The kernel
  * microtime grid is TR / 16; building at that precision with a grid-aligned
  * onset makes our convolution a sum of exact kernel samples, so the columns
  * must equal the SPM kernel (convolved with the drive) up to one common
  * factor, including the 25 - 32 s tail beyond the canonical's span. */
class SpmInformedDesignColumnSuite extends munit.FunSuite:
  private val tr = Spm.tr
  private val step = tr / Spm.microtimeResolution.toDouble
  private val scans = 24
  private val onset = 4.0
  private val frame = SamplingFrame(blockLens = Seq(scans), tr = Seq(tr))

  private def kernel(row: Int, column: Int): Double =
    if row < 0 || row >= Spm.rows then 0.0 else Spm.kernel(row * Spm.columns + column)

  private def build(duration: Double) =
    val table = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(onset)),
      "condition" -> Column.Strings(Vector("a")),
      "dur" -> Column.Doubles(Vector(duration))
    )
    val request = EventDesignRequest
      .fromText("onset ~ hrf(condition, basis = spmg3, temporal_derivative = \"spm-1s\", durations = dur, id = task)", table, frame)
      .fold(error => fail(error.toString), identity)
      .copy(options = BuildOptions(precision = Seconds(step)))
    EventModelBuilder.buildEither(request).fold(error => fail(error.message), identity)

  /** Our columns against `expected(scan, column)` up to one factor common to
    * all three columns, estimated from the canonical column only. */
  private def assertSpmColumns(duration: Double, expected: (Int, Int) => Double): Unit =
    val model = build(duration)
    assertEquals(model.designMatrix.cols, Spm.columns)
    assertEquals(model.designMatrix.rows, scans)
    val factor = (0 until scans).map(model.designMatrix(_, 0)).sum / (0 until scans).map(expected(_, 0)).sum
    assert(factor.isFinite && factor > 0.0, s"common factor $factor")
    (0 until Spm.columns).foreach { column =>
      val peak = (0 until scans).map(scan => math.abs(expected(scan, column))).max
      (0 until scans).foreach { scan =>
        assertEqualsDouble(
          model.designMatrix(scan, column) / factor,
          expected(scan, column),
          1e-10 * peak,
          s"duration=$duration scan=$scan column=$column"
        )
      }
    }

  private val scanTimes = frame.samples(global = true).map(_.value)

  /** Kernel row of the lag from the onset to a scan, on the microtime grid.
    * Scan times include the frame's start offset; they stay grid-aligned. */
  private def lagRow(scan: Int): Int =
    val position = (scanTimes(scan) - onset) / step
    assertEqualsDouble(position, math.rint(position), 1e-9, "scan times must be grid-aligned")
    math.rint(position).toInt

  test("an impulse realizes the SPM kernel samples at scan times, including the 25-32 s tail"):
    assertSpmColumns(0.0, (scan, column) => kernel(lagRow(scan), column))
    val model = build(0.0)
    // A scan 26-32 s after the onset: past the canonical span (24 s + 1 s),
    // inside SPM's window. Truncating at 25 s would make these exactly zero.
    val tailScan = (0 until scans).find(scan => scanTimes(scan) - onset > 26.0).get
    assert(scanTimes(tailScan) - onset <= 32.0)
    (1 until Spm.columns).foreach { column =>
      assert(math.abs(kernel(lagRow(tailScan), column)) > 0.0)
      assert(math.abs(model.designMatrix(tailScan, column)) > 0.0, s"tail of column $column was truncated")
    }
    // Scans more than 32 s after the onset lie beyond SPM's kernel window.
    val beyond = (0 until scans).filter(scan => scanTimes(scan) - onset > 32.0)
    assert(beyond.nonEmpty)
    beyond.foreach(scan => (0 until Spm.columns).foreach(column => assertEquals(model.designMatrix(scan, column), 0.0)))

  test("a short boxcar realizes the SPM kernel convolved with the drive, including the tail"):
    val duration = 2.0
    val bins = math.round(duration / step).toInt
    // Grid-aligned boxcar: trapezoid weights on the microtime grid.
    val weights = Vector.tabulate(bins + 1)(m => if m == 0 || m == bins then 0.5 else 1.0)
    assertSpmColumns(duration, (scan, column) => weights.indices.map(m => weights(m) * kernel(lagRow(scan) - m, column)).sum)
