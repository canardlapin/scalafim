package scalafim.phrfcmp.prep

import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*

import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan}
import scalafim.phrfcmp.ingest.Matrix

/**
  * The whitening transform against the S11 R fixtures (`tools/phrf-comparison/parity`): fixture Y (and trial X, F)
  * whitened with phi = 0.3125 per run, compared with the committed R `Y_white` etc. Relative error is
  * `max|a - b| / max|b|` with b the fixture, the S11 definition. Primary convention is exact_first; identity_first is the
  * secondary variant.
  */
class S11WhiteningParitySuite extends munit.FunSuite:

  private val Phi = 0.3125
  private val Tol = 1e-10

  private def fixtures: Path =
    def find(p: Path): Option[Path] =
      if p == null then None
      else
        val d = p.resolve("tools/phrf-comparison/parity/fixtures")
        if Files.isDirectory(d) then Some(d) else find(p.getParent)
    find(Paths.get(sys.props("user.dir")).toAbsolutePath).getOrElse(fail("S11 fixtures directory not found"))

  private def csv(p: Path): Matrix =
    val lines = Files.readAllLines(p).asScala.toVector.drop(1).filter(_.nonEmpty)
    val rows = lines.map(_.split(",").map(_.toDouble))
    Matrix.of(rows.length, rows.head.length, rows.flatten.toArray).fold(e => fail(e.message), identity)

  private def plan(runLens: Seq[Int], exactFirst: Boolean) =
    val starts = runLens.scanLeft(0)(_ + _)
    val segs = runLens.indices.map(r => TimeSegment(starts(r), starts(r + 1), r)).toVector
    WhiteningPlan.global(ArmaCoefficients.ar(Phi), segs, exactFirstAr1 = exactFirst)

  private def relErr(a: Matrix, b: Matrix): Double =
    assertEquals((a.rows, a.cols), (b.rows, b.cols))
    val diff = a.data.indices.map(i => math.abs(a.data(i) - b.data(i))).max
    diff / b.data.map(math.abs).max

  private val results = scala.collection.mutable.ArrayBuffer.empty[String]

  private def check(label: String, dir: Path, raw: String, white: String, runLens: Seq[Int], exactFirst: Boolean, rawDir: Path): Unit =
    val x = csv(rawDir.resolve(s"$raw.csv"))
    val expected = csv(dir.resolve(s"$white.csv"))
    val got = Whiten.columns(plan(runLens, exactFirst), x).fold(e => fail(e.message), identity)
    val err = relErr(got, expected)
    results += f"S11-WHITEN $label%-40s relerr=$err%.3e"
    assert(err <= Tol, s"$label relative error $err")

  private val cells = Seq(
    ("cond", "cond", Seq(200, 200, 200, 200), Seq("Y" -> "Y_white")),
    ("cond_close", "cond_close", Seq(200, 200, 200, 200), Seq("Y" -> "Y_white")),
    ("trial/tx", "trial/tx", Seq(150, 150, 150), Seq("Y" -> "Y_white", "X_trial" -> "X_trial_white", "F" -> "F_white")),
    ("trial/ts", "trial/ts", Seq(150, 150, 150), Seq("Y" -> "Y_white", "X_trial" -> "X_trial_white", "F" -> "F_white"))
  )

  for
    (name, sub, lens, pairs) <- cells
    (raw, white) <- pairs
  do
    test(s"exact_first (primary) whitening of $name/$raw matches R $white at 1e-10"):
      val d = fixtures.resolve(sub)
      check(s"exact_first $name/$raw", d, raw, white, lens, exactFirst = true, d)
    test(s"identity_first (secondary) whitening of $name/$raw matches R $white at 1e-10"):
      val d = fixtures.resolve(sub)
      check(s"identity_first $name/$raw", d.resolve("identity_first"), raw, white, lens, exactFirst = false, d)

  override def afterAll(): Unit = results.foreach(println)
