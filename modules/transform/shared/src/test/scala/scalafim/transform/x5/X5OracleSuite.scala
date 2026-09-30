package scalafim.transform.x5

import image4s.geometry.{Affine, D3, Frame, Point}
import scalafim.image.world.{FrameCatalog, WorldSpace}
import scalafim.transform.*
import scalafim.transform.field.DenseContext
import scalafim.transform.oracle.{OracleFixtures, OracleTable}

/** X5 nodes and chains against nitransforms (X5's reference implementation), on both platforms via h5py dumps. Dense
  * rows lie on the lattice, except on a field affine in space, where nitransforms' cubic and ScalaFIM's linear
  * interpolation agree off the lattice.
  */
class X5OracleSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val moving: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("x5 moving")))
  private val reference: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("x5 reference")))
  private val context = DenseContext(Frames[moving.type, reference.type](moving, reference))
  private val points = OracleTable.load("x5/points.tsv")

  test("linear, displacement, deformation and chained X5 files map points exactly as nitransforms does"):
    Vector("linear", "displacements", "deformations", "chain").foreach: name =>
      val chain = ok(X5Interpretation.interpret(X5Dumps.parse(OracleFixtures.text(s"x5/$name.nodes.txt")), context))
      points.keyed.filter(_._1 == name).foreach: (_, row) =>
        val pulled = ok(chain.composed.pullPoint(ok(Point.fromVector(reference, row.take(3))).asInstanceOf[Point[reference.type, D3]])).coordinates
        pulled.zip(row.slice(3, 6)).foreach((a, e) => assertEqualsDouble(a, e, 1e-5, s"$name at ${row.take(3)}")) // nitransforms maps in float32

  test("off the lattice, a field affine in space maps points as nitransforms does (cubic and linear agree there)"):
    // nitransforms' cubic B-spline differs from the analytic affine value by up to 7.4e-6 mm at these points (manifest).
    val chain = ok(X5Interpretation.interpret(X5Dumps.parse(OracleFixtures.text("x5/affine_displacements.nodes.txt")), context))
    val rows = points.keyed.filter(_._1 == "affine_displacements")
    assert(rows.size >= 8, "at least eight off-lattice points")
    val node = X5Dumps.parse(OracleFixtures.text("x5/affine_displacements.nodes.txt")).nodes.head
    val worldToIndex = ok(Affine.fromRowMajor[D3](node.domain.get.mapping)).inverse.rowMajor
    rows.foreach: (_, row) =>
      // the query is off the lattice: its continuous index has a fractional part on some axis
      val ijk = Vector.tabulate(3)(r => (0 until 3).map(c => worldToIndex(4 * r + c) * row(c)).sum + worldToIndex(4 * r + 3))
      assert(ijk.exists(v => math.abs(v - math.rint(v)) > 0.1), s"$ijk is on the lattice")
      val pulled = ok(chain.composed.pullPoint(ok(Point.fromVector(reference, row.take(3))).asInstanceOf[Point[reference.type, D3]])).coordinates
      pulled.zip(row.slice(3, 6)).foreach((a, e) => assertEqualsDouble(a, e, 2e-5, s"affine_displacements at ${row.take(3)}"))

  test("a single linear node is an affine; multi-node files need a chain"):
    assert(ok(X5Interpretation.interpret(X5Dumps.parse(OracleFixtures.text("x5/linear.nodes.txt")), context)).composed.isInstanceOf[WorldTransform.Linear[?, ?]])
    val chained = X5Dumps.parse(OracleFixtures.text("x5/chain.nodes.txt"))
    assertEquals(chained.chains, Vector(Vector(0, 1)))
    assert(X5Interpretation.interpret(chained.copy(chains = Vector.empty), context).isLeft)
    assert(X5Interpretation.interpretChain(chained, 3, context).isLeft)

/** Reads the one-JSON-object-per-line node dumps written by generate_x5_oracle.py (a fixed, flat schema). */
object X5Dumps:
  private def string(line: String, key: String): Option[String] =
    s""""$key": "([^"]*)"""".r.findFirstMatchIn(line).map(_.group(1)).filter(_.nonEmpty)
  private def ints(line: String, key: String): Vector[Int] =
    s""""$key": \\[([^\\]]*)\\]""".r.findFirstMatchIn(line).toVector.flatMap(_.group(1).split(",").map(_.trim).filter(_.nonEmpty).map(_.toInt))
  private def doubles(line: String, key: String): Vector[Double] =
    string(line, key).toVector.flatMap(_.split(" ").filter(_.nonEmpty).map(_.toDouble))

  def parse(text: String): X5File =
    val lines = text.linesIterator.filter(_.nonEmpty).toVector
    val nodes = lines.filter(_.startsWith("{")).map: line =>
      val domain = Option.when(line.contains("\"domain_size\""))(X5Domain(grid = true, ints(line, "domain_size"), doubles(line, "domain_mapping")))
      X5Node(
        string(line, "index").get.toInt,
        string(line, "type").get,
        string(line, "subtype"),
        string(line, "representation"),
        ints(line, "shape"),
        IArray.from(doubles(line, "transform")),
        domain
      )
    val chains = lines.filter(_.startsWith("chain ")).map(_.stripPrefix("chain ").split("/").toVector.map(_.toInt))
    X5File(nodes, chains)
