package scalafim.transform.itk

import image4s.geometry.{D3, Frame, Point}
import scalafim.image.world.{FrameCatalog, WorldSpace}
import scalafim.transform.*
import scalafim.transform.field.DenseContext
import scalafim.transform.oracle.{OracleFixtures, OracleTable}

/** ITK HDF5 composites (decoded dumps, so this runs on both platforms) against ITK's own TransformPoint. */
class ItkHdf5OracleSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val moving: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("moving")))
  private val fixed: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("fixed")))
  private val frames = Frames[moving.type, fixed.type](moving, fixed)

  private[itk] def decoded(name: String): ItkHdf5File =
    ItkHdf5Dumps.parse(OracleFixtures.text(s"itk_hdf5/${name.stripSuffix(".h5")}.components.txt"))

  private val points = OracleTable.load("itk_hdf5/points.tsv")

  private def lps(v: Vector[Double]) = Vector(-v(0), -v(1), v(2))

  test("affine-only, affine-then-warp and warp-then-affine composites reproduce ITK TransformPoint"):
    // ITK holds a displacement field's border value for half a voxel outside its lattice and uses zero displacement
    // beyond that band: DenseContext.itk (HoldBorderDisplacement). PreserveSource differs inside the band, where it
    // blends the border displacement towards zero across the whole first voxel.
    val context = DenseContext.itk(frames)
    Vector("affine.h5", "affine_warp.h5", "warp_affine.h5").foreach: name =>
      val chain = ok(ItkHdf5Interpretation.interpret(decoded(name), context))
      points.keyed.filter(_._1 == name).foreach: (_, row) =>
        val pulled = ok(chain.composed.pullPoint(ok(Point.fromVector(fixed, lps(row.take(3)))).asInstanceOf[Point[fixed.type, D3]])).coordinates
        lps(pulled).zip(row.slice(3, 6)).foreach((a, e) => assertEqualsDouble(a, e, 1e-9, s"$name at ${row.take(3)}"))

  test("a linear-only file fuses to an affine; composites with fields are dense chains without a forward map"):
    assert(ok(ItkHdf5Interpretation.interpret(decoded("affine.h5"), DenseContext(frames))).composed.isInstanceOf[WorldTransform.Linear[?, ?]])
    val dense = ok(ItkHdf5Interpretation.interpret(decoded("affine_warp.h5"), DenseContext(frames)))
    assertEquals(dense.stages.map(_.kind), Vector("AffineTransform_double_3_3", "DisplacementFieldTransform_double_3_3"))
    assert(dense.composed.push.isEmpty)

  test("the default boundary policy rejects points that leave a displacement lattice"):
    val dense = ok(ItkHdf5Interpretation.interpret(decoded("affine_warp.h5"), DenseContext(frames)))
    assert(dense.composed.pullPoint(ok(Point.fromVector(fixed, Vector(400.0, -400.0, 400.0))).asInstanceOf[Point[fixed.type, D3]]).isLeft)

  test("malformed composites are typed failures"):
    val twoMarkers = ItkHdf5File(decoded("affine_warp.h5").components :+ ItkHdf5Component(3, "CompositeTransform_double_3_3", IArray.empty, IArray.empty))
    assert(ItkHdf5Interpretation.interpret(twoMarkers, DenseContext(frames)).isLeft)
    val wrongSize = ItkHdf5File(Vector(ItkHdf5Component(0, "DisplacementFieldTransform_double_3_3", IArray(0.0, 0.0, 0.0), IArray.fill(18)(1.0))))
    assert(ItkHdf5Interpretation.interpret(wrongSize, DenseContext(frames)).isLeft)

/** Reads the `index<TAB>type<TAB>parameters<TAB>fixed` dumps written by generate_itk_hdf5_oracle.py. */
object ItkHdf5Dumps:
  def parse(text: String): ItkHdf5File =
    ItkHdf5File(
      text.linesIterator.filter(_.nonEmpty).map { line =>
        val cells = line.split("\t", -1)
        def values(cell: String) = IArray.from(cell.trim.split("\\s+").filter(_.nonEmpty).map(_.toDouble))
        ItkHdf5Component(cells(0).toInt, cells(1), values(cells(2)), values(cells(3)))
      }.toVector
    )
