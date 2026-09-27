package scalafim.transform.itk

import image4s.geometry.{D3, Frame, Point}
import scalafim.image.world.{FrameCatalog, WorldSpace}
import scalafim.transform.*
import scalafim.transform.field.DenseContext
import scalafim.transform.oracle.OracleFixtures

/** The SimpleITK HDF5 oracles (oracle/itk_hdf5_simpleitk) on both platforms, through h5py dumps of each file
  * (oracle/itk_hdf5_simpleitk_dumps). ItkHdf5SimpleItkSuite (JVM) checks that jHDF decodes the files to these dumps.
  * Rows are SimpleITK `TransformPoint` (fixed -> moving) converted to RAS, so each is a pullback.
  */
class ItkHdf5SimpleItkDumpSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val moving: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("simpleitk dump moving")))
  private val fixed: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("simpleitk dump fixed")))

  // ITK's own out-of-lattice behaviour: the border displacement for half a voxel, zero displacement beyond.
  private val itk = DenseContext.itk(Frames[moving.type, fixed.type](moving, fixed))

  private final case class Row(label: String, input: Vector[Double], output: Vector[Double])

  private val rows: Vector[Row] =
    OracleFixtures
      .text("itk_hdf5_simpleitk/point_oracles.tsv")
      .linesIterator
      .drop(1)
      .filter(_.nonEmpty)
      .map: line =>
        val cells = line.split('\t').toVector
        Row(cells.head, cells.slice(1, 4).map(_.toDouble), cells.slice(4, 7).map(_.toDouble))
      .toVector

  private def dump(name: String): ItkHdf5File =
    ItkHdf5Dumps.parse(OracleFixtures.text(s"itk_hdf5_simpleitk_dumps/${name.stripSuffix(".h5")}.components.txt"))

  private def pull(transform: WorldTransform[moving.type, fixed.type], ras: Vector[Double]): Vector[Double] =
    ok(transform.pullPoint(ok(Point.in(fixed)(ras(0), ras(1), ras(2))))).coordinates

  private def assertClose(actual: Vector[Double], expected: Vector[Double], tolerance: Double, clue: String)(using munit.Location): Unit =
    actual.zip(expected).foreach((a, e) => assertEqualsDouble(a, e, tolerance, clue))

  private def labelled(label: String): Vector[Row] =
    val selected = rows.filter(_.label == label)
    assert(selected.nonEmpty, s"no '$label' rows")
    selected

  test("an affine + displacement composite reproduces SimpleITK TransformPoint"):
    val composite = ok(ItkHdf5Interpretation.interpret(dump("composite_affine_displacement_double.h5"), itk))
    assertEquals(composite.stages.map(_.kind), Vector("AffineTransform_double_3_3", "DisplacementFieldTransform_double_3_3"))
    labelled("composite").foreach(row => assertClose(pull(composite.composed, row.input), row.output, 1e-10, s"composite at ${row.input}"))

  test("the legacy float32 file with historic Tranform* dataset names keeps the composite's meaning"):
    val legacy = dump("composite_affine_displacement_legacy_float.h5")
    assert(legacy.stages.forall(_.typeName.contains("_float_")))
    assertEquals(legacy.stages.map(_.parameters.length), dump("composite_affine_displacement_double.h5").stages.map(_.parameters.length))
    val composite = ok(ItkHdf5Interpretation.interpret(legacy, itk))
    labelled("composite").foreach(row => assertClose(pull(composite.composed, row.input), row.output, 2e-6, s"legacy composite at ${row.input}"))

  test("a constant displacement pair is a pullback and its supplied inverse"):
    val forward = ok(ItkHdf5Interpretation.interpret(dump("pullback_plus_one.h5"), itk)).composed
    val inverse = ok(ItkHdf5Interpretation.interpret(dump("pullback_minus_one.h5"), DenseContext.itk(Frames[fixed.type, moving.type](fixed, moving)))).composed
    labelled("constant-forward").foreach(row => assertClose(pull(forward, row.input), row.output, 1e-12, s"forward at ${row.input}"))
    labelled("constant-inverse").foreach: row =>
      assertClose(ok(inverse.pullPoint(ok(Point.in(moving)(row.input(0), row.input(1), row.input(2))))).coordinates, row.output, 1e-12, s"inverse at ${row.input}")

  test("an affine-only composite fuses to an exact affine whose inverse is the forward map"):
    val linear = ok(ItkHdf5Interpretation.interpret(dump("affine_only_forward_double.h5"), itk)).composed match
      case linear: WorldTransform.Linear[moving.type, fixed.type] @unchecked => linear
      case other                                                           => fail(s"expected a fused affine, got $other")
    labelled("affine-forward").foreach: row =>
      assertClose(pull(linear, row.input), row.output, 1e-10, s"pullback at ${row.input}")
      assertClose(ok(linear.mapPoint(ok(Point.in(moving)(row.output(0), row.output(1), row.output(2))))).coordinates, row.input, 1e-10, s"inverse at ${row.output}")

  test("BSpline components are a typed refusal on every platform"):
    ItkHdf5Interpretation.interpret(dump("unsupported_bspline.h5"), itk) match
      case Left(TransformError.Io(TransformIoError.Unsupported(UnsupportedFormat.ItkBSpline, reason))) =>
        assert(reason.contains("BSplineTransform_double_3_3"), reason)
      case other => fail(s"expected an explicit BSpline refusal, got $other")

  test("the truncated file's dump shows the missing fixed parameters the JVM container refuses"):
    // Refusing it is the HDF5 container's job (ItkHdf5SimpleItkSuite, JVM only): an ITK text file may carry an empty
    // fixed-parameter line, so the platform-neutral model cannot distinguish a truncated HDF5 component.
    val truncated = dump("malformed_missing_fixed.h5")
    assertEquals(truncated.components.map(c => (c.typeName, c.parameters.length, c.fixedParameters.length)), Vector(("AffineTransform_double_3_3", 12, 0)))
