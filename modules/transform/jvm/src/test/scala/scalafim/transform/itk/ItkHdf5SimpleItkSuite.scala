package scalafim.transform.itk

import image4s.geometry.{D3, Frame, Point}
import reframe4s.field.CoordinateBoundaryPolicy
import scalafim.image.world.{FrameCatalog, WorldSpace}
import scalafim.transform.*
import scalafim.transform.field.DenseContext
import scalafim.transform.oracle.OracleFixtures

import java.nio.file.{Path, Paths}

/** SimpleITK-written HDF5 files (oracle/itk_hdf5_simpleitk, see its manifest) read through jHDF and interpreted.
  *
  * Oracle rows are SimpleITK `TransformPoint` (fixed -> moving) with LPS converted to RAS, so every row is a pullback
  * the interpreted transform must reproduce. The files come from the spatial module's former HDF5 reader tests.
  */
class ItkHdf5SimpleItkSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val moving: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("simpleitk moving")))
  private val fixed: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("simpleitk fixed")))

  // ITK extends displacement fields by zero outside their lattice; PreserveSource is exactly that extension.
  private val itk = DenseContext(Frames[moving.type, fixed.type](moving, fixed), CoordinateBoundaryPolicy.PreserveSource)

  private final case class Row(label: String, input: Vector[Double], output: Vector[Double])

  private lazy val rows: Vector[Row] =
    OracleFixtures
      .text("itk_hdf5_simpleitk/point_oracles.tsv")
      .linesIterator
      .drop(1)
      .filter(_.nonEmpty)
      .map: line =>
        val cells = line.split('\t').toVector
        Row(cells.head, cells.slice(1, 4).map(_.toDouble), cells.slice(4, 7).map(_.toDouble))
      .toVector

  private def file(name: String): ItkHdf5File =
    ok(ItkHdf5Container.read(OracleFixtures.bytes(s"itk_hdf5_simpleitk/$name")))

  private def chain(name: String): TransformChain[moving.type, fixed.type] =
    ok(ItkHdf5Interpretation.interpret(file(name), itk))

  private def pull(transform: WorldTransform[moving.type, fixed.type], ras: Vector[Double]): Vector[Double] =
    ok(transform.pullPoint(ok(Point.in(fixed)(ras(0), ras(1), ras(2))))).coordinates

  private def assertClose(actual: Vector[Double], expected: Vector[Double], tolerance: Double, clue: String)(using munit.Location): Unit =
    actual.zip(expected).foreach((a, e) => assertEqualsDouble(a, e, tolerance, clue))

  private def path(name: String): Path =
    Paths.get(Option(getClass.getResource(s"/${OracleFixtures.Root}/itk_hdf5_simpleitk/$name")).getOrElse(fail(s"missing $name")).toURI)

  test("an affine + displacement composite reproduces SimpleITK TransformPoint"):
    val composite = chain("composite_affine_displacement_double.h5")
    assertEquals(composite.stages.map(_.kind), Vector("AffineTransform_double_3_3", "DisplacementFieldTransform_double_3_3"))
    assert(composite.composed.push.isEmpty, "a composite with a displacement field has no forward map")
    rows.filter(_.label == "composite").foreach: row =>
      assertClose(pull(composite.composed, row.input), row.output, 1e-10, s"composite at ${row.input}")

  test("historic Tranform* dataset names and float32 storage keep the composite's meaning"):
    val legacy = file("composite_affine_displacement_legacy_float.h5")
    assert(legacy.stages.forall(_.typeName.contains("_float_")))
    val double = file("composite_affine_displacement_double.h5")
    assertEquals(legacy.stages.map(_.parameters.length), double.stages.map(_.parameters.length))
    val composite = ok(ItkHdf5Interpretation.interpret(legacy, itk))
    rows.filter(_.label == "composite").foreach: row =>
      assertClose(pull(composite.composed, row.input), row.output, 2e-6, s"legacy composite at ${row.input}")

  test("a constant displacement pair is a pullback and its supplied inverse"):
    val forward = chain("pullback_plus_one.h5").composed
    val inverse = ok(ItkHdf5Interpretation.interpret(file("pullback_minus_one.h5"), DenseContext(Frames[fixed.type, moving.type](fixed, moving), CoordinateBoundaryPolicy.PreserveSource))).composed
    val paired = WorldTransform.Mapped(forward.pull, PushAvailability.FromAsset(inverse.pull, AssetRef("pullback_minus_one.h5", None)), forward.provenance)
    rows.filter(_.label == "constant-forward").foreach: row =>
      assertClose(pull(forward, row.input), row.output, 1e-12, s"forward at ${row.input}")
      val there = ok(paired.pullPoint(ok(Point.in(fixed)(row.input(0), row.input(1), row.input(2)))))
      assertClose(ok(paired.mapPoint(there)).coordinates, row.input, 1e-12, s"round trip at ${row.input}")
    rows.filter(_.label == "constant-inverse").foreach: row =>
      assertClose(ok(inverse.pullPoint(ok(Point.in(moving)(row.input(0), row.input(1), row.input(2))))).coordinates, row.output, 1e-12, s"inverse at ${row.input}")

  test("an affine-only composite fuses to an exact affine whose inverse is the forward map"):
    val linear = chain("affine_only_forward_double.h5").composed match
      case linear: WorldTransform.Linear[moving.type, fixed.type] @unchecked => linear
      case other                                                           => fail(s"expected a fused affine, got $other")
    rows.filter(_.label == "affine-forward").foreach: row =>
      assertClose(pull(linear, row.input), row.output, 1e-10, s"pullback at ${row.input}")
      assertClose(ok(linear.mapPoint(ok(Point.in(moving)(row.output(0), row.output(1), row.output(2))))).coordinates, row.input, 1e-10, s"inverse at ${row.output}")

  test("BSpline components and missing fixed parameters are typed refusals"):
    ItkHdf5Interpretation.interpret(file("unsupported_bspline.h5"), itk) match
      case Left(TransformError.Io(TransformIoError.Unsupported(UnsupportedFormat.ItkBSpline, reason))) =>
        assert(reason.contains("BSplineTransform_double_3_3"), reason)
      case other => fail(s"expected an explicit BSpline refusal, got $other")
    ItkHdf5Container.read(OracleFixtures.bytes("itk_hdf5_simpleitk/malformed_missing_fixed.h5")) match
      case Left(TransformIoError.Malformed(_, reason)) => assert(reason.contains("missing TransformFixedParameters"), reason)
      case other                                      => fail(s"expected a malformed-file refusal, got $other")

  test("jHDF decodes every readable file to the h5py dump the shared ItkHdf5SimpleItkDumpSuite interprets"):
    Vector(
      "affine_only_forward_double.h5",
      "composite_affine_displacement_double.h5",
      "composite_affine_displacement_legacy_float.h5",
      "pullback_plus_one.h5",
      "pullback_minus_one.h5",
      "unsupported_bspline.h5"
    ).foreach: name =>
      val dumped = ItkHdf5Dumps.parse(OracleFixtures.text(s"itk_hdf5_simpleitk_dumps/${name.stripSuffix(".h5")}.components.txt"))
      assertEquals(file(name), dumped, name)

  test("every fixture loads from disk with SHA-256 provenance matching its manifest"):
    val manifest = OracleFixtures.text("itk_hdf5_simpleitk/manifest.json")
    val recorded = "\"([^\"]+\\.h5)\":\\s*\"([0-9a-f]{64})\"".r.findAllMatchIn(manifest).map(m => m.group(1) -> m.group(2)).toVector
    assertEquals(recorded.size, 7)
    recorded.foreach: (name, digest) =>
      assertEquals(OracleFixtures.sha256Hex(s"itk_hdf5_simpleitk/$name"), digest, name)
    Vector("affine_only_forward_double.h5", "composite_affine_displacement_legacy_float.h5", "pullback_plus_one.h5").foreach: name =>
      val loaded = ok(TransformFiles.load(path(name)))
      assertEquals(loaded.format, TransformFormat.ItkHdf5, name)
      assertEquals(loaded.asset.sha256, recorded.toMap.get(name), name)
