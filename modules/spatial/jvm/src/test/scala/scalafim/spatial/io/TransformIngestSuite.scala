package scalafim.spatial.io

import reframe4s.field.CoordinateBoundaryPolicy
import scalafim.image.world.SubjectId
import scalafim.image.{SampleSpaces, SomeSampleSpace, SpatialPoint}
import scalafim.image.SampleSpaces.*
import scalafim.spatial.*
import scalafim.transform.{TransformError, TransformFormat, TransformIoError, WorldTransform}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** The spatial graph adapter over the transform codecs, on oracle-backed files (see the fixture manifests).
  *
  * Format conventions are the transform module's contract, tested there against native tools; this suite checks that
  * descriptors, domains, inverse assets and file endpoints become the right graph morphisms.
  */
class TransformIngestSuite extends munit.FunSuite:
  private def spatialValue[A](result: Either[SpatialError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def ioValue[A](result: Either[SpatialIoError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def linearValue[A](result: Either[LinearMapError, A]): A =
    result.fold(error => fail(error.getMessage), identity)

  private def fixture(relative: String): Path =
    Path.of(Option(getClass.getResource(s"/scalafim/spatial/io/$relative")).getOrElse(fail(s"missing fixture $relative")).toURI)

  private def manifestDigest(directory: String, name: String): String =
    val text = Files.readString(fixture(s"$directory/manifest.json"))
    s"\"${java.util.regex.Pattern.quote(name)}\":\\s*\"([0-9a-f]{64})\"".r.findFirstMatchIn(text).fold(fail(s"$name not in manifest"))(_.group(1))

  private def transformVector(map: CoordinateMap, point: Vector[Double]): Vector[Double] =
    spatialValue(map.transform(SpatialPoint.unsafeFromVector(point))).toVector

  private def assertPoint(actual: Vector[Double], expected: Vector[Double], tolerance: Double, clue: String)(using munit.Location): Unit =
    actual.zip(expected).foreach((a, e) => assertEqualsDouble(a, e, tolerance, clue))

  private def domain(name: String, space: SomeSampleSpace): Domain =
    val geometry = spatialValue(SamplingGeometry.volume(space))
    val reference = SpaceRef.Volume(spatialValue(SubjectId("sub-01").asSpatial), None, spatialValue(Modality(name)))
    spatialValue(Domain.build(spatialValue(DomainId(name)), reference, geometry))

  private val itkSpace: SomeSampleSpace = SampleSpaces(Vector(5, 2, 2), affine = Some(ProviderAffines.identity))

  private def descriptor(
      source: Domain,
      target: Domain,
      path: Path,
      format: TransformFormat,
      inverseQuality: InverseQuality = InverseQuality.Missing,
      endpoints: TransformFileEndpoints = TransformFileEndpoints.AsFile
  ): TransformDescriptor =
    ioValue(
      TransformDescriptor.fromFile(
        spatialValue(MorphismId(s"${source.id.value}-to-${target.id.value}")),
        source.id,
        target.id,
        path,
        format,
        inverseQuality = inverseQuality,
        endpoints = endpoints
      )
    )

  private final case class Row(label: String, input: Vector[Double], output: Vector[Double])

  private lazy val itkRows: Vector[Row] =
    Files.readAllLines(fixture("itk-hdf5/point_oracles.tsv")).asScala.toVector.drop(1).filter(_.nonEmpty).map: line =>
      val cells = line.split('\t').toVector
      Row(cells.head, cells.slice(1, 4).map(_.toDouble), cells.slice(4, 7).map(_.toDouble))

  private val itk = TransformLoadOptions(boundary = CoordinateBoundaryPolicy.PreserveSource)

  /** One `fsl_cases.tsv` volume: its dims and FSL-selected voxel-to-world matrix (the sform for cases 0 and 1). */
  private def fslDomain(name: String, index: Int): Domain =
    val table = Files.readAllLines(fixture("flirt/fsl_cases.tsv")).asScala.toVector
    val header = table.head.split('\t').toVector
    val row = table.tail.map(_.split('\t').toVector.map(_.toDouble)).find(_(0) == index.toDouble).getOrElse(fail(s"no FSL case $index"))
    val dims = Vector("nx", "ny", "nz").map(column => row(header.indexOf(column)).toInt)
    val v2w = row.slice(header.indexOf("v2w00"), header.indexOf("v2w00") + 16)
    domain(name, SampleSpaces(dims, affine = Some(ProviderAffines.fromRowMajor(v2w))))

  test("a FLIRT matrix between two FSL volumes becomes the RAS pullback fslpy's world mapping inverts"):
    val source = fslDomain("flirt-input", 0)
    val reference = fslDomain("flirt-reference", 1)
    val exact = ioValue(InverseQuality.exact("affine inverse"))
    val loaded = ioValue(descriptor(source, reference, fixture("flirt/flirt_0_to_1.mat"), TransformFormat.FslFlirt, exact).load(source, reference))
    val pairs = Files.readAllLines(fixture("flirt/flirt_pairs.tsv")).asScala.toVector
    val header = pairs.head.split('\t').toVector
    val row = pairs.tail.map(_.split('\t').toVector.map(_.toDouble)).find(r => r(0) == 0.0 && r(1) == 1.0).getOrElse(fail("no 0 -> 1 pair"))
    val world = row.slice(header.indexOf("world00"), header.indexOf("world00") + 16) // fslpy: source world -> reference world
    val reversed = spatialValue(loaded.morphism.reversed)
    Vector(Vector(10.0, -20.0, 30.0), Vector(-5.5, 3.25, 0.0), Vector(40.0, 12.0, -8.0)).foreach: p =>
      val q = Vector.tabulate(3)(r => world(4 * r) * p(0) + world(4 * r + 1) * p(1) + world(4 * r + 2) * p(2) + world(4 * r + 3))
      // The domain carries the sform as float32-stored in the header; FLIRT text has 10 decimals.
      assertPoint(transformVector(loaded.morphism.coordinateMap, q), p, 1e-5, s"pullback at $q")
      assertPoint(transformVector(reversed.coordinateMap, p), q, 1e-5, s"forward at $p")
    assert(loaded.transform.isInstanceOf[WorldTransform.Linear[?, ?]])
    assertEquals(loaded.morphism.kind, MorphismKind.Affine3D)
    assertEquals(loaded.provenance.format, TransformFormat.FslFlirt)
    assertEquals(loaded.provenance.primary.sha256, Some(manifestDigest("flirt", "flirt_0_to_1.mat")))

  test("an affine-only ITK HDF5 file routes either way: as stored, or reversed through its exact inverse"):
    val moving = domain("moving", itkSpace)
    val fixed = domain("fixed", itkSpace)
    val path = fixture("itk-hdf5/affine_only_forward_double.h5")
    val asFile = ioValue(descriptor(moving, fixed, path, TransformFormat.ItkHdf5).load(moving, fixed))
    // Declared the other way round: the graph edge fixed -> moving uses the file's forward map as its pullback.
    val reversed = ioValue(descriptor(fixed, moving, path, TransformFormat.ItkHdf5, endpoints = TransformFileEndpoints.Reversed).load(fixed, moving))
    itkRows.filter(_.label == "affine-forward").foreach: row =>
      assertPoint(transformVector(asFile.morphism.coordinateMap, row.input), row.output, 1e-10, s"as stored at ${row.input}")
      assertPoint(transformVector(reversed.morphism.coordinateMap, row.output), row.input, 1e-10, s"reversed at ${row.output}")
    assert(asFile.transform.isInstanceOf[WorldTransform.Linear[?, ?]])
    assertEquals(reversed.provenance.endpoints, TransformFileEndpoints.Reversed)

  test("an ITK HDF5 displacement pullback executes through the one-pass operator compiler"):
    val lps = ProviderAffines.fromRows(
      Vector(Vector(-1.0, 0.0, 0.0, 0.0), Vector(0.0, -1.0, 0.0, 0.0), Vector(0.0, 0.0, 1.0, 0.0), Vector(0.0, 0.0, 0.0, 1.0))
    )
    val space = SampleSpaces(Vector(5, 2, 2), affine = Some(lps))
    val source = domain("source", space)
    val target = domain("target", space)
    val loaded = ioValue(descriptor(source, target, fixture("itk-hdf5/pullback_plus_one.h5"), TransformFormat.ItkHdf5).load(source, target, itk))
    val graph = spatialValue(SpatialGraph.build(Vector(source, target), Vector(loaded.morphism)))
    val operator = spatialValue(
      OperatorCompiler.compile(graph, CompileRequest(source.id, target.id, sampling = SamplingPolicy.Nearest, roi = Some(Vector(1))))
    )
    val values = DoubleMatrix.fromRows(Vector.tabulate(source.nElements)(index => Vector(index.toDouble)))

    // The constant field pulls every target point one voxel along the grid's x axis (see the constant-forward rows).
    assertEquals(linearValue(operator.forward(values)).copyData.toVector, Vector(5.0))
    assertEquals(operator.provenance.compiler, "volume-pullback-fused-v1")
    assertEquals(loaded.morphism.kind, MorphismKind.Warp3D)
    assertEquals(loaded.provenance.primary.sha256, Some(manifestDigest("itk-hdf5", "pullback_plus_one.h5")))
    loaded.morphism.coordinateMap match
      case CoordinateMap.Geometric(binding) => assert(binding.containsDense && binding.inversePullback.isEmpty)
      case other                            => fail(s"expected a provider map, got $other")

  test("an affine + displacement composite keeps SimpleITK's point semantics in the graph"):
    val moving = domain("moving", itkSpace)
    val fixed = domain("fixed", itkSpace)
    val loaded = ioValue(descriptor(moving, fixed, fixture("itk-hdf5/composite_affine_displacement_double.h5"), TransformFormat.ItkHdf5).load(moving, fixed, itk))
    itkRows.filter(_.label == "composite").foreach: row =>
      assertPoint(transformVector(loaded.morphism.coordinateMap, row.input), row.output, 1e-10, s"composite at ${row.input}")

  test("a supplied inverse asset makes a dense route reversible, and a declared inverse without one is refused"):
    val moving = domain("moving", itkSpace)
    val fixed = domain("fixed", itkSpace)
    val quality = ioValue(InverseQuality.provided("paired HDF5", 1.0))
    val forward = descriptor(moving, fixed, fixture("itk-hdf5/pullback_plus_one.h5"), TransformFormat.ItkHdf5, quality)
    val inverseAsset = TransformAssetSpec(fixture("itk-hdf5/pullback_minus_one.h5"))

    forward.load(moving, fixed, itk).left.toOption match
      case Some(SpatialIoError.InvalidTransformDescriptor(_, reason)) => assert(reason.contains("requires an executable inverse asset"), reason)
      case other                                                      => fail(s"expected a missing-inverse refusal, got $other")

    val loaded = ioValue(forward.load(moving, fixed, itk, Some(inverseAsset)))
    val reversed = spatialValue(loaded.morphism.reversed)
    itkRows.filter(_.label == "constant-forward").foreach: row =>
      val pulled = transformVector(loaded.morphism.coordinateMap, row.input)
      assertPoint(pulled, row.output, 1e-12, s"pullback at ${row.input}")
      assertPoint(transformVector(reversed.coordinateMap, pulled), row.input, 1e-12, s"round trip at ${row.input}")
    assertEquals(loaded.morphism.inverse, Inverse.Provided("paired HDF5", 1.0))
    assertEquals(loaded.provenance.inverseAsset.flatMap(_._2.sha256), Some(manifestDigest("itk-hdf5", "pullback_minus_one.h5")))

  test("a reversed dense route needs the inverse asset; without it the refusal is typed"):
    val moving = domain("moving", itkSpace)
    val fixed = domain("fixed", itkSpace)
    val reversed = descriptor(fixed, moving, fixture("itk-hdf5/pullback_plus_one.h5"), TransformFormat.ItkHdf5, endpoints = TransformFileEndpoints.Reversed)
    reversed.load(fixed, moving, itk).left.toOption match
      case Some(SpatialIoError.TransformInterpretation(_, TransformError.NoForwardMap(_))) => ()
      case other => fail(s"expected a no-forward-map refusal, got $other")

    val loaded = ioValue(reversed.load(fixed, moving, itk, Some(TransformAssetSpec(fixture("itk-hdf5/pullback_minus_one.h5")))))
    itkRows.filter(_.label == "constant-inverse").foreach: row =>
      assertPoint(transformVector(loaded.morphism.coordinateMap, row.input), row.output, 1e-12, s"reversed pullback at ${row.input}")

  test("descriptor, tool and file mismatches are typed failures"):
    val moving = domain("moving", itkSpace)
    val fixed = domain("fixed", itkSpace)
    val path = fixture("itk-hdf5/pullback_plus_one.h5")
    val wrongTool = ioValue(
      TransformDescriptor.build(spatialValue(MorphismId("wrong-tool")), moving.id, fixed.id, TransformTool.FSL, TransformKind.Warp3D, path)
    )
    wrongTool.load(moving, fixed).left.toOption match
      case Some(SpatialIoError.InvalidTransformDescriptor(_, reason)) => assert(reason.contains("does not match"), reason)
      case other                                                      => fail(s"expected a tool mismatch, got $other")

    val affineOnly = ioValue(
      TransformDescriptor.build(spatialValue(MorphismId("affine-kind")), moving.id, fixed.id, TransformTool.ANTs, TransformKind.Affine3D, path)
    )
    assert(affineOnly.load(moving, fixed, itk).isLeft, "a dense file cannot back an Affine3D morphism")

    descriptor(moving, fixed, Path.of("/nonexistent/xfm.h5"), TransformFormat.ItkHdf5).load(moving, fixed).left.toOption match
      case Some(SpatialIoError.TransformRead(_, _: TransformIoError)) => ()
      case other                                                     => fail(s"expected a typed read failure, got $other")

    descriptor(moving, fixed, path, TransformFormat.ItkHdf5).load(fixed, moving).left.toOption match
      case Some(SpatialIoError.InvalidTransformDescriptor(_, reason)) => assert(reason.contains("does not match supplied domains"), reason)
      case other                                                      => fail(s"expected a route mismatch, got $other")
