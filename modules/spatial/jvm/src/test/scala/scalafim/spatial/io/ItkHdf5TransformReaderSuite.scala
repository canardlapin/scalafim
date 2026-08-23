package scalafim.spatial.io

import scalafim.image.{DMat, NeuroSpace}
import scalafim.linalg.{DoubleMatrix, LinearMapError}
import scalafim.spatial.*

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

class ItkHdf5TransformReaderSuite extends munit.FunSuite:

  private final case class PointOracle(label: String, input: Vector[Double], output: Vector[Double])

  private def spatialValue[A](result: Either[SpatialError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def ioValue[A](result: Either[SpatialIoError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def linearValue[A](result: Either[LinearMapError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def fixture(name: String): Path =
    Path.of(
      Option(getClass.getResource(s"/scalafim/spatial/io/itk-hdf5/$name"))
        .getOrElse(fail(s"missing fixture $name"))
        .toURI
    )

  private lazy val pointOracles: Vector[PointOracle] =
    Files
      .readAllLines(fixture("point_oracles.tsv"))
      .asScala
      .drop(1)
      .toVector
      .map { line =>
        val fields = line.split("\t").toVector
        PointOracle(fields.head, fields.slice(1, 4).map(_.toDouble), fields.slice(4, 7).map(_.toDouble))
      }

  private def domain(
    name: String,
    space: NeuroSpace = NeuroSpace(Vector(5, 2, 2), trans = Some(DMat.eye(4)))
  ): Domain =
    val id = spatialValue(DomainId(name))
    val subject = spatialValue(SubjectId("sub-01"))
    val modality = spatialValue(Modality(name))
    val geometry = spatialValue(SamplingGeometry.volume(space))
    spatialValue(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def descriptor(
    source: Domain,
    target: Domain,
    path: Path,
    inverseQuality: InverseQuality = InverseQuality.Missing
  ): TransformDescriptor =
    ioValue(
      TransformDescriptor.fromFile(
        spatialValue(MorphismId(s"${source.id.value}-to-${target.id.value}-${path.getFileName}")),
        source.id,
        target.id,
        path,
        TransformFileFormat.ANTsH5,
        inverseQuality = inverseQuality
      )
    )

  private def assertPoint(actual: Vector[Double], expected: Vector[Double], tolerance: Double): Unit =
    actual.zip(expected).foreach { case (observed, oracle) =>
      assertEqualsDouble(observed, oracle, tolerance)
    }

  test("jHDF reads canonical ITK metadata and numerically ordered components"):
    val loaded = ioValue(ItkHdf5TransformReader.read(fixture("composite_affine_displacement_double.h5")))

    assertEquals(loaded.components.map(_.index), Vector(0, 1, 2))
    assertEquals(
      loaded.components.map(_.transformType),
      Vector(
        "CompositeTransform_double_3_3",
        "AffineTransform_double_3_3",
        "DisplacementFieldTransform_double_3_3"
      )
    )
    assertEquals(loaded.components.map(_.provenance.parameterCount), Vector(0, 12, 72))
    assertEquals(loaded.components.map(_.provenance.fixedParameterCount), Vector(0, 3, 18))
    assert(loaded.metadata.itkVersion.exists(_.nonEmpty))
    assert(loaded.metadata.hdfVersion.exists(_.nonEmpty))
    assert(!loaded.provenance.usesLegacyDatasetAliases)

  test("decoded composite point transforms agree with an independent SimpleITK oracle"):
    val source = domain("source")
    val target = domain("target")
    val decoded = ioValue(
      AntsHdf5TransformAdapter.read(
        fixture("composite_affine_displacement_double.h5"),
        source.id,
        target.id,
        scalafim.image.Resample.Method.Linear,
        1.0
      )
    )

    pointOracles.filter(_.label == "composite").foreach { oracle =>
      assertPoint(spatialValue(decoded.coordinateMap.transform(oracle.input)), oracle.output, 1e-10)
    }
    decoded.coordinateMap match
      case CoordinateMap.Composite3D(composite) =>
        assertEquals(composite.components.length, 2)
        assert(composite.containsDense)
      case other => fail(s"expected composite map, got $other")

  test("historic Tranform aliases and float datasets retain the same semantics"):
    val source = domain("source")
    val target = domain("target")
    val decoded = ioValue(
      AntsHdf5TransformAdapter.read(
        fixture("composite_affine_displacement_legacy_float.h5"),
        source.id,
        target.id,
        scalafim.image.Resample.Method.Linear,
        1.0
      )
    )

    assert(decoded.provenance.usesLegacyDatasetAliases)
    assert(decoded.provenance.components.drop(1).forall(_.transformType.contains("_float_")))
    pointOracles.filter(_.label == "composite").foreach { oracle =>
      assertPoint(spatialValue(decoded.coordinateMap.transform(oracle.input)), oracle.output, 2e-6)
    }

  test("TransformAssetLoader executes an HDF5 pullback through the one-pass operator compiler"):
    val rasGrid =
      DMat.fromRows(
        Vector(
          Vector(-1.0, 0.0, 0.0, 0.0),
          Vector(0.0, -1.0, 0.0, 0.0),
          Vector(0.0, 0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    val space = NeuroSpace(Vector(5, 2, 2), trans = Some(rasGrid))
    val source = domain("source", space)
    val target = domain("target", space)
    val asset = descriptor(source, target, fixture("pullback_plus_one.h5"))
    val loaded = ioValue(asset.load(source, target))
    val graph = spatialValue(SpatialGraph.build(Vector(source, target), Vector(loaded.morphism)))
    val operator = spatialValue(
      OperatorCompiler.compile(
        graph,
        CompileRequest(source.id, target.id, sampling = SamplingPolicy.Nearest, roi = Some(Vector(1)))
      )
    )
    val values = DoubleMatrix.fromRows(Vector.tabulate(source.nElements)(index => Vector(index.toDouble)))
    val result = linearValue(operator.forward(values))

    assertEquals(result.copyData.toVector, Vector(5.0))
    assertEquals(operator.provenance.compiler, "volume-pullback-fused-v1")
    assertEquals(loaded.provenance.options.direction, TransformDirection.PullbackTargetToSource)
    assertEquals(loaded.provenance.container.map(_.components.length), Some(2))
    assert(loaded.provenance.normalization.contains("ordered-RAS-mm-composite"))

  test("a supplied inverse HDF5 asset makes a nonlinear composite reversible"):
    val source = domain("source")
    val target = domain("target")
    val quality = ioValue(InverseQuality.provided("paired-HDF5", 1.0))
    val asset = descriptor(source, target, fixture("pullback_plus_one.h5"), quality)
    val inverse = TransformAssetSpec(fixture("pullback_minus_one.h5"), TransformFileFormat.ANTsH5)
    val loaded = ioValue(asset.load(source, target, inverseAsset = Some(inverse)))
    val reversed = spatialValue(loaded.morphism.reversed)

    pointOracles.filter(_.label == "constant-forward").foreach { oracle =>
      val transformed = spatialValue(loaded.morphism.coordinateMap.transform(oracle.input))
      val recovered = spatialValue(reversed.coordinateMap.transform(transformed))
      assertPoint(transformed, oracle.output, 1e-12)
      assertPoint(recovered, oracle.input, 1e-12)
    }
    assertEquals(loaded.provenance.inverseAsset.map(_._2.sha256.length), Some(64))

  test("an affine-only forward HDF5 composite is inverted exactly"):
    val source = domain("source")
    val target = domain("target")
    val asset = descriptor(source, target, fixture("affine_only_forward_double.h5"))
    val forward =
      TransformLoadOptions(
        TransformDirection.ForwardSourceToTarget,
        TransformCoordinateConvention.LpsMillimeters
      )
    val loaded = ioValue(asset.load(source, target, options = Some(forward)))

    pointOracles.filter(_.label == "affine-forward").foreach { oracle =>
      assertPoint(
        spatialValue(loaded.morphism.coordinateMap.transform(oracle.output)),
        oracle.input,
        1e-10
      )
    }

  test("forward nonlinear, unsupported, malformed, and convention-mismatched HDF5 fail explicitly"):
    val source = domain("source")
    val target = domain("target")
    val forward =
      TransformLoadOptions(
        TransformDirection.ForwardSourceToTarget,
        TransformCoordinateConvention.LpsMillimeters
      )
    descriptor(source, target, fixture("pullback_plus_one.h5"))
      .load(source, target, options = Some(forward))
      .left
      .toOption match
      case Some(SpatialIoError.TransformConventionMismatch(_, reason)) =>
        assert(reason.contains("requires an inverse HDF5 asset"))
      case other => fail(s"expected nonlinear direction failure, got $other")

    descriptor(source, target, fixture("unsupported_bspline.h5")).load(source, target).left.toOption match
      case Some(SpatialIoError.UnsupportedItkTransformType(_, 0, transformType)) =>
        assertEquals(transformType, "BSplineTransform_double_3_3")
      case other => fail(s"expected unsupported ITK transform failure, got $other")

    descriptor(source, target, fixture("malformed_missing_fixed.h5")).load(source, target).left.toOption match
      case Some(SpatialIoError.MalformedTransformAsset(_, reason)) =>
        assert(reason.contains("missing TransformFixedParameters"))
      case other => fail(s"expected malformed HDF5 failure, got $other")

    val wrongConvention =
      TransformLoadOptions(
        TransformDirection.PullbackTargetToSource,
        TransformCoordinateConvention.RasMillimeters
      )
    descriptor(source, target, fixture("pullback_plus_one.h5"))
      .load(source, target, options = Some(wrongConvention))
      .left
      .toOption match
      case Some(SpatialIoError.TransformConventionMismatch(_, reason)) =>
        assert(reason.contains("encoded in LPS"))
      case other => fail(s"expected HDF5 convention failure, got $other")
