package scalafim.spatial.io

import narr.NArray
import scalafim.image.{Axis, DMat, NeuroSpace, NeuroVec}
import scalafim.image.io.Nifti
import scalafim.linalg.{DoubleMatrix, LinearMapError}
import scalafim.spatial.*

import java.nio.file.{Files, Path}

class TransformAssetLoaderSuite extends munit.FunSuite:

  private def spatialValue[A](result: Either[SpatialError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def ioValue[A](result: Either[SpatialIoError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def linearValue[A](result: Either[LinearMapError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def domain(name: String, space: NeuroSpace = NeuroSpace(Vector(2, 1, 1), trans = Some(DMat.eye(4)))): Domain =
    val id = spatialValue(DomainId(name))
    val subject = spatialValue(SubjectId("sub-01"))
    val modality = spatialValue(Modality(name))
    val geometry = spatialValue(SamplingGeometry.volume(space))
    spatialValue(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def descriptor(
    source: Domain,
    target: Domain,
    path: Path,
    format: TransformFileFormat,
    inverse: InverseQuality = InverseQuality.Missing
  ): TransformDescriptor =
    ioValue(
      TransformDescriptor.fromFile(
        spatialValue(MorphismId(s"${source.id.value}-to-${target.id.value}-${format.toString.toLowerCase}")),
        source.id,
        target.id,
        path,
        format,
        inverseQuality = inverse
      )
    )

  private def withDirectory[A](body: Path => A): A =
    val directory = Files.createTempDirectory("scalafim-transform-loader-")
    try body(directory)
    finally
      val children = Files.list(directory)
      try children.forEach(path => Files.deleteIfExists(path))
      finally children.close()
      Files.deleteIfExists(directory)

  private def writeDenseField(path: Path, space: NeuroSpace, components: Vector[Vector[Double]]): Unit =
    require(components.length == 3)
    require(components.forall(_.length == space.spatialDims.product))
    val values = NArray.ofSize[Double](components.map(_.length).sum)
    var component = 0
    var offset = 0
    while component < components.length do
      var i = 0
      while i < components(component).length do
        values(offset + i) = components(component)(i)
        i += 1
      offset += components(component).length
      component += 1
    val vectorSpace = space.addDim(3, Some(Axis("Vector")))
    Nifti.writeVec(path, NeuroVec.fromLinear(values, vectorSpace))

  test("ANTs, FSL, and AFNI affine adapters normalize native direction and orientation to one RAS pullback"):
    withDirectory { directory =>
      val source = domain("source")
      val target = domain("target")
      val antsPath = directory.resolve("source-to-targetGenericAffine.mat")
      val fslPath = directory.resolve("source-to-target.mat")
      val afniPath = directory.resolve("source-to-target.aff12.1D")

      Files.writeString(
        antsPath,
        """#Insight Transform File V1.0
          |#Transform 0
          |Transform: AffineTransform_double_3_3
          |Parameters: 1 0 0 0 1 0 0 0 1 2 0 0
          |FixedParameters: 4 5 6
          |""".stripMargin
      )
      Files.writeString(
        fslPath,
        """1 0 0 2
          |0 1 0 0
          |0 0 1 0
          |0 0 0 1
          |""".stripMargin
      )
      Files.writeString(afniPath, "1 0 0 2 0 1 0 0 0 0 1 0\n")

      val cases = Vector(
        descriptor(source, target, antsPath, TransformFileFormat.AntsAffine),
        descriptor(source, target, fslPath, TransformFileFormat.FslFlirt),
        descriptor(source, target, afniPath, TransformFileFormat.AfniAffine)
      )
      val expected = Vector(3.0, 2.0, 3.0)

      cases.foreach { asset =>
        val loaded = ioValue(asset.load(source, target))
        val actual = spatialValue(loaded.morphism.coordinateMap.transform(Vector(1.0, 2.0, 3.0)))
        actual.zip(expected).foreach { case (observed, oracle) =>
          assertEqualsDouble(observed, oracle, 1e-10)
        }
        assertEquals(loaded.provenance.primaryPath, asset.path.toAbsolutePath.normalize())
        assertEquals(loaded.provenance.fingerprint.sha256.length, 64)
        assert(loaded.provenance.normalization.contains("gale.linalg.DMat"))
      }
    }

  test("an ANTs LPS displacement fixture becomes an executable absolute RAS pullback"):
    withDirectory { directory =>
      val space = NeuroSpace(Vector(2, 1, 1), trans = Some(DMat.eye(4)))
      val source = domain("source", space)
      val target = domain("target", space)
      val path = directory.resolve("ants-Warp.nii")
      writeDenseField(path, space, Vector(Vector(1.0, 1.0), Vector(0.0, 0.0), Vector(0.0, 0.0)))

      val loaded = ioValue(descriptor(source, target, path, TransformFileFormat.AntsDisplacement).load(source, target))
      val atOne = spatialValue(loaded.morphism.coordinateMap.transform(Vector(1.0, 0.0, 0.0)))
      assertEqualsDouble(atOne(0), 0.0, 1e-10)
      assertEqualsDouble(atOne(1), 0.0, 1e-10)
      assertEqualsDouble(atOne(2), 0.0, 1e-10)

      val graph = spatialValue(SpatialGraph.build(Vector(source, target), Vector(loaded.morphism)))
      val operator = spatialValue(OperatorCompiler.compile(graph, CompileRequest(source.id, target.id)))
      val result = linearValue(operator.forward(DoubleMatrix.fromRows(Vector(Vector(10.0), Vector(20.0)))))
      assertEquals(result.copyData.toVector, Vector(0.0, 10.0))
      assertEquals(operator.provenance.compiler, "volume-pullback-fused-v1")
      assertEquals(loaded.provenance.options.convention, TransformCoordinateConvention.LpsMillimeters)
      assertEquals(loaded.provenance.normalization, "LpsMillimeters:Displacement:PullbackTargetToSource->absolute-RAS-mm-pullback")
    }

  test("dense inverse claims require an executable inverse asset and retain inverse quality"):
    withDirectory { directory =>
      val space = NeuroSpace(Vector(2, 1, 1), trans = Some(DMat.eye(4)))
      val source = domain("source", space)
      val target = domain("target", space)
      val forwardPath = directory.resolve("ants-Warp.nii")
      val inversePath = directory.resolve("ants-InverseWarp.nii")
      writeDenseField(forwardPath, space, Vector(Vector(1.0, 1.0), Vector(0.0, 0.0), Vector(0.0, 0.0)))
      writeDenseField(inversePath, space, Vector(Vector(-1.0, -1.0), Vector(0.0, 0.0), Vector(0.0, 0.0)))
      val quality = ioValue(InverseQuality.provided("inverse-warp", 0.9))
      val declared = descriptor(source, target, forwardPath, TransformFileFormat.AntsDisplacement, quality)

      declared.load(source, target).left.toOption match
        case Some(SpatialIoError.InvalidTransformDescriptor(_, reason)) =>
          assert(reason.contains("requires an executable inverse asset"))
        case other => fail(s"expected missing executable inverse failure, got $other")

      val inverseAsset = TransformAssetSpec(inversePath, TransformFileFormat.AntsDisplacement)
      val loaded = ioValue(declared.load(source, target, inverseAsset = Some(inverseAsset)))
      assertEquals(loaded.morphism.inverse, Inverse.Provided("inverse-warp", 0.9))
      assert(loaded.provenance.inverseAsset.nonEmpty)
      val reversed = spatialValue(loaded.morphism.reversed)
      val back = spatialValue(reversed.coordinateMap.transform(Vector(0.0, 0.0, 0.0)))
      assertEqualsDouble(back(0), 1.0, 1e-10)
    }

  test("malformed, convention-incompatible, and unsupported assets fail with typed IO errors"):
    withDirectory { directory =>
      val source = domain("source")
      val target = domain("target")
      val malformed = directory.resolve("broken.mat")
      Files.writeString(malformed, "1 2 3\n")
      descriptor(source, target, malformed, TransformFileFormat.FslFlirt).load(source, target).left.toOption match
        case Some(SpatialIoError.MalformedTransformAsset(path, reason)) =>
          assertEquals(path, malformed)
          assert(reason.contains("expected 16"))
        case other => fail(s"expected malformed asset error, got $other")

      val h5 = directory.resolve("composite.h5")
      Files.write(h5, Array[Byte](1, 2, 3))
      descriptor(source, target, h5, TransformFileFormat.ANTsH5).load(source, target).left.toOption match
        case Some(SpatialIoError.MalformedTransformAsset(path, reason)) =>
          assertEquals(path, h5)
          assert(reason.nonEmpty)
        case other => fail(s"expected malformed HDF5 error, got $other")

      val dense = directory.resolve("ants-Warp.nii")
      writeDenseField(dense, NeuroSpace(Vector(2, 1, 1), trans = Some(DMat.eye(4))), Vector.fill(3)(Vector(0.0, 0.0)))
      val forwardDense =
        TransformLoadOptions(
          TransformDirection.ForwardSourceToTarget,
          TransformCoordinateConvention.LpsMillimeters
        )
      descriptor(source, target, dense, TransformFileFormat.AntsDisplacement)
        .load(source, target, options = Some(forwardDense))
        .left
        .toOption match
        case Some(SpatialIoError.TransformConventionMismatch(path, reason)) =>
          assertEquals(path, dense)
          assert(reason.contains("cannot be inverted exactly"))
        case other => fail(s"expected convention mismatch, got $other")
    }
