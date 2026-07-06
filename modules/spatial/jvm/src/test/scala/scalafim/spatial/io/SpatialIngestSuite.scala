package scalafim.spatial.io

import scalafim.image.{DMat, NeuroSpace}
import scalafim.spatial.*

import java.nio.file.Path

class SpatialIngestSuite extends munit.FunSuite:

  private def value[A](result: Either[SpatialError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def ioValue[A](result: Either[SpatialIoError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def domain(name: String): Domain =
    val id = value(DomainId(name))
    val subject = value(SubjectId("sub-01"))
    val modality = value(Modality(name))
    val geometry = value(SamplingGeometry.volume(NeuroSpace(Vector(2, 1, 1), trans = Some(DMat.eye(4)))))
    value(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  test("missing inverse quality is explicit and becomes a non-geometric inverse"):
    val source = domain("func")
    val target = domain("t1w")
    val descriptor =
      ioValue(
        TransformDescriptor.build(
          id = value(MorphismId("func-to-t1w")),
          source = source.id,
          target = target.id,
          tool = TransformTool.ANTs,
          kind = TransformKind.Warp3D,
          path = Path.of("sub-01_from-bold_to-T1w_xfm.h5")
        )
      )

    val morphism = ioValue(descriptor.toMorphism)

    assertEquals(descriptor.inverseQuality, InverseQuality.Missing)
    assert(!descriptor.inverseQualityDeclared)
    assertEquals(
      descriptor.requireInverseQuality.left.toOption,
      Some(SpatialIoError.MissingInverseQuality("func-to-t1w"))
    )
    assertEquals(morphism.inverse, Inverse.None)

  test("fMRIPrep graph construction maps transform descriptors into typed spatial routes"):
    val func = domain("func")
    val t1w = domain("t1w")
    val inverseQuality = ioValue(InverseQuality.provided("xfm-sidecar", 0.8))
    val descriptor =
      ioValue(
        TransformDescriptor.build(
          id = value(MorphismId("func-to-t1w")),
          source = func.id,
          target = t1w.id,
          tool = TransformTool.FSL,
          kind = TransformKind.Affine3D,
          path = Path.of("sub-01_from-bold_to-T1w.mat"),
          inverseQuality = inverseQuality,
          coordinateMap = value(CoordinateMap.affine3D(DMat.eye(4)))
        )
      )
    val subject = value(SubjectId("sub-01"))
    val session = Some(value(SessionId("ses-01")))
    val spec = ioValue(FmriprepSpatialGraph.build(subject, session, Vector(func, t1w), Vector(descriptor)))
    val graph = ioValue(spec.toSpatialGraph)

    val forward = value(graph.path(func.id, t1w.id))
    val reverse = value(graph.path(t1w.id, func.id, allowInverses = true))

    assertEquals(spec.subject, subject)
    assertEquals(spec.session, session)
    assertEquals(descriptor.tool, TransformTool.FSL)
    assertEquals(forward.ids.map(_.value), Vector("func-to-t1w"))
    assertEquals(reverse.ids.map(_.value), Vector("func-to-t1w:inverse"))
    assertEqualsDouble(reverse.pathQuality, 0.8, 1e-12)

  test("transform file formats classify common neurotransform IO surfaces"):
    assertEquals(TransformFileFormat.detect(Path.of("xfm.h5")), Some(TransformFileFormat.ANTsH5))
    assertEquals(TransformFileFormat.detect(Path.of("register.lta")), Some(TransformFileFormat.FreeSurferLta))
    assertEquals(TransformFileFormat.detect(Path.of("warp.x5")), Some(TransformFileFormat.X5))
    assertEquals(TransformFileFormat.detect(Path.of("epi.aff12.1D")), Some(TransformFileFormat.AfniAffine))
    assertEquals(TransformFileFormat.detect(Path.of("sub-01_fnirt_warpcoef.nii.gz")), Some(TransformFileFormat.FslFnirt))
    assertEquals(TransformFileFormat.detect(Path.of("flirt.mat")), Some(TransformFileFormat.FslFlirt))

    val func = domain("func")
    val template = domain("template")
    val descriptor =
      ioValue(
        TransformDescriptor.fromFile(
          id = value(MorphismId("func-to-template")),
          source = func.id,
          target = template.id,
          path = Path.of("sub-01_from-func_to-template_xfm.h5"),
          format = TransformFileFormat.ANTsH5
        )
      )

    assertEquals(descriptor.tool, TransformTool.ANTs)
    assertEquals(descriptor.kind, TransformKind.Warp3D)

  test("inverse quality smart constructors reject invalid scores"):
    val result = InverseQuality.approximate("afni inverse", 1.1)

    result.left.toOption match
      case Some(SpatialIoError.InvalidTransformDescriptor(label, reason)) =>
        assertEquals(label, "approximate inverse")
        assert(reason.contains("quality must be in [0, 1]"))
      case other =>
        fail(s"expected invalid inverse quality, got $other")
