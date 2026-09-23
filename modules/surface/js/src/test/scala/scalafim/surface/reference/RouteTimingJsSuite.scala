package scalafim.surface.reference

import scalafim.image.*
import scalafim.image.SampleSpaces.indexToGrid3D
import scalafim.surface.*

import scala.scalajs.js

/** WS5 resource budget on Scala.js. There is no Scala.js NIfTI or point-map
  * directory reader, so this is a synthetic run of the same size: a
  * 97×115×97 volume on the res-2 grid (finite inside an ellipsoid, NaN
  * outside), a 32 492-vertex hemisphere inside it, and a synthetic displacement
  * point map on a 193×229×193 grid for the inverse placement at admission.
  * Enabled only with `SCALAFIM_JS_TIMING=1`; prints median warm timings.
  */
class RouteTimingJsSuite extends munit.FunSuite:
  override def munitTimeout = scala.concurrent.duration.Duration(20, "min")

  private def enabled: Boolean =
    js.typeOf(js.Dynamic.global.process) != "undefined" &&
      js.Dynamic.global.process.env.SCALAFIM_JS_TIMING.asInstanceOf[js.UndefOr[String]].toOption.contains("1")

  private def now(): Double = js.Dynamic.global.performance.now().asInstanceOf[Double]

  private def median(values: Seq[Double]): Double = values.sorted.apply(values.size / 2)

  test("synthetic same-size hemisphere: placement at admission and prepare+map timings"):
    assume(enabled, "set SCALAFIM_JS_TIMING=1 to run the Scala.js resource measurement")
    val release = TemplateRelease.unsafe("templateflow@synthetic")
    val a = TemplateFrame.make(TemplateId.unsafe("MNI152NLin2009cAsym"), release).toOption.get
    val b = TemplateFrame.make(TemplateId.unsafe("MNI152NLin6Asym"), release).toOption.get
    val grid = PointMapFixtures.affine(2, 0, 0, -96.5, 0, 2, 0, -132.5, 0, 0, 2, -78.5, 0, 0, 0, 1)
    val space = SampleSpaces(Vector(97, 115, 97), affine = Some(grid))
    val values = Array.tabulate(97 * 115 * 97) { o =>
      val g = space.indexToGrid3D(o)
      val (x, y, z) = ((g(0) - 48) / 40.0, (g(1) - 57) / 50.0, (g(2) - 48) / 40.0)
      if x * x + y * y + z * z <= 1.0 then math.sin(o.toDouble) else Double.NaN
    }
    val volume = SomeScalarVolume.unsafeCopyFromCanonicalArray(values, space, "synthetic")

    // 32 492 vertices on a strip spiralling over an ellipsoid inside the brain region.
    val n = 32492
    val coordinates = Array.tabulate(3 * n) { flat =>
      val i = flat / 3
      val t = i.toDouble / n
      val theta = math.acos(1 - 2 * t)
      val phi = i * 2.399963229728653
      flat % 3 match
        case 0 => 60.0 * math.sin(theta) * math.cos(phi)
        case 1 => -18.0 + 80.0 * math.sin(theta) * math.sin(phi)
        case _ => 18.0 + 60.0 * math.cos(theta)
    }
    val faces = Array.tabulate(3 * (n - 2))(f => f / 3 + f % 3)
    val geometry = SurfaceGeometry(TriangleMesh.fromArrays(coordinates, faces), Hemisphere.Left, SurfaceKind.Midthickness)
    val mesh = StandardCorticalMesh.FsLR32k
    val reference = CorticalMeshReference.make(mesh, geometry, MedialWallMask.fromCortexFlags(
      geometry.meshDomainEither.toOption.get, Vector.tabulate(n)(_ % 11 != 0)).toOption.get).toOption.get
    val declaration = FrameDeclaration.make(b, FrameBasis.literature("10.1093/cercor/bhr291", "synthetic timing fixture")
      .toOption.get, DataAsset.make("synthetic.surf.gii", "1" * 64).toOption.get).toOption.get
    val anatomy = SamplingAnatomy.make(reference,
      AnatomicalGeometry.Midthickness(DeclaredSurface.unsafeAssumeVerified(declaration, geometry))).toOption.get

    // Synthetic A -> B point map on the real field's grid size (193x229x193, 1 mm) plus an affine.
    val fieldGrid = PointMapFixtures.affine(1, 0, 0, -96, 0, 1, 0, -132, 0, 0, 1, -78, 0, 0, 0, 1)
    val dims = Vector(193, 229, 193)
    val count = dims.product
    // Smooth displacement (up to 1.5 mm), like a template-to-template warp.
    val field = Array.tabulate(3 * count) { flat =>
      val c = flat / count
      val v = flat % count
      val (i, j, k) = (v % 193, (v / 193) % 229, v / (193 * 229))
      1.5 * math.sin(i / 23.0 + 1.3 * c) * math.cos(j / 29.0) * math.sin(k / 31.0 + c)
    }
    val pointMap = DeclaredPointMap.unsafeAssumeVerified(a.template, b.template, Some(release), PointMap.make(Vector(
      PointMapStage.DisplacementStage(DisplacementField.make(dims, fieldGrid, field).toOption.get),
      PointMapStage.AffineStage(PointMapFixtures.affine(0.985, 0.001, -0.003, 0.69, -0.001, 0.986, -0.001, -0.27,
        0.003, 0.011, 0.972, 0.29, 0, 0, 0, 1)))).toOption.get)
    val bridge = FrameBridge.displacement(pointMap, PointMapUse.Inverse(InversePolicy.make(1e-6, 50).toOption.get)).toOption.get
    val request = RouteRequest(VolumeReference.make(a, space).toOption.get, mesh, CorticalHemisphere.Left,
      MappingMethod.MidthicknessNearest, ValueSemantics.Continuous)
    val source = DeclaredVolume.unsafeAssumeVerified(FrameDeclaration.make(a,
      FrameBasis.derived("synthetic", Vector(DataAsset.make("bundle", "2" * 64).toOption.get)).toOption.get,
      DataAsset.make("synthetic.nii.gz", "3" * 64).toOption.get).toOption.get, volume)

    def admit() = SurfaceRoute.admit(request, anatomy, Some(bridge)).fold(r => fail(r.message), r => r)
    val admission = (0 until 4).map { _ =>
      val t0 = now(); admit(); now() - t0
    }
    val route = admit()
    for _ <- 0 until 3 do route.map(source)
    val prepareAndMap = (0 until 7).map { _ =>
      val t0 = now(); route.map(source).fold(e => fail(e.message), identity); now() - t0
    }
    val mapped = route.map(source).toOption.get
    println(f"Scala.js synthetic same-size: admission+placement median ${median(admission.drop(1))}%.1f ms " +
      f"(first ${admission.head}%.1f ms); prepare+map median ${median(prepareAndMap)}%.1f ms, max ${prepareAndMap.max}%.1f ms; " +
      s"mapped ${mapped.count(VertexCoverage.Mapped)}, unavailable ${mapped.count(VertexCoverage.BridgeUnavailable)}")
    assert(mapped.count(VertexCoverage.Mapped) > n / 2, "fixture must exercise real lookups")
