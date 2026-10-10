package scalafim.examples.reference

import scalafim.surface.*
import scalafim.surface.reference.*
import java.nio.file.{Files, Path}

/** Ordinary public APIs only: locked bytes, declared route, values and picks.
  * `synthetic` is an asymmetric control, never anatomical registration evidence.
  * The real MNI152NLin2009cAsym -> fsLR 32k example is `scalafim.atlas.reference.Fslr32kRouteExample`,
  * which obtains the qualified route from the public atlas entry point.
  */
object FslrRouteExample:
  final case class ExampleResult(route: AdmittedSurfaceRoute, source: DeclaredVolume,
      mapped: MappedSurfaceValues, displays: Vector[DisplayedSurfaceValues], receipt: ujson.Value)

  private def checked[E, A](value: Either[E, A]): A = value.fold(e => throw new IllegalArgumentException(e.toString), identity)

  def main(args: Array[String]): Unit =
    require(args.length == 3, "synthetic <manifest> <receipt>")
    val (receipt, out) = args(0) match
      case "synthetic" => (synthetic(Path.of(args(1))).receipt, Path.of(args(2)))
      case other => throw new IllegalArgumentException(s"unknown example $other")
    Files.writeString(out, ujson.write(receipt, indent = 2))
    println(s"wrote $out")

  def synthetic(manifestPath: Path): ExampleResult =
    val raw = Files.readAllBytes(manifestPath)
    val lock = ujson.read(raw)
    require(lock("schema").str == "scalafim.surface-route-example/1")
    val directory = manifestPath.toAbsolutePath.getParent
    val frame = TemplateFrame.unsafe(lock("frame")("template").str, lock("frame")("release").str)
    val manifest = checked(DataAsset.make("manifest.json", AssetSha256.of(raw).value))
    val basis = checked(FrameBasis.derived(lock("recipe").str, Vector(manifest)))
    def asset(name: String): DataAsset =
      val entry = lock("assets").arr.find(_("path").str == name).get
      checked(DataAsset.make(name, entry("sha256").str))
    def declaration(name: String): FrameDeclaration = checked(FrameDeclaration.make(frame, basis, asset(name)))
    val source = checked(DeclaredVolumeReader.readNifti(directory.resolve("source.nii"), declaration("source.nii")))
    val anatomy = checked(DeclaredSurfaceReader.read(directory.resolve("midthickness.surf.gii"),
      declaration("midthickness.surf.gii"), Hemisphere.Left, SurfaceKind.Midthickness))
    val domain = checked(anatomy.geometry.meshDomainEither)
    val wall = checked(DeclaredMedialWallReader.read(directory.resolve("cortex.label.gii"), domain, asset("cortex.label.gii")))
    val mesh = checked(StandardCorticalMesh.declare(CorticalMeshFamily.FsLR, "synthetic-4", 4))
    val reference = checked(CorticalMeshReference.make(mesh, anatomy.geometry, wall))
    val request = RouteRequest(checked(VolumeReference.make(frame, source.volume.space)), mesh,
      CorticalHemisphere.Left, MappingMethod.MidthicknessNearest, ValueSemantics.Continuous)
    val route = checked(SurfaceRoute.admit(request,
      checked(SamplingAnatomy.make(reference, AnatomicalGeometry.Midthickness(anatomy)))))
    val prepared = checked(route.prepare(source))
    val mapped = checked(route.map(prepared))
    val inspections = Vector.tabulate(4)(i => checked(route.inspect(prepared, VertexId(i))))
    val displays = Vector("inflated" -> SurfaceKind.Inflated, "veryinflated" -> SurfaceKind.VeryInflated).map: (name, kind) =>
      val geometry = checked(DeclaredSurfaceReader.read(directory.resolve(s"$name.surf.gii"),
        declaration(s"$name.surf.gii"), Hemisphere.Left, kind)).geometry
      checked(mapped.onDisplay(checked(DisplaySurface.make(reference, geometry))))
    val receipt = ujson.Obj(
      "schema" -> "scalafim.surface-route-example-receipt/1",
      "manifestSha256" -> manifest.sha256.value,
      "qualification" -> route.disclosure.qualification.toString,
      "scope" -> "synthetic asymmetric control, not fsLR32k anatomical qualification",
      "source" -> source.declaration.display,
      "anatomy" -> anatomy.declaration.display,
      "medialWall" -> wall.asset.get.display,
      "method" -> route.disclosure.method.label,
      "lookup" -> route.disclosure.lookup,
      "displayPreservesMappedObject" -> displays.forall(_.mapped eq mapped),
      "vertices" -> ujson.Arr.from(inspections.map: r =>
        ujson.Obj("vertex" -> r.vertex.index, "coverage" -> r.coverage.toString,
          "value" -> r.value.fold[ujson.Value](ujson.Null)(ujson.Num(_)),
          "samples" -> ujson.Arr.from(r.samples.map: sample =>
            ujson.Obj("world" -> ujson.Arr(sample.world.x, sample.world.y, sample.world.z),
              "contribution" -> sample.contribution.toString))))
    )
    ExampleResult(route, source, mapped, displays, receipt)
