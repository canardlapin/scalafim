package scalafim.examples.reference

import scalafim.surface.*
import scalafim.surface.reference.*
import java.nio.file.{Files, Path}

/** Ordinary public APIs only: locked bytes, declared route, values and picks.
  * `synthetic` is an asymmetric control, never anatomical registration evidence.
  * `real` maps locked GM from MNI152NLin2009cAsym onto fsLR 32k (left) through the
  * pointwise-inverse bridge and writes the route, its disclosed approximation and
  * per-vertex inverse evidence. It qualifies no consumer's results.
  */
object FslrRouteExample:
  final case class ExampleResult(route: AdmittedSurfaceRoute, source: DeclaredVolume,
      mapped: MappedSurfaceValues, displays: Vector[DisplayedSurfaceValues], receipt: ujson.Value)

  private def checked[E, A](value: Either[E, A]): A = value.fold(e => throw new IllegalArgumentException(e.toString), identity)

  def main(args: Array[String]): Unit =
    require(args.length >= 3, "synthetic <manifest> <receipt> | real <lock> <cache> <receipt>")
    val (receipt, out) = args(0) match
      case "synthetic" => (synthetic(Path.of(args(1))).receipt, Path.of(args(2)))
      case "real" =>
        require(args.length == 4, "real <lock> <cache> <receipt>")
        (real(Path.of(args(1)), Path.of(args(2))), Path.of(args(3)))
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

  def real(lockPath: Path, cache: Path): ujson.Value =
    val bytes = Files.readAllBytes(lockPath)
    val lock = ujson.read(bytes)
    require(lock("schema").str == "scalafim.fslr-resource-lock/1")
    val assets = lock("assets").arr.toVector
    def entry(ending: String): ujson.Value = assets.find(_("path").str.endsWith(ending)).get
    def provenance(item: ujson.Value): AssetProvenance =
      val path = item("path").str
      checked(AssetProvenance.make(TemplateId.unsafe(path.stripPrefix("tpl-").takeWhile(_ != '/')),
        path, lock("catalogRevision").str, item("sha256").str))
    // Verify every locked resource before any candidate is reported.
    assets.foreach: item =>
      val path = cache.resolve(item("path").str)
      val raw = Files.readAllBytes(path)
      require(raw.length.toLong == item("bytes").num.toLong && AssetSha256.of(raw).value == item("sha256").str,
        s"locked resource differs: $path")
    val release = TemplateRelease.unsafe(lock("catalogRevision").str)
    val sourceFrame = checked(TemplateFrame.make(TemplateId.unsafe("MNI152NLin2009cAsym"), release))
    val anatomyFrame = checked(TemplateFrame.make(TemplateId.unsafe("MNI152NLin6Asym"), release))
    val gm = entry("res-01_label-GM_probseg.nii.gz")
    val gmBasis = checked(FrameBasis.literature("10.1016/j.neuroimage.2010.07.033", "locked TemplateFlow GM in its own declared frame"))
    val volume = checked(DeclaredVolumeReader.readNifti(cache.resolve(gm("path").str),
      checked(FrameDeclaration.make(sourceFrame, gmBasis, provenance(gm)))))
    val surfaceEntry = entry("hemi-L_midthickness.surf.gii")
    val conte69Basis = checked(FrameBasis.publisherMethods("10.1093/cercor/bhr291",
      "Van Essen et al. 2012, Cereb Cortex 22:2241, Materials and Methods p. 2245",
      PublishedRegistration.Affine, PublishedAggregate.GroupAverage(69),
      "linear volumetric registration between the individual subject and the MNI152_T1_1mm.nii.gz was performed " +
        "using ... (FLIRT) ... The resultant affine transform was applied to the FreeSurfer white, pial, and " +
        "midthickness surfaces ... The individual-subject midthickness surfaces were averaged"))
    val surface = checked(DeclaredSurfaceReader.read(cache.resolve(surfaceEntry("path").str),
      checked(FrameDeclaration.make(anatomyFrame, conte69Basis, provenance(surfaceEntry))), Hemisphere.Left, SurfaceKind.Midthickness))
    val maskEntry = entry("hemi-L_den-32k_desc-nomedialwall_dparc.label.gii")
    val wall = checked(DeclaredMedialWallReader.read(cache.resolve(maskEntry("path").str),
      checked(surface.geometry.meshDomainEither), provenance(maskEntry)))
    val reference = checked(CorticalMeshReference.make(StandardCorticalMesh.FsLR32k, surface.geometry, wall))
    val sourceReference = checked(VolumeReference.make(sourceFrame, volume.volume.space))
    val request = RouteRequest(sourceReference, StandardCorticalMesh.FsLR32k, CorticalHemisphere.Left,
      MappingMethod.MidthicknessNearest, ValueSemantics.Continuous)
    val anatomy = checked(SamplingAnatomy.make(reference, AnatomicalGeometry.Midthickness(surface)))
    val transform = entry("mode-image_xfm.h5")
    val manifest = entry("manifest.json")
    val map = checked(DeclaredPointMapReader.read(cache.resolve(manifest("path").str).getParent,
      transform("sha256").str, manifest("sha256").str))
    val bridge = checked(FrameBridge.displacement(map, PointMapUse.Inverse(InversePolicy.Default)))
    val route = checked(SurfaceRoute.admit(request, anatomy, Some(bridge)))
    val mapped = checked(route.map(volume))
    val summary = route.bridgePlacement.flatMap(_.inverseSummary).get
    ujson.Obj("schema" -> "scalafim.fslr-route-real/1", "lockSha256" -> AssetSha256.of(bytes).value,
      "source" -> volume.declaration.display, "anatomy" -> surface.declaration.display,
      "medialWall" -> wall.asset.get.display, "method" -> request.method.label,
      "pointMap" -> map.display, "pointMapManifest" -> map.manifest.sha256.value,
      "bridge" -> bridge.display, "bridgeExactness" -> route.disclosure.bridgeExactness.label,
      "inverse" -> ujson.Obj("converged" -> summary.converged,
        "unplaced" -> ujson.Obj.from(summary.unplaced.map((k, v) => k.label -> ujson.Num(v))),
        "worstResidualMm" -> summary.worstResidualMm.fold[ujson.Value](ujson.Null)(ujson.Num(_)),
        "worstIterations" -> summary.worstIterations.fold[ujson.Value](ujson.Null)(ujson.Num(_))),
      "coverage" -> ujson.Obj.from(VertexCoverage.values.map(c => c.toString -> ujson.Num(mapped.count(c)))),
      "remainingLimits" -> ujson.Arr(
        "anatomy frame is the publisher's declaration (affine-aligned 69-subject Conte69 average); it is not an " +
          "asset-specific registration proof and matches no single anatomy",
        "placements are numerical pointwise-inverse estimates with per-vertex residuals, not an exact inverse",
        "source-volume release, cohort and statistic provenance, and consumer qualification, belong to the consumer"))
