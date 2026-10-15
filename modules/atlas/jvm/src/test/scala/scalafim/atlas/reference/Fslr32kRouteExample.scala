package scalafim.atlas.reference

import scalafim.atlas.{Fslr32kFrom2009c, Fslr32kRoute}
import scalafim.atlas.io.StandardSurfaceRouteFiles
import scalafim.surface.CorticalHemisphere
import scalafim.surface.reference.*

import java.nio.file.{Files, Path}

/** A consumer's use of the qualified MNI152NLin2009cAsym -> fsLR 32k route, through public APIs only: obtain the
  * route from the TemplateFlow cache, admit it for one locked GM volume (left hemisphere), map, and write a receipt
  * carrying the digest-bound route identity, the disclosure fields and the per-vertex inverse evidence summary.
  * It qualifies no consumer's results.
  *
  * Run: `sbt "atlasJVM/Test/runMain scalafim.atlas.reference.Fslr32kRouteExample <templateflow-home> <receipt.json>"`
  */
object Fslr32kRouteExample:
  private val gmPath = "tpl-MNI152NLin2009cAsym/tpl-MNI152NLin2009cAsym_res-01_label-GM_probseg.nii.gz"

  def main(args: Array[String]): Unit =
    require(args.length == 2, "usage: Fslr32kRouteExample <templateflow-home> <receipt.json>")
    val out = Path.of(args(1))
    Files.writeString(out, ujson.write(real(Path.of(args(0))), indent = 2))
    println(s"wrote $out")

  private def checked[E, A](value: Either[E, A]): A = value.fold(e => throw new IllegalArgumentException(e.toString), identity)

  def real(cache: Path): ujson.Value =
    val route: Fslr32kRoute = StandardSurfaceRouteFiles.fsLR32kFrom2009c(roots = Vector(cache))
      .fold(r => throw new IllegalStateException(r.message), identity)
    // The source volume is the consumer's: here the locked TemplateFlow GM probability map, digest-checked.
    val gmAsset = checked(AssetProvenance.make(TemplateId.unsafe("MNI152NLin2009cAsym"), gmPath,
      Fslr32kFrom2009c.catalogRevision.value, RealAssets.gmProbsegSha256))
    val gmBasis = checked(FrameBasis.literature("10.1016/j.neuroimage.2010.07.033", "locked TemplateFlow GM in its own declared frame"))
    val volume = checked(DeclaredVolumeReader.readNifti(cache.resolve(gmPath),
      checked(FrameDeclaration.make(Fslr32kFrom2009c.sourceFrame, gmBasis, gmAsset))))
    val source = checked(VolumeReference.make(Fslr32kFrom2009c.sourceFrame, volume.volume.space))
    val admitted = route.admit(source, CorticalHemisphere.Left).fold(r => throw new IllegalStateException(r.message), identity)
    val mapped = checked(admitted.map(volume))
    val summary = admitted.bridgePlacement.flatMap(_.inverseSummary).get
    ujson.Obj("schema" -> "scalafim.fslr-route-real/2",
      "route" -> route.identity.token,
      "disclosure" -> ujson.Arr.from(route.disclosure.fields.map((k, v) => ujson.Obj(k -> v))),
      "source" -> volume.declaration.display, "hemisphere" -> admitted.request.hemisphere.code,
      "medialWall" -> admitted.disclosure.medialWallAsset.map(_.display).getOrElse("undeclared"),
      "inverse" -> ujson.Obj("converged" -> summary.converged,
        "unplaced" -> ujson.Obj.from(summary.unplaced.map((k, v) => k.label -> ujson.Num(v))),
        "worstResidualMm" -> summary.worstResidualMm.fold[ujson.Value](ujson.Null)(ujson.Num(_)),
        "worstIterations" -> summary.worstIterations.fold[ujson.Value](ujson.Null)(ujson.Num(_))),
      "coverage" -> ujson.Obj.from(VertexCoverage.values.map(c => c.toString -> ujson.Num(mapped.count(c)))))
