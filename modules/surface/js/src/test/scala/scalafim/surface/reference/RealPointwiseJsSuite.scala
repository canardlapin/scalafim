package scalafim.surface.reference

import scalafim.image.WorldPoint
import scalafim.surface.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.{Int8Array, Uint8Array}

@js.native
@JSImport("fs", JSImport.Namespace)
private object ReferenceNodeFs extends js.Object:
  def readFileSync(path: String): Uint8Array = js.native
  def existsSync(path: String): Boolean = js.native

@js.native
@JSImport("zlib", JSImport.Namespace)
private object ReferenceNodeZlib extends js.Object:
  def gunzipSync(data: Uint8Array): Uint8Array = js.native

/** Frozen budget P6 on Scala.js with the real field: the locked TemplateFlow point map and fsLR 32k midthickness
  * surfaces are digest-verified and decoded through the shared readers, then every vertex is placed by the
  * pointwise inverse and compared with the all-vertex SimpleITK oracle. Loading is timed separately; P6 times
  * placement of both hemispheres. Opt-in: `SCALAFIM_JS_REAL_TIMING=1` with the locked cache under
  * `$TEMPLATEFLOW_HOME` or `~/.cache/templateflow`.
  */
class RealPointwiseJsSuite extends munit.FunSuite:
  override def munitTimeout = scala.concurrent.duration.Duration(20, "min")

  private def env(name: String): Option[String] =
    js.Dynamic.global.process.env.selectDynamic(name).asInstanceOf[js.UndefOr[String]].toOption

  private def now(): Double = js.Dynamic.global.performance.now().asInstanceOf[Double]

  private lazy val root: String =
    env("TEMPLATEFLOW_HOME").getOrElse(s"${env("HOME").getOrElse("")}/.cache/templateflow")

  /** Typed-array copy: `Int8Array.toArray` grows a JS array element by element and exceeds V8's array limit at 200 MB. */
  private def read(path: String): Array[Byte] =
    val data = ReferenceNodeFs.readFileSync(path)
    js.typedarray.int8Array2ByteArray(new Int8Array(data.buffer, data.byteOffset, data.length))

  private def uint8(bytes: Array[Byte]): Uint8Array =
    val out = new Uint8Array(bytes.length)
    var i = 0
    while i < bytes.length do
      out(i) = (bytes(i) & 0xff).toShort
      i += 1
    out

  private val transformSha256 = "2e3869a07b96aec406e0419ca2e434afc54882d37cc212b933b139d1b63a4dfe"
  private val manifestSha256 = "34bdcea2dab7c0fccde6e6607cd080fbcea087abaf19721925b5b8fb61d85318"
  private val midthickness = Map(
    "L" -> "036a8b6c84fa4b581b7ad7b36d99190b57ad6755c9d7ef7adc3e9ffc6448f1af",
    "R" -> "9d2cef05096c433b134870456abebe7ff201cdda60ce5741d7b794018eeccda7")
  private val release = TemplateRelease.unsafe("templateflow@d79aacb1ad7d1c52e5d10ad88f48fd8af6e5ae56")

  /** The oracle fixture lives with the JVM test resources (it is consumed by JVM real-asset suites); read it from
    * the repository so both platforms are judged against the same bytes, digest-checked against its manifest.
    */
  private def oracle(h: String): Array[Double] =
    var dir = js.Dynamic.global.process.cwd().asInstanceOf[String]
    val relative = "modules/surface/jvm/src/test/resources/pointmap-real"
    while !ReferenceNodeFs.existsSync(s"$dir/$relative") && dir.contains("/") do dir = dir.substring(0, dir.lastIndexOf('/'))
    val manifest = ujson.read(read(s"$dir/$relative/fslr-inverse-oracle-all.json"))
    val entry = manifest("hemispheres")(h)
    val compressed = read(s"$dir/$relative/${entry("file").str}")
    assertEquals(AssetSha256.of(compressed).value, entry("sha256").str)
    val raw = ReferenceNodeZlib.gunzipSync(uint8(compressed))
    val view = new js.typedarray.DataView(raw.buffer, raw.byteOffset, raw.length)
    Array.tabulate(raw.length / 8)(i => view.getFloat64(8 * i, true))

  test("P6-JS: real-field pointwise placement of both hemispheres matches the oracle within budget"):
    assume(env("SCALAFIM_JS_REAL_TIMING").contains("1"), "set SCALAFIM_JS_REAL_TIMING=1 to run the real Scala.js measurement")
    val pointMapDir = s"$root/.templateflow4s/derived/point-map/$transformSha256"
    assume(ReferenceNodeFs.existsSync(s"$pointMapDir/manifest.json"), s"locked point map not found under $pointMapDir")
    val loadStart = now()
    val manifest = PointMapManifest.fromBytes(read(s"$pointMapDir/manifest.json"), manifestSha256).fold(e => fail(e.message), identity)
    val map = DeclaredPointMap.fromManifest(manifest, transformSha256, name => Some(read(s"$pointMapDir/$name")))
      .fold(e => fail(e.message), identity)
    val inverter = map.map.inverter(InversePolicy.Default).fold(e => fail(e.message), identity)
    val nlin6 = TemplateFrame.make(TemplateId.unsafe("MNI152NLin6Asym"), release).toOption.get
    val basis = FrameBasis.literature("10.1093/cercor/bhr291", "TemplateFlow tpl-fsLR midthickness").toOption.get
    val surfaces = Future.sequence(Vector("L" -> Hemisphere.Left, "R" -> Hemisphere.Right).map: (h, hemisphere) =>
      val archive = s"tpl-fsLR/tpl-fsLR_den-32k_hemi-${h}_midthickness.surf.gii"
      val declaration = FrameDeclaration.make(nlin6, basis,
        AssetProvenance.make(TemplateId.unsafe("fsLR"), archive, release.value, midthickness(h)).toOption.get).toOption.get
      DeclaredSurfaceReader.read(uint8(read(s"$root/$archive")), declaration, hemisphere, SurfaceKind.Midthickness)
        .map(r => h -> r.fold(e => fail(e.message), identity)))
    surfaces.map: loaded =>
      val loadSeconds = (now() - loadStart) / 1000.0
      val placeStart = now()
      val placed = loaded.map: (h, surface) =>
        val geometry = surface.geometry
        h -> Vector.tabulate(geometry.vertexCount): i =>
          val p = geometry.mesh.vertex(VertexId(i))
          val w = geometry.surfaceToWorld(Vector(p.x, p.y, p.z)).fold(e => fail(e.message), identity)
          inverter.place(WorldPoint(w(0), w(1), w(2)))
      val placeSeconds = (now() - placeStart) / 1000.0
      for (h, outcomes) <- placed do
        val solutions = oracle(h)
        val errors = outcomes.zipWithIndex.map:
          case (PointMapOutcome.Inverted(p, _, _), i) =>
            math.sqrt(math.pow(p.x - solutions(6 * i), 2) + math.pow(p.y - solutions(6 * i + 1), 2) +
              math.pow(p.z - solutions(6 * i + 2), 2))
          case (other, i) => fail(s"$h vertex $i not inverted: $other")
        println(s"[pointwise-js-receipt] ${ujson.write(ujson.Obj("hemisphere" -> h, "vertices" -> outcomes.length,
          "placementVsOracleMaxMm" -> errors.max))}")
        assert(errors.max <= 1e-6, s"$h: Scala.js placement vs oracle ${errors.max} mm")
      println(s"[pointwise-js-receipt] ${ujson.write(ujson.Obj("check" -> "P6-JS", "loadSeconds" -> loadSeconds,
        "placeBothHemispheresSeconds" -> placeSeconds))}")
      assert(placeSeconds <= 120.0, s"Scala.js placement took $placeSeconds s; frozen budget 120 s")
