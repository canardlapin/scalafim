package scalafim.surface.io

import scalafim.surface.*

import java.security.MessageDigest
import scala.io.Source

/** TemplateFlow registration spheres and fsaverage <-> fsLR resampling plans against Connectome Workbench 2.2.1
  * `-metric-resample BARYCENTRIC` (oracle: `template_sphere_oracle`, written by
  * `tools/transform/generate_template_sphere_resampling_oracle.py`). The spheres are not committed; tests that need
  * them skip with an explicit "asset missing" message when no local TemplateFlow cache holds them.
  */
class TemplateSphereFilesSuite extends munit.FunSuite:
  private val Oracle = "/scalafim/surface/template_sphere_oracle"

  /** Budgets fixed before the plans were compared (see the generator): ScalaFIM casts the radial ray where Workbench
    * takes the closest point, and Workbench computes in float32.
    */
  private val SmoothBudget = 5e-5
  private val HashBudget = 5e-5
  private val HashBudgetShare = 0.99
  private val HashBudgetMax = 1e-3

  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private def bytes(name: String): Array[Byte] =
    val stream = getClass.getResourceAsStream(s"$Oracle/$name")
    assert(stream != null, s"missing oracle file $name")
    try stream.readAllBytes()
    finally stream.close()

  private def table(name: String): Vector[Vector[String]] =
    new String(bytes(name), "UTF-8").linesIterator.drop(1).filter(_.nonEmpty).map(_.split('\t').toVector).toVector

  private def sha256(content: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(content).map(b => f"${b & 0xff}%02x").mkString

  private def present(surface: TemplateSurface, registration: SphereRegistration): java.nio.file.Path =
    val relative = TemplateSphereAssets.relativePath(surface, registration).getOrElse(fail(s"no asset for ${surface.display}"))
    val found = TemplateFlowCache.locate(relative)
    assume(found.nonEmpty, s"asset missing: $relative is in no TemplateFlow cache (${TemplateFlowCache.roots.mkString(", ")})")
    found.get

  private def onFsAverage(mesh: TemplateMesh, hemisphere: CorticalHemisphere): TemplateSphere[SphereRegistration.FsAverage.type] =
    val surface = TemplateSurface(mesh, hemisphere)
    ok(TemplateSphereFiles.load(surface, SphereRegistration.FsAverage, present(surface, SphereRegistration.FsAverage)))

  private def onFsLR(mesh: TemplateMesh, hemisphere: CorticalHemisphere): TemplateSphere[SphereRegistration.FsLR.type] =
    val surface = TemplateSurface(mesh, hemisphere)
    ok(TemplateSphereFiles.load(surface, SphereRegistration.FsLR, present(surface, SphereRegistration.FsLR)))

  /** The oracle's fields on the source sphere's own file coordinates, rounded to float32 as Workbench read them. */
  private def fields(surface: TemplateSurface, registration: SphereRegistration): Vector[Array[Double]] =
    val raw = ok(GiftiSurfaceReader.readEither(present(surface, registration), surface.hemisphere.tag, SurfaceKind.Sphere)).mesh.coordinates
    val n = raw.length / 3
    Vector(
      Array.tabulate(n)(i => (((i.toLong * 2654435761L) & 0xffffffffL) >>> 8).toDouble / (1 << 24).toDouble),
      Array.tabulate(n)(i => (raw(3 * i) / 100.0).toFloat.toDouble),
      Array.tabulate(n)(i => (raw(3 * i + 1) * raw(3 * i + 2) / 10000.0).toFloat.toDouble)
    )

  private def compare(name: String, plan: TemplateResamplingPlan, inputs: Vector[Array[Double]]): Unit =
    val rows = table(s"$name.tsv")
    assertEquals(rows.size, 1002, name)
    val outputs = inputs.map(values => ok(plan.resample(values)))
    val errors = Vector.tabulate(3)(field => rows.map(row => math.abs(outputs(field)(row(0).toInt) - row(1 + field).toDouble)))
    val Vector(hash, ramp, saddle) = errors: @unchecked
    assert(ramp.max <= SmoothBudget && saddle.max <= SmoothBudget, s"$name smooth fields: ramp ${ramp.max}, saddle ${saddle.max}")
    val withinHash = hash.count(_ <= HashBudget).toDouble / hash.size
    assert(withinHash >= HashBudgetShare && hash.max <= HashBudgetMax, s"$name hash field: $withinHash within $HashBudget, max ${hash.max}")
    println(f"$name vs Workbench: ramp max ${ramp.max}%.2e, saddle max ${saddle.max}%.2e, hash max ${hash.max}%.2e ($withinHash%.4f within $HashBudget)")

  test("the committed oracle matches its manifest and pins exactly the admitted spheres"):
    val manifest = new String(bytes("manifest.json"), "UTF-8")
    val recorded = "\"([A-Za-z0-9_]+\\.tsv)\":\\s*\"([0-9a-f]{64})\"".r.findAllMatchIn(manifest).map(m => m.group(1) -> m.group(2)).toVector
    assertEquals(recorded.size, 6)
    recorded.foreach((name, digest) => assertEquals(sha256(bytes(name)), digest, name))
    assert(manifest.contains("Version: 2.2.1"), "Workbench version")
    val spheres = table("spheres.tsv").map(cells => cells(0) -> cells).toMap
    assertEquals(spheres.keySet, TemplateSphereAssets.all.map(_.relativePath).toSet)
    TemplateSphereAssets.all.foreach: asset =>
      val cells = spheres(asset.relativePath)
      assertEquals(cells(1), asset.sha256, asset.relativePath)
      assertEquals((cells(2).toInt, cells(3).toInt), (asset.surface.mesh.vertices, asset.surface.mesh.faces), asset.relativePath)

  test("fsaverage 164k -> fsLR 32k on the fsaverage sphere matches Workbench, both hemispheres"):
    CorticalHemisphere.values.foreach: hemisphere =>
      val source = onFsAverage(TemplateMesh.FsAverage7, hemisphere)
      val target = onFsAverage(TemplateMesh.FsLR32k, hemisphere)
      val plan = ok(TemplateResampling.plan(source, target))
      assertEquals((plan.plan.referenceVertices, plan.plan.movingVertices), (32492, 163842))
      val name = s"${if hemisphere == CorticalHemisphere.Left then "L" else "R"}_fsaverage164k_to_fsLR32k"
      compare(name, plan, fields(source.surface, SphereRegistration.FsAverage))

  test("fsLR 32k -> fsaverage 164k on the fsaverage sphere matches Workbench"):
    val source = onFsAverage(TemplateMesh.FsLR32k, CorticalHemisphere.Left)
    val target = onFsAverage(TemplateMesh.FsAverage7, CorticalHemisphere.Left)
    compare("L_fsLR32k_to_fsaverage164k", ok(TemplateResampling.plan(source, target)), fields(source.surface, SphereRegistration.FsAverage))

  test("fsLR 32k -> 164k on the fsLR sphere and fsaverage -> fsaverage5 match Workbench"):
    val fsLRPlan = ok(TemplateResampling.plan(onFsLR(TemplateMesh.FsLR32k, CorticalHemisphere.Left), onFsLR(TemplateMesh.FsLR164k, CorticalHemisphere.Left)))
    compare("L_fsLR32k_to_fsLR164k", fsLRPlan, fields(TemplateSurface(TemplateMesh.FsLR32k, CorticalHemisphere.Left), SphereRegistration.FsLR))
    val decimation = ok(TemplateResampling.plan(onFsAverage(TemplateMesh.FsAverage7, CorticalHemisphere.Left), onFsAverage(TemplateMesh.FsAverage5, CorticalHemisphere.Left)))
    compare("L_fsaverage164k_to_fsaverage10k", decimation, fields(TemplateSurface(TemplateMesh.FsAverage7, CorticalHemisphere.Left), SphereRegistration.FsAverage))

  test("fsaverage5 and fsaverage6 are the nested vertex prefixes of fsaverage on its sphere"):
    CorticalHemisphere.values.foreach: hemisphere =>
      val full = onFsAverage(TemplateMesh.FsAverage7, hemisphere)
      Vector(TemplateMesh.FsAverage5, TemplateMesh.FsAverage6).foreach: mesh =>
        val plan = ok(TemplateResampling.plan(full, onFsAverage(mesh, hemisphere), SurfaceResampling.Method.Nearest))
        assertEquals(plan.plan.cols.toVector, Vector.range(0, mesh.vertices), s"${mesh.label} ${hemisphere.code}")

  test("every pinned sphere loads with its template's counts; other bytes are refused"):
    TemplateSphereAssets.all.foreach: asset =>
      val path = present(asset.surface, asset.registration)
      val loaded =
        asset.registration match
          case SphereRegistration.FsAverage => ok(TemplateSphereFiles.load(asset.surface, SphereRegistration.FsAverage, path)).sphere
          case SphereRegistration.FsLR      => ok(TemplateSphereFiles.load(asset.surface, SphereRegistration.FsLR, path)).sphere
      assertEquals((loaded.vertexCount, loaded.faceCount), (asset.surface.mesh.vertices, asset.surface.mesh.faces), asset.relativePath)
    // widening the registration type defeats the static check; the registrations are still compared at runtime
    val native: TemplateSphere[SphereRegistration] = onFsLR(TemplateMesh.FsLR32k, CorticalHemisphere.Left).asInstanceOf[TemplateSphere[SphereRegistration]]
    val deformed: TemplateSphere[SphereRegistration] = onFsAverage(TemplateMesh.FsAverage5, CorticalHemisphere.Left).asInstanceOf[TemplateSphere[SphereRegistration]]
    TemplateResampling.plan(native, deformed) match
      case Left(SurfaceError.InvalidGeometry(reason)) => assert(reason.contains("is on FsLR"), reason)
      case other                                      => fail(s"expected a registration refusal, got $other")
    val left32k = TemplateSurface(TemplateMesh.FsLR32k, CorticalHemisphere.Left)
    // the native fsLR sphere's bytes are not the fsaverage-registered asset's
    TemplateSphereFiles.load(left32k, SphereRegistration.FsAverage, present(left32k, SphereRegistration.FsLR)) match
      case Left(SurfaceError.ReadFailure(_, reason)) => assert(reason.contains("is not the pinned"), reason)
      case other                                     => fail(s"expected a digest refusal, got $other")
