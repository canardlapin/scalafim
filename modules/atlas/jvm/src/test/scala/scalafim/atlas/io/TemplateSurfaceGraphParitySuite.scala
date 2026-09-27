package scalafim.atlas.io

import gale.linalg.DMat
import scalafim.atlas.*
import scalafim.spatial.SpatialOperator
import scalafim.surface.*
import scalafim.surface.io.{GiftiSurfaceReader, TemplateFlowCache}

/** fsaverage <-> fsLR through the transform graph, on the TemplateFlow spheres, against Connectome Workbench 2.2.1
  * `-metric-resample BARYCENTRIC` (STP P7.04).
  *
  * The standard manifest is built with its surface spaces sampled on the fsaverage sphere; routes are compiled by
  * [[SpaceTransformGraph.vertexOperator]] and applied to the oracle's fields. The oracle (`template_sphere_oracle`, from
  * `tools/transform/generate_template_sphere_resampling_oracle.py`) and its budgets are the surface module's; the spheres
  * are not committed, so tests skip with an explicit "asset missing" message when no local TemplateFlow cache holds
  * them.
  */
class TemplateSurfaceGraphParitySuite extends munit.FunSuite:
  // Sampling a hemisphere plans every sphere-resampling step of the manifest on 164k-vertex spheres (about 15 s).
  override val munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(5, "min")

  private val Oracle = "/scalafim/surface/template_sphere_oracle"

  /** Budgets fixed with the oracle (see `scalafim.surface.io.TemplateSphereFilesSuite`). */
  private val SmoothBudget = 5e-5
  private val HashBudget = 5e-5
  private val HashBudgetShare = 0.99
  private val HashBudgetMax = 1e-3

  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private def table(name: String): Vector[Vector[String]] =
    val stream = getClass.getResourceAsStream(s"$Oracle/$name")
    assert(stream != null, s"missing oracle file $name")
    try new String(stream.readAllBytes(), "UTF-8").linesIterator.drop(1).filter(_.nonEmpty).map(_.split('\t').toVector).toVector
    finally stream.close()

  private val graphs = scala.collection.mutable.Map.empty[CorticalHemisphere, SpaceTransformGraph]

  /** The standard manifest with `hemisphere`'s surface spaces sampled on the fsaverage sphere; skips unless every
    * manifest surface space's sphere is cached.
    */
  private def graph(hemisphere: CorticalHemisphere): SpaceTransformGraph =
    graphs.getOrElseUpdate(
      hemisphere, {
        val loaded = ok(TemplateSurfaceSamplingFiles.onFsAverage(hemisphere))
        assume(loaded.absent.isEmpty, s"asset missing: ${loaded.absent.mkString(", ")} in no TemplateFlow cache (${TemplateFlowCache.roots.mkString(", ")})")
        ok(SpaceTransforms.graph(SpaceTransforms.manifest, sampling = loaded.sampling))
      }
    )

  /** The oracle's fields on the source sphere's own file coordinates, rounded to float32 as Workbench read them. */
  private def fields(surface: TemplateSurface): Vector[Array[Double]] =
    val relative = TemplateSphereAssets.relativePath(surface, SphereRegistration.FsAverage).getOrElse(fail(s"no asset for ${surface.display}"))
    val path = TemplateFlowCache.locate(relative).getOrElse(fail(s"$relative vanished"))
    val raw = ok(GiftiSurfaceReader.readEither(path, surface.hemisphere.tag, SurfaceKind.Sphere)).mesh.coordinates
    val n = raw.length / 3
    Vector(
      Array.tabulate(n)(i => (((i.toLong * 2654435761L) & 0xffffffffL) >>> 8).toDouble / (1 << 24).toDouble),
      Array.tabulate(n)(i => (raw(3 * i) / 100.0).toFloat.toDouble),
      Array.tabulate(n)(i => (raw(3 * i + 1) * raw(3 * i + 2) / 10000.0).toFloat.toDouble)
    )

  private def apply(operator: SpatialOperator, values: Array[Double]): Array[Double] =
    val builder = DMat.newBuilder(values.length, 1)
    values.indices.foreach(i => builder.writeLinear(i, values(i)))
    val out = ok(operator.forward(builder.result()))
    Array.tabulate(out.rows)(out(_, 0))

  private def compare(name: String, operator: SpatialOperator, inputs: Vector[Array[Double]]): Unit =
    val rows = table(s"$name.tsv")
    assertEquals(rows.size, 1002, name)
    val outputs = inputs.map(apply(operator, _))
    val errors = Vector.tabulate(3)(field => rows.map(row => math.abs(outputs(field)(row(0).toInt) - row(1 + field).toDouble)))
    val Vector(hash, ramp, saddle) = errors: @unchecked
    assert(ramp.max <= SmoothBudget && saddle.max <= SmoothBudget, s"$name smooth fields: ramp ${ramp.max}, saddle ${saddle.max}")
    val withinHash = hash.count(_ <= HashBudget).toDouble / hash.size
    assert(withinHash >= HashBudgetShare && hash.max <= HashBudgetMax, s"$name hash field: $withinHash within $HashBudget, max ${hash.max}")
    println(f"$name through the transform graph vs Workbench: ramp max ${ramp.max}%.2e, saddle max ${saddle.max}%.2e, hash max ${hash.max}%.2e")

  test("the sampled manifest routes fsaverage -> fsLR 32k as one available Workbench step, matching Workbench"):
    CorticalHemisphere.values.foreach: hemisphere =>
      val sampled = graph(hemisphere)
      val plan = ok(sampled.plan(SpaceId.FsAverage, SpaceId.FsLR32k, DataKind.Vertex))
      assertEquals(plan.status, TransformStatus.Available)
      assertEquals(plan.steps.map(_.backend), Vector(TransformBackend.Workbench))
      assert(plan.steps.head.notes.exists(_.contains("sha256=")), plan.steps.head.notes.toString)
      val operator = ok(sampled.vertexOperator(SpaceId.FsAverage, SpaceId.FsLR32k))
      assertEquals((operator.rows, operator.cols), (32492, 163842))
      val name = s"${if hemisphere == CorticalHemisphere.Left then "L" else "R"}_fsaverage164k_to_fsLR32k"
      compare(name, operator, fields(TemplateSurface(TemplateMesh.FsAverage7, hemisphere)))

  test("fsLR 32k -> fsaverage through the graph matches Workbench, and its adjoint is the transposed plan"):
    val sampled = graph(CorticalHemisphere.Left)
    val operator = ok(sampled.vertexOperator(SpaceId.FsLR32k, SpaceId.FsAverage))
    val inputs = fields(TemplateSurface(TemplateMesh.FsLR32k, CorticalHemisphere.Left))
    compare("L_fsLR32k_to_fsaverage164k", operator, inputs)
    val probe = inputs(2)
    val target = Array.tabulate(163842)(i => math.sin(i * 0.001))
    val builder = DMat.newBuilder(target.length, 1)
    target.indices.foreach(i => builder.writeLinear(i, target(i)))
    val pulled = ok(operator.map.transposeApplyTo(builder.result()))
    val left = apply(operator, probe).zip(target).map(_ * _).sum
    val right = probe.indices.map(i => probe(i) * pulled(i, 0)).sum
    assertEqualsDouble(left, right, 1e-9 * math.max(1.0, math.abs(left)))

  test("fsaverage -> fsaverage5 routes through the nearest-vertex manifest step and matches Workbench"):
    val sampled = graph(CorticalHemisphere.Left)
    val operator = ok(sampled.vertexOperator(SpaceId.FsAverage, SpaceId.FsAverage5))
    assertEquals(ok(sampled.plan(SpaceId.FsAverage, SpaceId.FsAverage5, DataKind.Vertex)).steps.map(_.backend), Vector(TransformBackend.SphereNearest))
    compare("L_fsaverage164k_to_fsaverage10k", operator, fields(TemplateSurface(TemplateMesh.FsAverage7, CorticalHemisphere.Left)))
    // fsaverage5 is fsaverage's nested vertex prefix, so the nearest-vertex route selects vertex i for row i
    val ids = Array.tabulate(163842)(_.toDouble)
    assertEquals(apply(operator, ids).toVector, Vector.tabulate(10242)(_.toDouble))

  test("a route through a space whose sphere is absent is refused, typed"):
    val loaded = ok(TemplateSurfaceSamplingFiles.onFsAverage(CorticalHemisphere.Left, Vector.empty))
    assert(loaded.sampling.isEmpty)
    assertEquals(loaded.absent.size, TemplateSurfaceSamplingFiles.meshes.size)
    val unsampled = ok(SpaceTransforms.graph(SpaceTransforms.manifest, sampling = loaded.sampling))
    assert(unsampled.vertexOperator(SpaceId.FsAverage, SpaceId.FsLR32k).left.exists(_.isInstanceOf[AtlasError.TransformNotExecutable]))
