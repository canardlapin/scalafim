package scalafim.atlas.reference

import scalafim.atlas.{Fslr32kFrom2009c, Fslr32kRoute}
import scalafim.image.*
import scalafim.surface.*
import scalafim.surface.reference.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** Measures what a candidate MNI152NLin6Asym -> MNI152NLin2009cAsym bridge does to mapped values, against the
  * all-vertex SimpleITK oracle placement (`RealInverseOracle`). Both sides run through the production route: the
  * candidate as the public standard route (`StandardSurfaceRouteFiles.fsLR32kFrom2009c`, frozen policy) on the declared
  * fsLR midthickness, the oracle as a same-frame route on anatomy
  * whose vertices sit at the oracle's 2009c positions. The runner knows nothing about where the volumes came
  * from; they arrive in a `scalafim.fslr-qualification-input/1` spec (see `FslrQualification`).
  *
  * Per volume and hemisphere, over cortical vertices: placement error, nearest-voxel, coverage (support) and
  * value disagreements, absolute value differences in volume units and relative to the oracle-mapped cortical
  * standard deviation, and sign flips. This is evidence about a candidate, not a qualification gate.
  *
  * Run: `sbt "atlasJVM/Test/runMain scalafim.atlas.reference.FslrBridgeDisagreement pointwise <spec.json> <out.json> ..."`
  * (several spec/output pairs share one bridge).
  */
object FslrBridgeDisagreement:
  def main(args: Array[String]): Unit =
    require(args.length >= 3 && args.length % 2 == 1,
      "usage: FslrBridgeDisagreement <candidate: pointwise> <spec.json> <out.json> [<spec.json> <out.json> ...]")
    val (candidateName, route) = args(0) match
      case "pointwise" => ("pointwise", PublicFslrRoute.route)
      case other => sys.error(s"unknown candidate '$other'")
    for Array(spec, out) <- args.drop(1).grouped(2) do measure(candidateName, route, Path.of(spec), Path.of(out))

  private def measure(candidateName: String, route: Fslr32kRoute, specPath: Path, outPath: Path): Unit =
    val bridge = route.bridge
    val inputs = FslrQualification.readInputs(specPath)
    val source = inputs.source
    val toVoxel = source.voxelToWorld
    def voxelOf(p: WorldPoint): Option[Vector[Int]] =
      toVoxel.inverse(Vector(p.x, p.y, p.z)).toOption.map(_.map(c => math.floor(c + 0.5).toInt))

    val hemispheres = ujson.Obj()
    for lock <- Fslr32kFrom2009c.hemispheres do
      val label = PublicFslrRoute.label(lock.hemisphere)
      val anatomy = route.anatomy(lock.hemisphere)
      val candidate = route.admit(source, lock.hemisphere).fold(r => sys.error(r.message), identity)
      val request = candidate.request
      val oracle = RealInverseOracle.solutions(label)
      val oracleRoute = SurfaceRoute.admit(request,
        RealInverseOracle.anatomy(label, anatomy, route.pointMap.source, Fslr32kFrom2009c.sourceFrame))
        .fold(r => sys.error(r.message), identity)
      val placement = candidate.bridgePlacement.getOrElse(sys.error("candidate route has no bridge placement"))
      val wall = anatomy.reference.medialWall
      val cortical = (0 until anatomy.reference.vertexCount).filter(v => wall.cortexAt(VertexId(v)).contains(true)).toVector
      val placed = cortical.map(v => v -> placement.outcomesAt(v).head.placed).toMap
      val placementErrors = cortical.flatMap(v => placed(v).map(p => distance(p, oracle(v).world)))
      val voxelDisagreements = cortical.count(v => placed(v).flatMap(voxelOf) != voxelOf(oracle(v).world))
      val volumes = ujson.Arr()
      for (name, declared) <- inputs.volumes do
        val got = candidate.map(declared).fold(e => sys.error(e.message), identity)
        val want = oracleRoute.map(declared).fold(e => sys.error(e.message), identity)
        val coverageDisagreements = cortical.filter(v => got.coverageAt(VertexId(v)) != want.coverageAt(VertexId(v)))
        val both = cortical.flatMap(v => got.valueAt(VertexId(v)).zip(want.valueAt(VertexId(v))).map((a, b) => (v, a, b)))
        val differing = both.filter((_, a, b) => a != b)
        val differences = differing.map((_, a, b) => math.abs(a - b)).sorted
        val reference = both.map(_._3)
        val mean = reference.sum / reference.length.max(1)
        val sd = math.sqrt(reference.map(x => (x - mean) * (x - mean)).sum / (reference.length - 1).max(1))
        val signFlips = both.count((_, a, b) => a != 0.0 && b != 0.0 && math.signum(a) != math.signum(b))
        val worst = differing.sortBy((_, a, b) => -math.abs(a - b)).take(5).map: (v, a, b) =>
          ujson.Obj("vertex" -> v, "candidate" -> a, "oracle" -> b, "placementErrorMm" -> placed(v).fold(Double.NaN)(distance(_, oracle(v).world)))
        volumes.value += ujson.Obj(
          "volume" -> name, "volumeAsset" -> declared.declaration.asset.display,
          "corticalVertices" -> cortical.length, "bothMapped" -> both.length,
          "coverageDisagreements" -> coverageDisagreements.length,
          "valueDisagreements" -> differing.length,
          "maxAbsDifference" -> differences.lastOption.getOrElse(0.0),
          "p99AbsDifferenceAmongDisagreements" -> nearestRank(differences, 0.99),
          "oracleCorticalSd" -> sd,
          "maxAbsDifferenceOverSd" -> differences.lastOption.getOrElse(0.0) / sd,
          "signFlips" -> signFlips,
          "worst" -> ujson.Arr.from(worst))
      hemispheres(label) = ujson.Obj(
        "corticalVertices" -> cortical.length,
        "bridgeUnavailable" -> cortical.count(v => placed(v).isEmpty),
        "placementErrorMaxMm" -> placementErrors.max, "placementErrorP99Mm" -> nearestRank(placementErrors.sorted, 0.99),
        "placementErrorOver005" -> placementErrors.count(_ > 0.05),
        "voxelDisagreements" -> voxelDisagreements,
        "volumes" -> volumes)
    val result = ujson.Obj(
      "schema" -> "scalafim.fslr-bridge-disagreement/1",
      "candidate" -> candidateName, "route" -> route.identity.token, "bridge" -> bridge.display,
      "bridgeExactness" -> bridge.exactness.label,
      "oracle" -> RealInverseOracle.manifest("description").str,
      "oracleFiles" -> ujson.Obj.from(Vector("L", "R").map(h => h -> RealInverseOracle.manifest("hemispheres")(h)("sha256"))),
      "spec" -> ujson.Obj("name" -> inputs.name, "sha256" -> inputs.specSha256),
      "hemispheres" -> hemispheres)
    Files.writeString(outPath, ujson.write(result, indent = 2), StandardCharsets.UTF_8)
    println(ujson.write(result))

  private def distance(p: WorldPoint, q: WorldPoint): Double =
    math.sqrt(math.pow(p.x - q.x, 2) + math.pow(p.y - q.y, 2) + math.pow(p.z - q.z, 2))

  private def nearestRank(sorted: Vector[Double], q: Double): Double =
    if sorted.isEmpty then 0.0 else sorted(math.max(0, math.ceil(q * sorted.length).toInt - 1))
