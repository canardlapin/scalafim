package scalafim.surface.reference

import scalafim.image.*
import scalafim.surface.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** Measures what a candidate MNI152NLin6Asym -> MNI152NLin2009cAsym bridge does to mapped values, against the
  * all-vertex SimpleITK oracle placement (`RealInverseOracle`). Both sides run through the production route: the
  * candidate as a bridged route on the declared fsLR midthickness, the oracle as a same-frame route on anatomy
  * whose vertices sit at the oracle's 2009c positions. The runner knows nothing about where the volumes came
  * from; they arrive in a `scalafim.fslr-qualification-input/1` spec (see `FslrQualification`).
  *
  * Per volume and hemisphere, over cortical vertices: placement error, nearest-voxel, coverage (support) and
  * value disagreements, absolute value differences in volume units and relative to the oracle-mapped cortical
  * standard deviation, and sign flips. This is evidence about a candidate, not a qualification gate.
  *
  * Run: `sbt "surfaceJVM/Test/runMain scalafim.surface.reference.FslrBridgeDisagreement pointwise <spec.json> <out.json> ..."`
  * (several spec/output pairs share one bridge).
  */
object FslrBridgeDisagreement:
  def main(args: Array[String]): Unit =
    require(args.length >= 3 && args.length % 2 == 1,
      "usage: FslrBridgeDisagreement <candidate: pointwise> <spec.json> <out.json> [<spec.json> <out.json> ...]")
    val (candidateName, bridge) = args(0) match
      case "pointwise" => ("pointwise",
        FrameBridge.displacement(RealAssets.pointMap, PointMapUse.Inverse(InversePolicy.Default)).fold(e => sys.error(e.message), identity))
      case other => sys.error(s"unknown candidate '$other'")
    for Array(spec, out) <- args.drop(1).grouped(2) do measure(candidateName, bridge, Path.of(spec), Path.of(out))

  private def measure(candidateName: String, bridge: FrameBridge, specPath: Path, outPath: Path): Unit =
    val inputs = FslrQualification.readInputs(specPath)
    val source = inputs.source
    val toVoxel = source.voxelToWorld
    def voxelOf(p: WorldPoint): Option[Vector[Int]] =
      toVoxel.inverse(Vector(p.x, p.y, p.z)).toOption.map(_.map(c => math.floor(c + 0.5).toInt))

    val hemispheres = ujson.Obj()
    for h <- RealAssets.hemispheres do
      val request = RouteRequest(source, StandardCorticalMesh.FsLR32k, h.reference.hemisphere,
        MappingMethod.MidthicknessNearest, ValueSemantics.Continuous)
      val candidate = SurfaceRoute.admit(request,
        SamplingAnatomy.make(h.reference, AnatomicalGeometry.Midthickness(h.surface)).fold(e => sys.error(e.message), identity),
        Some(bridge)).fold(r => sys.error(r.message), identity)
      val oracle = RealInverseOracle.solutions(h.label)
      val oracleRoute = SurfaceRoute.admit(request, oracleAnatomy(h, oracle)).fold(r => sys.error(r.message), identity)
      val placement = candidate.bridgePlacement.getOrElse(sys.error("candidate route has no bridge placement"))
      val cortical = (0 until h.surface.geometry.vertexCount).filter(h.cortex).toVector
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
      hemispheres(h.label) = ujson.Obj(
        "corticalVertices" -> cortical.length,
        "bridgeUnavailable" -> cortical.count(v => placed(v).isEmpty),
        "placementErrorMaxMm" -> placementErrors.max, "placementErrorP99Mm" -> nearestRank(placementErrors.sorted, 0.99),
        "placementErrorOver005" -> placementErrors.count(_ > 0.05),
        "voxelDisagreements" -> voxelDisagreements,
        "volumes" -> volumes)
    val result = ujson.Obj(
      "schema" -> "scalafim.fslr-bridge-disagreement/1",
      "candidate" -> candidateName, "bridge" -> bridge.display,
      "bridgeExactness" -> bridge.exactness.label,
      "oracle" -> RealInverseOracle.manifest("description").str,
      "oracleFiles" -> ujson.Obj.from(Vector("L", "R").map(h => h -> RealInverseOracle.manifest("hemispheres")(h)("sha256"))),
      "spec" -> ujson.Obj("name" -> inputs.name, "sha256" -> inputs.specSha256),
      "hemispheres" -> hemispheres)
    Files.writeString(outPath, ujson.write(result, indent = 2), StandardCharsets.UTF_8)
    println(ujson.write(result))

  /** The fsLR midthickness with every vertex at its oracle 2009c position, declared in 2009c by derivation. */
  private def oracleAnatomy(h: RealAssets.Hemisphere32k, oracle: Vector[RealInverseOracle.Solution]): SamplingAnatomy =
    val geometry = h.surface.geometry
    val coordinates = new Array[Double](3 * oracle.length)
    for (s, i) <- oracle.zipWithIndex do
      coordinates(3 * i) = s.world.x
      coordinates(3 * i + 1) = s.world.y
      coordinates(3 * i + 2) = s.world.z
    val placed = SurfaceGeometry(TriangleMesh.fromArrays(coordinates, geometry.mesh.faceIndices.clone()), geometry.hemisphere,
      geometry.kind)
    val oracleFile = DataAsset.make(RealInverseOracle.manifest("hemispheres")(h.label)("file").str,
      RealInverseOracle.manifest("hemispheres")(h.label)("sha256").str).fold(e => sys.error(e.message), identity)
    val basis = FrameBasis.derived("SimpleITK fixed-point inverse placement of the declared fsLR midthickness",
      Vector(h.surface.declaration.asset, RealAssets.pointMap.source)).fold(e => sys.error(e.message), identity)
    val declaration = FrameDeclaration.make(RealAssets.nlin2009c, basis, oracleFile).fold(e => sys.error(e.message), identity)
    SamplingAnatomy.make(h.reference, AnatomicalGeometry.Midthickness(DeclaredSurface.unsafeAssumeVerified(declaration, placed)))
      .fold(e => sys.error(e.message), identity)

  private def distance(p: WorldPoint, q: WorldPoint): Double =
    math.sqrt(math.pow(p.x - q.x, 2) + math.pow(p.y - q.y, 2) + math.pow(p.z - q.z, 2))

  private def nearestRank(sorted: Vector[Double], q: Double): Double =
    if sorted.isEmpty then 0.0 else sorted(math.max(0, math.ceil(q * sorted.length).toInt - 1))
