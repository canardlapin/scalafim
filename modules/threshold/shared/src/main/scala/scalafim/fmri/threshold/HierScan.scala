package scalafim.fmri.threshold

import gale.linalg.DMat
import scalafim.image.{SomeMaskVolume, SomeScalarVolume}

/** HierScan settings.
  *
  * Each tested node spends `gamma` of its alpha budget on the step-down test
  * of its children. The remaining `(1 - gamma)` is shared among the rejected
  * children in proportion to their prior mass, so a whole tree spends at most
  * `alpha`, as in `neurothresh::hier_descend`. Regions with fewer than
  * `minVoxels` voxels are not split and tested.
  */
final case class HierScanConfig(
    alpha: Alpha = Alpha.unsafe(0.05),
    alternative: ThresholdAlternative = ThresholdAlternative.Greater,
    kappas: Vector[Kappa] = Vector(Kappa.unsafe(1.0), Kappa.unsafe(2.0), Kappa.unsafe(4.0)),
    minVoxels: Int = 1,
    minAlpha: Double = 1e-6,
    maxDepth: Int = 32,
    priorEta: Double = 1.0,
    minPriorMass: Double = 1e-10,
    gamma: Double = 0.5
):
  require(kappas.nonEmpty, "kappas must be non-empty")
  require(minVoxels > 0, "minVoxels must be positive")
  require(minAlpha.isFinite && minAlpha > 0.0 && minAlpha < 1.0, "minAlpha must be finite and in (0, 1)")
  require(maxDepth > 0, "maxDepth must be positive")
  require(priorEta.isFinite && priorEta >= 0.0 && priorEta <= 1.0, "priorEta must be finite and in [0, 1]")
  require(minPriorMass.isFinite && minPriorMass >= 0.0, "minPriorMass must be finite and non-negative")
  require(gamma.isFinite && gamma > 0.0 && gamma <= 1.0, "gamma must be finite and in (0, 1]")

  def tail: Tail =
    Tail.fromAlternative(alternative)

final case class HierScanNodeTest(
    path: Vector[Int],
    depth: Int,
    childId: Int,
    regionSize: Int,
    priorMass: Double,
    scoreValue: ScoreValue,
    adjustedP: AdjustedP,
    rejected: Boolean
):
  def score: Double =
    scoreValue.toLegacyDouble

  def adjustedPValue: Double =
    adjustedP.value

/** A rejected region. Every rejected node is a hit, including a coarse region
  * whose own children are not rejected: the procedure then localizes the
  * effect to this region but no further. `alphaTest` is the level at which
  * its step-down test ran.
  */
final case class HierScanRegionHit(
    path: Vector[Int],
    depth: Int,
    region: ThresholdRegion,
    scoreValue: ScoreValue,
    adjustedP: AdjustedP,
    alphaTest: Double
):
  def score: Double =
    scoreValue.toLegacyDouble

  def adjustedPValue: Double =
    adjustedP.value

  def maskSpaceIndices: Vector[Int] =
    region.indicesVector

object HierScan:

  def run(
    stat: SomeScalarVolume[Double],
    nullDraw: NullDraw,
    mask: Option[SomeMaskVolume] = None,
    prior: Option[SomeScalarVolume[Double]] = None,
    config: HierScanConfig = HierScanConfig()
  ): Either[ThresholdError, HierScanResult] =
    runMap(StatisticMap.z(stat), nullDraw, mask, prior, config)

  def runMap(
    statistic: StatisticMap,
    nullDraw: NullDraw,
    mask: Option[SomeMaskVolume] = None,
    prior: Option[SomeScalarVolume[Double]] = None,
    config: HierScanConfig = HierScanConfig()
  ): Either[ThresholdError, HierScanResult] =
    for
      statisticField <- mask match
        case Some(m) => StatisticField.fromMap(statistic, m, config.alternative)
        case None    => StatisticField.fromMap(statistic, config.alternative)
      field = statisticField.evidence
      rawPriors <- prior match
        case Some(p) => PriorWeights.fromVolume(p, field)
        case None    => PriorWeights.uniform(field.size)
      priors <- rawPriors.shrinkToUniform(config.priorEta)
      root <- Octree.root(field, priors)
      scan <- scan(field, priors, root, NullDrawLedger(nullDraw, field.size), config, statistic.orientation)
      reject <- field.maskFromMaskSpace(scan.hitIndices, "HierScan")
    yield
      HierScanResult(
        reject = reject,
        significantRegions = scan.hits,
        nodeTests = scan.tests,
        params = params(config, nullDraw)
      )

  private def scan(
    field: MaskedField,
    priors: PriorWeights,
    root: ThresholdRegion,
    nullDraw: NullDrawLedger,
    config: HierScanConfig,
    orientation: EvidenceOrientation
  ): Either[ThresholdError, ScanOutput] =
    val builder = ScanBuilder()
    descend(
      region = root,
      path = Vector.empty,
      depth = 0,
      alphaBudget = config.alpha.value,
      field = field,
      priors = priors,
      nullDraw = nullDraw,
      config = config,
      orientation = orientation,
      builder = builder
    ).map(_ => builder.result())

  private def descend(
    region: ThresholdRegion,
    path: Vector[Int],
    depth: Int,
    alphaBudget: Double,
    field: MaskedField,
    priors: PriorWeights,
    nullDraw: NullDrawLedger,
    config: HierScanConfig,
    orientation: EvidenceOrientation,
    builder: ScanBuilder
  ): Either[ThresholdError, Unit] =
    if depth >= config.maxDepth || region.size < config.minVoxels || alphaBudget < config.minAlpha then Right(())
    else
      for
        children <- Octree.split(region, field, priors, config.minPriorMass)
        _ <-
          if children.isEmpty then Right(())
          else testChildren(children, path, depth, alphaBudget, field, priors, nullDraw, config, orientation, builder)
      yield ()

  private def testChildren(
    children: Vector[ThresholdRegion],
    parentPath: Vector[Int],
    parentDepth: Int,
    alphaBudget: Double,
    field: MaskedField,
    priors: PriorWeights,
    nullDraw: NullDrawLedger,
    config: HierScanConfig,
    orientation: EvidenceOrientation,
    builder: ScanBuilder
  ): Either[ThresholdError, Unit] =
    val observed = new Array[Double](children.length)
    var i = 0
    while i < children.length do
      ScoringInput(field, priors, children(i)) match
        case Left(err) => return Left(err)
        case Right(input) =>
          ScoreSet.omnibus(input, config.kappas) match
            case Left(err) => return Left(err)
            case Right(score) =>
              score.scoreValue.finiteOrError("observed child score") match
                case Left(err) => return Left(err)
                case Right(value) => observed(i) = value
      i += 1

    val alphaTest = config.gamma * alphaBudget
    for
      nullMatrix <- childNullMatrix(children, field, priors, nullDraw, config, orientation)
      nodeAlpha <- Alpha(alphaTest)
      // Child scores and null scores are already oriented by the statistic
      // field and transformDraw, so the step-down compares them as given.
      tests <- WestfallYoung.stepDown(observed, nullMatrix, nodeAlpha, ThresholdAlternative.Greater, nullDraw.reference)
      _ <- applyTests(children, tests, parentPath, parentDepth, alphaTest, alphaBudget - alphaTest, field, priors, nullDraw, config, orientation, builder)
    yield ()

  private def applyTests(
    children: Vector[ThresholdRegion],
    tests: Vector[AdjustedTest],
    parentPath: Vector[Int],
    parentDepth: Int,
    alphaTest: Double,
    alphaDescend: Double,
    field: MaskedField,
    priors: PriorWeights,
    nullDraw: NullDrawLedger,
    config: HierScanConfig,
    orientation: EvidenceOrientation,
    builder: ScanBuilder
  ): Either[ThresholdError, Unit] =
    var rejectedMass = 0.0
    var r = 0
    while r < tests.length do
      if tests(r).rejected then rejectedMass += children(tests(r).testIndex).priorMass
      r += 1
    var i = 0
    while i < tests.length do
      val test = tests(i)
      val child = children(test.testIndex)
      val childPath = parentPath :+ child.id
      val childDepth = parentDepth + 1
      builder.addTest(
        HierScanNodeTest(
          path = childPath,
          depth = childDepth,
          childId = child.id,
          regionSize = child.size,
          priorMass = child.priorMass,
          scoreValue = ScoreValue.unsafeFinite(test.score),
          adjustedP = test.adjustedP,
          rejected = test.rejected
        )
      )

      if test.rejected then
        builder.addHit(HierScanRegionHit(childPath, childDepth, child, ScoreValue.unsafeFinite(test.score), test.adjustedP, alphaTest))
        // Share the descendant budget among rejected children by prior mass.
        val childBudget =
          if rejectedMass > 0.0 then alphaDescend * child.priorMass / rejectedMass
          else 0.0
        descend(child, childPath, childDepth, childBudget, field, priors, nullDraw, config, orientation, builder) match
          case Left(err) => return Left(err)
          case Right(()) => ()
      i += 1
    Right(())

  private def childNullMatrix(
    children: Vector[ThresholdRegion],
    field: MaskedField,
    priors: PriorWeights,
    nullDraw: NullDrawLedger,
    config: HierScanConfig,
    orientation: EvidenceOrientation
  ): Either[ThresholdError, DMat] =
    val rows = nullDraw.size
    val cols = children.length
    val out = DMat.newBuilder(rows, cols)
    var row = 0
    while row < rows do
      nullDraw.fetch(row) match
        case Left(err) => return Left(err)
        case Right(raw) =>
          transformDraw(raw, config.alternative, orientation) match
            case Left(err) => return Left(ThresholdError.NullDrawFailed(row, err))
            case Right(draw) =>
              var col = 0
              while col < cols do
                ScoreSet.omnibus(children(col).indexArray, draw, priors, config.kappas) match
                  case Left(err) => return Left(err)
                  case Right(score) =>
                    score.scoreValue.finiteOrError("null child score") match
                      case Left(err) => return Left(err)
                      case Right(value) => out(row, col) = value
                col += 1
      row += 1
    Right(out.result())

  /** Orient one ledger-validated draw (correct length, finite values). The
    * alternative was admitted for the evidence by `StatisticField.fromMap`.
    */
  private def transformDraw(
    raw: Array[Double],
    alternative: ThresholdAlternative,
    orientation: EvidenceOrientation
  ): Either[ThresholdError, Array[Double]] =
    val out = new Array[Double](raw.length)
    var i = 0
    while i < raw.length do
      val rawValue = raw(i)
      if orientation == EvidenceOrientation.Unsigned && rawValue < 0.0 then
        return Left(ThresholdError.NegativeUnsignedEvidence(i, rawValue))
      val value = alternative.applyTo(rawValue)
      out(i) = value
      i += 1
    Right(out)

  private def params(config: HierScanConfig, nullDraw: NullDraw): Map[String, String] =
    Map(
      "alpha" -> config.alpha.value.toString,
      "alternative" -> config.alternative.toString,
      "tail" -> config.tail.toString,
      "kappas" -> config.kappas.map(_.value).mkString(","),
      "minVoxels" -> config.minVoxels.toString,
      "minAlpha" -> config.minAlpha.toString,
      "maxDepth" -> config.maxDepth.toString,
      "priorEta" -> config.priorEta.toString,
      "minPriorMass" -> config.minPriorMass.toString,
      "gamma" -> config.gamma.toString,
      "nPermutations" -> nullDraw.nPermutations.value.toString,
      "nullReference" -> nullDraw.reference.toString
    )

  private final class ScanBuilder:
    private val hitBuilder = Vector.newBuilder[HierScanRegionHit]
    private val testBuilder = Vector.newBuilder[HierScanNodeTest]
    private val indexBuilder = Array.newBuilder[Int]

    def addHit(hit: HierScanRegionHit): Unit =
      hitBuilder += hit
      indexBuilder ++= hit.region.indexArray

    def addTest(test: HierScanNodeTest): Unit =
      testBuilder += test

    def result(): ScanOutput =
      // Hits are nested, so the same voxel can appear in several of them.
      ScanOutput(hitBuilder.result(), testBuilder.result(), indexBuilder.result().distinct.sorted)

  private final case class ScanOutput(
      hits: Vector[HierScanRegionHit],
      tests: Vector[HierScanNodeTest],
      hitIndices: Array[Int]
  )
