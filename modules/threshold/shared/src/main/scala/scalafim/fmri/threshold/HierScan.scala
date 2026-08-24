package scalafim.fmri.threshold

import gale.linalg.DMat
import scalafim.image.{SomeMaskVolume, SomeScalarVolume}

final case class HierScanConfig(
    alpha: Alpha = Alpha.unsafe(0.05),
    alternative: ThresholdAlternative = ThresholdAlternative.Greater,
    kappas: Vector[Kappa] = Vector(Kappa.unsafe(1.0), Kappa.unsafe(2.0), Kappa.unsafe(4.0)),
    minVoxels: Int = 1,
    minAlpha: Double = 1e-6,
    maxDepth: Int = 32,
    priorEta: Double = 1.0,
    minPriorMass: Double = 1e-10
):
  require(kappas.nonEmpty, "kappas must be non-empty")
  require(minVoxels > 0, "minVoxels must be positive")
  require(minAlpha.isFinite && minAlpha > 0.0 && minAlpha < 1.0, "minAlpha must be finite and in (0, 1)")
  require(maxDepth > 0, "maxDepth must be positive")
  require(priorEta.isFinite && priorEta >= 0.0 && priorEta <= 1.0, "priorEta must be finite and in [0, 1]")
  require(minPriorMass.isFinite && minPriorMass >= 0.0, "minPriorMass must be finite and non-negative")

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

final case class HierScanRegionHit(
    path: Vector[Int],
    depth: Int,
    region: ThresholdRegion,
    scoreValue: ScoreValue,
    adjustedP: AdjustedP
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
      scan <- scan(field, priors, root, nullDraw, config, statistic.orientation)
      reject <- field.maskFromMaskSpace(scan.hitIndices, "HierScan")
      cutoff <- hitCutoff(scan.hits)
    yield
      HierScanResult(
        reject = reject,
        significantRegions = scan.hits,
        nodeTests = scan.tests,
        cutoff = cutoff,
        params = params(config, nullDraw)
      )

  private def scan(
    field: MaskedField,
    priors: PriorWeights,
    root: ThresholdRegion,
    nullDraw: NullDraw,
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
    nullDraw: NullDraw,
    config: HierScanConfig,
    orientation: EvidenceOrientation,
    builder: ScanBuilder
  ): Either[ThresholdError, Unit] =
    if depth >= config.maxDepth || region.size <= config.minVoxels || alphaBudget < config.minAlpha then Right(())
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
    nullDraw: NullDraw,
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

    for
      nullMatrix <- childNullMatrix(children, field, priors, nullDraw, config, orientation)
      nodeAlpha <- Alpha(alphaBudget)
      tests <- WestfallYoung.stepDown(observed, nullMatrix, nodeAlpha)
      _ <- applyTests(children, tests, parentPath, parentDepth, alphaBudget, field, priors, nullDraw, config, orientation, builder)
    yield ()

  private def applyTests(
    children: Vector[ThresholdRegion],
    tests: Vector[AdjustedTest],
    parentPath: Vector[Int],
    parentDepth: Int,
    alphaBudget: Double,
    field: MaskedField,
    priors: PriorWeights,
    nullDraw: NullDraw,
    config: HierScanConfig,
    orientation: EvidenceOrientation,
    builder: ScanBuilder
  ): Either[ThresholdError, Unit] =
    val childAlpha = alphaBudget / children.length.toDouble
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
        if terminal(child, childDepth, childAlpha, config) then
          builder.addHit(HierScanRegionHit(childPath, childDepth, child, ScoreValue.unsafeFinite(test.score), test.adjustedP))
        else
          descend(child, childPath, childDepth, childAlpha, field, priors, nullDraw, config, orientation, builder) match
            case Left(err) => return Left(err)
            case Right(()) => ()
      i += 1
    Right(())

  private def terminal(region: ThresholdRegion, depth: Int, alphaBudget: Double, config: HierScanConfig): Boolean =
    region.size <= config.minVoxels ||
      region.bbox.isSingleton ||
      depth >= config.maxDepth ||
      alphaBudget < config.minAlpha

  private def childNullMatrix(
    children: Vector[ThresholdRegion],
    field: MaskedField,
    priors: PriorWeights,
    nullDraw: NullDraw,
    config: HierScanConfig,
    orientation: EvidenceOrientation
  ): Either[ThresholdError, DMat] =
    val rows = nullDraw.nPermutations.value
    val cols = children.length
    val out = DMat.newBuilder(rows, cols)
    var row = 0
    while row < rows do
      nullDraw.draw(row) match
        case Left(err) => return Left(err)
        case Right(raw) =>
          transformDraw(raw, field, config.alternative, orientation) match
            case Left(err) => return Left(err)
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

  private def transformDraw(
    raw: Array[Double],
    field: MaskedField,
    alternative: ThresholdAlternative,
    orientation: EvidenceOrientation
  ): Either[ThresholdError, Array[Double]] =
    if raw.length != field.size then return Left(ThresholdError.ShapeMismatch("null draw", field.size.toString, raw.length.toString))
    alternative.validate(orientation) match
      case Left(err) => return Left(err)
      case Right(()) => ()
    val out = new Array[Double](raw.length)
    var i = 0
    while i < raw.length do
      val rawValue = raw(i)
      if !rawValue.isFinite then return Left(ThresholdError.NonFiniteData("null draw"))
      if orientation == EvidenceOrientation.Unsigned && rawValue < 0.0 then
        return Left(ThresholdError.NegativeUnsignedEvidence(i, rawValue))
      val value = alternative.applyTo(rawValue)
      out(i) = value
      i += 1
    Right(out)

  private def hitCutoff(hits: Vector[HierScanRegionHit]): Either[ThresholdError, ThresholdCutoff] =
    if hits.isEmpty then Right(ThresholdCutoff.NoRejections)
    else
      var minScore = Double.PositiveInfinity
      var i = 0
      while i < hits.length do
        hits(i).scoreValue.finiteOrError("hierarchical scan hit score") match
          case Left(err) => return Left(err)
          case Right(value) =>
            if value < minScore then minScore = value
        i += 1
      ThresholdCutoff.inclusive(minScore)

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
      "nPermutations" -> nullDraw.nPermutations.value.toString
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
      ScanOutput(hitBuilder.result(), testBuilder.result(), indexBuilder.result())

  private final case class ScanOutput(
      hits: Vector[HierScanRegionHit],
      tests: Vector[HierScanNodeTest],
      hitIndices: Array[Int]
  )
