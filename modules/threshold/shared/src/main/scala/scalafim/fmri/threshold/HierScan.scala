package scalafim.fmri.threshold

import scalafim.image.NeuroVol
import scalafim.linalg.DoubleMatrix

final case class HierScanConfig(
    alpha: Alpha = Alpha.unsafe(0.05),
    tail: Tail = Tail.Positive,
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

final case class HierScanNodeTest(
    path: Vector[Int],
    depth: Int,
    childId: Int,
    regionSize: Int,
    priorMass: Double,
    score: Double,
    adjustedP: Double,
    rejected: Boolean
)

final case class HierScanRegionHit(
    path: Vector[Int],
    depth: Int,
    region: Region,
    score: Double,
    adjustedP: Double
):
  def maskSpaceIndices: Vector[Int] =
    region.indicesVector

object HierScan:

  def run(
    stat: NeuroVol[Double],
    nullDraw: NullDraw,
    mask: Option[NeuroVol[Boolean]] = None,
    prior: Option[NeuroVol[Double]] = None,
    config: HierScanConfig = HierScanConfig()
  ): Either[ThresholdError, HierScanResult] =
    for
      field <- mask match
        case Some(m) => MaskedField.fromVolume(stat, m, config.tail)
        case None    => MaskedField.fromVolume(stat, config.tail)
      rawPriors <- prior match
        case Some(p) => PriorWeights.fromVolume(p, field)
        case None    => PriorWeights.uniform(field.size)
      priors <- rawPriors.shrinkToUniform(config.priorEta)
      root <- Octree.root(field, priors)
      scan <- scan(field, priors, root, nullDraw, config)
      reject <- field.maskFromMaskSpace(scan.hitIndices, "HierScan")
    yield
      val threshold =
        if scan.hits.isEmpty then Double.PositiveInfinity
        else scan.hits.map(_.score).min
      HierScanResult(
        reject = reject,
        significantRegions = scan.hits,
        nodeTests = scan.tests,
        threshold = threshold,
        params = params(config, nullDraw)
      )

  private def scan(
    field: MaskedField,
    priors: PriorWeights,
    root: Region,
    nullDraw: NullDraw,
    config: HierScanConfig
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
      builder = builder
    ).map(_ => builder.result())

  private def descend(
    region: Region,
    path: Vector[Int],
    depth: Int,
    alphaBudget: Double,
    field: MaskedField,
    priors: PriorWeights,
    nullDraw: NullDraw,
    config: HierScanConfig,
    builder: ScanBuilder
  ): Either[ThresholdError, Unit] =
    if depth >= config.maxDepth || region.size <= config.minVoxels || alphaBudget < config.minAlpha then Right(())
    else
      for
        children <- Octree.split(region, field, priors, config.minPriorMass)
        _ <-
          if children.isEmpty then Right(())
          else testChildren(children, path, depth, alphaBudget, field, priors, nullDraw, config, builder)
      yield ()

  private def testChildren(
    children: Vector[Region],
    parentPath: Vector[Int],
    parentDepth: Int,
    alphaBudget: Double,
    field: MaskedField,
    priors: PriorWeights,
    nullDraw: NullDraw,
    config: HierScanConfig,
    builder: ScanBuilder
  ): Either[ThresholdError, Unit] =
    val observed = new Array[Double](children.length)
    var i = 0
    while i < children.length do
      ScoreSet.omnibus(children(i).indexArray, field.data, priors, config.kappas) match
        case Left(err) => return Left(err)
        case Right(score) => observed(i) = score.score
      i += 1

    for
      nullMatrix <- childNullMatrix(children, field, priors, nullDraw, config)
      nodeAlpha <- Alpha(alphaBudget)
      tests <- WestfallYoung.stepDown(observed, nullMatrix, nodeAlpha)
      _ <- applyTests(children, tests, parentPath, parentDepth, alphaBudget, field, priors, nullDraw, config, builder)
    yield ()

  private def applyTests(
    children: Vector[Region],
    tests: Vector[AdjustedTest],
    parentPath: Vector[Int],
    parentDepth: Int,
    alphaBudget: Double,
    field: MaskedField,
    priors: PriorWeights,
    nullDraw: NullDraw,
    config: HierScanConfig,
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
          score = test.score,
          adjustedP = test.adjustedP,
          rejected = test.rejected
        )
      )

      if test.rejected then
        if terminal(child, childDepth, childAlpha, config) then
          builder.addHit(HierScanRegionHit(childPath, childDepth, child, test.score, test.adjustedP))
        else
          descend(child, childPath, childDepth, childAlpha, field, priors, nullDraw, config, builder) match
            case Left(err) => return Left(err)
            case Right(()) => ()
      i += 1
    Right(())

  private def terminal(region: Region, depth: Int, alphaBudget: Double, config: HierScanConfig): Boolean =
    region.size <= config.minVoxels ||
      region.bbox.isSingleton ||
      depth >= config.maxDepth ||
      alphaBudget < config.minAlpha

  private def childNullMatrix(
    children: Vector[Region],
    field: MaskedField,
    priors: PriorWeights,
    nullDraw: NullDraw,
    config: HierScanConfig
  ): Either[ThresholdError, DoubleMatrix] =
    val rows = nullDraw.nPermutations.value
    val cols = children.length
    val out = new Array[Double](rows * cols)
    var row = 0
    while row < rows do
      nullDraw.draw(row) match
        case Left(err) => return Left(err)
        case Right(raw) =>
          transformDraw(raw, field, config.tail) match
            case Left(err) => return Left(err)
            case Right(draw) =>
              var col = 0
              while col < cols do
                ScoreSet.omnibus(children(col).indexArray, draw, priors, config.kappas) match
                  case Left(err) => return Left(err)
                  case Right(score) => out(row * cols + col) = score.score
                col += 1
      row += 1
    Right(DoubleMatrix.unsafe(rows, cols, out))

  private def transformDraw(raw: Array[Double], field: MaskedField, tail: Tail): Either[ThresholdError, Array[Double]] =
    if raw.length != field.size then return Left(ThresholdError.ShapeMismatch("null draw", field.size.toString, raw.length.toString))
    val out = new Array[Double](raw.length)
    var i = 0
    while i < raw.length do
      val value = tail.applyTo(raw(i))
      if !value.isFinite then return Left(ThresholdError.NonFiniteData("null draw"))
      out(i) = value
      i += 1
    Right(out)

  private def params(config: HierScanConfig, nullDraw: NullDraw): Map[String, String] =
    Map(
      "alpha" -> config.alpha.value.toString,
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
