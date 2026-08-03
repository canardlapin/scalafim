package scalafim.surface.view.raster

import intaglio.*

enum SurfaceVisualQaError:
  case DimensionMismatch(expectedWidth: Int, expectedHeight: Int, observedWidth: Int, observedHeight: Int)

  def message: String =
    this match
      case DimensionMismatch(expectedWidth, expectedHeight, observedWidth, observedHeight) =>
        s"visual-QA raster dimensions differ: expected ${expectedWidth}x$expectedHeight, observed ${observedWidth}x$observedHeight"

final case class SurfaceVisualQaPolicy private (
  minimumForegroundPixelsPerHemisphere: Int,
  minimumMaskIntersectionOverUnion: Double,
  maximumCentroidDistancePixels: Double,
  maximumMeanInteriorChannelError: Double
)

object SurfaceVisualQaPolicy:
  def make(
    minimumForegroundPixelsPerHemisphere: Int,
    minimumMaskIntersectionOverUnion: Double,
    maximumCentroidDistancePixels: Double,
    maximumMeanInteriorChannelError: Double
  ): Either[String, SurfaceVisualQaPolicy] =
    if minimumForegroundPixelsPerHemisphere <= 0 then Left("minimum foreground pixels must be positive")
    else if !minimumMaskIntersectionOverUnion.isFinite || minimumMaskIntersectionOverUnion < 0.0 || minimumMaskIntersectionOverUnion > 1.0 then
      Left("minimum mask IoU must be finite and in [0, 1]")
    else if !maximumCentroidDistancePixels.isFinite || maximumCentroidDistancePixels < 0.0 then
      Left("maximum centroid distance must be finite and non-negative")
    else if !maximumMeanInteriorChannelError.isFinite || maximumMeanInteriorChannelError < 0.0 then
      Left("maximum mean channel error must be finite and non-negative")
    else Right(new SurfaceVisualQaPolicy(
      minimumForegroundPixelsPerHemisphere,
      minimumMaskIntersectionOverUnion,
      maximumCentroidDistancePixels,
      maximumMeanInteriorChannelError
    ))

  val NativeBackend: SurfaceVisualQaPolicy =
    make(4000, 0.90, 3.0, 24.0).fold(error => throw new IllegalStateException(error), identity)

  val Browser: SurfaceVisualQaPolicy = NativeBackend

final case class SurfaceVisualQaReceipt(
  width: Int,
  height: Int,
  expectedForegroundPixels: Int,
  observedForegroundPixels: Int,
  expectedLeftForegroundPixels: Int,
  expectedRightForegroundPixels: Int,
  observedLeftForegroundPixels: Int,
  observedRightForegroundPixels: Int,
  maskIntersectionPixels: Int,
  maskUnionPixels: Int,
  maskIntersectionOverUnion: Double,
  centroidDistancePixels: Double,
  interiorPixelsCompared: Int,
  meanInteriorChannelError: Double,
  maximumInteriorChannelError: Int
):
  def violations(policy: SurfaceVisualQaPolicy): Vector[String] =
    val failures = Vector.newBuilder[String]
    if expectedLeftForegroundPixels < policy.minimumForegroundPixelsPerHemisphere ||
      expectedRightForegroundPixels < policy.minimumForegroundPixelsPerHemisphere
    then failures += "reference fixture does not cover enough pixels in both hemispheres"
    if observedLeftForegroundPixels < policy.minimumForegroundPixelsPerHemisphere ||
      observedRightForegroundPixels < policy.minimumForegroundPixelsPerHemisphere
    then failures += "native rendering does not cover enough pixels in both hemispheres"
    if maskIntersectionOverUnion < policy.minimumMaskIntersectionOverUnion then
      failures += f"foreground mask IoU $maskIntersectionOverUnion%.6f is below ${policy.minimumMaskIntersectionOverUnion}%.6f"
    if centroidDistancePixels > policy.maximumCentroidDistancePixels then
      failures += f"foreground centroid distance $centroidDistancePixels%.6f exceeds ${policy.maximumCentroidDistancePixels}%.6f pixels"
    if interiorPixelsCompared == 0 then failures += "no interior pixels were available for color comparison"
    else if meanInteriorChannelError > policy.maximumMeanInteriorChannelError then
      failures += f"mean interior channel error $meanInteriorChannelError%.6f exceeds ${policy.maximumMeanInteriorChannelError}%.6f"
    failures.result()

object SurfaceVisualQa:
  private val ForegroundLimit = 250

  def compare(expected: RasterImage, observed: RasterImage): Either[SurfaceVisualQaError, SurfaceVisualQaReceipt] =
    if expected.dimensions != observed.dimensions then
      Left(SurfaceVisualQaError.DimensionMismatch(expected.width, expected.height, observed.width, observed.height))
    else Right(compareUnsafe(expected, observed))

  private def compareUnsafe(expected: RasterImage, observed: RasterImage): SurfaceVisualQaReceipt =
    val width = expected.width
    val height = expected.height
    val midpoint = width / 2
    var expectedForeground = 0
    var observedForeground = 0
    var expectedLeft = 0
    var expectedRight = 0
    var observedLeft = 0
    var observedRight = 0
    var intersection = 0
    var union = 0
    var expectedX = 0.0
    var expectedY = 0.0
    var observedX = 0.0
    var observedY = 0.0
    var y = 0
    while y < height do
      var x = 0
      while x < width do
        val expectedIsForeground = foreground(expected.pixelUnsafe(x, y))
        val observedIsForeground = foreground(observed.pixelUnsafe(x, y))
        if expectedIsForeground then
          expectedForeground += 1
          expectedX += x
          expectedY += y
          if x < midpoint then expectedLeft += 1 else expectedRight += 1
        if observedIsForeground then
          observedForeground += 1
          observedX += x
          observedY += y
          if x < midpoint then observedLeft += 1 else observedRight += 1
        if expectedIsForeground && observedIsForeground then intersection += 1
        if expectedIsForeground || observedIsForeground then union += 1
        x += 1
      y += 1

    val expectedCentroidX = if expectedForeground == 0 then 0.0 else expectedX / expectedForeground
    val expectedCentroidY = if expectedForeground == 0 then 0.0 else expectedY / expectedForeground
    val observedCentroidX = if observedForeground == 0 then 0.0 else observedX / observedForeground
    val observedCentroidY = if observedForeground == 0 then 0.0 else observedY / observedForeground
    val centroidDistance = math.hypot(observedCentroidX - expectedCentroidX, observedCentroidY - expectedCentroidY)

    var interiorPixels = 0
    var channelError = 0L
    var maximumError = 0
    y = 1
    while y + 1 < height do
      var x = 1
      while x + 1 < width do
        if interior(expected, x, y) then
          val reference = expected.pixelUnsafe(x, y)
          val actual = observed.pixelUnsafe(x, y)
          val redError = math.abs(reference.red - actual.red)
          val greenError = math.abs(reference.green - actual.green)
          val blueError = math.abs(reference.blue - actual.blue)
          channelError += redError.toLong + greenError.toLong + blueError.toLong
          maximumError = math.max(maximumError, math.max(redError, math.max(greenError, blueError)))
          interiorPixels += 1
        x += 1
      y += 1

    SurfaceVisualQaReceipt(
      width,
      height,
      expectedForeground,
      observedForeground,
      expectedLeft,
      expectedRight,
      observedLeft,
      observedRight,
      intersection,
      union,
      if union == 0 then 1.0 else intersection.toDouble / union.toDouble,
      centroidDistance,
      interiorPixels,
      if interiorPixels == 0 then 0.0 else channelError.toDouble / (interiorPixels.toDouble * 3.0),
      maximumError
    )

  private def interior(image: RasterImage, x: Int, y: Int): Boolean =
    foreground(image.pixelUnsafe(x, y)) &&
      foreground(image.pixelUnsafe(x - 1, y)) &&
      foreground(image.pixelUnsafe(x + 1, y)) &&
      foreground(image.pixelUnsafe(x, y - 1)) &&
      foreground(image.pixelUnsafe(x, y + 1))

  private def foreground(pixel: Rgba32): Boolean =
    pixel.alpha > 0 && (pixel.red < ForegroundLimit || pixel.green < ForegroundLimit || pixel.blue < ForegroundLimit)
