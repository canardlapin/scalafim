package scalafim.group.research.bootstrap

/** SplitMix64 (Steele, Lea & Flood 2014): a 64-bit counter-based generator whose
  * output is a pure function of its seed, identical on the JVM and Scala.js
  * (Long arithmetic is exact on both). Mutable by design: one instance is one
  * stream lane, advanced by the draw loop without allocation.
  */
final class SplitMix64 private (private var state: Long):
  private var spare = 0.0
  private var hasSpare = false

  def nextLong(): Long =
    state += SplitMix64.Increment
    SplitMix64.mix64(state)

  /** Uniform on [0, 1) with 53 random bits. */
  def nextDouble(): Double = (nextLong() >>> 11).toDouble * SplitMix64.Unit53

  /** Uniform on the open interval (0, 1). */
  def nextOpenDouble(): Double = ((nextLong() >>> 12).toDouble + 0.5) * SplitMix64.Unit52

  /** Standard normal by Marsaglia's polar method; the second variate of each pair is cached. */
  def nextGaussian(): Double =
    if hasSpare then
      hasSpare = false
      spare
    else
      var u = 0.0
      var v = 0.0
      var s = 0.0
      while s >= 1.0 || s == 0.0 do
        u = 2.0 * nextDouble() - 1.0
        v = 2.0 * nextDouble() - 1.0
        s = u * u + v * v
      val m = math.sqrt(-2.0 * math.log(s) / s)
      spare = v * m
      hasSpare = true
      u * m

  /** Gamma(shape, 1) by Marsaglia & Tsang (2000); shapes below one use the
    * Gamma(shape + 1) * U^(1/shape) boost.
    */
  def nextGamma(shape: Double): Double =
    require(shape > 0.0 && shape.isFinite, s"gamma shape must be positive and finite, got $shape")
    if shape < 1.0 then nextGamma(shape + 1.0) * math.pow(nextOpenDouble(), 1.0 / shape)
    else
      val d = shape - 1.0 / 3.0
      val c = 1.0 / math.sqrt(9.0 * d)
      var result = -1.0
      while result < 0.0 do
        val x = nextGaussian()
        val t = 1.0 + c * x
        if t > 0.0 then
          val v = t * t * t
          val u = nextOpenDouble()
          val x2 = x * x
          if u < 1.0 - 0.0331 * x2 * x2 || math.log(u) < 0.5 * x2 + d * (1.0 - v + math.log(v)) then result = d * v
      result

  /** A chi-square variate with `df` degrees of freedom (df may be fractional). */
  def nextChiSquare(df: Double): Double = 2.0 * nextGamma(df / 2.0)

  /** A fair sign, +1 or -1, from the top bit. */
  def nextSign(): Double = if nextLong() < 0L then -1.0 else 1.0

object SplitMix64:
  val Increment: Long = 0x9e3779b97f4a7c15L
  private val Unit53 = 1.0 / (1L << 53).toDouble
  private val Unit52 = 1.0 / (1L << 52).toDouble

  def mix64(input: Long): Long =
    var z = input
    z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
    z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
    z ^ (z >>> 31)

  def fromSeed(seed: Long): SplitMix64 = new SplitMix64(seed)

  /** FNV-1a 64 over the UTF-16 code units of an ASCII identifier. */
  def fnv1a64(text: String): Long =
    var h = 0xcbf29ce484222325L
    var i = 0
    while i < text.length do
      h = (h ^ (text.charAt(i).toLong & 0xffL)) * 0x100000001b3L
      i += 1
    h

/** Declaration §5 seed roots: pilot 2026100101-05 and confirmation 2026100201-05. */
enum Phase(val base: Long):
  case Pilot extends Phase(2026100100L)
  case Confirmation extends Phase(2026100200L)
  /** Harness-only root family for fixtures and native tests; never a pilot or confirmation root. */
  case Harness extends Phase(2026093000L)

  def root(stream: StreamKind): Long = base + stream.code

/** The five declared streams (§5); the code is the last two digits of the root. */
enum StreamKind(val code: Int):
  case Outcome extends StreamKind(1)
  case FirstLevel extends StreamKind(2)
  case Bootstrap extends StreamKind(3)
  case SignFlip extends StreamKind(4)
  case Power extends StreamKind(5)

/** Independent sub-streams inside one keyed stream, so that a scheme that skips
  * a variate kind never shifts the others (this is what pairs the candidates).
  */
enum Lane(val code: Int):
  case SubjectU extends Lane(1)
  case SubjectE extends Lane(2)
  case FirstLevelChi extends Lane(3)
  case HierarchySigma extends Lane(4)
  case FirstLevelSeries extends Lane(5)
  case DrawU extends Lane(11)
  case DrawE extends Lane(12)
  case DrawChi extends Lane(13)
  case DrawPosterior extends Lane(14)
  case Signs extends Lane(21)
  case TieBreak extends Lane(22)

/** Whether a study belongs to the null stream or the independent power stream (§5). */
enum StudyPurpose:
  case Null, Power

/** A stream key (root, cell ID, study, stream), exactly the declared SplitMix64 key.
  * Lanes refine it; the lane seed is a SplitMix64 mix of every key component.
  */
final case class StreamKey(root: Long, cell: CellId, study: Int, stream: StreamKind):
  require(root > 0L, "stream root must be positive")
  require(study >= 0, "study index must be non-negative")

  def seed(lane: Lane): Long =
    import SplitMix64.mix64
    var h = mix64(root)
    h = mix64(h ^ SplitMix64.fnv1a64(cell.value))
    h = mix64(h + SplitMix64.Increment * (study.toLong + 1L))
    h = mix64(h ^ (stream.code.toLong << 32))
    mix64(h + SplitMix64.Increment * lane.code.toLong)

  def lane(lane: Lane): SplitMix64 = SplitMix64.fromSeed(seed(lane))

object StreamKey:
  /** Null studies use the per-stream roots; power studies take every component
    * from the power root, so the power stream is independent of the null stream.
    */
  def of(phase: Phase, purpose: StudyPurpose, cell: CellId, study: Int, stream: StreamKind): StreamKey =
    val root = purpose match
      case StudyPurpose.Null => phase.root(stream)
      case StudyPurpose.Power => phase.root(StreamKind.Power)
    StreamKey(root, cell, study, stream)
