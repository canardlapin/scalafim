package scalafim.phrfcmp.score

/** sigma of the centred paired differences with the realized df, or `Degenerate` for a trial cell whose true
  * within-condition variance is zero (protocol section 6, R3 trial endpoint, undefined cases: "the cell reports E-dev
  * MSE only"; no Fisher-z endpoint exists, so no sigma is reported for that cell).
  */
enum SigmaValue:
  case Estimate(sigma: Double, df: Int)
  case Degenerate

/** One gating pair's sigma entry. */
final case class SigmaEntry(pair: GatingPair, value: SigmaValue)

/** The 23 sigmas, exactly one per gating pair, in `GatingPair.all` order. */
final case class SigmaTable(entries: Vector[SigmaEntry]):
  require(entries.map(_.pair) == GatingPair.all, "sigma table must hold exactly the 23 gating pairs in order")

/** ICC of an error or of the coverage indicator; `Degenerate` when undefined (or, for coverage, when the pooled
  * proportion lies outside [0.05, 0.95]), so the field cannot encode which side of the band a method is on.
  */
enum IccValue:
  case Estimate(icc: Double, upper80: Double)
  case Degenerate

/** PHRF-only ICCs in C-TG-.5. */
final case class IccBlock(signedRelativeEPeakError: IccValue, tauError: IccValue, coverageIndicator: IccValue)

/** One pooled rate per method: refused-or-failed voxels over attempted voxels, over all cells the method ran in. */
final case class PooledRate(method: Method, rate: Double, attempted: Long)

final case class PooledRefusals(rates: Vector[PooledRate]):
  require(rates.map(_.method) == Method.all, "one pooled rate per method, in order")

final case class TimingSummary(median: Double, min: Double, max: Double)

/** Alpha-cache item (c): reported as not possible through the public API, with the per-alpha flatness ratio. */
enum AlphaCacheReport:
  case NotPossibleViaPublicApi(flatnessRatio: Double)

final case class TimingBlock(
    trialMlPerVoxel: TimingSummary,
    trialPreparationPerAlpha: TimingSummary,
    alphaCache: AlphaCacheReport,
    glmsingleDataset: TimingSummary,
    coldConditionPreparation: TimingSummary
)

/** The closed pilot output (design 3.1): nothing else leaves the custodian. */
final case class PilotWhitelist(sigmas: SigmaTable, icc: IccBlock, pooledRefusals: PooledRefusals, timing: TimingBlock)

object PilotWhitelist:
  val SchemaVersion: String = "phrf-cmp-pilot-whitelist-1"

/** Locale-free, platform-identical number rendering (no `String.format`, which does not link on Scala.js). */
private[score] object Fmt:
  /** 13 significant digits in scientific form, for example `1.234560000000e-01`; finite input only. */
  def sci12(d: Double): String =
    require(!d.isNaN && !d.isInfinite, "finite numbers only")
    if d == 0.0 then "0.000000000000e+00"
    else
      val bd = new java.math.BigDecimal(math.abs(d)).round(new java.math.MathContext(13, java.math.RoundingMode.HALF_EVEN))
      val digits = bd.unscaledValue.toString.padTo(13, '0')
      val exp = bd.precision - bd.scale - 1
      val e = math.abs(exp).toString
      (if d < 0 then "-" else "") + digits.head + "." + digits.slice(1, 13) + "e" + (if exp < 0 then "-" else "+") + (if e.length < 2 then "0" + e else e)

  /** Two decimals, half-even. */
  def fixed2(d: Double): String =
    new java.math.BigDecimal(d).setScale(2, java.math.RoundingMode.HALF_EVEN).toPlainString

/** The single writer: fixed key set, fixed order, locale-free 12-digit scientific numbers, ASCII only. */
object WhitelistWriter:
  private def num(d: Double): String = Fmt.sci12(d)

  private def icc(v: IccValue): String = v match
    case IccValue.Estimate(i, u) => s"""{"icc":${num(i)},"upper80":${num(u)}}"""
    case IccValue.Degenerate     => "\"degenerate\""

  private def summary(t: TimingSummary): String = s"""{"median":${num(t.median)},"min":${num(t.min)},"max":${num(t.max)}}"""

  def json(w: PilotWhitelist): String =
    val sig = w.sigmas.entries
      .map { e =>
        val (s, d) = e.value match
          case SigmaValue.Estimate(sd, df) => (num(sd), df.toString)
          case SigmaValue.Degenerate       => ("\"degenerate\"", "\"degenerate\"")
        s"""{"cell":"${e.pair.cell.id}","comparator":"${e.pair.comparator.code}","sigma":$s,"df":$d}"""
      }
      .mkString("[", ",", "]")
    val rates = w.pooledRefusals.rates
      .map(r => s"""{"method":"${r.method.code}","rate":${num(r.rate)},"attempted":${r.attempted}}""")
      .mkString("[", ",", "]")
    val cache = w.timing.alphaCache match
      case AlphaCacheReport.NotPossibleViaPublicApi(r) => s"""{"status":"not-possible-via-public-api","flatnessRatio":${num(r)}}"""
    s"""{"schema":"${PilotWhitelist.SchemaVersion}","sigmas":$sig,""" +
      s""""icc":{"cell":"C-TG-.5","signedRelativeEPeakError":${icc(w.icc.signedRelativeEPeakError)},"tauError":${icc(w.icc.tauError)},"coverageIndicator":${icc(w.icc.coverageIndicator)}},""" +
      s""""pooledRefusals":$rates,""" +
      s""""timing":{"trialMlPerVoxel":${summary(w.timing.trialMlPerVoxel)},"trialPreparationPerAlpha":${summary(w.timing.trialPreparationPerAlpha)},"alphaCache":$cache,"glmsingleDataset":${summary(w.timing.glmsingleDataset)},"coldConditionPreparation":${summary(w.timing.coldConditionPreparation)}}}"""

  def bytes(w: PilotWhitelist): Array[Byte] = json(w).getBytes("UTF-8")

  def sha256Hex(w: PilotWhitelist): String = scalafim.phrfcmp.ingest.Digests.sha256Hex(bytes(w))
