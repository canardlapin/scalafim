package scalafim.group.research.bootstrap

/** Immutable cell identifier, `F-[stress-]n{n}-D{I|G}-V{pattern}-T{0|2}-N{inf|nu}` (declaration §5).
  * F is the family letter: C core Gaussian, S stress, H hierarchy (EB positive control).
  */
opaque type CellId = String

object CellId:
  private val Pattern = "^[CSH]-([a-z0-9]+-)?n[0-9]+-D[IG]-V[a-z]+-T[02]-N(inf|[0-9]+)$".r

  def parse(text: String): Either[String, CellId] =
    if Pattern.matches(text) then Right(text) else Left(s"not a cell id: $text")

  private[bootstrap] def unsafe(text: String): CellId = text

  extension (id: CellId) def value: String = id

enum Family(val code: String):
  case Core extends Family("C")
  case Stress extends Family("S")
  case Hierarchy extends Family("H")

enum DesignKind(val code: String):
  /** Intercept only; contrast c = (1). */
  case Intercept extends DesignKind("I")
  /** Intercept + quarter-group indicator + smooth covariate z_i = (i - .5)/n - .5; contrast = group coefficient. */
  case QuarterGroupSmooth extends DesignKind("G")

enum VariancePattern(val code: String):
  case Flat extends VariancePattern("flat")
  case Spread extends VariancePattern("spread")
  /** Spread values sorted so that the quarter group and high z get the largest sigma^2 (G only). */
  case Reversed extends VariancePattern("rev")
  /** Stress: one dominant-precision subject (precision 10) among precision .04 subjects. */
  case Dominant extends VariancePattern("dom")
  /** Family H: sigma_i^2 ~ scaled-inv-chi^2(10, .52), drawn per study. */
  case ScaledInverseChiSquare extends VariancePattern("sichi")

enum Tau2Level(val code: String, val value: Double):
  case Zero extends Tau2Level("0", 0.0)
  case Point2 extends Tau2Level("2", 0.2)

/** First-level df; `Infinite` means v is the known sigma^2. */
enum NuLevel:
  case Infinite
  case Finite(nu: Int)

  def code: String = this match
    case Infinite => "inf"
    case Finite(nu) => nu.toString

  def value: Double = this match
    case Infinite => Double.PositiveInfinity
    case Finite(nu) => nu.toDouble

/** Stress kinds (§5, family S, as enumerated in v1 and extended to nu 3/4/5 in v2). */
enum StressKind(val code: String):
  case Leverage extends StressKind("lev")
  case DominantPrecision extends StressKind("dom")
  case LowDf extends StressKind("nu")
  case DeclaredDf4 extends StressKind("df4")
  case DeclaredDf16 extends StressKind("df16")
  case StudentT3 extends StressKind("t3")
  case Lognormal extends StressKind("lnorm")
  case Autoregressive extends StressKind("ar1")

/** Subject-level law of u and e. */
enum ErrorLaw:
  case Gaussian, StudentT3, Lognormal

/** One design cell. Construct through `Cell.of`; every accessor is a pure function of the fields. */
final case class Cell private (
    family: Family,
    stress: Option[StressKind],
    n: Int,
    design: DesignKind,
    variances: VariancePattern,
    tau2: Tau2Level,
    trueNu: NuLevel
):
  val id: CellId =
    CellId.unsafe(
      s"${family.code}-${stress.fold("")(_.code + "-")}n$n-D${design.code}-V${variances.code}-T${tau2.code}-N${trueNu.code}"
    )

  /** The df the bootstrap candidates are told; differs from the truth only in the df-misspecification stress. */
  def declaredNu: NuLevel = stress match
    case Some(StressKind.DeclaredDf4) => NuLevel.Finite(4)
    case Some(StressKind.DeclaredDf16) => NuLevel.Finite(16)
    case _ => trueNu

  def errorLaw: ErrorLaw = stress match
    case Some(StressKind.StudentT3) => ErrorLaw.StudentT3
    case Some(StressKind.Lognormal) => ErrorLaw.Lognormal
    case _ => ErrorLaw.Gaussian

  def p: Int = design match
    case DesignKind.Intercept => 1
    case DesignKind.QuarterGroupSmooth => 3

  def terms: Vector[String] = design match
    case DesignKind.Intercept => Vector("intercept")
    case DesignKind.QuarterGroupSmooth => Vector("intercept", "group", "z")

  def quarter: Int = n / 4

  /** Row-major n x p design. Leverage stress adds 5 to z of subject 1 (index 0). */
  def designRowMajor: Array[Double] =
    design match
      case DesignKind.Intercept => Array.fill(n)(1.0)
      case DesignKind.QuarterGroupSmooth =>
        Array.tabulate(n * 3) { k =>
          val i = k / 3
          k % 3 match
            case 0 => 1.0
            case 1 => if i < quarter then 1.0 else 0.0
            case _ =>
              val z = (i + 0.5) / n - 0.5
              Canonical.round15(if stress.contains(StressKind.Leverage) && i == 0 then z + 5.0 else z)
        }

  def contrast: Array[Double] = design match
    case DesignKind.Intercept => Array(1.0)
    case DesignKind.QuarterGroupSmooth => Array(0.0, 1.0, 0.0)

  def researchDesign: ResearchDesign =
    ResearchDesign.of(n, p, designRowMajor, terms, contrast).fold(e => throw new IllegalStateException(e.message), identity)

  /** Fixed true sigma_i^2 (rounded to 15 significant digits, as serialized); None for family H. */
  def sigma2: Option[Array[Double]] =
    def spread: Array[Double] = Array.tabulate(n)(i => Canonical.round15(0.04 * math.pow(25.0, i.toDouble / (n - 1))))
    variances match
      case VariancePattern.Flat => Some(Array.fill(n)(0.52))
      case VariancePattern.Spread => Some(spread)
      case VariancePattern.Reversed =>
        val values = spread.sorted(using Ordering.Double.TotalOrdering).reverse
        // Rank subjects: quarter group first, then by descending z; largest sigma^2 goes first.
        val order = (0 until n).sortBy(i => (if i < quarter then 0 else 1, -i))
        val out = new Array[Double](n)
        order.zipWithIndex.foreach((subject, rank) => out(subject) = values(rank))
        Some(out)
      case VariancePattern.Dominant => Some(Array.tabulate(n)(i => if i == 0 then 0.1 else 25.0))
      case VariancePattern.ScaledInverseChiSquare => None

  /** Power alternative (§5): delta = 1.96 sqrt(c'(X'W X)^{-1}c), W = diag(1/(sigma^2 + tau^2)). Core cells only. */
  def powerDelta: Option[Double] =
    if family != Family.Core then None
    else
      sigma2.map { s2 =>
        val d = researchDesign
        val solver = new PmSolver(n, p, d.x)
        val y = new Array[Double](n)
        val status = solver.fit(y, s2, n - p, TauPolicy.Fixed(tau2.value))
        require(status.ok, s"power delta fit failed: ${status.message}")
        Canonical.round15(1.96 * math.sqrt(solver.quadratic(d.contrast)))
      }

  /** Autoregressive stress: first-level length, AR coefficient, and regressor (intercept + block regressor, nominal df 8). */
  def firstLevelSeries: Option[FirstLevelSeries] =
    if stress.contains(StressKind.Autoregressive) then Some(FirstLevelSeries.Declared) else None

final case class FirstLevelSeries(length: Int, rho: Double, regressor: Vector[Double]):
  def nominalDf: Int = length - 2

object FirstLevelSeries:
  val Declared: FirstLevelSeries = FirstLevelSeries(10, 0.4, Vector(0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0))

object Cell:
  def of(
      family: Family, stress: Option[StressKind], n: Int, design: DesignKind,
      variances: VariancePattern, tau2: Tau2Level, trueNu: NuLevel
  ): Either[String, Cell] =
    if n < 8 || n % 4 != 0 then Left(s"n must be a multiple of 4 and at least 8, got $n")
    else if variances == VariancePattern.Reversed && design != DesignKind.QuarterGroupSmooth then Left("rev needs design G")
    else if (variances == VariancePattern.ScaledInverseChiSquare) != (family == Family.Hierarchy) then
      Left("sichi variances belong to family H only")
    else if (variances == VariancePattern.Dominant) != stress.contains(StressKind.DominantPrecision) then
      Left("dom variances belong to the dominant-precision stress only")
    else if (family == Family.Stress) != stress.isDefined then Left("only family S carries a stress kind")
    else
      trueNu match
        case NuLevel.Finite(nu) if nu < 1 => Left(s"nu must be positive, got $nu")
        case _ => Right(Cell(family, stress, n, design, variances, tau2, trueNu))

  private def unsafe(family: Family, stress: Option[StressKind], n: Int, design: DesignKind, variances: VariancePattern, tau2: Tau2Level, nu: NuLevel): Cell =
    of(family, stress, n, design, variances, tau2, nu).fold(e => throw new IllegalArgumentException(e), identity)

  import DesignKind.*
  import VariancePattern.*
  import NuLevel.*

  /** Core Gaussian: n{8,20,80} x {I: flat, spread; G: flat, spread, rev} x tau2{0,.2} x nu{inf,40,8} = 90. */
  val core: Vector[Cell] =
    for
      n <- Vector(8, 20, 80)
      (d, v) <- Vector(Intercept -> Flat, Intercept -> Spread, QuarterGroupSmooth -> Flat, QuarterGroupSmooth -> Spread, QuarterGroupSmooth -> Reversed)
      t <- Vector(Tau2Level.Zero, Tau2Level.Point2)
      nu <- Vector(Infinite, Finite(40), Finite(8))
    yield unsafe(Family.Core, None, n, d, v, t, nu)

  /** Stress S (24). Unspecified dimensions are harness choices recorded in the manifest. */
  val stress: Vector[Cell] =
    import StressKind.*
    val s = Family.Stress
    Vector(Tau2Level.Zero, Tau2Level.Point2).map(t => unsafe(s, Some(Leverage), 20, QuarterGroupSmooth, Flat, t, Finite(8))) ++
      Vector(20, 80).map(n => unsafe(s, Some(DominantPrecision), n, Intercept, Dominant, Tau2Level.Zero, Finite(8))) ++
      (for n <- Vector(20, 80); nu <- Vector(3, 4, 5) yield unsafe(s, Some(LowDf), n, QuarterGroupSmooth, Reversed, Tau2Level.Zero, Finite(nu))) ++
      (for k <- Vector(DeclaredDf4, DeclaredDf16); n <- Vector(20, 80) yield unsafe(s, Some(k), n, QuarterGroupSmooth, Reversed, Tau2Level.Zero, Finite(8))) ++
      (for k <- Vector(StudentT3, Lognormal); n <- Vector(20, 80); t <- Vector(Tau2Level.Zero, Tau2Level.Point2)
      yield unsafe(s, Some(k), n, Intercept, Flat, t, Finite(8))) ++
      Vector(20, 80).map(n => unsafe(s, Some(Autoregressive), n, Intercept, Flat, Tau2Level.Zero, Finite(8)))

  /** Family H (3): n{8,20,80}, I, tau2 = .2, nu = 8, sigma^2 drawn per study. */
  val hierarchy: Vector[Cell] =
    Vector(8, 20, 80).map(n => unsafe(Family.Hierarchy, None, n, Intercept, ScaledInverseChiSquare, Tau2Level.Point2, Finite(8)))

  val all: Vector[Cell] = core ++ stress ++ hierarchy

  def byId(id: CellId): Option[Cell] = all.find(_.id == id)

/** Canonical numeric formatting shared by the manifest and the generator. */
object Canonical:
  private val Context = new java.math.MathContext(15, java.math.RoundingMode.HALF_EVEN)

  /** Round to 15 significant digits; the serialized and the simulated value are then the same double. */
  def round15(d: Double): Double =
    if d == 0.0 || !d.isFinite then d else new java.math.BigDecimal(d).round(Context).doubleValue

  def number(d: Double): String =
    require(d.isFinite, s"canonical numbers must be finite, got $d")
    if d == 0.0 then "0"
    else
      val bd = new java.math.BigDecimal(d).round(Context).stripTrailingZeros
      if bd.scale <= 0 && bd.precision - bd.scale <= 15 then bd.toBigInteger.toString else bd.toString
