package scalafim.fmri.design.basis

import scala.collection.immutable.VectorMap

enum BasisModulation:
  case Parametric, Amplitude

final case class BasisEntry(
    className: String,
    prefix: Option[String],
    modulation: BasisModulation = BasisModulation.Parametric,
    description: Option[String] = None,
    formulaNames: Vector[String] = Vector.empty
):
  require(className.trim.nonEmpty, "className must be non-empty")
  prefix.foreach(p => require(p.trim.nonEmpty, "prefix must be non-empty when supplied"))
  formulaNames.foreach(n => require(n.trim.nonEmpty, "formulaNames must be non-empty"))

  val normalizedFormulaNames: Vector[String] =
    if formulaNames.isEmpty then Vector(className)
    else formulaNames

/** Immutable metadata registry for parametric bases.
  *
  * The R package uses mutable package environments and S3 class vectors for
  * extension discovery. The Scala port intentionally keeps the same useful
  * information, but exposes it as immutable values that callers compose and
  * pass explicitly to formula builders or metadata functions when they need
  * non-default entries.
  */
final case class BasisRegistry(entries: VectorMap[String, BasisEntry]):

  def register(entry: BasisEntry): BasisRegistry =
    copy(entries = entries.updated(entry.className, entry))

  def registerBasis(
      className: String,
      prefix: Option[String] = None,
      modulation: BasisModulation = BasisModulation.Parametric,
      description: Option[String] = None,
      formulaNames: Seq[String] = Seq.empty
  ): BasisRegistry =
    register(
      BasisEntry(
        className = className,
        prefix = prefix,
        modulation = modulation,
        description = description,
        formulaNames = formulaNames.toVector
      )
    )

  def listRegisteredBases(): Vector[String] =
    entries.keys.toVector.sorted

  def getBasisEntry(className: String): Option[BasisEntry] =
    entries.get(className)

  def getBasisEntry(classNames: Seq[String]): Option[BasisEntry] =
    classNames.iterator.flatMap(entries.get).nextOption()

  def getFormulaEntry(formulaName: String): Option[BasisEntry] =
    val needle = normalize(formulaName)
    entries.valuesIterator.find { entry =>
      entry.normalizedFormulaNames.exists(n => normalize(n) == needle)
    }

  def parametricPrefixes: Vector[String] =
    entries.valuesIterator
      .collect {
        case BasisEntry(_, Some(prefix), BasisModulation.Parametric, _, _) => s"${prefix}_"
      }
      .toVector
      .distinct

  private def normalize(s: String): String =
    s.trim.toLowerCase

object BasisRegistry:

  val default: BasisRegistry =
    BasisRegistry(VectorMap.empty)
      .registerBasis(
        "Poly",
        prefix = Some("poly"),
        description = Some("Orthogonal polynomial expansion"),
        formulaNames = Seq("poly")
      )
      .registerBasis(
        "BSpline",
        prefix = Some("bs"),
        description = Some("B-spline basis expansion"),
        formulaNames = Seq("bspline")
      )
      .registerBasis(
        "Scale",
        prefix = Some("z"),
        description = Some("Z-score scaling"),
        formulaNames = Seq("scale")
      )
      .registerBasis(
        "ScaleWithin",
        prefix = Some("z"),
        description = Some("Within-group z-score scaling"),
        formulaNames = Seq("scalewithin")
      )
      .registerBasis(
        "Standardized",
        prefix = Some("std"),
        description = Some("Standardized mean/sd scaling"),
        formulaNames = Seq("standardized")
      )
      .registerBasis(
        "RobustScale",
        prefix = Some("robz"),
        description = Some("Robust median/MAD scaling"),
        formulaNames = Seq("robustscale")
      )
      .registerBasis(
        "Ident",
        prefix = None,
        modulation = BasisModulation.Amplitude,
        description = Some("Identity basis: variables become column names"),
        formulaNames = Seq("ident")
      )
