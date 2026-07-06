package scalafim.fmri.design.hrf

import scalafim.fmri.hrf.Hrf

import scala.collection.immutable.VectorMap

final case class HrfBasisEntry(
    name: String,
    aliases: Vector[String],
    labels: Vector[String] = Vector.empty,
    fallbackPrefix: String = "component",
    zeroBasedFallback: Boolean = false,
    description: Option[String] = None
):
  require(name.trim.nonEmpty, "name must be non-empty")
  require(aliases.nonEmpty && aliases.forall(_.trim.nonEmpty), "aliases must be non-empty")
  require(fallbackPrefix.trim.nonEmpty, "fallbackPrefix must be non-empty")

  def matches(hrfName: String): Boolean =
    val n = normalize(hrfName)
    aliases.exists(alias => n.contains(normalize(alias)))

  def label(basisIx: Int): String =
    require(basisIx >= 1, "basisIx is 1-based and must be >= 1")
    if basisIx <= labels.length then labels(basisIx - 1)
    else
      val k = if zeroBasedFallback then basisIx - 1 else basisIx
      f"${fallbackPrefix}_$k%02d"

  private def normalize(s: String): String =
    s.trim.toLowerCase

/** Immutable HRF metadata registry.
  *
  * This covers Scala-native introspection only. The R package also lets
  * packages register arbitrary S3 hrfspec classes and formula functions in a
  * mutable environment; the Scala port represents that information as data in
  * [[ExternalHrfSpecRegistry]] and does not dynamically evaluate unknown
  * formula functions.
  */
final case class HrfBasisRegistry(entries: Vector[HrfBasisEntry]):

  def register(entry: HrfBasisEntry): HrfBasisRegistry =
    copy(entries = entries.filterNot(_.name == entry.name) :+ entry)

  def listRegisteredHrfBases(): Vector[String] =
    entries.map(_.name).sorted

  def getBasisEntry(hrfName: String): Option[HrfBasisEntry] =
    entries.find(_.matches(hrfName))

  def labelFor(hrf: Hrf, basisIx: Int): String =
    labelForName(hrf.name, basisIx)

  def labelForName(hrfName: String, basisIx: Int): String =
    getBasisEntry(hrfName).fold(f"component_$basisIx%02d")(_.label(basisIx))

object HrfBasisRegistry:

  val default: HrfBasisRegistry =
    HrfBasisRegistry(
      Vector(
        HrfBasisEntry(
          name = "SPMG3",
          aliases = Vector("SPMG3"),
          labels = Vector("canonical", "derivative", "dispersion"),
          description = Some("SPMG canonical HRF with temporal and dispersion derivatives")
        ),
        HrfBasisEntry(
          name = "SPMG2",
          aliases = Vector("SPMG2"),
          labels = Vector("canonical", "derivative"),
          description = Some("SPMG canonical HRF with temporal derivative")
        ),
        HrfBasisEntry(
          name = "SPMG1",
          aliases = Vector("SPMG1"),
          labels = Vector("canonical"),
          description = Some("SPMG canonical HRF")
        ),
        HrfBasisEntry(
          name = "fir",
          aliases = Vector("fir"),
          fallbackPrefix = "lag",
          zeroBasedFallback = true,
          description = Some("Finite impulse response basis")
        ),
        HrfBasisEntry(
          name = "bspline",
          aliases = Vector("bspline"),
          description = Some("B-spline HRF basis")
        ),
        HrfBasisEntry(
          name = "tent",
          aliases = Vector("tent"),
          description = Some("Tent HRF basis")
        ),
        HrfBasisEntry(
          name = "fourier",
          aliases = Vector("fourier"),
          description = Some("Fourier HRF basis")
        )
      )
    )

final case class ExternalHrfSpecEntry(
    specClass: String,
    packageName: String,
    convolvedClass: Option[String] = None,
    requiresExternalProcessing: Boolean = false,
    formulaFunctions: Vector[String] = Vector.empty
):
  require(specClass.trim.nonEmpty, "specClass must be non-empty")
  require(packageName.trim.nonEmpty, "packageName must be non-empty")
  formulaFunctions.foreach(f => require(f.trim.nonEmpty, "formulaFunctions must be non-empty"))

/** Immutable equivalent of R's external hrfspec registry.
  *
  * It records which external spec classes exist and which formula function
  * names they claim. It does not provide dynamic S3 dispatch or evaluation of
  * those functions; Scala callers supply actual constructors/functions in
  * ordinary typed parameters such as `hrfFuns`.
  */
final case class ExternalHrfSpecRegistry(entries: VectorMap[String, ExternalHrfSpecEntry]):

  def register(entry: ExternalHrfSpecEntry): ExternalHrfSpecRegistry =
    copy(entries = entries.updated(entry.specClass, entry))

  def registerHrfSpecExtension(
      specClass: String,
      packageName: String,
      convolvedClass: Option[String] = None,
      requiresExternalProcessing: Boolean = false,
      formulaFunctions: Seq[String] = Seq.empty
  ): ExternalHrfSpecRegistry =
    register(
      ExternalHrfSpecEntry(
        specClass = specClass,
        packageName = packageName,
        convolvedClass = convolvedClass,
        requiresExternalProcessing = requiresExternalProcessing,
        formulaFunctions = formulaFunctions.toVector
      )
    )

  def listExternalHrfSpecs(): Vector[String] =
    entries.keys.toVector.sorted

  def isExternalHrfSpec(className: String): Boolean =
    entries.contains(className)

  def isExternalHrfSpec(classNames: Seq[String]): Boolean =
    classNames.exists(entries.contains)

  def getExternalHrfSpecInfo(specClass: String): Option[ExternalHrfSpecEntry] =
    entries.get(specClass)

  def requiresExternalProcessing(className: String): Boolean =
    entries.get(className).exists(_.requiresExternalProcessing)

  def requiresExternalProcessing(classNames: Seq[String]): Boolean =
    classNames.iterator.flatMap(entries.get).nextOption().exists(_.requiresExternalProcessing)

  def getExternalHrfSpecFunctions(specClass: String): Vector[String] =
    entries.get(specClass) match
      case Some(entry) if entry.formulaFunctions.nonEmpty => entry.formulaFunctions
      case Some(_)                                        => legacyFunctions(specClass)
      case None                                           => Vector.empty

  def getAllExternalHrfFunctions(): Vector[String] =
    listExternalHrfSpecs().flatMap(getExternalHrfSpecFunctions).distinct

  private def legacyFunctions(specClass: String): Vector[String] =
    specClass match
      case "afni_hrfspec"           => Vector("afni_hrf")
      case "afni_trialwise_hrfspec" => Vector("afni_trialwise")
      case _                        => Vector.empty

object ExternalHrfSpecRegistry:
  val empty: ExternalHrfSpecRegistry = ExternalHrfSpecRegistry(VectorMap.empty)
