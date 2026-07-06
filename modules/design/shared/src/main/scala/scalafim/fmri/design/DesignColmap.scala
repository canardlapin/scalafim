package scalafim.fmri.design

import scalafim.fmri.design.baseline.BaselineModel
import scalafim.fmri.design.basis.{BasisEntry, BasisModulation, BasisRegistry, ParametricBasis}
import scalafim.fmri.design.event.{
  ContinuousEvent,
  ConvolvedTerm,
  CovariateConvolvedTerm,
  EventModel,
  EventModelTerm,
  EventTermColumnRole
}
import scalafim.fmri.design.hrf.HrfBasisRegistry

enum ModelSource:
  case Event, Baseline

enum ColumnRole:
  case Task, Trial, TrialAggregate, Covariate, Drift, Intercept, Nuisance, Baseline

enum ModulationType:
  case Amplitude, Parametric, Covariate

final case class DesignColumnMeta(
    col: Int,
    name: String,
    termTag: Option[String],
    termIndex: Option[Int],
    condition: Option[String],
    run: Option[Int],
    role: ColumnRole,
    modelSource: ModelSource,
    basisName: Option[String],
    basisIx: Option[Int],
    basisTotal: Option[Int],
    basisLabel: Option[String],
    prettyName: String,
    isBlockDiagonal: Boolean,
    modulationType: Option[ModulationType],
    modulationId: Option[String]
)

object DesignColmap:

  trait HasDesignColmap[A]:
    def designColmap(a: A): Vector[DesignColumnMeta]

  object HasDesignColmap:
    given HasDesignColmap[EventModel] with
      def designColmap(a: EventModel): Vector[DesignColumnMeta] = forEventModel(a)

    given HasDesignColmap[BaselineModel] with
      def designColmap(a: BaselineModel): Vector[DesignColumnMeta] = forBaselineModel(a)

  extension [A](a: A)(using ev: HasDesignColmap[A])
    def designColmap: Vector[DesignColumnMeta] =
      ev.designColmap(a)

  def forEventModel(
      x: EventModel,
      basisRegistry: BasisRegistry = BasisRegistry.default,
      hrfRegistry: HrfBasisRegistry = HrfBasisRegistry.default
  ): Vector[DesignColumnMeta] =
    val nCols = x.designMatrix.cols
    if nCols == 0 then return Vector.empty

    val termKeys = x.termKeys
    val termList = x.terms.map(_._2)
    require(termList.length == x.termSpans.length, "internal: termSpans length mismatch")

    // Column -> term index/tag (1-based termIndex for parity with R)
    val termIndexByCol = Array.fill(nCols)(-1)
    val termTagByCol = new Array[String](nCols)
    var ti = 0
    while ti < x.termSpans.length do
      val (start, endExcl) = x.termSpans(ti)
      var c = start
      while c < endExcl do
        termIndexByCol(c) = ti + 1
        termTagByCol(c) = termKeys(ti)
        c += 1
      ti += 1

    // Per-term metadata
    val perTermBasisName = new Array[Option[String]](termList.length)
    val perTermBasisTotal = new Array[Option[Int]](termList.length)
    val perTermModType = new Array[ModulationType](termList.length)
    val perTermModId = new Array[Option[String]](termList.length)

    ti = 0
    while ti < termList.length do
      val term = termList(ti)

      val basisMetadata = modulationMetadata(term, basisRegistry)
      if basisMetadata.nonEmpty then
        val (_, entry) = basisMetadata.head
        perTermModType(ti) =
          entry.modulation match
            case BasisModulation.Parametric => ModulationType.Parametric
            case BasisModulation.Amplitude  => ModulationType.Amplitude
        perTermModId(ti) =
          val ids = basisMetadata.map(_._1.argName).filter(_.nonEmpty).distinct
          if ids.isEmpty then None else Some(ids.mkString("_"))
      else if term.isCovariate then
        perTermModType(ti) = ModulationType.Covariate
        val mid =
          term match
            case cv: CovariateConvolvedTerm => Some(cv.name)
            case _                          => None
        perTermModId(ti) = mid
      else
        perTermModType(ti) = ModulationType.Amplitude
        perTermModId(ti) = None

      term match
        case ct: ConvolvedTerm =>
          perTermBasisName(ti) = Some(ct.hrf.name)
          perTermBasisTotal(ti) = Some(ct.hrf.nbasis)
        case _ =>
          perTermBasisName(ti) = None
          perTermBasisTotal(ti) = None

      ti += 1

    val out = Vector.newBuilder[DesignColumnMeta]
    var col0 = 0
    while col0 < nCols do
      val name = x.columnNames(col0)
      val (baseNoBasis, basisIx) = stripBasisSuffix(name)

      val termIndex0 = termIndexByCol(col0)
      val termIndexOpt = if termIndex0 >= 1 then Some(termIndex0) else None
      val termTagOpt = if termIndex0 >= 1 then Some(termTagByCol(col0)) else None

      val condition0 =
        termTagOpt match
          case Some(tag) if baseNoBasis.startsWith(tag + "_") => Some(baseNoBasis.substring(tag.length + 1))
          case _                                              => Some(baseNoBasis)

      val basisName = termIndexOpt.flatMap(i => perTermBasisName(i - 1))
      val basisTotal = termIndexOpt.flatMap(i => perTermBasisTotal(i - 1))
      val basisLabel = for
        bix <- basisIx
        bn <- basisName
      yield hrfRegistry.labelForName(bn, bix)

      val modulationType = termIndexOpt.map(i => perTermModType(i - 1))
      val modulationId = termIndexOpt.flatMap(i => perTermModId(i - 1))

      val prettyName =
        modulationType match
          case Some(ModulationType.Parametric) =>
            (basisIx, modulationId) match
              case (None, Some(mid)) => mid
              case (Some(_), Some(mid)) =>
                val suf =
                  basisLabel.getOrElse {
                    val bixStr = basisIx.map(i => f"_b$i%02d").getOrElse("")
                    bixStr.stripPrefix("_")
                  }
                s"${mid}_$suf"
              case _ => name
          case _ => name

      out += DesignColumnMeta(
        col = col0 + 1,
        name = name,
        termTag = termTagOpt,
        termIndex = termIndexOpt,
        condition = condition0,
        run = None,
        role = termIndexOpt match
          case Some(i) =>
            val localCol = col0 - x.termSpans(i - 1)._1
            eventColumnRole(termList(i - 1), localCol)
          case None => ColumnRole.Task,
        modelSource = ModelSource.Event,
        basisName = basisName,
        basisIx = basisIx,
        basisTotal = basisTotal,
        basisLabel = basisLabel,
        prettyName = prettyName,
        isBlockDiagonal = false,
        modulationType = modulationType,
        modulationId = modulationId
      )
      col0 += 1

    out.result()

  def forBaselineModel(x: BaselineModel): Vector[DesignColumnMeta] =
    val nCols = x.designMatrix.cols
    if nCols == 0 then return Vector.empty

    val termKeys = x.termKeys
    val termList = x.terms.map(_._2)
    require(termList.length == x.termSpans.length, "internal: termSpans length mismatch")

    val termIndexByCol = Array.fill(nCols)(-1)
    val termTagByCol = new Array[String](nCols)
    var ti = 0
    while ti < x.termSpans.length do
      val (start, endExcl) = x.termSpans(ti)
      var c = start
      while c < endExcl do
        termIndexByCol(c) = ti + 1
        termTagByCol(c) = termKeys(ti)
        c += 1
      ti += 1

    val out = Vector.newBuilder[DesignColumnMeta]
    var col0 = 0
    while col0 < nCols do
      val name = x.columnNames(col0)
      val termIndex0 = termIndexByCol(col0)
      val termIndexOpt = if termIndex0 >= 1 then Some(termIndex0) else None
      val termTagOpt = if termIndex0 >= 1 then Some(termTagByCol(col0)) else None

      val key = termTagOpt.getOrElse("baseline")
      val role =
        key match
          case "drift"    => ColumnRole.Drift
          case "block"    => ColumnRole.Intercept
          case "nuisance" => ColumnRole.Nuisance
          case _          => ColumnRole.Baseline

      val (basisName, run, basisIx, basisLabel, basisTotal, isBlockDiag) =
        role match
          case ColumnRole.Drift =>
            val (r, k) = parseBlockAndComponent(name, blockMarker = "_block_")
            val total =
              if k.nonEmpty then maxComponentIndex(x.terms(termIndex0 - 1)._2.columnNames, blockMarker = "_block_")
              else None
            (
              Some(x.driftSpec.basis.toString.toLowerCase),
              r,
              k,
              k.map(i => f"component_$i%02d"),
              total,
              r.nonEmpty
            )
          case ColumnRole.Intercept =>
            if name.endsWith("_global") then
              (Some("constant"), None, Some(1), Some("intercept"), Some(1), false)
            else
              val r = parseTrailingInt(name, sep = "_")
              val total = Some(x.samplingFrame.nBlocks)
              (Some("constant"), r, None, Some("intercept"), total, true)
          case ColumnRole.Nuisance =>
            val r = parseAfterHash(name)
            val k = parseTrailingInt(name, sep = "_")
            (Some("nuisance"), r, k, k.map(i => f"component_$i%02d"), None, true)
          case ColumnRole.Baseline =>
            (Some("baseline"), None, None, None, None, false)
          case ColumnRole.Task | ColumnRole.Trial | ColumnRole.TrialAggregate | ColumnRole.Covariate =>
            (Some("baseline"), None, None, None, None, false)

      out += DesignColumnMeta(
        col = col0 + 1,
        name = name,
        termTag = termTagOpt,
        termIndex = termIndexOpt,
        condition = None,
        run = run,
        role = role,
        modelSource = ModelSource.Baseline,
        basisName = basisName,
        basisIx = basisIx,
        basisTotal = basisTotal,
        basisLabel = basisLabel,
        prettyName = name,
        isBlockDiagonal = isBlockDiag,
        modulationType = None,
        modulationId = None
      )
      col0 += 1

    out.result()

  private def stripBasisSuffix(name: String): (String, Option[Int]) =
    val idx = name.lastIndexOf("_b")
    if idx < 0 || idx + 2 >= name.length then (name, None)
    else
      val digits = name.substring(idx + 2)
      if digits.nonEmpty && digits.forall(_.isDigit) then (name.substring(0, idx), Some(digits.toInt))
      else (name, None)

  private def modulationMetadata(
      term: EventModelTerm,
      basisRegistry: BasisRegistry
  ): Vector[(ParametricBasis, BasisEntry)] =
    term match
      case ct: ConvolvedTerm =>
        ct.term.events.collect {
          case e: ContinuousEvent if e.basis.nonEmpty =>
            e.basis.flatMap(b => basisRegistry.getBasisEntry(b.registryKeys).map(entry => b -> entry))
        }.flatten
      case _ => Vector.empty

  private def eventColumnRole(term: EventModelTerm, localCol: Int): ColumnRole =
    term.resolvedColumnRoles(localCol) match
      case EventTermColumnRole.Task           => ColumnRole.Task
      case EventTermColumnRole.Trial          => ColumnRole.Trial
      case EventTermColumnRole.TrialAggregate => ColumnRole.TrialAggregate
      case EventTermColumnRole.Covariate      => ColumnRole.Covariate

  private def parseTrailingInt(name: String, sep: String): Option[Int] =
    val idx = name.lastIndexOf(sep)
    if idx < 0 || idx + sep.length >= name.length then None
    else
      val digits = name.substring(idx + sep.length)
      if digits.nonEmpty && digits.forall(_.isDigit) then Some(digits.toInt) else None

  private def parseAfterHash(name: String): Option[Int] =
    val idx = name.lastIndexOf('#')
    if idx < 0 || idx + 1 >= name.length then None
    else
      val rest = name.substring(idx + 1)
      val end = rest.indexOf('_')
      val digits = if end < 0 then rest else rest.substring(0, end)
      if digits.nonEmpty && digits.forall(_.isDigit) then Some(digits.toInt) else None

  private def parseBlockAndComponent(name: String, blockMarker: String): (Option[Int], Option[Int]) =
    val blockIx = name.lastIndexOf(blockMarker)
    if blockIx < 0 then (None, None)
    else
      val runDigits = name.substring(blockIx + blockMarker.length)
      val run = if runDigits.nonEmpty && runDigits.forall(_.isDigit) then Some(runDigits.toInt) else None

      val before = name.substring(0, blockIx)
      val kDigits = before.reverseIterator.takeWhile(_.isDigit).toVector.reverse.mkString
      val k = if kDigits.nonEmpty then Some(kDigits.toInt) else None
      (run, k)

  private def maxComponentIndex(names: Vector[String], blockMarker: String): Option[Int] =
    val ks = names.iterator.flatMap { n =>
      val (_, k) = parseBlockAndComponent(n, blockMarker)
      k
    }.toVector
    ks.maxOption
