package scalafim.fmri.design.baseline

import scalafim.fmri.design.*
import scalafim.fmri.design.basis.ParametricBasis
import scalafim.fmri.design.linalg.QrDecomposition
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

import scala.util.control.NonFatal

enum BaselineBasis:
  case Constant, Poly, Bs, Ns
  case Dct(cutoff: DctCutoffPeriod)

  /** Stable basis identity, including the physical cutoff when it defines the basis. */
  def id: String = this match
    case Constant => "constant"
    case Poly => "poly"
    case Bs => "bs"
    case Ns => "ns"
    case Dct(cutoff) => s"dct_ii_period_bits_${java.lang.Double.doubleToLongBits(cutoff.seconds)}"

  private[baseline] def label: String = this match
    case Dct(_) => "dct_ii"
    case _ => id

object BaselineBasis:
  def parse(s: String): BaselineBasis =
    s.trim.toLowerCase match
      case "constant" => Constant
      case "poly"     => Poly
      case "bs"       => Bs
      case "ns"       => Ns
      case "dct"      => throw new IllegalArgumentException("DCT requires an explicit cutoff: BaselineBasis.Dct(DctCutoffPeriod.unsafeSeconds(seconds))")
      case other      => throw new IllegalArgumentException(s"Unknown baseline basis: '$other'")

enum Intercept:
  case Runwise, Global, None

object Intercept:
  def parse(s: String): Intercept =
    s.trim.toLowerCase match
      case "runwise" => Runwise
      case "global"  => Global
      case "none"    => None
      case other     => throw new IllegalArgumentException(s"Unknown intercept: '$other'")

enum NuisanceCheck:
  case Warn, Error, Drop, None

object NuisanceCheck:
  def parse(s: String): NuisanceCheck =
    s.trim.toLowerCase match
      case "warn"  => Warn
      case "error" => Error
      case "drop"  => Drop
      case "none"  => None
      case other   => throw new IllegalArgumentException(s"Unknown nuisance check: '$other'")

enum NaAction:
  case Drop, Zero, Median

object NaAction:
  def parse(s: String): NaAction =
    s.trim.toLowerCase match
      case "drop"   => Drop
      case "zero"   => Zero
      case "median" => Median
      case other    => throw new IllegalArgumentException(s"Unknown NA action: '$other'")

enum NuisanceIssue(val label: String):
  case NonFinite extends NuisanceIssue("non_finite")
  case ZeroVariance extends NuisanceIssue("zero_variance")
  case Duplicate extends NuisanceIssue("duplicate")
  case RankDeficientNuisance extends NuisanceIssue("rank_deficient_nuisance")
  case RankDeficientWithBaseline extends NuisanceIssue("rank_deficient_with_baseline")
  case AliasedColumns extends NuisanceIssue("aliased_columns")

final case class NuisanceDuplicate(column: String, duplicates: String, correlation: Double)

final case class NuisanceProblem(
    block: Int,
    issue: NuisanceIssue,
    columns: Vector[String],
    detail: String
):
  def issueLabel: String = issue.label

final case class NuisanceBlockReport(
    block: Int,
    baselineRank: Int,
    baselineColumns: Int,
    nuisanceRank: Int,
    nuisanceColumns: Int,
    rankWithBaseline: Int,
    columnsWithBaseline: Int,
    nonFinite: Vector[String],
    zeroVariance: Vector[String],
    duplicatePairs: Vector[NuisanceDuplicate],
    aliasedColumns: Vector[String],
    keep: Vector[Boolean],
    retainedColumns: Vector[String],
    droppedColumns: Vector[String]
)

final case class NuisanceReport(
    problems: Vector[NuisanceProblem],
    byBlock: Vector[NuisanceBlockReport],
    nuisanceList: Vector[Mat],
    columnNames: Vector[Vector[String]]
):
  def ok: Boolean = problems.isEmpty
  def retainedByBlock: Vector[Vector[String]] = byBlock.map(_.retainedColumns)
  def droppedByBlock: Vector[Vector[String]] = byBlock.map(_.droppedColumns)

  def format(dropped: Boolean = false): String =
    if ok then "baseline_model(): nuisance_list passed validation."
    else
      val lines = Vector.newBuilder[String]
      lines += "baseline_model(): nuisance_list has column/rank problems."

      byBlock.foreach { block =>
        val blockLines = Vector.newBuilder[String]
        if block.nonFinite.nonEmpty then
          blockLines += s"  Non-finite columns: ${block.nonFinite.mkString(", ")}."
        if block.zeroVariance.nonEmpty then
          blockLines += s"  Zero-variance columns: ${block.zeroVariance.mkString(", ")}."
        if block.duplicatePairs.nonEmpty then
          val dup = block.duplicatePairs
            .map(d => f"${d.column} duplicates ${d.duplicates} (r = ${d.correlation}%.6g)")
            .mkString("; ")
          blockLines += s"  Duplicate or near-duplicate columns: $dup."
        if block.aliasedColumns.nonEmpty then
          blockLines += s"  Aliased columns: ${block.aliasedColumns.mkString(", ")}."
        if block.rankWithBaseline < block.columnsWithBaseline then
          blockLines += s"  Rank with baseline terms: ${block.rankWithBaseline} < ${block.columnsWithBaseline} columns."

        val chunk = blockLines.result()
        if chunk.nonEmpty then
          lines += s"nuisance_list[[${block.block}]]:"
          lines ++= chunk
      }

      if dropped then
        lines += "Dropped non-finite, zero-variance, and rank-aliased nuisance columns."
      else
        lines += "Use nuisanceCheck = NuisanceCheck.Error to stop or NuisanceCheck.Drop to remove columns that do not increase rank."

      lines.result().mkString("\n")

final case class CleanedNuisance(nuisanceList: Vector[Mat], report: NuisanceReport)

enum BaselineError:
  case InvalidInput(detail: String)
  case MissingNuisance
  case NuisanceProblems(report: NuisanceReport)

  def message: String =
    this match
      case InvalidInput(detail) =>
        detail
      case MissingNuisance =>
        "BaselinePlan.nuisance is required for nuisance validation"
      case NuisanceProblems(report) =>
        report.format()

object BaselineError:
  def fromThrowable(throwable: Throwable): BaselineError =
    val detail = Option(throwable.getMessage).filter(_.nonEmpty).getOrElse(throwable.toString)
    BaselineError.InvalidInput(detail)

final case class NuisancePolicy(
    check: NuisanceCheck = NuisanceCheck.Warn,
    naAction: NaAction = NaAction.Drop,
    tol: Double = BaselineModel.DefaultNuisanceTol,
    duplicateThreshold: Double = 1.0 - BaselineModel.DefaultNuisanceTol
):
  require(tol.isFinite && tol >= 0.0, "nuisance tolerance must be non-negative and finite")
  require(
    duplicateThreshold.isFinite && duplicateThreshold >= 0.0 && duplicateThreshold <= 1.0,
    "nuisance duplicate threshold must be in [0, 1]"
  )

final case class NuisanceInput(
    matrices: Vector[Mat],
    names: Option[Vector[Vector[String]]] = _root_.scala.None,
    policy: NuisancePolicy = NuisancePolicy()
):
  require(matrices.nonEmpty, "nuisance matrices must be non-empty")
  names.foreach { byBlock =>
    require(byBlock.length == matrices.length, "nuisance names length must match nuisance matrices length")
    byBlock.zip(matrices).zipWithIndex.foreach { case ((blockNames, mat), b) =>
      require(blockNames.length == mat.cols, s"nuisance names length must match matrix columns for block ${b + 1}")
    }
  }

object NuisanceInput:
  def fromSeq(
      matrices: Seq[Mat],
      names: Option[Seq[Seq[String]]] = _root_.scala.None,
      policy: NuisancePolicy = NuisancePolicy()
  ): NuisanceInput =
    NuisanceInput(
      matrices = matrices.toVector,
      names = names.map(_.map(_.toVector).toVector),
      policy = policy
    )

final case class BaselinePlan(
    samplingFrame: SamplingFrame,
    basis: BaselineBasis = BaselineBasis.Constant,
    degree: Int = 1,
    intercept: Intercept = Intercept.Runwise,
    nuisance: Option[NuisanceInput] = _root_.scala.None
):
  require(degree >= 1, "baseline degree must be at least 1")

final case class BaselineSpec(
    degree: Int,
    basis: BaselineBasis,
    intercept: Intercept,
    name: String
):
  def construct(samplingFrame: SamplingFrame): BaselineTerm =
    BaselineTerm.fromSpec(this, samplingFrame)

object BaselineSpec:
  def apply(
      degree: Int = 1,
      basis: BaselineBasis = BaselineBasis.Constant,
      name: Option[String] = None,
      intercept: Intercept = Intercept.Runwise
  ): BaselineSpec =
    val degree0 =
      if basis == BaselineBasis.Constant then 1 else degree
    val name0 = name.getOrElse(basis match
      case BaselineBasis.Dct(_) => s"baseline_${basis.label}"
      case _ => s"baseline_${basis.id}_${degree0}"
    )
    BaselineSpec(degree0, basis, intercept, name0)

final case class BaselineTerm(
    varName: String,
    data: Mat,
    columnNames: Vector[String],
    colInd: Vector[Vector[Int]],
    rowInd: Vector[Vector[Int]]
):
  def designMatrix(blockId: Option[Int] = None, allRows: Boolean = false): Mat =
    blockId match
      case None => data
      case Some(b) =>
        require(b >= 0 && b < rowInd.length, "blockId out of range")
        val cols = colInd(b)
        if cols.isEmpty then
          val outRows = if allRows then data.rows else rowInd(b).length
          Mat.zeros(outRows, 0)
        else if allRows then BaselineTerm.subset(data, rows = 0 until data.rows, cols = cols)
        else BaselineTerm.subset(data, rows = rowInd(b), cols = cols)

object BaselineTerm:

  def fromSpec(spec: BaselineSpec, samplingFrame: SamplingFrame): BaselineTerm =
    val bl = samplingFrame.blockLens
    require(bl.nonEmpty && bl.forall(_ > 0), "samplingFrame.blockLens must be non-empty and positive")
    val nb = bl.length
    val totalRows = bl.sum

    val rowInd = blockRowIndices(bl)

    if spec.basis == BaselineBasis.Constant && spec.intercept == Intercept.Global then
      val out = Array.fill(totalRows)(1.0)
      BaselineTerm(
        varName = spec.name,
        data = Mat.unsafe(totalRows, 1, out),
        columnNames = Vector(s"base_${spec.basis.label}"),
        colInd = Vector.fill(nb)(Vector(0)),
        rowInd = rowInd
      )
    else
      val perBlock = bl.zipWithIndex.map { case (blockLen, block) =>
        lazy val x = (1 to blockLen).iterator.map(_.toDouble).toVector
        spec.basis match
          case BaselineBasis.Constant =>
            Mat.unsafe(blockLen, 1, Array.fill(blockLen)(1.0))
          case BaselineBasis.Poly =>
            ParametricBasis.Poly.fit(x, degree = spec.degree, argName = "x").y
          case BaselineBasis.Bs =>
            ParametricBasis.BSpline.fit(x, degree = spec.degree, argName = "x").y
          case BaselineBasis.Ns =>
            ParametricBasis.NSpline.fit(x, df = spec.degree, argName = "x").y
          case BaselineBasis.Dct(cutoff) =>
            DctDrift.matrix(blockLen, samplingFrame.tr(block), cutoff)
      }

      val totalColsLong = perBlock.iterator.map(_.cols.toLong).sum
      require(totalColsLong <= Int.MaxValue && totalRows.toLong * totalColsLong <= Int.MaxValue,
        "baseline matrix exceeds storable elements")
      val totalCols = totalColsLong.toInt
      val out = new Array[Double](totalRows * totalCols)

      val colInd = Vector.newBuilder[Vector[Int]]
      val colNames = new Array[String](totalCols)

      var rowOffset = 0
      var colOffset = 0
      var b = 0
      while b < nb do
        val blockMat = perBlock(b)
        val blockLen = blockMat.rows
        val colsThis = blockMat.cols

        val colIdx = (colOffset until (colOffset + colsThis)).toVector
        colInd += colIdx

        var r = 0
        while r < blockLen do
          val dstBase = (rowOffset + r) * totalCols + colOffset
          System.arraycopy(blockMat.data, r * colsThis, out, dstBase, colsThis)
          r += 1

        var j = 0
        while j < colsThis do
          colNames(colOffset + j) = s"base_${spec.basis.label}${j + 1}_block_${b + 1}"
          j += 1

        rowOffset += blockLen
        colOffset += colsThis
        b += 1

      BaselineTerm(
        varName = spec.name,
        data = Mat.unsafe(totalRows, totalCols, out),
        columnNames = colNames.toVector,
        colInd = colInd.result(),
        rowInd = rowInd
      )

  def blockIntercept(vname: String, samplingFrame: SamplingFrame, intercept: Intercept): BaselineTerm =
    require(intercept != Intercept.None, "intercept must not be None")
    val bl = samplingFrame.blockLens
    require(bl.nonEmpty && bl.forall(_ > 0), "samplingFrame.blockLens must be non-empty and positive")
    val nb = bl.length
    val totalRows = bl.sum
    val rowInd = blockRowIndices(bl)

    if nb == 1 || intercept == Intercept.Global then
      val out = Array.fill(totalRows)(1.0)
      BaselineTerm(
        varName = vname,
        data = Mat.unsafe(totalRows, 1, out),
        columnNames = Vector(s"${vname}_global"),
        colInd = Vector.fill(nb)(Vector(0)),
        rowInd = rowInd
      )
    else
      val out = new Array[Double](totalRows * nb)
      var rowOffset = 0
      var b = 0
      while b < nb do
        val len = bl(b)
        var r = 0
        while r < len do
          out((rowOffset + r) * nb + b) = 1.0
          r += 1
        rowOffset += len
        b += 1

      BaselineTerm(
        varName = vname,
        data = Mat.unsafe(totalRows, nb, out),
        columnNames = (1 to nb).map(i => s"${vname}_$i").toVector,
        colInd = (0 until nb).map(b => Vector(b)).toVector,
        rowInd = rowInd
      )

  def nuisance(
      nuisanceList: Seq[Mat],
      samplingFrame: SamplingFrame,
      names: Option[Seq[Seq[String]]] = None,
      prefix: String = "nuis"
  ): BaselineTerm =
    val bl = samplingFrame.blockLens
    require(bl.nonEmpty && bl.forall(_ > 0), "samplingFrame.blockLens must be non-empty and positive")
    val nb = bl.length
    require(nuisanceList.length == nb, "nuisanceList length must match samplingFrame.nBlocks")
    nuisanceList.zipWithIndex.foreach { case (m, b) =>
      require(m.rows == bl(b), s"nuisance matrix row mismatch for block $b")
    }
    names.foreach { byBlock =>
      require(byBlock.length == nuisanceList.length, "nuisance names must match nuisance runs")
      byBlock.zip(nuisanceList).zipWithIndex.foreach { case ((columns, matrix), block) =>
        require(columns.length == matrix.cols, s"nuisance names must match columns for block $block")
      }
    }

    val totalRows = bl.sum
    val totalCols = nuisanceList.map(_.cols).sum
    val out = new Array[Double](totalRows * totalCols)

    val rowInd = blockRowIndices(bl)
    val colInd = Vector.newBuilder[Vector[Int]]
    val colNames = Vector.newBuilder[String]

    var rowOffset = 0
    var colOffset = 0
    var b = 0
    while b < nb do
      val m = nuisanceList(b)
      val len = m.rows
      val cols = m.cols

      val colIdx = (colOffset until (colOffset + cols)).toVector
      colInd += colIdx

      var r = 0
      while r < len do
        val dstBase = (rowOffset + r) * totalCols + colOffset
        System.arraycopy(m.data, r * cols, out, dstBase, cols)
        r += 1

      names match
        case Some(byBlock) =>
          colNames ++= byBlock(b)
        case None =>
          val blk = f"${b + 1}%02d"
          var j = 0
          while j < cols do
            colNames += s"${prefix}#${blk}_${j + 1}"
            j += 1

      rowOffset += len
      colOffset += cols
      b += 1

    BaselineTerm(
      varName = "nuisance",
      data = Mat.unsafe(totalRows, totalCols, out),
      columnNames = colNames.result(),
      colInd = colInd.result(),
      rowInd = rowInd
    )

  private def blockRowIndices(blockLens: Vector[Int]): Vector[Vector[Int]] =
    val out = Vector.newBuilder[Vector[Int]]
    var rowOffset = 0
    var b = 0
    while b < blockLens.length do
      val len = blockLens(b)
      out += (rowOffset until (rowOffset + len)).toVector
      rowOffset += len
      b += 1
    out.result()

  private def subset(mat: Mat, rows: collection.IndexedSeq[Int], cols: collection.IndexedSeq[Int]): Mat =
    val out = new Array[Double](rows.length * cols.length)
    var rOut = 0
    while rOut < rows.length do
      val rIn = rows(rOut)
      var cOut = 0
      while cOut < cols.length do
        out(rOut * cols.length + cOut) = mat.data(rIn * mat.cols + cols(cOut))
        cOut += 1
      rOut += 1
    Mat.unsafe(rows.length, cols.length, out)

final case class BaselineModel(
    terms: Vector[(String, BaselineTerm)],
    driftSpec: BaselineSpec,
    samplingFrame: SamplingFrame,
    designMatrix: Mat,
    columnNames: Vector[String],
    termSpans: Vector[(Int, Int)],
    colIndices: Map[String, Vector[Int]],
    nuisanceReport: Option[NuisanceReport] = None,
    compiledSchema: Option[DesignSchema] = None
):
  def termKeys: Vector[String] = terms.map(_._1)

  def designSchema: DesignSchema =
    compiledSchema.getOrElse(
      DesignSchema.legacy(
        matrix = designMatrix,
        samplingFrame = samplingFrame,
        columnNames = columnNames,
        source = ModelSource.Baseline
      )
    )

  def designSchemaValidation: Either[DesignError, Unit] =
    compiledSchema match
      case Some(schema) => schema.validate
      case None         => Right(())

  def designMatrixFor(blockId: Option[Int] = None, allRows: Boolean = false): Mat =
    val mats = terms.map { case (_, t) => t.designMatrix(blockId = blockId, allRows = allRows) }
    if mats.isEmpty then Mat.zeros(0, 0)
    else
      val rows = mats.head.rows
      mats.tail.foreach(m => require(m.rows == rows, "row mismatch across terms"))
      mats.reduce(_ ++ _)

object BaselineModel:

  val DefaultNuisanceTol: Double = 1.4901161193847656e-8

  def build(plan: BaselinePlan): BaselineModel =
    unsafe(buildEither(plan))

  def buildEither(plan: BaselinePlan): Either[BaselineError, BaselineModel] =
    val policy = plan.nuisance.map(_.policy).getOrElse(NuisancePolicy())
    buildEither(
      samplingFrame = plan.samplingFrame,
      basis = plan.basis,
      degree = plan.degree,
      intercept = plan.intercept,
      nuisanceList = plan.nuisance.map(_.matrices),
      nuisanceCheck = policy.check,
      naAction = policy.naAction,
      nuisanceNames = plan.nuisance.flatMap(_.names),
      nuisanceTol = policy.tol,
      duplicateThreshold = policy.duplicateThreshold
    )

  def build(
      samplingFrame: SamplingFrame,
      basis: BaselineBasis = BaselineBasis.Constant,
      degree: Int = 1,
      intercept: Intercept = Intercept.Runwise,
      nuisanceList: Option[Seq[Mat]] = None,
      nuisanceCheck: NuisanceCheck = NuisanceCheck.Warn,
      naAction: NaAction = NaAction.Drop,
      nuisanceNames: Option[Seq[Seq[String]]] = None,
      nuisanceTol: Double = DefaultNuisanceTol,
      duplicateThreshold: Double = 1.0 - DefaultNuisanceTol
  ): BaselineModel =
    unsafe(buildEither(
      samplingFrame = samplingFrame,
      basis = basis,
      degree = degree,
      intercept = intercept,
      nuisanceList = nuisanceList,
      nuisanceCheck = nuisanceCheck,
      naAction = naAction,
      nuisanceNames = nuisanceNames,
      nuisanceTol = nuisanceTol,
      duplicateThreshold = duplicateThreshold
    ))

  def buildEither(
      samplingFrame: SamplingFrame,
      basis: BaselineBasis = BaselineBasis.Constant,
      degree: Int = 1,
      intercept: Intercept = Intercept.Runwise,
      nuisanceList: Option[Seq[Mat]] = None,
      nuisanceCheck: NuisanceCheck = NuisanceCheck.Warn,
      naAction: NaAction = NaAction.Drop,
      nuisanceNames: Option[Seq[Seq[String]]] = None,
      nuisanceTol: Double = DefaultNuisanceTol,
      duplicateThreshold: Double = 1.0 - DefaultNuisanceTol
  ): Either[BaselineError, BaselineModel] =
    try
      if basis == BaselineBasis.Bs || basis == BaselineBasis.Ns then
        require(degree > 2, "'bs' and 'ns' bases must have degree >= 3")

      val driftSpec = BaselineSpec(degree = degree, basis = basis, intercept = intercept)
      val drift = driftSpec.construct(samplingFrame)

      val blockTerm =
        if intercept != Intercept.None && basis != BaselineBasis.Constant then
          Some(BaselineTerm.blockIntercept("constant", samplingFrame, intercept))
        else None

      val baselineTerms = Vector.newBuilder[BaselineTerm]
      baselineTerms += drift
      blockTerm.foreach(baselineTerms += _)

      val checkedNuisanceEither: Either[BaselineError, Option[(Vector[Mat], Vector[Vector[String]], Option[NuisanceReport])]] =
        nuisanceList match
          case None => Right(None)
          case Some(ns) =>
            val prepared = prepareNuisance(ns, samplingFrame, nuisanceNames, naAction)
            if nuisanceCheck == NuisanceCheck.None then
              Right(Some((prepared.matrices, prepared.names, _root_.scala.None)))
            else
              val report = checkPreparedNuisance(
                prepared,
                baselineTerms.result(),
                nuisanceTol,
                duplicateThreshold
              )
              if !report.ok then
                nuisanceCheck match
                  case NuisanceCheck.Error =>
                    Left(BaselineError.NuisanceProblems(report))
                  case NuisanceCheck.Drop =>
                    Right(Some((dropNuisanceColumns(report), report.retainedByBlock, Some(report))))
                  case NuisanceCheck.Warn =>
                    Right(Some((prepared.matrices, prepared.names, Some(report))))
                  case NuisanceCheck.None =>
                    Right(Some((prepared.matrices, prepared.names, _root_.scala.None)))
              else Right(Some((prepared.matrices, prepared.names, Some(report))))

      checkedNuisanceEither.map { checkedNuisance =>
        val nuisTerm = checkedNuisance.map { case (mats, names, _) =>
          BaselineTerm.nuisance(mats, samplingFrame, Some(names))
        }
        val nuisanceReport = checkedNuisance.flatMap(_._3)

        val terms = Vector.newBuilder[(String, BaselineTerm)]
        terms += ("drift" -> drift)
        blockTerm.foreach(t => terms += ("block" -> t))
        nuisTerm.foreach(t => terms += ("nuisance" -> t))
        val ts = terms.result()

        val totalRows = samplingFrame.blockLens.sum
        ts.foreach { case (_, t) => require(t.data.rows == totalRows, "term matrix row mismatch with samplingFrame") }

        val totalCols = ts.map(_._2.data.cols).sum
        val out = new Array[Double](totalRows * totalCols)
        val colNames = Vector.newBuilder[String]

        val spans = Vector.newBuilder[(Int, Int)]
        val indices = scala.collection.mutable.LinkedHashMap.empty[String, Vector[Int]]

        var colOffset = 0
        var i = 0
        while i < ts.length do
          val (key, term) = ts(i)
          val cols = term.data.cols

          var r = 0
          while r < totalRows do
            System.arraycopy(term.data.data, r * cols, out, r * totalCols + colOffset, cols)
            r += 1

          colNames ++= term.columnNames
          spans += ((colOffset, colOffset + cols))
          indices.update(key, (colOffset until (colOffset + cols)).toVector)

          colOffset += cols
          i += 1

        val matrix = Mat.unsafe(totalRows, totalCols, out)
        val compiledSchema =
          DesignSchema.validated(
            matrix = matrix,
            rows = RowLayout.fromSamplingFrame(samplingFrame),
            columns = structuralColumns(ts, driftSpec),
            audit = DesignAudit(
              policyReceipts = Vector(
                PolicyReceipt("baseline", driftSpec.basis match
                  case BaselineBasis.Dct(_) => s"basis=${driftSpec.basis.id};intercept=${driftSpec.intercept}"
                  case _ => s"basis=${driftSpec.basis};degree=${driftSpec.degree};intercept=${driftSpec.intercept}"
                )
              ) ++ (driftSpec.basis match
                case BaselineBasis.Dct(cutoff) => DctDrift.receipts(samplingFrame, cutoff)
                case _ => Vector.empty
              )
            )
          ).fold(error => throw new IllegalArgumentException(error.message), identity)

        BaselineModel(
          terms = ts,
          driftSpec = driftSpec,
          samplingFrame = samplingFrame,
          designMatrix = matrix,
          columnNames = colNames.result(),
          termSpans = spans.result(),
          colIndices = indices.toMap,
          nuisanceReport = nuisanceReport,
          compiledSchema = Some(compiledSchema)
        )
      }
    catch
      case NonFatal(t) => Left(BaselineError.fromThrowable(t))

  private def structuralColumns(
      terms: Vector[(String, BaselineTerm)],
      driftSpec: BaselineSpec
  ): Vector[StructuralColumn] =
    val out = Vector.newBuilder[StructuralColumn]
    var ordinal = 1
    terms.foreach { case (key, term) =>
      var local = 0
      while local < term.data.cols do
        val memberships = term.colInd.zipWithIndex.flatMap { case (cols, block0) =>
          val pos = cols.indexOf(local)
          if pos < 0 then None else Some((block0, pos))
        }
        val runScope =
          if memberships.length == 1 then RunScope.Run(RunIndex.unsafeOneBased(memberships.head._1 + 1))
          else RunScope.Global
        val component =
          memberships.headOption.map { case (_, pos) =>
            BasisElementRef(
              basisId =
                if key == "drift" then driftSpec.basis.id
                else if key == "block" then "constant"
                else key,
              index = BasisIndex.unsafeOneBased(pos + 1),
              role = Some(scalafim.fmri.hrf.BasisRole.Generic(pos + 1))
            )
          }
        val origin =
          key match
            case "drift" =>
              StructuralColumnOrigin.Drift(TermId.unsafe(key), component, runScope)
            case "block" =>
              StructuralColumnOrigin.Intercept(runScope)
            case "nuisance" =>
              StructuralColumnOrigin.Nuisance(
                term = TermId.unsafe(key),
                regressor = ModulatorId.unsafe(term.columnNames(local)),
                runScope = runScope
              )
            case _ =>
              StructuralColumnOrigin.Baseline(
                term = TermId.unsafe(key),
                role = ColumnRole.Baseline,
                component = component,
                runScope = runScope
              )
        val label = term.columnNames(local)
        out += StructuralColumn.fromOrigin(ordinal, origin, label, label).fold(
          error => throw new IllegalArgumentException(error.message),
          identity
        )
        ordinal += 1
        local += 1
    }
    out.result()

  def checkNuisance(plan: BaselinePlan): NuisanceReport =
    unsafe(checkNuisanceEither(plan))

  def checkNuisanceEither(plan: BaselinePlan): Either[BaselineError, NuisanceReport] =
    requireNuisanceEither(plan).flatMap { nuisance =>
      checkNuisanceEither(
        nuisanceList = nuisance.matrices,
        samplingFrame = plan.samplingFrame,
        basis = plan.basis,
        degree = plan.degree,
        intercept = plan.intercept,
        nuisanceNames = nuisance.names,
        naAction = nuisance.policy.naAction,
        tol = nuisance.policy.tol,
        duplicateThreshold = nuisance.policy.duplicateThreshold
      )
    }

  def checkNuisance(
      nuisanceList: Seq[Mat],
      samplingFrame: SamplingFrame,
      basis: BaselineBasis = BaselineBasis.Constant,
      degree: Int = 1,
      intercept: Intercept = Intercept.Runwise,
      nuisanceNames: Option[Seq[Seq[String]]] = None,
      naAction: NaAction = NaAction.Drop,
      tol: Double = DefaultNuisanceTol,
      duplicateThreshold: Double = 1.0 - DefaultNuisanceTol
  ): NuisanceReport =
    unsafe(checkNuisanceEither(
      nuisanceList = nuisanceList,
      samplingFrame = samplingFrame,
      basis = basis,
      degree = degree,
      intercept = intercept,
      nuisanceNames = nuisanceNames,
      naAction = naAction,
      tol = tol,
      duplicateThreshold = duplicateThreshold
    ))

  def checkNuisanceEither(
      nuisanceList: Seq[Mat],
      samplingFrame: SamplingFrame,
      basis: BaselineBasis = BaselineBasis.Constant,
      degree: Int = 1,
      intercept: Intercept = Intercept.Runwise,
      nuisanceNames: Option[Seq[Seq[String]]] = None,
      naAction: NaAction = NaAction.Drop,
      tol: Double = DefaultNuisanceTol,
      duplicateThreshold: Double = 1.0 - DefaultNuisanceTol
  ): Either[BaselineError, NuisanceReport] =
    try
      if basis == BaselineBasis.Bs || basis == BaselineBasis.Ns then
        require(degree > 2, "'bs' and 'ns' bases must have degree >= 3")

      val driftSpec = BaselineSpec(degree = degree, basis = basis, intercept = intercept)
      val drift = driftSpec.construct(samplingFrame)
      val blockTerm =
        if intercept != Intercept.None && basis != BaselineBasis.Constant then
          Some(BaselineTerm.blockIntercept("constant", samplingFrame, intercept))
        else _root_.scala.None
      val baselineTerms = Vector(drift) ++ blockTerm.toVector
      val prepared = prepareNuisance(nuisanceList, samplingFrame, nuisanceNames, naAction)

      Right(checkPreparedNuisance(prepared, baselineTerms, tol, duplicateThreshold))
    catch
      case NonFatal(t) => Left(BaselineError.fromThrowable(t))

  def cleanNuisance(plan: BaselinePlan): CleanedNuisance =
    unsafe(cleanNuisanceEither(plan))

  def cleanNuisanceEither(plan: BaselinePlan): Either[BaselineError, CleanedNuisance] =
    requireNuisanceEither(plan).flatMap { nuisance =>
      cleanNuisanceEither(
        nuisanceList = nuisance.matrices,
        samplingFrame = plan.samplingFrame,
        basis = plan.basis,
        degree = plan.degree,
        intercept = plan.intercept,
        nuisanceNames = nuisance.names,
        naAction = nuisance.policy.naAction,
        tol = nuisance.policy.tol,
        duplicateThreshold = nuisance.policy.duplicateThreshold
      )
    }

  def cleanNuisance(
      nuisanceList: Seq[Mat],
      samplingFrame: SamplingFrame,
      basis: BaselineBasis = BaselineBasis.Constant,
      degree: Int = 1,
      intercept: Intercept = Intercept.Runwise,
      nuisanceNames: Option[Seq[Seq[String]]] = None,
      naAction: NaAction = NaAction.Drop,
      tol: Double = DefaultNuisanceTol,
      duplicateThreshold: Double = 1.0 - DefaultNuisanceTol
  ): CleanedNuisance =
    unsafe(cleanNuisanceEither(
      nuisanceList = nuisanceList,
      samplingFrame = samplingFrame,
      basis = basis,
      degree = degree,
      intercept = intercept,
      nuisanceNames = nuisanceNames,
      naAction = naAction,
      tol = tol,
      duplicateThreshold = duplicateThreshold
    ))

  def cleanNuisanceEither(
      nuisanceList: Seq[Mat],
      samplingFrame: SamplingFrame,
      basis: BaselineBasis = BaselineBasis.Constant,
      degree: Int = 1,
      intercept: Intercept = Intercept.Runwise,
      nuisanceNames: Option[Seq[Seq[String]]] = None,
      naAction: NaAction = NaAction.Drop,
      tol: Double = DefaultNuisanceTol,
      duplicateThreshold: Double = 1.0 - DefaultNuisanceTol
  ): Either[BaselineError, CleanedNuisance] =
    checkNuisanceEither(
      nuisanceList = nuisanceList,
      samplingFrame = samplingFrame,
      basis = basis,
      degree = degree,
      intercept = intercept,
      nuisanceNames = nuisanceNames,
      naAction = naAction,
      tol = tol,
      duplicateThreshold = duplicateThreshold
    ).map { report =>
      CleanedNuisance(dropNuisanceColumns(report), report)
    }

  private def requireNuisanceEither(plan: BaselinePlan): Either[BaselineError, NuisanceInput] =
    plan.nuisance.toRight(BaselineError.MissingNuisance)

  private def unsafe[A](result: Either[BaselineError, A]): A =
    result.fold(error => throw new IllegalArgumentException(error.message), identity)

  private final case class PreparedNuisance(matrices: Vector[Mat], names: Vector[Vector[String]])

  private def prepareNuisance(
      nuisanceList: Seq[Mat],
      samplingFrame: SamplingFrame,
      nuisanceNames: Option[Seq[Seq[String]]],
      naAction: NaAction
  ): PreparedNuisance =
    val bl = samplingFrame.blockLens
    require(bl.nonEmpty && bl.forall(_ > 0), "samplingFrame.blockLens must be non-empty and positive")
    require(nuisanceList.length == bl.length, "nuisanceList length must match samplingFrame.nBlocks")
    nuisanceNames.foreach(ns => require(ns.length == nuisanceList.length, "nuisanceNames length must match nuisanceList length"))

    val matrices = nuisanceList.zipWithIndex.map { case (m, b) =>
      require(m.rows == bl(b), s"nuisance matrix row mismatch for block $b")
      repairNa(m, naAction)
    }.toVector

    val names = matrices.zipWithIndex.map { case (m, b) =>
      val raw = nuisanceNames.map(_(b))
      nuisanceColumnNames(m.cols, raw)
    }

    PreparedNuisance(matrices, names)

  private def checkPreparedNuisance(
      nuisance: PreparedNuisance,
      baselineTerms: Vector[BaselineTerm],
      tol: Double,
      duplicateThreshold: Double
  ): NuisanceReport =
    require(tol >= 0.0, "tol must be non-negative")
    require(duplicateThreshold >= 0.0 && duplicateThreshold <= 1.0, "duplicateThreshold must be in [0, 1]")

    val blocks = Vector.newBuilder[NuisanceBlockReport]
    val problems = Vector.newBuilder[NuisanceProblem]

    var b = 0
    while b < nuisance.matrices.length do
      val mat = nuisance.matrices(b)
      val names = nuisance.names(b)
      val finiteMat = finiteCopy(mat)
      val baselineMat = baselineMatrixForBlock(baselineTerms, b, tol)

      val nonFiniteFlags = columnFlags(mat)(v => !v.isFinite)
      val zeroVarianceFlags = zeroVarianceColumns(finiteMat, tol)
      val duplicatePairs = duplicatePairsFor(finiteMat, names, zeroVarianceFlags, nonFiniteFlags, duplicateThreshold)
      val keep = incrementalRankKeep(finiteMat, baselineMat, zeroVarianceFlags, nonFiniteFlags, tol)

      val nonFinite = flaggedNames(names, nonFiniteFlags)
      val zeroVariance = flaggedNames(names, zeroVarianceFlags)
      val aliased = names.zip(keep).zip(zeroVarianceFlags.zip(nonFiniteFlags)).collect {
        case ((name, false), (false, false)) => name
      }.toVector

      val nuisanceRank = rank(finiteMat, tol)
      val baselineRank = rank(baselineMat, tol)
      val withBaseline = hcat(Vector(baselineMat, finiteMat))
      val rankWithBaseline = rank(withBaseline, tol)
      val columnsWithBaseline = baselineMat.cols + finiteMat.cols

      if nonFinite.nonEmpty then
        problems += NuisanceProblem(
          block = b + 1,
          issue = NuisanceIssue.NonFinite,
          columns = nonFinite,
          detail = "contains NaN or infinite values"
        )
      if zeroVariance.nonEmpty then
        problems += NuisanceProblem(
          block = b + 1,
          issue = NuisanceIssue.ZeroVariance,
          columns = zeroVariance,
          detail = "column has no within-run variance"
        )
      if duplicatePairs.nonEmpty then
        val detail = duplicatePairs
          .map(d => f"${d.column} duplicates ${d.duplicates} (r = ${d.correlation}%.6g)")
          .mkString("; ")
        problems += NuisanceProblem(
          block = b + 1,
          issue = NuisanceIssue.Duplicate,
          columns = duplicatePairs.map(_.column),
          detail = detail
        )
      if nuisanceRank < finiteMat.cols then
        problems += NuisanceProblem(
          block = b + 1,
          issue = NuisanceIssue.RankDeficientNuisance,
          columns = names,
          detail = s"nuisance rank: $nuisanceRank < ${finiteMat.cols} columns"
        )
      if rankWithBaseline < columnsWithBaseline then
        problems += NuisanceProblem(
          block = b + 1,
          issue = NuisanceIssue.RankDeficientWithBaseline,
          columns = names,
          detail = s"rank with baseline terms: $rankWithBaseline < $columnsWithBaseline columns"
        )
      if aliased.nonEmpty then
        problems += NuisanceProblem(
          block = b + 1,
          issue = NuisanceIssue.AliasedColumns,
          columns = aliased,
          detail = "columns do not increase rank after baseline and earlier nuisance columns"
        )

      val retained = names.zip(keep).collect { case (name, true) => name }.toVector
      val dropped = names.zip(keep).collect { case (name, false) => name }.toVector

      blocks += NuisanceBlockReport(
        block = b + 1,
        baselineRank = baselineRank,
        baselineColumns = baselineMat.cols,
        nuisanceRank = nuisanceRank,
        nuisanceColumns = finiteMat.cols,
        rankWithBaseline = rankWithBaseline,
        columnsWithBaseline = columnsWithBaseline,
        nonFinite = nonFinite,
        zeroVariance = zeroVariance,
        duplicatePairs = duplicatePairs,
        aliasedColumns = aliased,
        keep = keep,
        retainedColumns = retained,
        droppedColumns = dropped
      )

      b += 1

    NuisanceReport(
      problems = problems.result(),
      byBlock = blocks.result(),
      nuisanceList = nuisance.matrices,
      columnNames = nuisance.names
    )

  private def dropNuisanceColumns(report: NuisanceReport): Vector[Mat] =
    report.nuisanceList.zip(report.byBlock).map { case (mat, block) =>
      selectColumns(mat, block.keep.zipWithIndex.collect { case (true, j) => j }.toVector)
    }

  private def repairNa(mat: Mat, naAction: NaAction): Mat =
    naAction match
      case NaAction.Drop => mat
      case NaAction.Zero =>
        val out = mat.data.clone
        var i = 0
        while i < out.length do
          if out(i).isNaN then out(i) = 0.0
          i += 1
        Mat.unsafe(mat.rows, mat.cols, out)
      case NaAction.Median =>
        val out = mat.data.clone
        var c = 0
        while c < mat.cols do
          var hasMissing = false
          var r = 0
          while r < mat.rows do
            if out(r * mat.cols + c).isNaN then hasMissing = true
            r += 1

          if hasMissing then
            val finite = Vector.newBuilder[Double]
            r = 0
            while r < mat.rows do
              val v = out(r * mat.cols + c)
              if v.isFinite then finite += v
              r += 1

            val values = finite.result().sorted
            if values.nonEmpty then
              val med =
                if values.length % 2 == 1 then values(values.length / 2)
                else
                  val hi = values.length / 2
                  (values(hi - 1) + values(hi)) / 2.0
              r = 0
              while r < mat.rows do
                val idx = r * mat.cols + c
                if out(idx).isNaN then out(idx) = med
                r += 1
          c += 1
        Mat.unsafe(mat.rows, mat.cols, out)

  private def nuisanceColumnNames(cols: Int, rawNames: Option[Seq[String]]): Vector[String] =
    val raw = rawNames match
      case Some(ns) =>
        require(ns.length == cols, "nuisanceNames block length must match nuisance matrix columns")
        ns.toVector
      case _ =>
        (1 to cols).map(i => s"V$i").toVector

    makeUnique(raw.zipWithIndex.map { case (name, i) =>
      val trimmed = Option(name).fold("")(_.trim)
      if trimmed.isEmpty then s"V${i + 1}" else trimmed
    })

  private def makeUnique(names: Vector[String]): Vector[String] =
    val seen = scala.collection.mutable.HashMap.empty[String, Int]
    names.map { name =>
      val n = seen.getOrElse(name, 0)
      seen.update(name, n + 1)
      if n == 0 then name else s"${name}_$n"
    }

  private def baselineMatrixForBlock(terms: Vector[BaselineTerm], block: Int, tol: Double): Mat =
    val mats = terms.map { term =>
      val mat = term.designMatrix(blockId = Some(block), allRows = false)
      selectColumns(mat, activeColumns(mat, tol))
    }
    hcat(mats)

  private def activeColumns(mat: Mat, tol: Double): Vector[Int] =
    val out = Vector.newBuilder[Int]
    var c = 0
    while c < mat.cols do
      var active = false
      var r = 0
      while r < mat.rows && !active do
        val v = mat.data(r * mat.cols + c)
        active = v.isFinite && math.abs(v) > tol
        r += 1
      if active then out += c
      c += 1
    out.result()

  private def selectColumns(mat: Mat, cols: Vector[Int]): Mat =
    if cols.isEmpty then Mat.zeros(mat.rows, 0)
    else
      val out = new Array[Double](mat.rows * cols.length)
      var r = 0
      while r < mat.rows do
        var j = 0
        while j < cols.length do
          out(r * cols.length + j) = mat.data(r * mat.cols + cols(j))
          j += 1
        r += 1
      Mat.unsafe(mat.rows, cols.length, out)

  private def hcat(mats: Seq[Mat]): Mat =
    if mats.isEmpty then Mat.zeros(0, 0)
    else
      val rows = mats.head.rows
      mats.foreach(m => require(m.rows == rows, "row mismatch across matrices"))
      val cols = mats.map(_.cols).sum
      val out = new Array[Double](rows * cols)

      var colOffset = 0
      mats.foreach { mat =>
        var r = 0
        while r < rows do
          System.arraycopy(mat.data, r * mat.cols, out, r * cols + colOffset, mat.cols)
          r += 1
        colOffset += mat.cols
      }
      Mat.unsafe(rows, cols, out)

  private def finiteCopy(mat: Mat): Mat =
    val out = mat.data.clone
    var i = 0
    while i < out.length do
      if !out(i).isFinite then out(i) = 0.0
      i += 1
    Mat.unsafe(mat.rows, mat.cols, out)

  private def columnFlags(mat: Mat)(predicate: Double => Boolean): Vector[Boolean] =
    Vector.tabulate(mat.cols) { c =>
      var found = false
      var r = 0
      while r < mat.rows && !found do
        found = predicate(mat.data(r * mat.cols + c))
        r += 1
      found
    }

  private def zeroVarianceColumns(mat: Mat, tol: Double): Vector[Boolean] =
    Vector.tabulate(mat.cols) { c =>
      var min = Double.PositiveInfinity
      var max = Double.NegativeInfinity
      var maxAbs = 0.0
      var r = 0
      while r < mat.rows do
        val v = mat.data(r * mat.cols + c)
        min = math.min(min, v)
        max = math.max(max, v)
        maxAbs = math.max(maxAbs, math.abs(v))
        r += 1
      math.abs(max - min) <= tol * math.max(1.0, maxAbs)
    }

  private def duplicatePairsFor(
      mat: Mat,
      names: Vector[String],
      zeroVariance: Vector[Boolean],
      nonFinite: Vector[Boolean],
      duplicateThreshold: Double
  ): Vector[NuisanceDuplicate] =
    val keep = names.indices.filterNot(j => zeroVariance(j) || nonFinite(j)).toVector
    if keep.length < 2 then Vector.empty
    else
      val out = Vector.newBuilder[NuisanceDuplicate]
      var a = 0
      while a < keep.length - 1 do
        var b = a + 1
        while b < keep.length do
          val c1 = keep(a)
          val c2 = keep(b)
          val cor = correlation(mat, c1, c2)
          if cor.isFinite && math.abs(cor) >= duplicateThreshold then
            out += NuisanceDuplicate(names(c2), names(c1), cor)
          b += 1
        a += 1
      out.result()

  private def correlation(mat: Mat, c1: Int, c2: Int): Double =
    var mean1 = 0.0
    var mean2 = 0.0
    var r = 0
    while r < mat.rows do
      mean1 += mat.data(r * mat.cols + c1)
      mean2 += mat.data(r * mat.cols + c2)
      r += 1
    mean1 /= mat.rows.toDouble
    mean2 /= mat.rows.toDouble

    var ss1 = 0.0
    var ss2 = 0.0
    var cross = 0.0
    r = 0
    while r < mat.rows do
      val x = mat.data(r * mat.cols + c1) - mean1
      val y = mat.data(r * mat.cols + c2) - mean2
      ss1 += x * x
      ss2 += y * y
      cross += x * y
      r += 1

    val denom = math.sqrt(ss1 * ss2)
    if denom == 0.0 then Double.NaN else cross / denom

  private def incrementalRankKeep(
      mat: Mat,
      baselineMat: Mat,
      zeroVariance: Vector[Boolean],
      nonFinite: Vector[Boolean],
      tol: Double
  ): Vector[Boolean] =
    val keep = Array.fill(mat.cols)(true)
    var current = baselineMat
    var currentRank = rank(current, tol)

    var c = 0
    while c < mat.cols do
      if zeroVariance(c) || nonFinite(c) then keep(c) = false
      else
        val candidate = hcat(Vector(current, selectColumns(mat, Vector(c))))
        val candidateRank = rank(candidate, tol)
        if candidateRank > currentRank then
          current = candidate
          currentRank = candidateRank
        else keep(c) = false
      c += 1

    keep.toVector

  private def flaggedNames(names: Vector[String], flags: Vector[Boolean]): Vector[String] =
    names.zip(flags).collect { case (name, true) => name }.toVector

  private def rank(mat: Mat, tol: Double): Int =
    if mat.cols == 0 || mat.rows == 0 then 0
    else QrDecomposition.decompose(mat.data, mat.rows, mat.cols, pivoting = true, tol = tol).rank
