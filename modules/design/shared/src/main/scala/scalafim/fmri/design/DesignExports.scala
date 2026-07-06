package scalafim.fmri.design

import scalafim.fmri.design.baseline.{BaselineModel, BaselineTerm}
import scalafim.fmri.design.contrast.ContrastRegistry.*
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

enum CorrelationMethod:
  case Pearson, Spearman

enum PlotBlockAxis:
  case Global, Run

enum PlotLabelMode:
  case Auto, Compact, None

enum ContrastScaleMode:
  case Auto, Diverging, OneSided

final case class DesignMatrixRow(scan: Int, values: Vector[Double])

final case class DesignMatrixTable(
    columnNames: Vector[String],
    rows: Vector[DesignMatrixRow],
    metadata: Vector[DesignColumnMeta]
)

final case class DesignMapCell(scan: Int, col: Int, regressor: String, value: Double)

final case class DesignMapData(
    cells: Vector[DesignMapCell],
    nScans: Int,
    regressors: Vector[String],
    blockSeparators: Vector[Int],
    metadata: Vector[DesignColumnMeta],
    rendererNote: String = DesignExports.RendererNote
)

final case class CorrelationCell(
    row: Int,
    col: Int,
    var1: String,
    var2: String,
    correlation: Option[Double]
)

final case class CorrelationMapData(
    cells: Vector[CorrelationCell],
    regressors: Vector[String],
    method: CorrelationMethod,
    halfMatrix: Boolean,
    limits: Option[(Double, Double)],
    rendererNote: String = DesignExports.RendererNote
)

final case class TracePoint(
    time: Double,
    response: Double,
    regressor: String,
    block: Int,
    group: String
)

final case class BlockBoundary(block: Option[Int], time: Double)

final case class EventPlotData(
    points: Vector[TracePoint],
    regressors: Vector[String],
    boundaries: Vector[BlockBoundary],
    useFacets: Boolean,
    facetByBlock: Boolean,
    suppressLabels: Boolean,
    blockAxis: PlotBlockAxis,
    rendererNote: String = DesignExports.RendererNote
)

final case class BaselinePlotData(
    termName: String,
    points: Vector[TracePoint],
    conditions: Vector[String],
    rendererNote: String = DesignExports.RendererNote
)

final case class ContrastWeightCell(
    contrastName: String,
    regressor: String,
    row: Int,
    col: Int,
    weight: Double
)

final case class ContrastPlotData(
    cells: Vector[ContrastWeightCell],
    contrastNames: Vector[String],
    regressors: Vector[String],
    scaleMode: ContrastScaleMode,
    limits: Option[(Double, Double)],
    rendererNote: String = DesignExports.RendererNote
)

object DesignExports:

  val RendererNote: String =
    "Rendering is intentionally adapter-based; these shared data rows are suitable for JVM and Scala.js plotting layers."

  trait DesignSource[A]:
    def designMatrix(a: A): Mat
    def columnNames(a: A): Vector[String]
    def samplingFrame(a: A): SamplingFrame
    def metadata(a: A): Vector[DesignColumnMeta]

  object DesignSource:
    given DesignSource[EventModel] with
      def designMatrix(a: EventModel): Mat = a.designMatrix
      def columnNames(a: EventModel): Vector[String] = a.columnNames
      def samplingFrame(a: EventModel): SamplingFrame = a.samplingFrame
      def metadata(a: EventModel): Vector[DesignColumnMeta] = DesignColmap.forEventModel(a)

    given DesignSource[BaselineModel] with
      def designMatrix(a: BaselineModel): Mat = a.designMatrix
      def columnNames(a: BaselineModel): Vector[String] = a.columnNames
      def samplingFrame(a: BaselineModel): SamplingFrame = a.samplingFrame
      def metadata(a: BaselineModel): Vector[DesignColumnMeta] = DesignColmap.forBaselineModel(a)

  extension [A](a: A)(using source: DesignSource[A])
    def designMeta: Vector[DesignColumnMeta] =
      source.metadata(a)

    def designTable: DesignMatrixTable =
      DesignExports.designTable(source.designMatrix(a), source.columnNames(a), source.metadata(a))

    def designMap(blockSeparators: Boolean = true): DesignMapData =
      DesignExports.designMap(
        source.designMatrix(a),
        source.columnNames(a),
        source.samplingFrame(a),
        source.metadata(a),
        blockSeparators = blockSeparators
      )

    def correlationMap(
        method: CorrelationMethod = CorrelationMethod.Pearson,
        halfMatrix: Boolean = false,
        absoluteLimits: Boolean = true
    ): CorrelationMapData =
      DesignExports.correlationMap(
        source.designMatrix(a),
        source.columnNames(a),
        method = method,
        halfMatrix = halfMatrix,
        absoluteLimits = absoluteLimits
      )

  def designTable(mat: Mat, columnNames: Vector[String], metadata: Vector[DesignColumnMeta] = Vector.empty): DesignMatrixTable =
    require(columnNames.length == mat.cols, "columnNames length must match matrix columns")
    val rows = Vector.tabulate(mat.rows) { r =>
      val values = Vector.tabulate(mat.cols)(c => mat.data(r * mat.cols + c))
      DesignMatrixRow(scan = r + 1, values = values)
    }
    DesignMatrixTable(columnNames, rows, metadata)

  def designMap(
      mat: Mat,
      columnNames: Vector[String],
      samplingFrame: SamplingFrame,
      metadata: Vector[DesignColumnMeta],
      blockSeparators: Boolean
  ): DesignMapData =
    require(columnNames.length == mat.cols, "columnNames length must match matrix columns")
    val cells = Vector.newBuilder[DesignMapCell]
    var r = 0
    while r < mat.rows do
      var c = 0
      while c < mat.cols do
        cells += DesignMapCell(scan = r + 1, col = c + 1, regressor = columnNames(c), value = mat.data(r * mat.cols + c))
        c += 1
      r += 1

    val separators =
      if !blockSeparators then Vector.empty
      else samplingFrame.blockLens.scanLeft(0)(_ + _).drop(1).dropRight(1)

    DesignMapData(
      cells = cells.result(),
      nScans = mat.rows,
      regressors = columnNames,
      blockSeparators = separators,
      metadata = metadata
    )

  def correlationMap(
      mat: Mat,
      columnNames: Vector[String],
      method: CorrelationMethod,
      halfMatrix: Boolean,
      absoluteLimits: Boolean
  ): CorrelationMapData =
    require(columnNames.length == mat.cols, "columnNames length must match matrix columns")
    val cells = Vector.newBuilder[CorrelationCell]
    val observed = Vector.newBuilder[Double]
    var i = 0
    while i < mat.cols do
      var j = 0
      while j < mat.cols do
        val corr =
          if halfMatrix && j > i then None
          else
            val c = columnCorrelation(mat, i, j, method)
            if c.isFinite then
              observed += c
              Some(c)
            else None
        cells += CorrelationCell(row = i + 1, col = j + 1, var1 = columnNames(i), var2 = columnNames(j), correlation = corr)
        j += 1
      i += 1

    val limits =
      if absoluteLimits then Some((-1.0, 1.0))
      else
        val xs = observed.result()
        if xs.isEmpty then None else Some((xs.min, xs.max))

    CorrelationMapData(cells.result(), columnNames, method, halfMatrix, limits)

  def eventPlotData(
      model: EventModel,
      termName: Option[String] = None,
      facetThreshold: Int = Int.MaxValue,
      labelMode: PlotLabelMode = PlotLabelMode.Auto,
      maxLabels: Int = 30,
      blockAxis: PlotBlockAxis = PlotBlockAxis.Global,
      facetByBlock: Boolean = false,
      showBlockBounds: Boolean = true
  ): EventPlotData =
    val selected = selectEventColumns(model, termName)
    val regressors = selected.map(model.columnNames)
    val times = model.samplingFrame.samples(global = blockAxis == PlotBlockAxis.Global).map(_.value)
    val blocks = model.samplingFrame.blockIdsPerSample.map(_ + 1)
    require(times.length == model.designMatrix.rows && blocks.length == model.designMatrix.rows, "samplingFrame/design row mismatch")

    val points = Vector.newBuilder[TracePoint]
    selected.foreach { c =>
      val reg = model.columnNames(c)
      var r = 0
      while r < model.designMatrix.rows do
        val block = blocks(r)
        points += TracePoint(
          time = times(r),
          response = model.designMatrix.data(r * model.designMatrix.cols + c),
          regressor = reg,
          block = block,
          group = s"$reg#$block"
        )
        r += 1
    }

    val useFacets = selected.length > facetThreshold
    val suppressLabels =
      labelMode == PlotLabelMode.None ||
        (labelMode == PlotLabelMode.Auto && selected.length > maxLabels)

    EventPlotData(
      points = points.result(),
      regressors = regressors,
      boundaries = if showBlockBounds then blockBoundaries(model.samplingFrame, blockAxis, facetByBlock) else Vector.empty,
      useFacets = useFacets,
      facetByBlock = facetByBlock && model.samplingFrame.nBlocks > 1,
      suppressLabels = suppressLabels,
      blockAxis = blockAxis
    )

  def baselinePlotData(
      model: BaselineModel,
      termName: Option[String] = None,
      zeroTol: Double = 1.4901161193847656e-8
  ): BaselinePlotData =
    require(model.terms.nonEmpty, "Baseline model contains no terms")
    val termKey = selectBaselineTerm(model, termName, zeroTol)
    val term = model.terms.collectFirst { case (`termKey`, t) => t }.get
    val mat = term.data
    val times = model.samplingFrame.samples(global = false).map(_.value)
    val blocks = model.samplingFrame.blockIdsPerSample.map(_ + 1)
    require(times.length == mat.rows && blocks.length == mat.rows, "samplingFrame/design row mismatch")

    val points = Vector.newBuilder[TracePoint]
    var c = 0
    while c < mat.cols do
      val reg = term.columnNames(c)
      var b = 1
      while b <= model.samplingFrame.nBlocks do
        if hasSignalInBlock(mat, c, blocks, b, zeroTol) then
          var r = 0
          while r < mat.rows do
            if blocks(r) == b then
              points += TracePoint(
                time = times(r),
                response = mat.data(r * mat.cols + c),
                regressor = reg,
                block = b,
                group = s"$reg#$b"
              )
            r += 1
        b += 1
      c += 1

    BaselinePlotData(termName = termKey, points = points.result(), conditions = term.columnNames)

  def contrastPlotData(
      model: EventModel,
      includeFContrasts: Boolean = false,
      scaleMode: ContrastScaleMode = ContrastScaleMode.Auto,
      absoluteLimits: Boolean = false,
      maxInter: Int = 4
  ): ContrastPlotData =
    val attached = model.contrastWeights
    val fcons = if includeFContrasts then model.fContrastWeights(maxInter = maxInter) else scala.collection.immutable.VectorMap.empty
    val weights = attached ++ fcons
    require(weights.nonEmpty, "No contrasts found in this event model")

    val cells = Vector.newBuilder[ContrastWeightCell]
    val contrastNames = Vector.newBuilder[String]
    val observed = Vector.newBuilder[Double]
    var contrastRow = 0

    weights.foreach { case (contrastKey, cw) =>
      var k = 0
      while k < cw.weights.cols do
        val rowName =
          if cw.weights.cols == 1 then contrastKey
          else s"${contrastKey}_component${k + 1}"
        contrastRow += 1
        contrastNames += rowName

        var r = 0
        while r < cw.weights.rows do
          val w = cw.weights.data(r * cw.weights.cols + k)
          observed += w
          cells += ContrastWeightCell(
            contrastName = rowName,
            regressor = cw.condNames(r),
            row = contrastRow,
            col = r + 1,
            weight = w
          )
          r += 1
        k += 1
    }

    val ws = observed.result()
    val actualMode =
      scaleMode match
        case ContrastScaleMode.Auto if ws.exists(_ < 0.0) => ContrastScaleMode.Diverging
        case ContrastScaleMode.Auto                       => ContrastScaleMode.OneSided
        case other                                        => other
    val limits =
      if ws.isEmpty then None
      else
        actualMode match
          case ContrastScaleMode.Diverging =>
            Some((if absoluteLimits then -1.0 else ws.min, if absoluteLimits then 1.0 else ws.max))
          case ContrastScaleMode.OneSided =>
            Some((if absoluteLimits then 0.0 else ws.min, if absoluteLimits then 1.0 else ws.max))
          case ContrastScaleMode.Auto =>
            None

    ContrastPlotData(
      cells = cells.result(),
      contrastNames = contrastNames.result(),
      regressors = model.columnNames,
      scaleMode = actualMode,
      limits = limits
    )

  private def selectEventColumns(model: EventModel, termName: Option[String]): Vector[Int] =
    termName match
      case None => model.columnNames.indices.toVector
      case Some(name) =>
        val byTerm = model.colIndices.get(name).toVector.flatten
        val byPrefix =
          model.columnNames.zipWithIndex.collect {
            case (col, i) if col == name || col.startsWith(name + "_") || col.startsWith(name + ".") || col.startsWith(name + "[") => i
          }
        val selected = (byTerm ++ byPrefix).distinct.sorted
        if selected.isEmpty then throw new IllegalArgumentException(s"No columns found matching term name: $name")
        selected

  private def selectBaselineTerm(model: BaselineModel, termName: Option[String], zeroTol: Double): String =
    val termKeys = model.termKeys
    termName match
      case Some(name) =>
        val exact = termKeys.filter(_ == name)
        if exact.length == 1 then exact.head
        else
          val partial = termKeys.filter(_.toLowerCase.contains(name.toLowerCase))
          partial match
            case Vector(one) => one
            case Vector()    => throw new IllegalArgumentException(s"Specified term_name '$name' not found. Available terms: ${termKeys.mkString(", ")}")
            case many        => throw new IllegalArgumentException(s"Specified term_name '$name' matches multiple terms: ${many.mkString(", ")}")
      case None =>
        model.terms.find { case (_, term) => !baselineTermIsConstant(term, model.samplingFrame, zeroTol) }.map(_._1).getOrElse(termKeys.head)

  private def baselineTermIsConstant(term: BaselineTerm, samplingFrame: SamplingFrame, zeroTol: Double): Boolean =
    val mat = term.data
    val blocks = samplingFrame.blockIdsPerSample.map(_ + 1)
    var c = 0
    while c < mat.cols do
      var b = 1
      while b <= samplingFrame.nBlocks do
        var seen = false
        var first = 0.0
        var differs = false
        var r = 0
        while r < mat.rows && !differs do
          if blocks(r) == b then
            val v = mat.data(r * mat.cols + c)
            if !seen then
              first = v
              seen = true
            else if math.abs(v - first) > zeroTol then differs = true
          r += 1
        if differs then return false
        b += 1
      c += 1
    true

  private def hasSignalInBlock(mat: Mat, col: Int, blocks: Vector[Int], block: Int, zeroTol: Double): Boolean =
    var r = 0
    while r < mat.rows do
      if blocks(r) == block && math.abs(mat.data(r * mat.cols + col)) > zeroTol then return true
      r += 1
    false

  private def blockBoundaries(samplingFrame: SamplingFrame, axis: PlotBlockAxis, facetByBlock: Boolean): Vector[BlockBoundary] =
    val runLengths = samplingFrame.blockLens.zip(samplingFrame.tr).map { case (len, tr) => len.toDouble * tr.value }
    if facetByBlock then
      runLengths.zipWithIndex.flatMap { case (len, b) =>
        Vector(BlockBoundary(Some(b + 1), 0.0), BlockBoundary(Some(b + 1), len))
      }.toVector
    else
      axis match
        case PlotBlockAxis.Run =>
          runLengths.flatMap(len => Vector(0.0, len)).distinct.sorted.map(t => BlockBoundary(None, t)).toVector
        case PlotBlockAxis.Global =>
          val ends = runLengths.scanLeft(0.0)(_ + _).drop(1)
          val starts = 0.0 +: ends.dropRight(1)
          (starts ++ ends).distinct.sorted.map(t => BlockBoundary(None, t)).toVector

  private def columnCorrelation(mat: Mat, c1: Int, c2: Int, method: CorrelationMethod): Double =
    val pairs = Vector.newBuilder[(Double, Double)]
    var r = 0
    while r < mat.rows do
      val a = mat.data(r * mat.cols + c1)
      val b = mat.data(r * mat.cols + c2)
      if a.isFinite && b.isFinite then pairs += ((a, b))
      r += 1
    val xs = pairs.result()
    if xs.length < 2 then Double.NaN
    else
      method match
        case CorrelationMethod.Pearson =>
          pearson(xs.map(_._1), xs.map(_._2))
        case CorrelationMethod.Spearman =>
          pearson(ranks(xs.map(_._1)), ranks(xs.map(_._2)))

  private def pearson(x: Vector[Double], y: Vector[Double]): Double =
    require(x.length == y.length, "correlation vectors must match")
    if x.length < 2 then Double.NaN
    else
      val mx = x.sum / x.length.toDouble
      val my = y.sum / y.length.toDouble
      var ssx = 0.0
      var ssy = 0.0
      var cross = 0.0
      var i = 0
      while i < x.length do
        val dx = x(i) - mx
        val dy = y(i) - my
        ssx += dx * dx
        ssy += dy * dy
        cross += dx * dy
        i += 1
      val denom = math.sqrt(ssx * ssy)
      if denom == 0.0 then Double.NaN else cross / denom

  private def ranks(xs: Vector[Double]): Vector[Double] =
    val sorted = xs.zipWithIndex.sortBy(_._1)
    val out = Array.fill(xs.length)(0.0)
    var i = 0
    while i < sorted.length do
      var j = i + 1
      while j < sorted.length && sorted(j)._1 == sorted(i)._1 do j += 1
      val avgRank = (i + 1 + j).toDouble / 2.0
      var k = i
      while k < j do
        out(sorted(k)._2) = avgRank
        k += 1
      i = j
    out.toVector
