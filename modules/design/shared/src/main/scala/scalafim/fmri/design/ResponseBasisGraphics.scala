package scalafim.fmri.design

import scalafim.fmri.hrf.*
import intaglio.*

/** Native basis geometry and exact functional receipts, rendered through Intaglio.
  * Call plotSelected for a legible subset of a wide FIR or spline basis.
  */
object ResponseBasisGraphics:
  private final case class Point(time: Double, value: Double, label: String, series: String)
  private val boundary = "Readout boundary"
  private val textStyle = GraphicParams.unsafe(stroke=None,fill=Some(Rgba.unsafe(65,82,76)),fontSize=Length.pointsUnsafe(8))
  private val theme = Theme.minimal.copy(axis=Theme.minimal.axis.copy(text=textStyle,title=textStyle),
    legend=Theme.minimal.legend.copy(text=textStyle,title=textStyle))

  def plot(review: ResponseBasisReview, width: Double = 640, height: Double = 280): Either[GraphicsError,TrainedPlot] =
    plotSelected(review,review.basisIds,width,height)

  def plotSelected(review: ResponseBasisReview, ids: Vector[BasisElementId],
      width: Double = 640, height: Double = 280, maximumCurves: Int = 12): Either[GraphicsError,TrainedPlot] =
    val selected = ids.flatMap(id => review.curves.find(_.element.id == id))
    if !width.isFinite || !height.isFinite || width <= 0 || height <= 0 then
      Left(GraphicsError.EmptyGeometry("Basis plot dimensions must be positive and finite."))
    else if ids.isEmpty || ids.distinct.size != ids.size || selected.size != ids.size || ids.size > maximumCurves then
      Left(GraphicsError.EmptyGeometry("Choose unique known basis coordinates within the declared curve budget."))
    else
      val curves = selected.flatMap { curve =>
        val values = curve.geometry match
          case BasisPreviewGeometry.Sampled(points) => points
          case BasisPreviewGeometry.FirStep(from,until,height) => Vector(
            BasisPreviewPoint(from,0),BasisPreviewPoint(from,height),
            BasisPreviewPoint(until,height),BasisPreviewPoint(until,0))
        values.map(p => Point(p.time.value,p.value,curve.label,"basis:"+curve.element.id.value))
      }
      val values = review.curves.flatMap(c => c.values ++ (c.geometry match
        case BasisPreviewGeometry.FirStep(_,_,height) => Vector(height)
        case _ => Vector.empty)) :+ 0.0
      val low = values.min
      val high = values.max
      val (bottom,top) = if low == high then (low-1.0,high+1.0) else (low,high)
      val boundaries = review.readout.toVector.flatMap(_.functional match
        case ResponseFunctional.At(time) => Vector(time)
        case ResponseFunctional.WindowMean(from,until) => Vector(from,until)
        case ResponseFunctional.WindowIntegral(from,until) => Vector(from,until))
      val markers = boundaries.zipWithIndex.flatMap { (time,index) =>
        Vector(Point(time.value,bottom,boundary,s"readout-boundary-$index"),Point(time.value,top,boundary,s"readout-boundary-$index"))
      }
      val labels = selected.map(_.label) ++ Option.when(markers.nonEmpty)(boundary)
      val colors = selected.map(c => DesignGraphics.defaultColors((c.element.index-1)%DesignGraphics.defaultColors.size)) ++
        Option.when(markers.nonEmpty)(Rgba.unsafe(145,151,147))
      val description = s"Native response basis ${review.descriptor.canonicalId}; fixed full-basis axes; no display normalization; model components, not fitted responses. " +
        s"Showing ${ids.size}/${review.curves.size} coordinates. Sample times (seconds): ${review.times.map(_.value).mkString(",")}. " +
        selected.map(c => s"id=${c.element.id.value}; role=${c.element.role.stableLabel}; values=${c.values.mkString(",")}; geometry=${c.geometry match { case BasisPreviewGeometry.FirStep(from,until,height) => s"FirStep(${from.value},${until.value},$height)"; case _: BasisPreviewGeometry.Sampled => "Sampled" }}").mkString("\n") +
        review.readout.fold("")(r => s"\nReadout=${r.functional}; units=${r.units}; weight axis=${review.basisIds.map(_.value).mkString(",")}; exact basis weights=${r.weights.mkString(",")}; discretization=${r.discretization}. Display samples and curve selection do not determine these weights.")
      for
        time <- ContinuousScale.fixed("Response time",review.times.map(_.value),Palette.numeric)
        value <- ContinuousScale.fixed("Basis value",Vector(bottom,top),Palette.numeric)
        domain <- DiscreteDomain.ordered(labels)
        color <- DiscreteScale("Basis",domain,DiscretePalette.valuesUnsafe(colors))
        program <- intaglio.plot(curves ++ markers).aes(_.time,_.value).group(_.series)
          .encode(Aesthetic.X,_.time,time).encode(Aesthetic.Y,_.value,value)
          .encode(Aesthetic.Color,_.label,color).geomLine(params=Some(GraphicParams.unsafe(lineWidth=1.3)))
          .axisTitles("Response time (s)","Basis value").build
        trained <- PlotCompiler.resolve(program.plot.withDescription(description),PlotCompilerOptions(
          policy=Some(LayoutPolicy(referenceDevice=DeviceContext.unsafe(width,height),outerMarginPt=6,
            axisFontPt=8,axisTitleFontPt=8,legendFontPt=8,legendTitleFontPt=8,legendKeyPt=3)),theme=theme,guides=GuidePolicy.Derived()))
      yield trained
