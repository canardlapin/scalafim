package scalafim.graphics.svg

import scalafim.graphics.*

final case class SvgOptions private (width: Int, height: Int, title: Option[String])

object SvgOptions:
  val default: SvgOptions =
    unsafe()

  def apply(width: Int = 640, height: Int = 480, title: Option[String] = None): Either[SvgRenderError, SvgOptions] =
    if width <= 0 || height <= 0 then Left(SvgRenderError.InvalidDocumentSize(width, height))
    else Right(new SvgOptions(width, height, title))

  def unsafe(width: Int = 640, height: Int = 480, title: Option[String] = None): SvgOptions =
    apply(width, height, title).orThrow

final case class SvgDocument(value: String):
  override def toString: String =
    value

object SvgRenderer:
  def render(scene: Scene, options: SvgOptions = SvgOptions.default): Either[SvgRenderError, SvgDocument] =
    val out = new StringBuilder
    line(
      out,
      0,
      s"""<svg xmlns="http://www.w3.org/2000/svg" width="${options.width}" height="${options.height}" viewBox="0 0 ${options.width} ${options.height}">"""
    )
    options.title.foreach(title => line(out, 1, s"<title>${escapeText(title)}</title>"))
    writeAll(scene.grobs, out, 1).map { _ =>
      line(out, 0, "</svg>")
      SvgDocument(out.result())
    }

  private def writeAll(grobs: Vector[Grob], out: StringBuilder, indent: Int): Either[SvgRenderError, Unit] =
    var idx = 0
    var result: Either[SvgRenderError, Unit] = Right(())
    while idx < grobs.length && result.isRight do
      result = writeGrob(grobs(idx), out, indent)
      idx += 1
    result

  private def writeGrob(grob: Grob, out: StringBuilder, indent: Int): Either[SvgRenderError, Unit] =
    grob.viewport match
      case Some(viewport) =>
        writeViewportOpen(viewport, grob.name, out, indent).flatMap { _ =>
          writeGrobBody(grob, out, indent + 1).map { _ =>
            line(out, indent, "</svg>")
          }
        }
      case None =>
        writeGrobBody(grob, out, indent)

  private def writeGrobBody(grob: Grob, out: StringBuilder, indent: Int): Either[SvgRenderError, Unit] =
    grob match
      case points: Grob.Points =>
        writePoints(points, out, indent)
      case lines: Grob.Lines =>
        writeLines(lines, out, indent)
      case segments: Grob.Segments =>
        writeSegments(segments, out, indent)
      case rect: Grob.Rect =>
        writeRect(rect, out, indent)
      case circle: Grob.Circle =>
        writeCircle(circle, out, indent)
      case text: Grob.Text =>
        writeText(text, out, indent)
      case group: Grob.Group =>
        val attrs = commonAttrs(group.name, group.gp)
        line(out, indent, s"<g$attrs>")
        writeAll(group.children, out, indent + 1).map { _ =>
          line(out, indent, "</g>")
        }

  private def writePoints(points: Grob.Points, out: StringBuilder, indent: Int): Either[SvgRenderError, Unit] =
    renderLength(points.size).flatMap { radius =>
      var idx = 0
      var result: Either[SvgRenderError, Unit] = Right(())
      while idx < points.points.length && result.isRight do
        result = renderPoint(points.points(idx)).map { case (x, y) =>
          points.shape match
            case PointShape.Circle =>
              line(out, indent, s"""<circle${commonAttrs(points.name, points.gp)} cx="$x" cy="$y" r="$radius" />""")
            case PointShape.Square =>
              line(out, indent, s"""<rect${commonAttrs(points.name, points.gp)} x="$x" y="$y" width="$radius" height="$radius" />""")
            case PointShape.Triangle =>
              line(out, indent, s"""<path${commonAttrs(points.name, points.gp)} d="M $x $y l $radius 0 l 0 $radius z" />""")
            case PointShape.Cross =>
              line(out, indent, s"""<text${textAttrs(points.name, points.gp)} x="$x" y="$y">+</text>""")
        }
        idx += 1
      result
    }

  private def writeLines(lines: Grob.Lines, out: StringBuilder, indent: Int): Either[SvgRenderError, Unit] =
    var idx = 0
    var result: Either[SvgRenderError, Unit] = Right(())
    while idx + 1 < lines.points.length && result.isRight do
      result =
        for
          p0 <- renderPoint(lines.points(idx))
          p1 <- renderPoint(lines.points(idx + 1))
        yield
          line(
            out,
            indent,
            s"""<line${lineAttrs(lines.name, lines.gp)} x1="${p0._1}" y1="${p0._2}" x2="${p1._1}" y2="${p1._2}" />"""
          )
      idx += 1
    result

  private def writeSegments(segments: Grob.Segments, out: StringBuilder, indent: Int): Either[SvgRenderError, Unit] =
    var idx = 0
    var result: Either[SvgRenderError, Unit] = Right(())
    while idx < segments.segments.length && result.isRight do
      val (from, to) = segments.segments(idx)
      result =
        for
          p0 <- renderPoint(from)
          p1 <- renderPoint(to)
        yield
          line(
            out,
            indent,
            s"""<line${lineAttrs(segments.name, segments.gp)} x1="${p0._1}" y1="${p0._2}" x2="${p1._1}" y2="${p1._2}" />"""
          )
      idx += 1
    result

  private def writeRect(rect: Grob.Rect, out: StringBuilder, indent: Int): Either[SvgRenderError, Unit] =
    for
      center <- renderPoint(rect.center)
      size <- renderSize(rect.size)
      x <- anchoredX(center._1, size._1, rect.anchor.horizontal)
      y <- anchoredY(center._2, size._2, rect.anchor.vertical)
    yield
      line(out, indent, s"""<rect${commonAttrs(rect.name, rect.gp)} x="$x" y="$y" width="${size._1}" height="${size._2}" />""")

  private def writeCircle(circle: Grob.Circle, out: StringBuilder, indent: Int): Either[SvgRenderError, Unit] =
    for
      center <- renderPoint(circle.center)
      radius <- renderLength(circle.radius)
    yield
      line(out, indent, s"""<circle${commonAttrs(circle.name, circle.gp)} cx="${center._1}" cy="${center._2}" r="$radius" />""")

  private def writeText(text: Grob.Text, out: StringBuilder, indent: Int): Either[SvgRenderError, Unit] =
    renderPoint(text.at).map { case (x, y) =>
      val anchor = textAnchor(text.anchor.horizontal)
      val baseline = dominantBaseline(text.anchor.vertical)
      val rotation =
        if text.rotationDegrees == 0.0 then ""
        else s" transform=\"rotate(${format(text.rotationDegrees)} $x $y)\""
      line(
        out,
        indent,
        s"""<text${textAttrs(text.name, text.gp)} x="$x" y="$y" text-anchor="$anchor" dominant-baseline="$baseline"$rotation>${escapeText(text.label)}</text>"""
      )
    }

  private def writeViewportOpen(
      viewport: Viewport,
      name: Option[GraphicsName],
      out: StringBuilder,
      indent: Int
  ): Either[SvgRenderError, Unit] =
    for
      origin <- renderPoint(viewport.origin)
      size <- renderSize(viewport.size)
    yield
      val overflow =
        viewport.clip match
          case Clip.On  => "hidden"
          case Clip.Off => "visible"
      val rotate =
        if viewport.angleDegrees == 0.0 then ""
        else s" transform=\"rotate(${format(viewport.angleDegrees)} ${origin._1} ${origin._2})\""
      val dataName = name.map(n => s""" data-name="${escapeAttr(n.value)}"""").getOrElse("")
      val viewBox =
        s"${format(viewport.xScale.lower)} ${format(viewport.yScale.lower)} ${format(viewport.xScale.width)} ${format(viewport.yScale.width)}"
      line(
        out,
        indent,
        s"""<svg$dataName x="${origin._1}" y="${origin._2}" width="${size._1}" height="${size._2}" viewBox="$viewBox" overflow="$overflow"$rotate>"""
      )

  private def renderPoint(point: Point): Either[SvgRenderError, (String, String)] =
    for
      x <- renderLength(point.x)
      y <- renderLength(point.y)
    yield (x, y)

  private def renderSize(size: Size): Either[SvgRenderError, (String, String)] =
    for
      width <- renderLength(size.width)
      height <- renderLength(size.height)
    yield (width, height)

  private def renderLength(expr: LengthExpr): Either[SvgRenderError, String] =
    expr match
      case LengthExpr.Const(length) =>
        renderScalarLength(length)
      case LengthExpr.Add(left, right) =>
        for
          l <- renderLength(left)
          r <- renderLength(right)
        yield s"calc($l + $r)"
      case LengthExpr.Sub(left, right) =>
        for
          l <- renderLength(left)
          r <- renderLength(right)
        yield s"calc($l - $r)"
      case LengthExpr.Mul(factor, LengthExpr.Const(length)) =>
        renderScalarLength(Length.unsafe(length.value * factor, length.unit))
      case LengthExpr.Mul(_, _) =>
        Left(SvgRenderError.UnsupportedLengthExpression("non-scalar multiplication"))

  private def renderScalarLength(length: Length): Either[SvgRenderError, String] =
    length.unit match
      case LengthUnit.Npc =>
        Right(s"${format(length.value * 100.0)}%")
      case LengthUnit.Native =>
        Right(format(length.value))
      case LengthUnit.Cm =>
        Right(s"${format(length.value)}cm")
      case LengthUnit.Mm =>
        Right(s"${format(length.value)}mm")
      case LengthUnit.Inch =>
        Right(s"${format(length.value)}in")
      case LengthUnit.Point =>
        Right(s"${format(length.value)}pt")
      case LengthUnit.Line =>
        Left(SvgRenderError.UnsupportedLengthUnit(LengthUnit.Line))

  private def anchoredX(x: String, width: String, just: HJust): Either[SvgRenderError, String] =
    just match
      case HJust.Left   => Right(x)
      case HJust.Center => Right(s"calc($x - $width / 2)")
      case HJust.Right  => Right(s"calc($x - $width)")

  private def anchoredY(y: String, height: String, just: VJust): Either[SvgRenderError, String] =
    just match
      case VJust.Top    => Right(y)
      case VJust.Center => Right(s"calc($y - $height / 2)")
      case VJust.Bottom => Right(s"calc($y - $height)")

  private def commonAttrs(name: Option[GraphicsName], gp: GraphicParams): String =
    val attrs = new StringBuilder
    name.foreach(n => attrs.append(s""" data-name="${escapeAttr(n.value)}""""))
    appendPaint(attrs, "stroke", gp.stroke)
    appendPaint(attrs, "fill", gp.fill)
    attrs.append(s""" stroke-width="${format(gp.lineWidth)}"""")
    lineTypeAttr(gp.lineType).foreach(attrs.append)
    if gp.alpha != 1.0 then attrs.append(s""" opacity="${format(gp.alpha)}"""")
    attrs.result()

  private def lineAttrs(name: Option[GraphicsName], gp: GraphicParams): String =
    val attrs = new StringBuilder
    name.foreach(n => attrs.append(s""" data-name="${escapeAttr(n.value)}""""))
    appendPaint(attrs, "stroke", gp.stroke)
    attrs.append(""" fill="none"""")
    attrs.append(s""" stroke-width="${format(gp.lineWidth)}"""")
    lineTypeAttr(gp.lineType).foreach(attrs.append)
    if gp.alpha != 1.0 then attrs.append(s""" opacity="${format(gp.alpha)}"""")
    attrs.result()

  private def textAttrs(name: Option[GraphicsName], gp: GraphicParams): String =
    val attrs = new StringBuilder
    name.foreach(n => attrs.append(s""" data-name="${escapeAttr(n.value)}""""))
    appendPaint(attrs, "fill", gp.fill.orElse(gp.stroke).orElse(Some(Rgba.Black)))
    attrs.append(""" stroke="none"""")
    gp.fontFamily.foreach(family => attrs.append(s""" font-family="${escapeAttr(family)}""""))
    attrs.append(s""" font-size="${format(gp.fontSize.value)}pt"""")
    if gp.alpha != 1.0 then attrs.append(s""" opacity="${format(gp.alpha)}"""")
    attrs.result()

  private def appendPaint(out: StringBuilder, attr: String, color: Option[Rgba]): Unit =
    color match
      case Some(rgba) =>
        out.append(s""" $attr="${hex(rgba)}"""")
        if rgba.alpha != 1.0 then out.append(s""" $attr-opacity="${format(rgba.alpha)}"""")
      case None =>
        out.append(s""" $attr="none"""")

  private def lineTypeAttr(lineType: LineType): Option[String] =
    lineType match
      case LineType.Solid  => None
      case LineType.Dashed => Some(""" stroke-dasharray="6 4"""")
      case LineType.Dotted => Some(""" stroke-dasharray="1 3"""")

  private def textAnchor(just: HJust): String =
    just match
      case HJust.Left   => "start"
      case HJust.Center => "middle"
      case HJust.Right  => "end"

  private def dominantBaseline(just: VJust): String =
    just match
      case VJust.Bottom => "text-after-edge"
      case VJust.Center => "middle"
      case VJust.Top    => "text-before-edge"

  private def hex(color: Rgba): String =
    def channel(value: Int): String =
      val s = value.toHexString
      if s.length == 1 then "0" + s else s
    "#" + channel(color.red) + channel(color.green) + channel(color.blue)

  private def line(out: StringBuilder, indent: Int, value: String): Unit =
    out.append("  " * indent).append(value).append("\n")

  private def format(value: Double): String =
    if value == 0.0 then "0"
    else
      val text = value.toString
      if text.endsWith(".0") then text.dropRight(2) else text

  private def escapeText(value: String): String =
    value
      .replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")

  private def escapeAttr(value: String): String =
    escapeText(value).replace("\"", "&quot;")
