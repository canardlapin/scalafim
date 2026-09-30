import java.io.File

/** STP P1.07 exit gate: no public signature outside the IO packages mentions a world frame erased to `Frame[D3]`.
  *
  * A type application is frame-erased when one of its type arguments is `Frame[D3]` (for example
  * `SpatialPullback[Frame[D3], T]`, `Grid[Frame[D3], D3]`; `image4s.geometry.`-qualified spellings included), or when
  * it is reframe4s' `FrameErasedMap[D3]`. A public type alias for `Frame[D3]` is flagged too, since it would hide the
  * erasure from this scan. Frames themselves (`def frame: Frame[D3]`) are values, not erasures, and are allowed.
  * Existentials (`GridSpec[?]`) are the honest spelling of a runtime frame and are allowed.
  *
  * The scan strips comments and string literals and matches type applications across line breaks. An occurrence is
  * exempt only when it belongs to a `private`/`private[this]` definition (or one nested in such a definition), or to a
  * definition local to a method or value body. Qualified access such as `private[scalafim]` is visible to other
  * modules, so it is not exempt. Expressions directly in a public method's body are flagged, conservatively.
  *
  * Known limits: a private alias of an erased type used in a public signature, or an erasure spelled through an
  * alias of `D3`, is not followed; every file under an `io` package is exempt wholesale.
  *
  * Written for the sbt 1.x meta-build (Scala 2.12).
  */
object FrameErasureGate {
  /** Type constructors whose frame arguments must stay typed. */
  val FramedTypes: Seq[String] =
    Seq(
      "SpatialMap",
      "SpatialPullback",
      "GridSpec",
      "Point",
      "Vec",
      "Grid",
      "SampleSpace",
      "WorldBox",
      "WorldTransform",
      "FramedAffine",
      "FrameAlignment",
      "WorldLink",
      "SurfaceCameraPose",
      "SurfaceDisplayFrame",
      "SurfaceVolumeCursor",
      "SurfaceCursorHit"
    )

  /** Files where frame erasure is the point, with the reason recorded next to each entry in build.sbt. Paths are
    * relative to the repository root, so a same-named file elsewhere is still checked.
    */
  val Allowlist: Set[String] = Set(
    "modules/spatial/shared/src/main/scala/scalafim/spatial/Morphism.scala",
    "modules/transform/shared/src/main/scala/scalafim/transform/StageChain.scala"
  )

  final case class Violation(path: String, line: Int, text: String) {
    override def toString: String = s"$path:$line: $text"
  }

  private val ErasedArgument = "Frame[D3]"
  private val ErasedMap = """\bFrameErasedMap\s*\[\s*D3\s*\]""".r
  private val TypeApplication = ("""\b(""" + FramedTypes.mkString("|") + """)\s*\[""").r
  private val Declaration =
    """^(\s*)((?:@[\w.]+(?:\([^)]*\))?\s+|(?:private|protected)(?:\s*\[[^\]]*\])?\s+|override\s+|final\s+|inline\s+|implicit\s+|lazy\s+|sealed\s+|abstract\s+|case\s+|opaque\s+|open\s+|transparent\s+|infix\s+)*)(def|val|var|type|class|object|trait|enum|given|extension|case)\b""".r
  private val ErasedAlias =
    """(?m)^\s*((?:@[\w.]+(?:\([^)]*\))?\s+|(?:private|protected)(?:\s*\[[^\]]*\])?\s+|opaque\s+|final\s+)*)type\s+\w+\s*(?:\[[^\]]*\])?\s*(?:<:[^=\n]*)?=\s*([^\n]+)$""".r

  /** IO packages decode containers at runtime and may carry erased frames; they are not public frame APIs. */
  def isIoSource(relativePath: String): Boolean =
    relativePath.contains("/io/")

  def check(repository: File): Seq[Violation] = {
    val root = repository.getCanonicalFile.toPath
    val modules = new File(repository, "modules")
    sources(modules)
      .map(file => root.relativize(file.getCanonicalFile.toPath).toString.replace(File.separatorChar, '/'))
      .filter(path => path.contains("/src/main/scala/") && !Allowlist.contains(path) && !isIoSource(path))
      .sorted
      .flatMap(path => scan(path, new String(java.nio.file.Files.readAllBytes(new File(repository, path).toPath), "UTF-8")))
  }

  /** Frame-erased public signatures in one source text. */
  def scan(path: String, source: String): Seq[Violation] = {
    val code = stripCommentsAndStrings(source)
    val lines = code.split("\n", -1).toVector
    val lineStarts = lines.scanLeft(0)((offset, line) => offset + line.length + 1)
    def lineOf(offset: Int): Int = {
      var index = java.util.Arrays.binarySearch(lineStarts.toArray, offset)
      if (index < 0) index = -index - 2
      index
    }
    val erasedApplications =
      TypeApplication.findAllMatchIn(code).flatMap { m =>
        typeArguments(code, m.end - 1) match {
          case Some(arguments) if arguments.exists(argument => normalize(argument) == ErasedArgument) => Some(m.start)
          case _ => None
        }
      }.toVector
    val erasedMaps = ErasedMap.findAllMatchIn(code).map(_.start).toVector
    val erasedAliases =
      ErasedAlias.findAllMatchIn(code).filter(m => normalize(m.group(2)) == ErasedArgument).map(_.start(2)).toVector
    (erasedApplications ++ erasedMaps ++ erasedAliases).map(lineOf).distinct.sorted.flatMap { line =>
      if (exempt(lines, line)) None
      else Some(Violation(path, line + 1, source.split("\n", -1).lift(line).getOrElse("").trim))
    }
  }

  /** Top-level type arguments of the application whose `[` is at `open`, with nested brackets balanced. */
  private def typeArguments(code: String, open: Int): Option[Vector[String]] = {
    var depth = 0
    var index = open
    var start = open + 1
    val arguments = Vector.newBuilder[String]
    while (index < code.length) {
      code.charAt(index) match {
        case '[' => depth += 1
        case ']' =>
          depth -= 1
          if (depth == 0) {
            arguments += code.substring(start, index)
            return Some(arguments.result())
          }
        case ',' if depth == 1 =>
          arguments += code.substring(start, index)
          start = index + 1
        case _ => ()
      }
      index += 1
    }
    None
  }

  private def normalize(text: String): String =
    text.replaceAll("\\s+", "").replace("_root_.", "").replace("image4s.geometry.", "")

  /** Exempt when the owning definition, or one enclosing it, is `private`/`private[this]`, or when the owning
    * definition is local to a term body (an enclosing `def`/`val`/`var`/`given` at lower indentation).
    */
  private def exempt(lines: Vector[String], line: Int): Boolean = {
    def indent(text: String): Int = text.prefixLength(_ == ' ')
    def declarationAt(index: Int): Option[(Int, String, String)] =
      Declaration.findFirstMatchIn(lines(index)).flatMap { m =>
        // `case X(...) =>` is a match clause, not an enum case declaration.
        if (m.group(3) == "case" && lines(index).contains("=>")) None
        else Some((m.group(1).length, m.group(2), m.group(3)))
      }
    def isPrivate(modifiers: String): Boolean =
      """(?:^|\s)private(?:\s*\[\s*this\s*\])?\s""".r.findFirstIn(" " + modifiers).nonEmpty
    // A line closing a multi-line signature (`): T =`) belongs to the declaration opened at its own indentation.
    def closesSignature(text: String): Boolean = {
      val trimmed = text.trim
      trimmed.startsWith(")") || trimmed.startsWith("]")
    }
    val lineIndent = indent(lines(line))
    // The owning definition: the line itself when it declares something; otherwise the nearest enclosing (shallower)
    // declaration, never a preceding sibling, whose modifiers say nothing about this line.
    val owner =
      (line to 0 by -1).iterator.flatMap(index => declarationAt(index).map(index -> _)).find { case (index, (ind, _, _)) =>
        index == line || ind < lineIndent || (ind == lineIndent && closesSignature(lines(line)))
      }
    owner match {
      case None => false
      case Some((ownerLine, (ownerIndent, ownerModifiers, _))) =>
        if (isPrivate(ownerModifiers)) true
        else {
          // Walk outwards through strictly shallower declarations.
          var bound = ownerIndent
          var result = false
          var index = ownerLine - 1
          while (index >= 0 && bound > 0 && !result) {
            val text = lines(index)
            if (text.trim.nonEmpty && indent(text) < bound && !closesSignature(text)) {
              declarationAt(index) match {
                case Some((ind, modifiers, keyword)) =>
                  if (isPrivate(modifiers)) result = true
                  else if (Set("def", "val", "var", "given").contains(keyword) && """\bwith\s*$""".r.findFirstIn(text).isEmpty) result = true
                  bound = ind
                case None =>
                  bound = indent(text)
              }
            }
            index -= 1
          }
          result
        }
    }
  }

  /** Replace comments and string literals with spaces, keeping every newline so offsets map to the same lines. */
  def stripCommentsAndStrings(source: String): String = {
    val out = new StringBuilder(source.length)
    var index = 0
    def blank(c: Char): Char = if (c == '\n') '\n' else ' '
    while (index < source.length) {
      if (source.startsWith("//", index)) {
        while (index < source.length && source.charAt(index) != '\n') { out.append(' '); index += 1 }
      } else if (source.startsWith("/*", index)) {
        var depth = 0
        var done = false
        while (index < source.length && !done) {
          if (source.startsWith("/*", index)) { depth += 1; out.append("  "); index += 2 }
          else if (source.startsWith("*/", index)) {
            depth -= 1; out.append("  "); index += 2
            if (depth == 0) done = true
          } else { out.append(blank(source.charAt(index))); index += 1 }
        }
      } else if (source.startsWith("\"\"\"", index)) {
        out.append("   "); index += 3
        while (index < source.length && !source.startsWith("\"\"\"", index)) { out.append(blank(source.charAt(index))); index += 1 }
        if (index < source.length) { out.append("   "); index += 3 }
      } else if (source.charAt(index) == '"') {
        out.append(' '); index += 1
        while (index < source.length && source.charAt(index) != '"' && source.charAt(index) != '\n') {
          if (source.charAt(index) == '\\' && index + 1 < source.length) { out.append("  "); index += 2 }
          else { out.append(' '); index += 1 }
        }
        if (index < source.length && source.charAt(index) == '"') { out.append(' '); index += 1 }
      } else if (source.charAt(index) == '\'' && index + 3 < source.length && source.charAt(index + 1) == '\\' && source.charAt(index + 3) == '\'') {
        out.append("    "); index += 4
      } else if (source.charAt(index) == '\'' && index + 2 < source.length && source.charAt(index + 2) == '\'') {
        out.append("   "); index += 3
      } else {
        out.append(source.charAt(index)); index += 1
      }
    }
    out.toString
  }

  private def sources(directory: File): Seq[File] =
    Option(directory.listFiles()).toSeq.flatten.flatMap { file =>
      if (file.isDirectory) { if (file.getName == "target" || file.getName.startsWith(".")) Nil else sources(file) }
      else if (file.getName.endsWith(".scala")) Seq(file)
      else Nil
    }

  /** Known-bad and known-good snippets; the gate refuses to run if its matcher misclassifies any of them. */
  def selfTest(): Unit = {
    val bad = Seq(
      "object A:\n  def f(p: SpatialPullback[Frame[D3], Frame[D3]]): Unit = ()\n",
      "object A:\n  def f: SpatialMap[\n    Frame[D3],\n    Frame[D3],\n    D3\n  ] = ???\n",
      "object A:\n  def g(x: Grid[Frame[D3], D3]): Int = 0\n",
      "object A:\n  val s: SampleSpace[ Frame[ D3 ] , D3] = ???\n",
      "object A:\n  def box: WorldBox[Frame[D3]] = ???\n",
      "object A:\n  def m: FrameErasedMap[D3] = ???\n",
      "object A:\n  def h(p: Point[Frame[D3], D3]): Unit = ()\n",
      "object A:\n  private[scalafim] def f(p: GridSpec[Frame[D3]]): Unit = ()\n",
      "object A:\n  // private helper below\n  def f(p: GridSpec[Frame[D3]]): Unit = ()\n",
      "final class C(val pullback: SpatialPullback[Frame[D3], Frame[D3]])\n",
      "object A:\n  private def helper = 1\n  @targetName(\"f\") def f(p: GridSpec[Frame[D3]]): Unit = ()\n",
      "object A:\n  private val x = 1\n  @inline def f(p: GridSpec[Frame[D3]]): Unit = ()\n",
      "enum E:\n  case C(p: GridSpec[Frame[D3]])\n",
      "object A:\n  def f(p: GridSpec[image4s.geometry.Frame[D3]]): Unit = ()\n",
      "object A:\n  type AnyFrame = Frame[D3]\n",
      "object A:\n  def a: FrameAlignment[D3, Frame[D3], Frame[D3]] = ???\n",
      "object A:\n  private def g(\n      x: Int\n  ): Int = x\n  def f(\n      p: Grid[Frame[D3], D3]\n  ): Unit = ()\n"
    )
    val good = Seq(
      "object A:\n  private def f(p: SpatialPullback[Frame[D3], Frame[D3]]): Unit = ()\n",
      "object A:\n  private[this] val s: Grid[Frame[D3], D3] = ???\n",
      "object A:\n  /** SpatialPullback[Frame[D3], Frame[D3]] in a comment */\n  def f: Int = 0\n",
      "object A:\n  def f: String = \"GridSpec[Frame[D3]]\"\n",
      "object A:\n  def frame: Frame[D3] = ???\n  def g(x: GridSpec[?]): Int = 0\n  def h[F <: Frame[D3]](p: Point[F, D3]): Int = 0\n",
      "object A:\n  def f(): Unit =\n    val local: SampleSpace[Frame[D3], D3] = ???\n    ()\n",
      "object A:\n  private def f(\n      p: SpatialMap[\n        Frame[D3], Frame[D3], D3]\n  ): Unit = ()\n",
      "object A:\n  def f(\n      x: Int\n  ): Unit =\n    val local: SampleSpace[Frame[D3], D3] = ???\n    ()\n",
      "object A:\n  private def f(\n      x: Int\n  ): SampleSpace[Frame[D3], D3] = ???\n",
      "object A:\n  private type Erased = Frame[D3]\n  type Typed = GridSpec[?]\n",
      "object A:\n  def f(x: Int): Int = x match\n    case 1 => 2\n    case _ => 3\n",
      "object A:\n  given ord: Ordering[Int] with\n    def compare(a: Int, b: Int): Int = 0\n  private given g: Rebind[GridSpec] with\n    def f(p: GridSpec[Frame[D3]]): Unit = ()\n"
    )
    val missed = bad.filter(snippet => scan("bad.scala", snippet).isEmpty)
    val flagged = good.filter(snippet => scan("good.scala", snippet).nonEmpty)
    if (missed.nonEmpty || flagged.nonEmpty)
      sys.error(
        "frame-erasure gate self-test failed:\n" +
          missed.map(s => s"  not flagged: ${s.replace("\n", "\\n")}").mkString("\n") +
          flagged.map(s => s"  wrongly flagged: ${s.replace("\n", "\\n")}").mkString("\n")
      )
  }
}
