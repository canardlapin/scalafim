package scalafim.fmri.design

object Names:

  def zeroPad(i: Int, nTotal: Int): String =
    require(i >= 0, "i must be >= 0")
    val logWidth =
      if nTotal < 1 then 1
      else math.ceil(math.log10(nTotal.toDouble + 1e-9)).toInt
    val width =
      if nTotal > 1 then math.max(2, logWidth)
      else logWidth
    val fmt = s"%0${width}d"
    fmt.format(i)

  def zeroPad(is: Seq[Int], nTotal: Int): Vector[String] =
    is.iterator.map(i => zeroPad(i, nTotal)).toVector

  /** Rough equivalent of R's `make.names(..., unique = FALSE)` used throughout the original package.
    *
    * The goal here is parity with the scalafim.fmri.design naming tests (not a fully general R port).
    */
  def sanitize(x: String, allowDot: Boolean = true): String =
    val replaced = x.map {
      case c if c.isLetterOrDigit || c == '.' || c == '_' => c
      case _                                              => '.'
    }.mkString

    val prefixed =
      if replaced.isEmpty then "X"
      else
        val first = replaced.head
        val needsPrefix =
          !(first.isLetter || first == '.') ||
            (first == '.' && replaced.length >= 2 && replaced.charAt(1).isDigit)
        if needsPrefix then s"X$replaced" else replaced

    if allowDot then prefixed
    else
      // Match the R helper: normalize separators and collapse sequences.
      val dotsToUnderscore = prefixed.replace('.', '_')
      val collapsed = dotsToUnderscore.replaceAll("_+", "_")
      collapsed.stripPrefix("_").stripSuffix("_")

  def sanitizeAll(xs: Seq[String], allowDot: Boolean = true): Vector[String] =
    xs.iterator.map(x => sanitize(x, allowDot)).toVector

  def sanitizeLevel(level: String): String =
    val sanitized = sanitize(level, allowDot = true)
    if level.nonEmpty && level.head.isDigit && sanitized.startsWith("X") then sanitized.drop(1) else sanitized

  def basisSuffix(j: Int, nbasis: Int): String =
    s"_b${zeroPad(j, nbasis)}"

  def featureSuffix(j: Int, nFeatures: Int): String =
    s"f${zeroPad(j, nFeatures)}"

  def levelToken(variable: String, level: String): String =
    val v = sanitize(variable, allowDot = true)
    val l = sanitizeLevel(level)
    s"$v.$l"

  def continuousToken(colName: String): String =
    sanitize(colName, allowDot = true)

  def makeCondTag(tokens: Seq[String]): String =
    tokens.mkString("_")

  def addBasis(condTags: Vector[String], nbasis: Int): Vector[String] =
    if nbasis <= 1 then condTags
    else
      val suffixes = (1 to nbasis).map(j => basisSuffix(j, nbasis)).toVector
      // Match R's `as.vector(outer(cond_tags, suffixes, paste0))` (column-major flatten).
      suffixes.flatMap(suf => condTags.map(_ + suf))

  def makeColumnNames(termTag: Option[String], condTags: Vector[String], nbasis: Int): Vector[String] =
    termTag.foreach(tag => require(!tag.contains("__"), "termTag must not contain double underscores"))
    val full = addBasis(condTags, nbasis)
    termTag match
      case None      => full
      case Some(tag) => full.map(ct => s"${tag}_$ct")

  /** Like R's `make.unique(tags, sep = "#")`, but avoids generating names that already exist in the
    * original input (so existing explicit `#n` tags stay untouched).
    */
  def makeUniqueTags(tags: Seq[String]): Vector[String] =
    val original = tags.toSet
    val used = scala.collection.mutable.HashSet.empty[String]

    def nextAvailable(base: String): String =
      var k = 1
      var cand = s"$base#$k"
      while used.contains(cand) || original.contains(cand) do
        k += 1
        cand = s"$base#$k"
      cand

    tags.iterator.map { t =>
      val out =
        if !used.contains(t) then t
        else nextAvailable(t)
      used.add(out)
      out
    }.toVector
