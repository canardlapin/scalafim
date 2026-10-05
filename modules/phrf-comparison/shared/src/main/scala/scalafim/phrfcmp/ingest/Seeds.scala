package scalafim.phrfcmp.ingest

/**
  * Stream-seed derivation, a bit-exact port of `phrf_gen/seeds.py`: SplitMix64 over
  * `root ^ k` for k in (fnv1a64(cell id), dataset index, purpose index), on unsigned 64-bit `Long`s.
  */
object Seeds:

  /** The four purposes the generator records in its manifest, with their indices in `seeds.py` PURPOSES. */
  val ManifestPurposes: Vector[(String, Int)] = Vector("design" -> 0, "truth" -> 1, "noise" -> 2, "nullcal" -> 3)

  /** Protocol section 8 "Denylisted seeds" (v0a). Callers pin the denylist they expect; this is the frozen value. */
  val ProtocolDenylist: Set[Long] =
    Set(7000930101L, 7000930201L, 7000930102L, 7000930103L, 101L, 102L, 103L, 20260909L, 20260910L, 20260911L, 11L)

  private val Gamma = 0x9e3779b97f4a7c15L

  def splitmix64(x: Long): Long =
    var z = x + Gamma
    z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
    z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
    z ^ (z >>> 31)

  def fnv1a64(s: String): Long =
    var h = 0xcbf29ce484222325L
    s.getBytes("UTF-8").foreach { b =>
      h = (h ^ (b & 0xff).toLong) * 0x100000001b3L
    }
    h

  def streamSeed(root: Long, cellId: String, dataset: Int, purposeIndex: Int): Long =
    var s = root
    s = splitmix64(s ^ fnv1a64(cellId))
    s = splitmix64(s ^ dataset.toLong)
    splitmix64(s ^ purposeIndex.toLong)

  /** True if the 64-bit seed, or its low 32 bits, is a denylisted value. */
  def denylistHit(seed: Long, denylist: Set[Long]): Boolean =
    denylist.contains(seed) || denylist.contains(seed & 0xffffffffL)

  def unsigned(seed: Long): String = java.lang.Long.toUnsignedString(seed)

  /** Exact decimal digit strings of the `streams` and `stream_denylist_check` seeds, scanned from raw JSON text. */
  final case class RawSeeds(streams: Map[String, String], checkSeeds: Map[String, String], checkHits: Map[String, Boolean])

  /** Never goes through `Double`: JSON integers above 2^53 would be rounded. Returns a detail on malformed input. */
  def scanRaw(json: String): Either[String, RawSeeds] =
    val names = ManifestPurposes.map(_._1)
    for
      sb <- block(json, "streams")
      cb <- block(json, "stream_denylist_check")
      streams <- traverse(names)(p => number(sb, p).map(p -> _))
      checks <- traverse(names)(p => inner(cb, p).flatMap(seedAndHit).map(p -> _))
    yield RawSeeds(
      streams.toMap,
      checks.map((p, x) => p -> x._1).toMap,
      checks.map((p, x) => p -> (x._2 == "true")).toMap
    )

  private def traverse[A](xs: Vector[String])(f: String => Either[String, A]): Either[String, Vector[A]] =
    xs.foldLeft[Either[String, Vector[A]]](Right(Vector.empty))((acc, k) => for v <- acc; x <- f(k) yield v :+ x)

  private def number(b: String, key: String): Either[String, String] =
    val re = ("\"" + key + "\"\\s*:\\s*(0|[1-9][0-9]*)\\s*(?:[,}]|$)").r
    re.findFirstMatchIn(b).map(_.group(1)).toRight(s"no exact integer for '$key'")

  private def inner(b: String, key: String): Either[String, String] =
    val re = ("\"" + key + "\"\\s*:\\s*\\{([^{}]*)\\}").r
    re.findFirstMatchIn(b).map(_.group(1)).toRight(s"no object for '$key'")

  private def seedAndHit(b: String): Either[String, (String, String)] =
    val s = "\"seed\"\\s*:\\s*(0|[1-9][0-9]*)\\s*(?:,|$)".r.findFirstMatchIn(b).map(_.group(1))
    val h = "\"hit\"\\s*:\\s*(true|false)\\s*(?:,|$)".r.findFirstMatchIn(b).map(_.group(1))
    (s, h) match
      case (Some(a), Some(c)) => Right((a, c))
      case _                  => Left("stream check entry lacks an exact seed or a hit flag")

  /** The `{...}` block following a unique top-level-looking `"key"`; refuses duplicates. */
  private def block(json: String, key: String): Either[String, String] =
    val needle = "\"" + key + "\""
    val first = json.indexOf(needle)
    if first < 0 then Left(s"no '$key' member")
    else if json.indexOf(needle, first + 1) >= 0 then Left(s"duplicate '$key' member")
    else
      val open = json.indexOf('{', first)
      if open < 0 then Left(s"'$key' is not an object")
      else
        var depth = 0
        var i = open
        var end = -1
        while end < 0 && i < json.length do
          json.charAt(i) match
            case '{' => depth += 1
            case '}' =>
              depth -= 1
              if depth == 0 then end = i
            case _ =>
          i += 1
        if end < 0 then Left(s"unterminated '$key' object") else Right(json.substring(open + 1, end))
