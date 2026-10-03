package scalafim.fmri.mvpa.artifacts

import scala.util.control.NonFatal
import ujson.*

/** Platform-neutral v1 envelope validation. JVM persistence supplies physical
  * object verification; JS can still reject an unsupported document before any
  * scientific or numerical interpretation is attempted. */
private[artifacts] object PatternProfileMetadata:
  val Schema = "scalafim-mvpa-pattern-profile-v1"

  def document(content: Value): String =
    ujson.write(Obj("Schema" -> Schema, "Content" -> content), indent = 2) + "\n"

  def content(text: String, maximumBytes: Int): Either[PatternArchiveError, Value] =
    if text.getBytes("UTF-8").length > maximumBytes then Left(PatternArchiveError.Unsupported("metadata exceeds configured byte limit"))
    else
      try
        val root = ujson.read(text)
        if root("Schema").str != Schema then Left(PatternArchiveError.Unsupported("unsupported pattern profile schema"))
        else Right(root("Content"))
      catch case NonFatal(error) => Left(PatternArchiveError.Invalid(Option(error.getMessage).getOrElse("invalid pattern profile metadata")))

  /** There is no JS local-object adapter in v1, so inspection stays explicit. */
  def durableReadUnavailable: Either[PatternArchiveError, Nothing] =
    Left(PatternArchiveError.Unsupported("local verified-object pattern archive reading is JVM-only"))
