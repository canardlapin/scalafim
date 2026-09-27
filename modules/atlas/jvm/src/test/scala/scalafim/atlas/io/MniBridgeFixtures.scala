package scalafim.atlas.io

import java.nio.file.Path

import scalafim.atlas.{AtlasError, MniTemplateBridge, TemplateFlowXfm}
import scalafim.surface.io.TemplateFlowCache

/** The TemplateFlow MNI152NLin6Asym -> MNI152NLin2009cAsym bridge, loaded and inverted at most once per test run.
  *
  * Decoding the 200 MB composite takes seconds and qualifying its numerical inverse over a minute, so every suite that
  * needs them (the bridge's own suite, the surface chain scenario) shares these values.
  */
object MniBridgeFixtures:
  /** The composite's file in a local TemplateFlow cache, if any. */
  def located: Option[Path] = TemplateFlowCache.locate(TemplateFlowXfm.Mni6ToMni2009c.relativePath)

  /** The admitted composite; `None` when no cache holds it. */
  lazy val bridge: Option[Either[AtlasError, MniTemplateBridge]] = located.map(MniTemplateBridgeFiles.load)

  /** The composite with the default qualified numerical inverse attached (its forward map). */
  lazy val inverted: Option[Either[AtlasError, MniTemplateBridge]] =
    bridge.map: loaded =>
      val started = System.nanoTime()
      val result = loaded.flatMap(_.withNumericalInverse())
      result.foreach: value =>
        println(f"numerical inverse on MNI152NLin6Asym res-01: ${(System.nanoTime() - started) / 1e9}%.1f s; ${value.transform.provenance.describe}")
      result
