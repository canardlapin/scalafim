package scalafim.atlas.scenarios

/** Scala.js reads no TemplateFlow cache: the template leg is always absent here. */
object TemplateLegPlatform:
  lazy val assets: TemplateLegAssets =
    TemplateLegAssets.Absent(Vector("TemplateFlow assets (Scala.js reads no local TemplateFlow cache)"))
