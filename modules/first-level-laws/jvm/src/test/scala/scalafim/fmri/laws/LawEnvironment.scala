package scalafim.fmri.laws

private[laws] object LawEnvironment:
  def get(name: String): Option[String] = sys.env.get(name)
