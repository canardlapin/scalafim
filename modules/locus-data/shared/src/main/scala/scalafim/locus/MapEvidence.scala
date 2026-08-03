package scalafim.locus

/** Compatibility constructors over the certified locus4s map wrappers.
  *
  * ScalaFIM does not maintain a second validation algebra. The core wrappers own
  * the proof and its error type.
  */
object Injection:
  def validate[X, Y](
      mapping: TotalMap[X, Y]
  ): Either[locus4s.CertifiedMapError, locus4s.Injection[X, Y]] =
    locus4s.Injection.fromTotalMap(mapping)

object Surjection:
  def validate[X, Y](
      mapping: TotalMap[X, Y]
  ): Either[locus4s.CertifiedMapError, locus4s.Surjection[X, Y]] =
    locus4s.Surjection.fromTotalMap(mapping)

object Bijection:
  def validate[X, Y](
      mapping: TotalMap[X, Y]
  ): Either[locus4s.CertifiedMapError, locus4s.Bijection[X, Y]] =
    locus4s.Bijection.fromTotalMap(mapping)
