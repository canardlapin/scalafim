package scalafim

package object locus:
  type SpaceKey = locus4s.DomainId
  type FiniteDomain[S] = locus4s.FiniteDomain[S]
  type FiniteSpace[S] = locus4s.FiniteSpace[S]
  type SomeFiniteDomain = locus4s.SomeFiniteDomain
  type Point[S] = locus4s.Index[S]
  type Region[S] = locus4s.Region[S]
  type Selection[S] = locus4s.Selection[S]
  type TotalMap[X, Y] = locus4s.TotalMap[X, Y]
  type PartialMap[X, Y] = locus4s.PartialMap[X, Y]
  type Injection[X, Y] = locus4s.Injection[X, Y]
  type Surjection[X, Y] = locus4s.Surjection[X, Y]
  type Bijection[X, Y] = locus4s.Bijection[X, Y]
  type Relation[X, Y] = locus4s.Relation[X, Y]
  type SpaceMismatch = locus4s.SpaceMismatch
  type PointError = locus4s.IndexError
  type RegionError = locus4s.RegionError
  type SelectionError = locus4s.SelectionError
  type TotalMapError = locus4s.TotalMapError
  type RelationError = locus4s.RelationError
  type MapEvidenceError = locus4s.CertifiedMapError
  type IndexedField[S, +A] = locus4s.data.VectorField[S, A]
  type IndexedFieldError = locus4s.data.FieldConstructionError
  type Section[S, +A] = locus4s.data.Section[S, A]
  type SectionLookupError = locus4s.data.SectionLookupError
  type SectionSelectionError = locus4s.data.SectionSelectionError

  val Region: locus4s.Region.type = locus4s.Region
  val Selection: locus4s.Selection.type = locus4s.Selection
  val TotalMap: locus4s.TotalMap.type = locus4s.TotalMap
  val PartialMap: locus4s.PartialMap.type = locus4s.PartialMap
  val Relation: locus4s.Relation.type = locus4s.Relation
  val SpaceMismatch: locus4s.SpaceMismatch.type = locus4s.SpaceMismatch
  val PointError: locus4s.IndexError.type = locus4s.IndexError
  val RegionError: locus4s.RegionError.type = locus4s.RegionError
  val SelectionError: locus4s.SelectionError.type = locus4s.SelectionError
  val TotalMapError: locus4s.TotalMapError.type = locus4s.TotalMapError
  val RelationError: locus4s.RelationError.type = locus4s.RelationError
  val IndexedField: locus4s.data.VectorField.type = locus4s.data.VectorField
  val IndexedFieldError: locus4s.data.FieldConstructionError.type =
    locus4s.data.FieldConstructionError
  val SectionLookupError: locus4s.data.SectionLookupError.type =
    locus4s.data.SectionLookupError
  val SectionSelectionError: locus4s.data.SectionSelectionError.type =
    locus4s.data.SectionSelectionError

  object SpaceKey:
    def make(value: String): Either[locus4s.DomainError, SpaceKey] =
      locus4s.DomainId.parse(value)

    def unsafe(value: String): SpaceKey =
      make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension [S](space: FiniteDomain[S])
    def sameIdentityAs[T](that: FiniteDomain[T]): Boolean =
      space.sameRuntimeOwnerAs(that)

    def requirePoint(ordinal: Int): Either[PointError, Point[S]] =
      space.index(ordinal)

  extension [X, Y](injection: Injection[X, Y])
    def mapping: TotalMap[X, Y] =
      injection.toTotalMap

  extension [X, Y](surjection: Surjection[X, Y])
    def mapping: TotalMap[X, Y] =
      surjection.toTotalMap

  extension [X, Y](bijection: Bijection[X, Y])
    def mapping: TotalMap[X, Y] =
      bijection.toTotalMap

  private[scalafim] def mismatch[A, B](
      expected: FiniteDomain[A],
      actual: FiniteDomain[B]
  ): SpaceMismatch =
    SpaceMismatch.between(expected, actual)
