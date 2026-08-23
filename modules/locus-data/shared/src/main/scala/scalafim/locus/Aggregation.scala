package scalafim.locus

import cats.kernel.CommutativeMonoid
import locus4s.PartialSurjection
import locus4s.data.{Aggregation as LocusAggregation, Field}

object Aggregation:
  def foldMapBy[X, P, A, M](
      assignment: PartialSurjection[X, P],
      field: Field[X, A]
  )(
      contribution: A => M
  )(using monoid: CommutativeMonoid[M]): Either[SpaceMismatch, Field[P, M]] =
    LocusAggregation
      .foldMapByChecked(assignment, field)(monoid.empty)(contribution)(monoid.combine)
