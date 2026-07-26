package scalafim.locus.laws

final case class LawFailure(law: String, detail: String)

object ReferenceLaws:
  def booleanAlgebra(
      size: Int,
      bounds: LawBounds = LawBounds()
  ): Either[EnumerationError, Vector[LawFailure]] =
    Exhaustive.regions(size, bounds).map: regions =>
      val empty = ReferenceRegion(size, Set.empty)
      val whole = ReferenceRegion(size, (0 until size).toSet)
      val failures = Vector.newBuilder[LawFailure]
      regions.foreach: a =>
        check("union-empty", a.union(empty) == a, a.toString, failures)
        check("intersect-whole", a.intersect(whole) == a, a.toString, failures)
        check("union-complement", a.union(a.complement) == whole, a.toString, failures)
        check("intersect-complement", a.intersect(a.complement) == empty, a.toString, failures)
        regions.foreach: b =>
          check(
            "subset-meet",
            a.subsetOf(b) == (a.intersect(b) == a),
            s"a=$a b=$b",
            failures
          )
          regions.foreach: c =>
            check(
              "distributivity",
              a.intersect(b.union(c)) == a.intersect(b).union(a.intersect(c)),
              s"a=$a b=$b c=$c",
              failures
            )
      failures.result()

  def relationCategory(
      size: Int,
      bounds: LawBounds = LawBounds()
  ): Either[EnumerationError, Vector[LawFailure]] =
    Exhaustive.relations(size, size, bounds).map: relations =>
      val identity = ReferenceRelation.identity(size)
      val failures = Vector.newBuilder[LawFailure]
      relations.foreach: first =>
        check("left-identity", identity.andThen(first) == first, first.toString, failures)
        check("right-identity", first.andThen(identity) == first, first.toString, failures)
        check("converse-involution", first.converse.converse == first, first.toString, failures)
        relations.foreach: second =>
          check(
            "dagger",
            first.andThen(second).converse == second.converse.andThen(first.converse),
            s"first=$first second=$second",
            failures
          )
          relations.foreach: third =>
            check(
              "associativity",
              first.andThen(second).andThen(third) == first.andThen(second.andThen(third)),
              s"first=$first second=$second third=$third",
              failures
            )
      failures.result()

  private def check(
      law: String,
      condition: Boolean,
      detail: => String,
      failures: scala.collection.mutable.Builder[LawFailure, Vector[LawFailure]]
  ): Unit =
    if !condition then failures += LawFailure(law, detail)
