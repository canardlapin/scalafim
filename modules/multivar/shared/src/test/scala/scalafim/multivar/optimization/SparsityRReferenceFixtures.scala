package scalafim.multivar
package optimization


/** Frozen outputs produced with base R arithmetic from the definitions used by
  * `soft_threshold`, row/group shrinkage, and simplex projection. The fixture is
  * intentionally tiny enough to audit by hand.
  */
object SparsityRReferenceFixtures:
  val coefficients: Vector[Vector[Double]] =
    Vector(Vector(3.0, -1.0), Vector(0.5, -0.25), Vector(-4.0, 2.0))

  val l1AtOne: Vector[Vector[Double]] =
    Vector(Vector(2.0, 0.0), Vector(0.0, 0.0), Vector(-3.0, 1.0))

  val rowL21AtOne: Vector[Vector[Double]] =
    Vector(
      Vector(2.051316701949486, -0.683772233983162),
      Vector(0.0, 0.0),
      Vector(-3.1055728090000843, 1.5527864045000421)
    )

  val simplexInput: Vector[Vector[Double]] =
    Vector(Vector(0.2, 0.5), Vector(-0.1, 0.5), Vector(2.0, 0.5))

  val simplexProjection: Vector[Vector[Double]] =
    Vector(
      Vector(0.0, 1.0 / 3.0),
      Vector(0.0, 1.0 / 3.0),
      Vector(1.0, 1.0 / 3.0)
    )

  val monotoneInput: Vector[Vector[Double]] =
    Vector(Vector(3.0), Vector(1.0), Vector(2.0))

  val monotoneProjection: Vector[Vector[Double]] =
    Vector(Vector(2.0), Vector(2.0), Vector(2.0))
