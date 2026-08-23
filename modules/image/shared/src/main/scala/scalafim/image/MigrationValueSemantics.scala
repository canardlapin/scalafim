package scalafim.image

import image4s.Categorical
import image4s.Continuous
import image4s.Mask
import image4s.ValueSemantics

/** Temporary output-semantics policy for code still using the pre-native
  * generic image builders.
  *
  * Native APIs state their semantic tag directly. This policy exists only so
  * the coordinated source migration can remove `ScalaFimValues` before every
  * producer has acquired a precise result type. It is deliberately closed:
  * an unconstrained `A` gets no universal image semantics.
  */
sealed trait MigrationValueSemantics[A]:
  type Sem
  def evidence: ValueSemantics[A, Sem]

object MigrationValueSemantics:
  type Aux[A, S] = MigrationValueSemantics[A] { type Sem = S }

  given doubleContinuous: MigrationValueSemantics[Double] with
    type Sem = Continuous
    val evidence: ValueSemantics[Double, Continuous] = summon

  given floatContinuous: MigrationValueSemantics[Float] with
    type Sem = Continuous
    val evidence: ValueSemantics[Float, Continuous] = summon

  given booleanMask: MigrationValueSemantics[Boolean] with
    type Sem = Mask
    val evidence: ValueSemantics[Boolean, Mask] = summon

  given intCategorical: MigrationValueSemantics[Int] with
    type Sem = Categorical
    val evidence: ValueSemantics[Int, Categorical] = summon
