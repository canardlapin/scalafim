package scalafim.phrfcmp.score

/** Which end of the scale is worse for a score. */
enum Worse:
  /** Errors (MISE): the worst value is the largest. */
  case Higher
  /** Scores (Fisher z): the worst value is the lowest. */
  case Lower

/** Result of worst-in-family imputation over the gating arms of one family on one dataset.
  *
  * `values(a)` holds arm `a`'s score at each retained voxel (observed where estimated, otherwise the worst observed
  * among the family's arms at that voxel). A voxel at which no arm produced an estimate is excluded. The counts and
  * the mask are sealed-side quantities: they never reach the whitelist (design 3.2, imputation channel).
  */
final class Imputed private[score] (
    private[phrfcmp] val values: Vector[Vector[Double]],
    private[phrfcmp] val completeCase: Vector[Boolean],
    private[phrfcmp] val imputedPerArm: Vector[Int],
    private[phrfcmp] val excluded: Int
):
  override def toString: String = "Imputed(<redacted>)"

object Imputation:
  /** `arms(a)(v)` is arm a's score at voxel v, or `None` when the arm refused or failed there. Imputation uses only
    * the arms passed in, so ablations, hybrids and secondary arms (not passed) can never change a score.
    */
  def worstInFamily(arms: Vector[Vector[Option[Double]]], worse: Worse): Imputed =
    val nv = if arms.isEmpty then 0 else arms.head.length
    require(arms.forall(_.length == nv), "arms must cover the same voxels")
    val out = Vector.fill(arms.length)(Vector.newBuilder[Double])
    val mask = Vector.newBuilder[Boolean]
    val imputed = new Array[Int](arms.length)
    var excluded = 0
    var v = 0
    while v < nv do
      val observed = arms.flatMap(_(v))
      if observed.isEmpty then excluded += 1
      else
        val w = worse match
          case Worse.Higher => observed.max
          case Worse.Lower  => observed.min
        mask += (observed.length == arms.length)
        var a = 0
        while a < arms.length do
          arms(a)(v) match
            case Some(x) => out(a) += x
            case None =>
              out(a) += w
              imputed(a) += 1
          a += 1
      v += 1
    new Imputed(out.map(_.result()), mask.result(), imputed.toVector, excluded)
