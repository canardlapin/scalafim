package scalafim.fmri.mvpa

enum FeatureSetKind:
  case Region
  case Searchlight

final case class FeatureSetPlan private (
    name: String,
    kind: FeatureSetKind,
    featureSets: Vector[FeatureSet]
):
  require(name.nonEmpty, "feature set plan name must be non-empty")
  require(featureSets.nonEmpty, "feature set plan must be non-empty")

  def size: Int = featureSets.length

object FeatureSetPlan:
  def apply(
      name: String,
      kind: FeatureSetKind,
      featureSets: Seq[FeatureSet]
  ): Either[MvpaError, FeatureSetPlan] =
    val trimmed = name.trim
    val sets = featureSets.toVector
    if trimmed.isEmpty then Left(MvpaError.InvalidFeatureSetPlan("feature set plan name must be non-empty"))
    else if sets.isEmpty then Left(MvpaError.InvalidFeatureSetPlan("feature set plan must contain at least one feature set"))
    else if sets.map(_.id.value).distinct.length != sets.length then
      Left(MvpaError.InvalidFeatureSetPlan("feature set plan ids must be unique"))
    else Right(new FeatureSetPlan(trimmed, kind, sets))

  def regional(name: String, featureSets: Seq[FeatureSet]): Either[MvpaError, FeatureSetPlan] =
    FeatureSetPlan(name, FeatureSetKind.Region, featureSets)

  def searchlight(name: String, featureSets: Seq[FeatureSet]): Either[MvpaError, FeatureSetPlan] =
    FeatureSetPlan(name, FeatureSetKind.Searchlight, featureSets)

  def fromLabelVector(
      name: String,
      labels: IndexedSeq[Int],
      background: Set[Int] = Set(0)
  ): Either[MvpaError, FeatureSetPlan] =
    val regionIds = labels.filterNot(background.contains).distinct.sorted
    if regionIds.exists(_ < 0) then
      Left(MvpaError.InvalidFeatureSetPlan("regional labels must be non-negative after background removal"))
    else
      buildSets(regionIds) { regionId =>
        val indices = labels.indices.filter(index => labels(index) == regionId).toVector
        FeatureSet(RoiId(regionId), indices, label = Some(regionId.toString))
      }.flatMap(sets => regional(name, sets))

  def fromNeighborhoods(
      name: String,
      neighborhoods: Seq[(Int, Seq[Int])]
  ): Either[MvpaError, FeatureSetPlan] =
    buildSets(neighborhoods) { case (center, indices) =>
      if center < 0 then Left(MvpaError.InvalidFeatureSetPlan("searchlight centers must be non-negative"))
      else FeatureSet(RoiId(center), indices, center = Some(center), label = Some(center.toString))
    }.flatMap(sets => searchlight(name, sets))

  private def buildSets[A](
      items: Seq[A]
  )(build: A => Either[MvpaError, FeatureSet]): Either[MvpaError, Vector[FeatureSet]] =
    val out = Vector.newBuilder[FeatureSet]
    var error: MvpaError | Null = null
    val iterator = items.iterator
    while iterator.hasNext && error == null do
      build(iterator.next()) match
        case Right(featureSet) => out += featureSet
        case Left(e) => error = e
    error match
      case null => Right(out.result())
      case e => Left(e)
