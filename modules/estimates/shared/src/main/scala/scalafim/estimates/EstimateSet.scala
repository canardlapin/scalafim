package scalafim.estimates

final case class EstimateUnit(
    dataset: DatasetId,
    unit: UnitId,
    revision: UnitRevisionId,
    catalog: EstimandCatalog,
    domain: EstimateDomain,
    observations: Vector[Observation],
    bindings: Vector[EstimandBinding],
    products: Vector[ProductDescriptor],
    outcomes: Map[ProductId, ProductOutcome],
    estimability: EstimabilityEvidence,
    provenance: EstimateProvenance,
    covariance: Vector[CovarianceDescriptor] = Vector.empty,
    statistics: Vector[StatisticSemantics] = Vector.empty,
    degreesOfFreedom: Vector[DegreesOfFreedom] = Vector.empty,
    marginalUncertainty: Vector[MarginalUncertaintyDescriptor] = Vector.empty
):
  require(Invariants.unique(observations.map(_.id)))
  require(observations.forall(_.participant.dataset == dataset))
  require(Invariants.unique(products.map(_.id)))
  require(products.exists(p => p.kind == ProductKind.Effect || p.kind.isInstanceOf[ProductKind.Statistic]))
  require(bindings.map(_.estimand).distinct.size == bindings.size)
  require(bindings.forall(b => catalog.entry(b.estimand).nonEmpty))
  require(products.forall(p => p.observations.forall(id => observations.exists(_.id == id))))
  require(products.forall(p => p.targets.estimands.forall(id => catalog.entry(id).exists(_.unitScope.forall(_ == unit)))))
  require(products.forall: p =>
    val hypothesis = p.kind.isInstanceOf[ProductKind.Statistic]
    p.targets.estimands.forall(id => !hypothesis || catalog.entry(id).exists(_.kind == EstimandKind.Hypothesis))
  )
  require(outcomes.values.forall(_.valid) && estimability.valid)
  require(outcomes.forall:
    case (requested, ProductOutcome.Available(id)) => requested == id && products.exists(_.id == id)
    case _ => true
  )
  require(products.forall(p => outcomes.get(p.id).contains(ProductOutcome.Available(p.id))))
  require(covariance.map(_.product).distinct.size == covariance.size)
  require(covariance.forall: c =>
    val covarianceProduct = products.find(_.id == c.product)
    val effect = products.find(_.id == c.effects)
    covarianceProduct.exists(_.kind == ProductKind.Covariance) && effect.exists(_.kind == ProductKind.Effect) &&
      covarianceProduct.get.targets.estimands == effect.get.targets.estimands &&
      covarianceProduct.get.observations == effect.get.observations && covarianceProduct.get.pooling == effect.get.pooling &&
      (c.equation match
        case CovarianceEquation.Absolute => true
        case CovarianceEquation.Normalized(scale) => products.exists(p => p.id == scale && p.kind == ProductKind.ResidualVariance &&
          p.observations == covarianceProduct.get.observations && p.targets.estimands == covarianceProduct.get.targets.estimands && p.pooling == covarianceProduct.get.pooling))
  )
  require(products.filter(_.kind == ProductKind.Covariance).forall(p => covariance.exists(_.product == p.id)))
  require(degreesOfFreedom.forall:
    case DegreesOfFreedom(_, DfValue.Product(id), _, _) =>
      products.exists(p => p.id == id && p.kind == ProductKind.DegreesOfFreedomValues)
    case _ => true
  )
  require(statistics.map(_.product).distinct.size == statistics.size)
  require(products.filter(_.kind.isInstanceOf[ProductKind.Statistic]).forall(p => statistics.exists(_.product == p.id)))
  require(statistics.forall: semantics =>
    products.find(_.id == semantics.product).exists: statistic =>
      def referenceDf(df: DegreesOfFreedom): Boolean = df.value match
        case DfValue.Scalar(value) => value.isFinite && value > 0.0
        case DfValue.Product(id) => products.exists(p => p.id == id && p.kind == ProductKind.DegreesOfFreedomValues &&
          p.observations == statistic.observations && p.targets == statistic.targets && p.pooling == statistic.pooling)
        case _ => false
      def linkValid(link: StatisticProductLink, kind: ProductKind): Boolean =
        products.find(_.id == link.product).exists: target =>
          target.kind == kind && target.observations == statistic.observations && target.pooling == statistic.pooling &&
            (link.correspondence match
              case StatisticCorrespondence.Unknown(_) => true
              case StatisticCorrespondence.Known(mapping) =>
                mapping.map(_.hypothesis).toSet == statistic.targets.estimands.toSet &&
                  mapping.forall(entry => entry.targets.forall(target.targets.estimands.contains)))
      def known(link: StatisticProductLink): Option[Map[EstimandId, Set[EstimandId]]] = link.correspondence match
        case StatisticCorrespondence.Known(mapping) => Some(mapping.map(entry => entry.hypothesis -> entry.targets.toSet).toMap)
        case StatisticCorrespondence.Unknown(_) => None
      val distributionValid = semantics.distribution match
        case ReferenceDistribution.StudentT(df) => referenceDf(df)
        case ReferenceDistribution.FisherF(numerator, denominator) => referenceDf(numerator) && referenceDf(denominator)
        case _ => true
      val distributionMatchesKind = (statistic.kind, semantics.distribution) match
        case (ProductKind.Statistic(_), ReferenceDistribution.Unknown(_)) => true
        case (ProductKind.Statistic(StatisticKind.T), ReferenceDistribution.StudentT(_)) => true
        case (ProductKind.Statistic(StatisticKind.F), ReferenceDistribution.FisherF(_, _)) => true
        case (ProductKind.Statistic(StatisticKind.Z), ReferenceDistribution.Normal) => true
        case (ProductKind.Statistic(StatisticKind.P), _) => true
        case _ => false
      val linksAgree = (semantics.effect.flatMap(known), semantics.standardError.flatMap(known)) match
        case (Some(effects), Some(errors)) =>
          effects == errors && semantics.effect.flatMap(link => products.find(_.id == link.product)).exists: effect =>
            semantics.standardError.flatMap(link => products.find(_.id == link.product)).exists(_.units == effect.units)
        case _ => true
      distributionMatchesKind && distributionValid &&
        semantics.effect.forall(linkValid(_, ProductKind.Effect)) &&
        semantics.standardError.forall(linkValid(_, ProductKind.StandardError)) && linksAgree
  )
  require(marginalUncertainty.map(_.product).distinct.size == marginalUncertainty.size)
  require(marginalUncertainty.forall: descriptor =>
    val uncertainty = products.find(_.id == descriptor.product)
    val effect = products.find(_.id == descriptor.effects)
    val aligned = uncertainty.exists(p => (p.kind == ProductKind.StandardError || p.kind == ProductKind.Variance) &&
      effect.exists(e => e.kind == ProductKind.Effect && p.observations == e.observations &&
        p.targets == e.targets && p.pooling == e.pooling))
    val validDf = descriptor.origin match
      case MarginalVarianceOrigin.Estimated(df) =>
        degreesOfFreedom.contains(df) && (df.value match
          case DfValue.Product(id) =>
            products.find(_.id == id).exists(p => p.kind == ProductKind.DegreesOfFreedomValues &&
              uncertainty.exists(u => p.observations == u.observations && p.targets == u.targets && p.pooling == u.pooling))
          case DfValue.Scalar(_) => true
          case _ => false)
      case _ => true
    aligned && validDf
  )

final case class PinnedUnit(unit: UnitId, revision: UnitRevisionId, manifest: FileReference)

enum UnitOutcome:
  case Published(reference: PinnedUnit)
  case Failed(reason: String)
  case Missing(reason: String)

/** Intended membership survives partial execution; coverage never drops failures. */
final case class EstimateCollection(
    dataset: DatasetId,
    revision: CollectionRevisionId,
    model: ModelRevisionId,
    units: Map[UnitId, UnitOutcome]
):
  require(units.nonEmpty)
  require(units.forall:
    case (id, UnitOutcome.Published(ref)) => id == ref.unit
    case (_, UnitOutcome.Failed(reason)) => Invariants.text(reason)
    case (_, UnitOutcome.Missing(reason)) => Invariants.text(reason)
  )
  def unitsPublished: Boolean = units.values.forall(_.isInstanceOf[UnitOutcome.Published])

final case class PinnedEstimateSet(revision: CollectionRevisionId, manifest: FileReference)
