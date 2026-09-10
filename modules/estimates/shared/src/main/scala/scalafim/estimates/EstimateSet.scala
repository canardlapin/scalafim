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
    degreesOfFreedom: Vector[DegreesOfFreedom] = Vector.empty
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
  require(statistics.map(_.product).distinct.size == statistics.size)
  require(products.filter(_.kind.isInstanceOf[ProductKind.Statistic]).forall(p => statistics.exists(_.product == p.id)))
  require(statistics.forall(s => products.exists(p => p.id == s.product && p.kind.isInstanceOf[ProductKind.Statistic]) &&
    s.effect.forall(id => products.exists(p => p.id == id && p.kind == ProductKind.Effect)) &&
    s.standardError.forall(id => products.exists(p => p.id == id && p.kind == ProductKind.StandardError))))

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
