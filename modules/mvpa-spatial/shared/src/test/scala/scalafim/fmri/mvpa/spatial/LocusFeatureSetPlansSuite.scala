package scalafim.fmri.mvpa.spatial

import locus4s.{DomainRegistry, PartialSurjection}
import scalafim.fmri.mvpa.{FeatureSetKind, RoiId}
import scalafim.locus.{
  CenteredSearchlight,
  DomainFactory,
  FiniteSpace,
  Region,
  Relation,
  Searchlight,
  Selection,
  SpaceKey
}

class LocusFeatureSetPlansSuite extends munit.FunSuite:

  private val voxelResolution =
    DomainFactory.unsafeRestore(
      DomainRegistry.empty,
      SpaceKey.unsafe("mvpa-voxels"),
      5
    )
  private type Voxel = voxelResolution.S
  private val voxels: FiniteSpace[Voxel] = voxelResolution.space

  private val parcelResolution =
    DomainFactory.unsafeRestore(
      voxelResolution.registry,
      SpaceKey.unsafe("mvpa-parcels"),
      2
    )
  private type Parcel = parcelResolution.S
  private val parcels: FiniteSpace[Parcel] = parcelResolution.space

  test("regions use ambient order while selections preserve explicit order"):
    val region = Region.fromOrdinals(voxels, Vector(4, 1, 3)).toOption.get
    val selection = Selection.fromOrdinals(voxels, Vector(4, 1, 3)).toOption.get

    val regionSet =
      LocusFeatureSetPlans
        .fromRegion(RoiId(7), region, Some("region"))
        .toOption
        .get
    val selectionSet =
      LocusFeatureSetPlans
        .fromSelection(RoiId(8), selection, Some("selection"))
        .toOption
        .get

    assertEquals(
      regionSet.featureIndices.map(_.value),
      Vector(1, 3, 4)
    )
    assertEquals(
      selectionSet.featureIndices.map(_.value),
      Vector(4, 1, 3)
    )

  test("parcel assignments become regional plans from one materialized fiber relation"):
    val assignment =
      PartialSurjection
        .fromOptionalTargetOrdinals(
          voxels,
          parcels,
          Vector(Some(0), None, Some(1), Some(0), Some(1))
        )
        .toOption
        .get
    val plan =
      LocusFeatureSetPlans
        .fromParcelAssignment(
          "parcels",
          assignment,
          parcel => Some(s"parcel-${parcel.value}")
        )
        .toOption
        .get

    assertEquals(plan.kind, FeatureSetKind.Region)
    assertEquals(plan.featureSets.map(_.id.value), Vector(0, 1))
    assertEquals(
      plan.featureSets.map(_.featureIndices.map(_.value)),
      Vector(Vector(0, 3), Vector(2, 4))
    )

  test("all-parcel performance court stays O(|X| + |P|) through one fiber relation"):
    val sourceCount = 50000
    val parcelCount = 2000
    val sourceResolution =
      DomainFactory.unsafeEphemeral("mvpa-performance-sources", sourceCount)
    type Source = sourceResolution.S
    val sources = sourceResolution.value
    val targetResolution =
      DomainFactory.unsafeEphemeral("mvpa-performance-parcels", parcelCount)
    type Target = targetResolution.S
    val targets = targetResolution.value
    val assignment =
      PartialSurjection
        .fromOptionalTargetOrdinals(
          sources,
          targets,
          Vector.tabulate(sourceCount)(source => Some(source % parcelCount))
        )
        .toOption
        .get
    var labelCalls = 0

    val plan =
      LocusFeatureSetPlans
        .fromParcelAssignment(
          "performance-court",
          assignment,
          target =>
            labelCalls += 1
            Some(s"parcel-${target.value}")
        )
        .toOption
        .get

    assertEquals(plan.featureSets.length, parcelCount)
    assertEquals(plan.featureSets.iterator.map(_.featureIndices.length).sum, sourceCount)
    assertEquals(labelCalls, parcelCount)
    assertEquals(
      plan.featureSets.head.featureIndices.map(_.value).take(3),
      Vector(0, 2000, 4000)
    )

  test("centered locus searchlights become searchlight plans without geometry"):
    val centers = Region.fromOrdinals(voxels, Vector(2, 0)).toOption.get
    val relation =
      Relation
        .fromOrdinalRows(
          voxels,
          voxels,
          Array(
            Array(0, 1),
            Array.emptyIntArray,
            Array(2, 3, 4),
            Array.emptyIntArray,
            Array.emptyIntArray
          ).iterator.map(_.iterator)
        )
        .toOption
        .get
    val searchlight = Searchlight.make(centers, relation).toOption.get
    val centered = CenteredSearchlight.validate(searchlight).toOption.get
    val plan =
      LocusFeatureSetPlans
        .fromSearchlight(
          "searchlights",
          centered,
          center => Some(s"center-${center.value}")
        )
        .toOption
        .get

    assertEquals(plan.kind, FeatureSetKind.Searchlight)
    assertEquals(plan.featureSets.map(_.id.value), Vector(0, 2))
    assertEquals(
      plan.featureSets.map(_.featureIndices.map(_.value)),
      Vector(Vector(0, 1), Vector(2, 3, 4))
    )
    assertEquals(
      plan.featureSets.map(_.center.map(_.value)),
      Vector(Some(0), Some(2))
    )

  test("center inclusion is established before the MVPA adapter"):
    val centers = Region.fromOrdinals(voxels, Vector(0)).toOption.get
    val relation =
      Relation
        .fromOrdinalRows(
          voxels,
          voxels,
          Array(
            Array(1),
            Array.emptyIntArray,
            Array.emptyIntArray,
            Array.emptyIntArray,
            Array.emptyIntArray
          ).iterator.map(_.iterator)
        )
        .toOption
        .get
    val searchlight = Searchlight.make(centers, relation).toOption.get

    assertEquals(
      CenteredSearchlight.validate(searchlight).left.toOption.map(_.message),
      Some("searchlight neighborhood at 0 does not contain its center")
    )
