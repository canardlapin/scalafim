package scalafim.image

import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.FrameId
import image4s.geometry.Grid
import image4s.geometry.GridId
import image4s.geometry.LengthUnit
import image4s.locus.GridDomain
import locus4s.DomainRegistry
import locus4s.Region

class KMeansParcellationSuite extends munit.FunSuite:
  private val frame =
    right(
      Frame.persistentNamed[D3](
        right(FrameId.parse("kmeans-parcellation-suite-frame")),
        "k-means parcellation suite",
        LengthUnit.Millimeter,
        CoordinateConvention.RAS
      )
    )
  private val grid =
    right(
      Grid.createPersistent(
        right(GridId.parse("kmeans-parcellation-suite-grid")),
        frame
      )(Vector(4, 1, 1), Affine.identity[D3])
    )
  private val domainResolution =
    right(
      GridDomain.register(
        grid,
        "k-means voxels",
        DomainRegistry.empty
      )
    )
  private val domain = domainResolution.value
  private val active = Region.whole(domain.space)

  test("world-coordinate k-means returns one exact assignment"):
    val result =
      right(
        KMeans.partitionRegion(
          domain,
          active,
          k = 2,
          seed = 42,
          init = KMeans.Init.KMeansPlusPlus
        )
      )
    val partition = result.value

    assertEquals(partition.parcels.size, 2)
    assertEquals(partition.support, active)
    assert(result.iterations > 0)
    assertEquals(
      partition.parcelMetadata.toVector.map(_.label),
      Vector("Cluster_1", "Cluster_2")
    )
    val fibers = right(partition.fibers)
    assert(
      partition.parcels.indices.forall(parcel =>
        fibers.row(parcel).cardinality > 0
      )
    )

  test("one cluster is valid and has the whole active fiber"):
    val selected =
      right(Region.fromOrdinals(domain.space, Vector(1, 2)))
    val result =
      right(KMeans.partitionRegion(domain, selected, k = 1))
    val partition = result.value
    val only = partition.parcels.indexOption(0).get

    assertEquals(result.iterations, 0)
    assertEquals(
      partition.region(only).ordinalsInDomainOrder.toVector,
      Vector(1, 2)
    )

  test("invalid policy and foreign owners fail before fitting"):
    assert(
      KMeans.partitionRegion(domain, active, k = 0) match
        case Left(KMeansPartitionError.InvalidClusterCount(0, 4)) => true
        case _                                                    => false
    )
    assert(
      KMeans.partitionRegion(domain, active, k = 2, iterMax = 0) match
        case Left(KMeansPartitionError.InvalidMaximumIterations(0)) => true
        case _                                                       => false
    )

    val foreignResolution =
      right(
        GridDomain.register(
          grid,
          "foreign k-means voxels",
          DomainRegistry.empty
        )
      )
    val foreign = Region.whole(foreignResolution.value.space)
    assert(
      KMeans.partitionRegion(domain, foreign, k = 2) match
        case Left(KMeansPartitionError.WrongVoxelOwner(_)) => true
        case _                                             => false
    )

  private def right[E, A](value: Either[E, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, found Left($error)")
