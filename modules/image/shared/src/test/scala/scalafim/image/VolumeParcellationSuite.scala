package scalafim.image

import image4s.Axis
import image4s.AxisKind
import image4s.NonSpatialAxes
import image4s.SampleSpace
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
import locus4s.FiniteDomain
import locus4s.PartialSurjection
import locus4s.Region
import locus4s.Relation
import locus4s.data.VectorField
import ravel.DType.given
import ravel.NDArray
import ravel.Shape
import spire.std.double.given

class VolumeParcellationSuite extends munit.FunSuite:
  private val frame =
    right(
      Frame.persistentNamed[D3](
        right(FrameId.parse("volume-parcellation-suite-frame")),
        "volume parcellation suite",
        LengthUnit.Millimeter,
        CoordinateConvention.RAS
      )
    )
  private val affine =
    right(
      Affine.fromRowMajor[D3](
        Vector(
          2.0,
          0.0,
          0.0,
          10.0,
          0.0,
          3.0,
          0.0,
          20.0,
          0.0,
          0.0,
          4.0,
          30.0,
          0.0,
          0.0,
          0.0,
          1.0
        )
      )
    )
  private val grid =
    right(
      Grid.createPersistent(
        right(GridId.parse("volume-parcellation-suite-grid")),
        frame
      )(Vector(2, 2, 1), affine)
    )
  private val domainResolution =
    right(
      GridDomain.register(
        grid,
        "volume parcellation voxels",
        DomainRegistry.empty
      )
    )
  private type Voxel = domainResolution.S
  private val domain = domainResolution.value
  private val assignments =
    Vector(Some(0), Some(0), None, Some(1))
  private val parcellationResolution =
    right(
      VolumeParcellation.resolve(
        domain,
        "volume parcellation parcels",
        Vector("anterior", "posterior"),
        assignments
      )
    )
  private type Parcel = parcellationResolution.P
  private val parcellation = parcellationResolution.value
  private val time = right(Axis.create("time", 3, AxisKind.Time))
  private val seriesSpace =
    SampleSpace.create(
      grid,
      right(NonSpatialAxes.from(Vector(time)))
    )
  private val series =
    right(
      NeuroSeries.continuous(
        seriesSpace,
        NDArray.tabulate[Double](2, 2, 1, 3): (x, y, _, t) =>
          100.0 * x + 10.0 * y + t
      )
    )

  test("one partial surjection is the exact support and fiber truth"):
    assert(parcellation.domain eq domain)
    assertEquals(
      parcellation.support.ordinalsInDomainOrder.toVector,
      Vector(0, 1, 3)
    )
    val first = parcellation.parcels.indexOption(0).get
    val second = parcellation.parcels.indexOption(1).get
    assertEquals(
      parcellation.region(first).ordinalsInDomainOrder.toVector,
      Vector(0, 1)
    )
    assertEquals(
      parcellation.region(second).ordinalsInDomainOrder.toVector,
      Vector(3)
    )
    assertEquals(parcellation.metadataAt(first), "anterior")
    assertEquals(parcellation.metadataAt(second), "posterior")
    assertEquals(
      right(parcellation.fibers).row(first),
      parcellation.region(first)
    )

    val labels =
      right(
        VectorField.fromValues(
          parcellation.parcels,
          Vector(7, 9)
        )
      )
    val dense = right(parcellation.renderCategorical(labels, 0))
    assertEquals(dense.data.shape, Shape(2, 2, 1))
    assertEquals(dense.data.iterator.toVector, Vector(7, 7, 0, 9))

  test("parcel series are parcel-major with contiguous time rows"):
    val reduced = right(ParcelSeries.reduceMean(series, parcellation))
    assert(reduced.parcellation eq parcellation)
    assertEquals(reduced.data.shape, Shape(2, 3))
    assertEquals(
      reduced.data.iterator.toVector,
      Vector(5.0, 6.0, 7.0, 110.0, 111.0, 112.0)
    )

    val first = parcellation.parcels.indexOption(0).get
    val firstSeries = reduced.seriesAt(first)
    assert(firstSeries.isContiguous)
    assertEquals(firstSeries.iterator.toVector, Vector(5.0, 6.0, 7.0))
    assertEquals(
      right(reduced.fieldAt(1)).toVector,
      Vector(6.0, 111.0)
    )

    val dense = right(reduced.toDense(-1.0))
    assertEquals(
      dense.data.iterator.toVector,
      Vector(
        5.0,
        6.0,
        7.0,
        5.0,
        6.0,
        7.0,
        -1.0,
        -1.0,
        -1.0,
        110.0,
        111.0,
        112.0
      )
    )

  test("restricted reduction requires an explicit empty-parcel policy"):
    val firstVoxelOnly =
      right(Region.fromOrdinals(domain.space, Vector(0)))

    ParcelSeries.reduceMean(
      series,
      parcellation,
      firstVoxelOnly,
      EmptyParcelPolicy.Reject
    ) match
      case Left(ParcelSeriesError.EmptyParcel(1)) => ()
      case other => fail(s"expected empty parcel 1, found $other")

    val filled =
      right(
        ParcelSeries.reduceMean(
          series,
          parcellation,
          firstVoxelOnly,
          EmptyParcelPolicy.Fill(-99.0)
        )
      )
    assertEquals(
      filled.data.iterator.toVector,
      Vector(0.0, 1.0, 2.0, -99.0, -99.0, -99.0)
    )

  test("dynamic boundaries reject foreign voxel, parcel, and support owners"):
    val foreignVoxelResolution =
      right(
        GridDomain.register(
          grid,
          "foreign parcellation voxels",
          DomainRegistry.empty
        )
      )
    val foreignAssignment =
      right(
        PartialSurjection.fromOptionalTargetOrdinals(
          foreignVoxelResolution.value.space,
          parcellation.parcels,
          assignments
        )
      )
    assert(
      VolumeParcellation.create(
        domain,
        foreignAssignment,
        parcellation.parcelMetadata
      ) match
        case Left(VolumeParcellationError.WrongVoxelOwner(_)) => true
        case _                                                => false
    )

    val foreignParcelResolution =
      right(FiniteDomain.ephemeral("foreign parcels", 2))
    val foreignLabels =
      right(
        VectorField.fromValues(
          foreignParcelResolution.value,
          Vector(7, 9)
        )
      )
    assert(
      parcellation.renderCategorical(foreignLabels, 0) match
        case Left(VolumeParcellationError.WrongParcelOwner(_)) => true
        case _                                                 => false
    )

    val foreignSupport =
      right(
        Region.fromOrdinals(
          foreignVoxelResolution.value.space,
          Vector(0)
        )
      )
    assert(
      ParcelSeries.reduceMean(
        series,
        parcellation,
        foreignSupport,
        EmptyParcelPolicy.Reject
      ) match
        case Left(ParcelSeriesError.WrongSupport(_)) => true
        case _                                       => false
    )

  test("grid and world centroids use the complete affine"):
    val gridCentroids =
      right(
        VolumeParcellationGeometry.centroids(
          parcellation,
          frame = SpatialCoordinateFrame.Grid
        )
      ).toVector
    val worldCentroids =
      right(
        VolumeParcellationGeometry.centroids(
          parcellation,
          frame = SpatialCoordinateFrame.World
        )
      ).toVector

    assertEquals(gridCentroids, Vector(Vector(0.0, 0.5, 0.0), Vector(1.0, 1.0, 0.0)))
    assertEquals(worldCentroids, Vector(Vector(10.0, 21.5, 30.0), Vector(12.0, 23.0, 30.0)))

  test("parcel neighborhoods are centered exact relations"):
    val nearestOne =
      right(
        ExactParcelSearchlight.nearest(
          parcellation,
          1,
          SpatialCoordinateFrame.World
        )
      )
    assertEquals(nearestOne.relation, Relation.identity(parcellation.parcels))

    val radiusBelow =
      right(
        ExactParcelSearchlight.withinRadius(
          parcellation,
          2.49,
          SpatialCoordinateFrame.World
        )
      )
    assertEquals(radiusBelow.relation, Relation.identity(parcellation.parcels))

    val radiusBoundary =
      right(
        ExactParcelSearchlight.withinRadius(
          parcellation,
          2.5,
          SpatialCoordinateFrame.World
        )
      )
    val first = parcellation.parcels.indexOption(0).get
    val second = parcellation.parcels.indexOption(1).get
    assertEquals(
      radiusBoundary.relation.row(first).ordinalsInDomainOrder.toVector,
      Vector(0, 1)
    )
    assertEquals(
      radiusBoundary.relation.row(second).ordinalsInDomainOrder.toVector,
      Vector(0, 1)
    )

    val secondOnly =
      right(Region.fromOrdinals(parcellation.parcels, Vector(1)))
    val restricted =
      right(
        ExactParcelSearchlight.nearest(
          parcellation,
          2,
          secondOnly,
          SpatialCoordinateFrame.World
        )
      )
    assert(restricted.relation.row(first).isEmpty)
    assertEquals(
      restricted.relation.row(second).ordinalsInDomainOrder.toVector,
      Vector(0, 1)
    )

  test("parcel neighborhood windows retain exact support and center position"):
    val reduced = right(ParcelSeries.reduceMean(series, parcellation))
    val neighborhoods =
      right(
        ExactParcelSearchlight.nearest(
          parcellation,
          2,
          SpatialCoordinateFrame.World
        )
      )
    val second = parcellation.parcels.indexOption(1).get
    val window =
      right(
        ParcelSeriesWindow.materialize(
          reduced,
          neighborhoods,
          second
        )
      )

    assertEquals(window.selection.ordinals.toVector, Vector(0, 1))
    assertEquals(window.centerPosition, 1)
    assertEquals(window.data.shape, Shape(2, 3))
    assertEquals(
      window.data.iterator.toVector,
      reduced.data.iterator.toVector
    )
    assert(window.seriesAtPosition(0).isContiguous)

    val clonedParcellation =
      right(
        VolumeParcellation.create(
          domain,
          parcellation.assignment,
          parcellation.parcelMetadata
        )
      )
    val clonedNeighborhoods =
      right(
        ExactParcelSearchlight.nearest(
          clonedParcellation,
          2,
          SpatialCoordinateFrame.World
        )
      )
    assert(
      ParcelSeriesWindow.materialize(
        reduced,
        clonedNeighborhoods,
        second
      ) match
        case Left(ParcelNeighborhoodError.ParcellationMismatch) => true
        case _                                                  => false
    )

  test("parcel constructors and geometry reject invalid policy"):
    val channel = right(Axis.create("channel", 3, AxisKind.Channel))
    assert(
      ParcelSeries.create(
        parcellation,
        channel,
        NDArray.tabulate[Double](2, 3)((parcel, time) => parcel + time)
      ) match
        case Left(ParcelSeriesError.ExpectedTimeAxis(AxisKind.Channel)) => true
        case _                                                         => false
    )
    assert(
      ParcelSeries.create(
        parcellation,
        time,
        NDArray.tabulate[Double](3, 2)((parcel, time) => parcel + time)
      ) match
        case Left(ParcelSeriesError.DataShapeMismatch(_, _)) => true
        case _                                                => false
    )
    assert(
      VolumeParcellationGeometry
        .centroids(parcellation, tolerance = 0.0)
        .isLeft
    )
    assert(ExactParcelSearchlight.nearest(parcellation, 0).isLeft)
    assert(ExactParcelSearchlight.withinRadius(parcellation, -1.0).isLeft)

  private def right[E, A](value: Either[E, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, found Left($error)")
