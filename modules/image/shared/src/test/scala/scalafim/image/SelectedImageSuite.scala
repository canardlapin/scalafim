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
import locus4s.Selection
import ravel.DType.given
import ravel.NDArray
import ravel.Shape
import scalafim.image.Ops.*
import spire.std.double.given

class SelectedImageSuite extends munit.FunSuite:
  private val frame =
    right(
      Frame.persistentNamed[D3](
        right(FrameId.parse("selected-image-suite-frame")),
        "selected image suite",
        LengthUnit.Millimeter,
        CoordinateConvention.RAS
      )
    )
  private val grid =
    right(
      Grid.createPersistent(
        right(GridId.parse("selected-image-suite-grid")),
        frame
      )(Vector(2, 3, 2), Affine.identity[D3])
    )
  private val domainResolution =
    right(GridDomain.register(grid, "selected image voxels", DomainRegistry.empty))
  private type Voxel = domainResolution.S
  private val domain = domainResolution.value
  private val selection =
    right(Selection.fromOrdinals(domain.space, Vector(7, 1, 10)))
  private val time =
    right(Axis.create("time", 3, AxisKind.Time))
  private val volumeSpace =
    SampleSpace.create(grid, NonSpatialAxes.empty)
  private val seriesSpace =
    SampleSpace.create(
      grid,
      right(NonSpatialAxes.from(Vector(time)))
    )
  private val volume =
    right(
      NeuroVolume.continuous(
        volumeSpace,
        NDArray.tabulate[Double](2, 3, 2): (x, y, z) =>
          100.0 * x + 10.0 * y + z
      )
    )
  private val series =
    right(
      NeuroSeries.continuous(
        seriesSpace,
        NDArray.tabulate[Double](2, 3, 2, 3): (x, y, z, t) =>
          1000.0 * x + 100.0 * y + 10.0 * z + t
      )
    )

  test("selected volume retains exact selection and compact Ravel order"):
    val selected =
      right(
        SelectedVolume.gather(
          domain,
          SomeNeuroVolume.eraseSpace(volume),
          selection
        )
      )

    assert(selected.selection eq selection)
    assertEquals(selected.data.shape, Shape(3))
    assertEquals(selected.data.iterator.toVector, Vector(101.0, 1.0, 120.0))

    val scattered = right(selected.toDense(-1.0))
    assertEqualsDouble(scattered.data(1, 0, 1), 101.0, 0.0)
    assertEqualsDouble(scattered.data(0, 0, 1), 1.0, 0.0)
    assertEqualsDouble(scattered.data(1, 2, 0), 120.0, 0.0)
    assertEqualsDouble(scattered.data(0, 0, 0), -1.0, 0.0)

  test("selected series are position-time with contiguous voxel rows"):
    val selected = right(
      SelectedSeries.gather(
        domain,
        SomeNeuroSeries.eraseSpace(series),
        selection
      )
    )

    assert(selected.selection eq selection)
    assertEquals(selected.data.shape, Shape(3, 3))
    assertEquals(
      selected.data.iterator.toVector,
      Vector(
        1010.0,
        1011.0,
        1012.0,
        10.0,
        11.0,
        12.0,
        1200.0,
        1201.0,
        1202.0
      )
    )
    val native = selected.selected
    val first = native.selection.positions.indexOption(0).get
    val firstSeries = native.seriesAt(first)
    assert(firstSeries.isContiguous)
    assertEquals(firstSeries.iterator.toVector, Vector(1010.0, 1011.0, 1012.0))

    val scattered = right(selected.toDense(-1.0))
    assertEqualsDouble(scattered.data(1, 0, 1, 2), 1012.0, 0.0)
    assertEqualsDouble(scattered.data(0, 0, 0, 2), -1.0, 0.0)

  test("require, drop, and fill make missing-support policy explicit"):
    val selected = right(
      SelectedSeries.gather(
        domain,
        SomeNeuroSeries.eraseSpace(series),
        selection
      )
    )
    val requested =
      right(Selection.fromOrdinals(domain.space, Vector(1, 0, 10)))

    selected.reselect(requested, MissingVoxelPolicy.RequireCovered) match
      case Left(SelectedImageError.OutsideSupport(missing)) =>
        assertEquals(missing.cardinality, 1)
        assertEquals(missing.ordinalsInDomainOrder.toVector, Vector(0))
      case other =>
        fail(s"expected uncovered-support error, found $other")

    val dropped =
      right(selected.reselect(requested, MissingVoxelPolicy.DropMissing))
    assertEquals(dropped.selection.ordinals.toVector, Vector(1, 10))
    assertEquals(
      dropped.data.iterator.toVector,
      Vector(10.0, 11.0, 12.0, 1200.0, 1201.0, 1202.0)
    )

    val filled =
      right(selected.reselect(requested, MissingVoxelPolicy.Fill(-5.0)))
    assert(filled.selection eq requested)
    assertEquals(
      filled.data.iterator.toVector,
      Vector(10.0, 11.0, 12.0, -5.0, -5.0, -5.0, 1200.0, 1201.0, 1202.0)
    )

  test("selected arithmetic requires exact order or explicit support policies"):
    val left = right(
      SelectedSeries.gather(
        domain,
        SomeNeuroSeries.eraseSpace(series),
        selection
      )
    )
    val rightSupport =
      right(Selection.fromOrdinals(domain.space, Vector(1, 0, 10)))
    val rightSeries =
      right(
        SelectedSeries.gather(
          domain,
          SomeNeuroSeries.eraseSpace(series),
          rightSupport
        )
      )

    left.addExact(rightSeries) match
      case Left(SelectedImageError.SelectionOrderMismatch(actualLeft, actualRight)) =>
        assertEquals(actualLeft, Vector(7, 1, 10))
        assertEquals(actualRight, Vector(1, 0, 10))
      case other =>
        fail(s"expected selected-order mismatch, found $other")

    val squared = right(left.multiplyExact(left))
    val normalized = right(squared.divideExact(left))
    assertEquals(normalized.data.iterator.toVector, left.data.iterator.toVector)

    val union =
      right(Selection.fromOrdinals(domain.space, Vector(7, 1, 0, 10)))
    val combined =
      right(
        SelectedSeries.combineAt(
          left,
          rightSeries,
          union,
          MissingVoxelPolicy.Fill(0.0),
          MissingVoxelPolicy.Fill(0.0)
        )(_ + _)
      )

    assertEquals(combined.selection.ordinals.toVector, Vector(7, 1, 0, 10))
    assertEquals(
      combined.data.iterator.toVector,
      Vector(
        1010.0,
        1011.0,
        1012.0,
        20.0,
        22.0,
        24.0,
        0.0,
        1.0,
        2.0,
        2400.0,
        2402.0,
        2404.0
      )
    )

  test("foreign domain owners fail closed"):
    val foreignResolution =
      right(GridDomain.register(grid, "foreign owner", DomainRegistry.empty))
    val foreignSelection =
      right(
        Selection.fromOrdinals(
          foreignResolution.value.space,
          Vector(7, 1, 10)
        )
      )

    assert(
      SelectedSeries.gather(
        domain,
        SomeNeuroSeries.eraseSpace(series),
        foreignSelection
      ) match
        case Left(SelectedImageError.Provider(_)) => true
        case _                                    => false
    )

    val selected = right(
      SelectedSeries.gather(
        domain,
        SomeNeuroSeries.eraseSpace(series),
        selection
      )
    )
    assert(
      selected.reselect(foreignSelection, MissingVoxelPolicy.DropMissing) match
        case Left(SelectedImageError.SelectionSpace(_)) => true
        case _                                           => false
    )

  test("selected series reject non-Time trailing axes"):
    val channel = right(Axis.create("channel", 3, AxisKind.Channel))
    SelectedSeries.create(
      domain,
      selection,
      channel,
      NDArray.tabulate[Double](3, 3)((position, item) => position * 10.0 + item)
    ) match
      case Left(SelectedImageError.ExpectedSingleTimeAxis(actual)) =>
        assertEquals(actual, Vector(AxisKind.Channel))
      case other =>
        fail(s"expected Time-axis error, found $other")

  private def right[E, A](value: Either[E, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, found Left($error)")
