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
import locus4s.data.VectorField
import ravel.DType.given

class ConnectedComponentsSuite extends munit.FunSuite:
  private val frame =
    right(
      Frame.persistentNamed[D3](
        right(FrameId.parse("connected-components-suite-frame")),
        "connected components suite",
        LengthUnit.Millimeter,
        CoordinateConvention.RAS
      )
    )
  private val grid =
    right(
      Grid.createPersistent(
        right(GridId.parse("connected-components-suite-grid")),
        frame
      )(Vector(3, 3, 1), Affine.identity[D3])
    )
  private val domainResolution =
    right(
      GridDomain.register(
        grid,
        "connected component voxels",
        DomainRegistry.empty
      )
    )
  private val domain = domainResolution.value

  test("components retain only one exact partial-surjection result"):
    val active =
      right(Region.fromOrdinals(domain.space, Vector(0, 1, 3, 8)))
    val result =
      right(
        ConnectedComponents.fromRegion(
          domain,
          active,
          SpatialConnectivity3D.Face6
        )
      )
    val components = result.value

    assertEquals(components.parcels.size, 2)
    assertEquals(components.support, active)
    val first = components.parcels.indexOption(0).get
    val second = components.parcels.indexOption(1).get
    assertEquals(
      components.region(first).ordinalsInDomainOrder.toVector,
      Vector(0, 1, 3)
    )
    assertEquals(
      components.region(second).ordinalsInDomainOrder.toVector,
      Vector(8)
    )
    assertEquals(components.metadataAt(first).cardinality, 3)
    assertEquals(components.metadataAt(second).cardinality, 1)

    val labels =
      VectorField.tabulate(components.parcels)(index => index.ordinal + 1)
    val denseLabels = right(components.renderCategorical(labels, 0))
    assertEquals(
      denseLabels.data.iterator.toVector,
      Vector(1, 1, 0, 1, 0, 0, 0, 0, 2)
    )
    val sizes = components.parcelMetadata.map(_.cardinality)
    val denseSizes = right(components.renderCategorical(sizes, 0))
    assertEquals(
      denseSizes.data.iterator.toVector,
      Vector(3, 3, 0, 3, 0, 0, 0, 0, 1)
    )

  test("connectivity policy distinguishes diagonal contact"):
    val diagonal =
      right(Region.fromOrdinals(domain.space, Vector(0, 4)))
    val face =
      right(
        ConnectedComponents.fromRegion(
          domain,
          diagonal,
          SpatialConnectivity3D.Face6
        )
      ).value
    val corner =
      right(
        ConnectedComponents.fromRegion(
          domain,
          diagonal,
          SpatialConnectivity3D.FaceEdgeCorner26
        )
      ).value

    assertEquals(face.parcels.size, 2)
    assertEquals(corner.parcels.size, 1)
    assertEquals(
      corner.region(corner.parcels.indexOption(0).get)
        .ordinalsInDomainOrder
        .toVector,
      Vector(0, 4)
    )

  test("semantic mask input resolves through the exact grid owner"):
    val active =
      right(Region.fromOrdinals(domain.space, Vector(0, 1, 3, 8)))
    val mask = right(Mask.fromRegion(domain, active))
    val result =
      right(
        ConnectedComponents.fromMask(
          domain,
          mask,
          SpatialConnectivity3D.Face6
        )
      ).value

    assertEquals(result.support, active)
    assertEquals(result.parcels.size, 2)

  test("empty masks produce the valid empty parcel domain"):
    val result =
      right(
        ConnectedComponents.fromRegion(
          domain,
          Region.empty(domain.space),
          SpatialConnectivity3D.Face6
        )
      ).value

    assertEquals(result.parcels.size, 0)
    assert(result.support.isEmpty)
    assertEquals(right(result.fibers).pairCount, 0)
    val labels = VectorField.tabulate(result.parcels)(_ => 1)
    val dense = right(result.renderCategorical(labels, 0))
    assertEquals(dense.data.iterator.toVector, Vector.fill(9)(0))

  test("foreign active-region owners fail closed"):
    val foreignResolution =
      right(
        GridDomain.register(
          grid,
          "foreign connected component voxels",
          DomainRegistry.empty
        )
      )
    val foreign =
      right(
        Region.fromOrdinals(
          foreignResolution.value.space,
          Vector(0, 1)
        )
      )

    assert(
      ConnectedComponents.fromRegion(domain, foreign) match
        case Left(ConnectedComponentsError.WrongVoxelOwner(_)) => true
        case _                                                 => false
    )

  private def right[E, A](value: Either[E, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, found Left($error)")
