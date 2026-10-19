package scalafim.image

import SampleSpaces.*

import GridDomainOps.*
import locus4s.CenteredNeighborhoodSystem
import locus4s.DomainRegistry
import locus4s.Relation
import ravel.NDArray
import spire.std.int.given
import scala.compiletime.testing.typeCheckErrors

class VolumeSearchlightSuite extends munit.FunSuite:
  private val volumeSpace =
    ProviderSpaces.volume(SampleSpaces(Vector(3, 3, 1)))
  private val packedDomain =
    right(
      GridDomain.register(
        volumeSpace.grid,
        "searchlight voxels",
        DomainRegistry.empty
      )
    )
  private type S = packedDomain.S
  private val domain = packedDomain.value

  test("radius zero is the identity relation"):
    val radius = SearchlightRadius.make(0.0).toOption.get
    val searchlight = ExactVolumeSearchlight.metricBalls(domain, radius).toOption.get

    assertEquals(
      ambientRelation(searchlight),
      Relation
        .fromOrdinalRows(
          domain.space,
          domain.space,
          Iterator.tabulate(domain.space.size)(ordinal => Iterator.single(ordinal))
        )
        .toOption
        .get
    )

  test("metric balls are symmetric, monotone, and obey triangle composition"):
    val radiusOne = SearchlightRadius.make(1.0).toOption.get
    val radiusTwo = SearchlightRadius.make(2.0).toOption.get
    val one = ambientRelation(ExactVolumeSearchlight.metricBalls(domain, radiusOne).toOption.get)
    val two = ambientRelation(ExactVolumeSearchlight.metricBalls(domain, radiusTwo).toOption.get)

    val rows = one.ordinalRows
    rows.zipWithIndex.foreach: (targets, source) =>
      targets.foreach: target =>
        assert(
          rows(target).contains(source),
          s"expected symmetric edge $target -> $source"
        )
    assert(one.subsetOf(two))
    assert(one.andThen(one).subsetOf(two))

  test("closed metric balls include points exactly on the radius boundary"):
    val radius = SearchlightRadius.make(1.0).toOption.get
    val relation =
      ambientRelation(ExactVolumeSearchlight.metricBalls(domain, radius).toOption.get)
    val center = domain.space.indexOption(4).get

    assertEquals(
      relation.row(center).ordinalsInDomainOrder.toVector,
      Vector(1, 3, 4, 5, 7)
    )

  test("exact cube, ellipsoid, and blobby geometries retain centered relations"):
    val radius = SearchlightRadius.make(1.0).toOption.get
    val center = domain.space.indexOption(4).get
    val cube = ExactVolumeSearchlight.cubes(domain, radius).toOption.get
    val ellipsoid =
      ExactVolumeSearchlight
        .ellipsoids(domain, radius, Vector(2.0, 1.0, 1.0))
        .toOption
        .get
    val blobby =
      ExactVolumeSearchlight
        .blobbyBalls(
          domain,
          radius,
          drop = 1.0,
          edgeFraction = 1.0,
          rng = new scala.util.Random(0L)
        )
        .toOption
        .get

    assertEquals(
      ambientRelation(cube).row(center).ordinalsInDomainOrder.toVector,
      Vector(0, 1, 2, 3, 4, 5, 6, 7, 8)
    )
    assertEquals(
      ambientRelation(ellipsoid).row(center).ordinalsInDomainOrder.toVector,
      Vector(3, 4, 5)
    )
    assertEquals(
      ambientRelation(blobby).row(center).ordinalsInDomainOrder.toVector,
      Vector(4)
    )

  test("exact neighborhood shape policies reject invalid parameters"):
    val radius = SearchlightRadius.make(1.0).toOption.get

    assert(
      ExactVolumeSearchlight
        .ellipsoids(domain, radius, Vector(1.0, 0.0, 1.0))
        .isLeft
    )
    assert(
      ExactVolumeSearchlight
        .blobbyBalls(
          domain,
          radius,
          drop = 1.1,
          edgeFraction = 0.5,
          rng = new scala.util.Random(0L)
        )
        .isLeft
    )

  test("relation rows materialize an exact selected window"):
    val values = Array.ofDim[Int](volumeSpace.grid.nVoxels)
    var i = 0
    while i < values.length do
      values(i) = i + 1
      i += 1
    val volume = SomeLabelVolume.unsafeCopyFromCanonicalArray[Int](values, volumeSpace)
    val field = domain.fieldOf(volume).toOption.get
    val radius = SearchlightRadius.make(1.0).toOption.get
    val searchlight = ExactVolumeSearchlight.metricBalls(domain, radius).toOption.get
    val center = domain.space.indexOption(4).get
    val materialized =
      ExactVolumeSearchlight
        .materializeCategorical(domain, searchlight, searchlight.centers.indexAtValidatedOrdinal(4), field)
        .toOption
        .get

    assertEquals(
      materialized.values.selection.region.ordinalsInDomainOrder.toVector,
      Vector(1, 3, 4, 5, 7)
    )
    assertEquals(
      materialized.values.data.iterator.toVector,
      Vector(2, 4, 5, 6, 8)
    )
    assertEquals(materialized.centerPosition, 2)

  test("value support restricts materialization without changing metric geometry"):
    val values = PrimitiveBuffers.fillConst[Int](volumeSpace.grid.nVoxels, 1)
    values(1) = 0
    val volume = SomeLabelVolume.unsafeCopyFromCanonicalArray[Int](values, volumeSpace)
    val field = domain.fieldOf(volume).toOption.get
    val radius = SearchlightRadius.make(1.0).toOption.get
    val searchlight = ExactVolumeSearchlight.metricBalls(domain, radius).toOption.get
    val center = domain.space.indexOption(4).get
    val geometry = ambientRelation(searchlight).row(center)
    val nonZero = domain.supportWhere(field)(_ != 0).toOption.get
    val materialized =
      ExactVolumeSearchlight
        .materializeCategorical(
          domain,
          searchlight,
          searchlight.centers.indexAtValidatedOrdinal(4),
          field,
          support = Some(nonZero)
        )
        .toOption
        .get

    assertEquals(geometry.ordinalsInDomainOrder.toVector, Vector(1, 3, 4, 5, 7))
    assertEquals(
      materialized.values.selection.region.ordinalsInDomainOrder.toVector,
      Vector(3, 4, 5, 7)
    )
    assertEquals(ambientRelation(searchlight).row(center), geometry)

  test("prepared native-volume execution reuses exact support and Ravel order"):
    val data =
      NDArray.tabulate[Double](3, 3, 1): (x, y, _) =>
        10.0 * x + y
    val native: SomeScalarVolume[Double] =
      NeuroVolume
        .continuous(volumeSpace, data)
        .map(SomeNeuroVolume.eraseSpace)
        .fold(error => fail(error.message), identity)
    val field =
      domain
        .spatialField(native.sampled)
        .fold(error => fail(error.message), identity)
    val radius = SearchlightRadius.make(1.0).toOption.get
    val searchlight =
      ExactVolumeSearchlight.metricBalls(domain, radius).toOption.get
    val center = domain.space.indexOption(4).get
    val prepared =
      ExactVolumeSearchlight
        .prepare(searchlight, searchlight.centers.indexAtValidatedOrdinal(4))
        .fold(error => fail(error.message), identity)
    val fromField =
      ExactVolumeSearchlight
        .materializeContinuous(domain, searchlight, searchlight.centers.indexAtValidatedOrdinal(4), field)
        .fold(error => fail(error.message), identity)
    val fromNative =
      ExactVolumeSearchlight
        .materializePreparedVolume(domain, prepared, native)
        .fold(error => fail(error.message), identity)

    assertEquals(
      fromNative.values.selection.ordinals.toVector,
      fromField.values.selection.ordinals.toVector
    )
    assertEquals(
      fromNative.values.data.iterator.toVector,
      fromField.values.data.iterator.toVector
    )
    assertEquals(fromNative.centerPosition, fromField.centerPosition)

  test("seeded sparse geometries retain canonical ambient rows"):
    val centers = right(locus4s.Region.fromOrdinals(domain.space, Vector(8, 0, 4)))
    val radius = right(SearchlightRadius.make(1.5))
    val ellipsoid = right(ExactVolumeSearchlight.ellipsoids(
      domain, radius, Vector(1.0, 0.8, 1.0), centers,
      jitter = 0.25, rng = new scala.util.Random(731L)
    ))
    val blobby = right(ExactVolumeSearchlight.blobbyBalls(
      domain, radius, drop = 0.5, edgeFraction = 0.5, centers = centers,
      rng = new scala.util.Random(17L)
    ))
    // Captured from the original ambient-row representation before provider migration.
    assertEquals(Vector(0, 4, 8).map(i => i -> ambientRelation(ellipsoid).ordinalRows(i).toVector), Vector(
      0 -> Vector(0, 1, 3),
      4 -> Vector(0, 1, 2, 3, 4, 5, 6, 7, 8),
      8 -> Vector(4, 5, 6, 7, 8)
    ))
    assertEquals(Vector(0, 4, 8).map(i => i -> ambientRelation(blobby).ordinalRows(i).toVector), Vector(
      0 -> Vector(0, 1, 3),
      4 -> Vector(0, 3, 4, 5, 6),
      8 -> Vector(4, 5, 7, 8)
    ))

  test("sparse neighborhoods own only compact center rows and windows retain voxel centers"):
    val centers = right(locus4s.Region.fromOrdinals(domain.space, Vector(8, 4)))
    val searchlight = right(ExactVolumeSearchlight.metricBalls(domain, right(SearchlightRadius.make(1.0)), centers))
    assertEquals(searchlight.centers.size, 2)
    assertEquals(searchlight.membership.csr.rowOffsets.length, 3)
    assertEquals(searchlight.centerEmbedding.toTotalMap.targetOrdinals.toVector, Vector(4, 8))
    assert(searchlight.ambient.sameRuntimeOwnerAs(domain.space))
    val center = searchlight.centers.indexAtValidatedOrdinal(0)
    val prepared = right(ExactVolumeSearchlight.prepare(searchlight, center))
    assertEquals(prepared.center.ordinal, 4)
    assertEquals(prepared.centerPosition, 2)
    assertEquals(prepared.selection.ordinals.toVector, Vector(1, 3, 4, 5, 7))
    val field = locus4s.data.Field.view(domain.space)(index => index.ordinal + 1)
    val window = right(ExactVolumeSearchlight.materializeCategorical(domain, searchlight, center, field))
    assertEquals(window.center.ordinal, 4)
    assertEquals(window.centerPosition, 2)
    assertEquals(window.values.data.iterator.toVector, Vector(2, 4, 5, 6, 8))

  test("restriction retains center ownership and reports missing ambient centers"):
    val centers = right(locus4s.Region.fromOrdinals(domain.space, Vector(8, 4)))
    val searchlight = right(ExactVolumeSearchlight.metricBalls(domain, right(SearchlightRadius.make(1.0)), centers))
    val support = right(locus4s.Region.fromOrdinals(domain.space, Vector(3, 4, 5, 8)))
    val restricted = right(ExactVolumeSearchlight.restrictTargets(searchlight, support))
    assert(restricted.centers.sameRuntimeOwnerAs(searchlight.centers))
    assert(restricted.centerEmbedding eq searchlight.centerEmbedding)
    assertEquals(restricted.membership.ordinalRows.iterator.map(_.toVector).toVector, Vector(Vector(3, 4, 5), Vector(5, 8)))
    val excluded = right(locus4s.Region.fromOrdinals(domain.space, Vector(0, 8)))
    assertEquals(ExactVolumeSearchlight.restrictTargets(searchlight, excluded).left.toOption, Some(ExactVolumeSearchlightError.MissingCenter(4)))
    assertEquals(ExactVolumeSearchlight.prepare(searchlight, searchlight.centers.indexAtValidatedOrdinal(0), Some(excluded)).left.toOption, Some(ExactVolumeSearchlightError.CenterExcluded(4)))

  test("empty center support has zero compact rows"):
    val searchlight = right(ExactVolumeSearchlight.metricBalls(domain, right(SearchlightRadius.make(1.0)), locus4s.Region.empty(domain.space)))
    assertEquals(searchlight.centers.size, 0)
    assertEquals(searchlight.membership.pairCount, 0)
    assertEquals(searchlight.membership.csr.rowOffsets.length, 0)
    assert(searchlight.centers.index(0).isLeft)

  test("window preparation rejects an ambient voxel index where a center index is required"):
    val errors = typeCheckErrors("""
      import locus4s.{CenteredNeighborhoodSystem, Index}
      import scalafim.image.ExactVolumeSearchlight
      def invalid[C, S](searchlight: CenteredNeighborhoodSystem[C, S], voxel: Index[S]) =
        ExactVolumeSearchlight.prepare(searchlight, voxel)
    """)
    assert(errors.nonEmpty)
    assert(errors.exists(_.lineContent.contains("ExactVolumeSearchlight.prepare")))

  // Test-only expansion keeps the original ambient relation laws unchanged while production
  // uses the compact provider representation exclusively.
  private def ambientRelation[C](system: CenteredNeighborhoodSystem[C, S]): Relation[S, S] =
    val rows = Array.fill(domain.space.size)(Vector.empty[Int])
    system.centers.indices.foreach: center =>
      rows(system.center(center).ordinal) = system.neighborhood(center).ordinalsInDomainOrder.toVector
    right(Relation.fromOrdinalRows(domain.space, domain.space, rows.iterator))

  private def right[E, A](value: Either[E, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, found Left($error)")
