package scalafim.image

import image4s.geometry.{Affine, CoordinateConvention, D3, Frame, FrameId, LengthUnit, Grid, GridId}
import image4s.locus.GridDomain
import locus4s.{DomainRegistry, Region}

class VolumeDilationSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val frame = right(Frame.persistentNamed[D3](right(FrameId.parse("volume-dilation-frame")),
    "dilation fixture", LengthUnit.Millimeter, CoordinateConvention.RAS))
  private val affine = right(Affine.fromRowMajor[D3](Vector(
    2.0, 0.8, 0.0, 10.0,
    0.0, 1.0, 0.2, -4.0,
    0.0, 0.0, 1.5, 8.0,
    0.0, 0.0, 0.0, 1.0)))
  private val grid = right(Grid.createPersistent(right(GridId.parse("volume-dilation-grid")), frame)(Vector(3, 4, 2), affine))
  private val packed = right(GridDomain.register(grid, "dilation voxels", DomainRegistry.empty))
  private val domain = packed.value

  test("bounded kernel agrees with independent all-pairs reference on a sheared grid"):
    val seeds = Map(0 -> 0, 7 -> 1, 19 -> 0, 23 -> 1)
    val source = right(VolumeParcellation.resolve(domain, "dilation parcels", Vector("A", "B"),
      domain.space.indices.map(p => seeds.get(p.ordinal)))).value
    val maskOrdinals = (0 until 24).filter(i => seeds.contains(i) || i % 3 != 1).toVector
    val mask = right(Region.fromOrdinals(domain.space, maskOrdinals))
    def coords(i: Int): Vector[Double] = Vector((i / 8).toDouble, ((i / 2) % 4).toDouble, (i % 2).toDouble)
    def world(i: Int): Vector[Double] =
      val c = coords(i)
      Vector(2.0 * c(0) + 0.8 * c(1) + 10.0, c(1) + 0.2 * c(2) - 4.0, 1.5 * c(2) + 8.0)
    for
      metric <- DilationMetric.values
      radius <- Vector(0.0, 1.0, 2.5, 4.0)
      tie <- DilationTie.values
    do
      val actual = right(VolumeDilation(source, right(DilationRadius.from(radius)), metric, tie, mask))
      val expected = (0 until 24).map: target =>
        seeds.get(target).orElse:
          if !maskOrdinals.contains(target) then None
          else
            val eligible = seeds.toVector.map: (seed, parcel) =>
              val a = if metric == DilationMetric.GridEuclidean then coords(seed) else world(seed)
              val b = if metric == DilationMetric.GridEuclidean then coords(target) else world(target)
              val distance = a.zip(b).map((x, y) => (x - y) * (x - y)).sum
              (distance, parcel)
            .filter(_._1 <= radius * radius)
            if eligible.isEmpty then None
            else
              val nearest = eligible.map(_._1).min
              val parcels = eligible.filter(_._1 == nearest).map(_._2).distinct.sorted
              if tie == DilationTie.LeaveUnassigned && parcels.size > 1 then None else parcels.headOption
      assertEquals(actual.toPartialMap.optionalTargetOrdinals, expected.toVector)

  test("empty assignment stays empty and foreign mask owners are checked"):
    val empty = right(VolumeParcellation.resolve(domain, "empty dilation parcels", Vector.empty[String],
      Vector.fill(domain.space.size)(None))).value
    val result = right(VolumeDilation(empty, right(DilationRadius.from(2.0)), DilationMetric.WorldEuclidean,
      DilationTie.LeaveUnassigned, Region.whole(domain.space)))
    assert(result.support.isEmpty)
    val foreign = right(GridDomain.register(right(Grid.createPersistent(right(GridId.parse("foreign-volume-dilation-grid")), frame)(Vector(3, 4, 2), affine)),
      "foreign dilation voxels", DomainRegistry.empty)).value
    val erased = Region.whole(foreign.space).asInstanceOf[Region[packed.S]]
    assert(VolumeDilation(empty, right(DilationRadius.from(2.0)), DilationMetric.GridEuclidean,
      DilationTie.LeaveUnassigned, erased).isLeft)

  test("world radius and bounds retain finite very large and very small scales"):
    Vector(1e200, 1e-200).foreach: spacing =>
      val scaled = right(Affine.fromRowMajor[D3](Vector(
        spacing, 0.0, 0.0, 0.0,
        0.0, spacing, 0.0, 0.0,
        0.0, 0.0, spacing, 0.0,
        0.0, 0.0, 0.0, 1.0)))
      val scaledGrid = right(Grid.createPersistent(right(GridId.parse(s"dilation-scale-$spacing")), frame)(
        Vector(3, 1, 1), scaled))
      val packedScale = right(GridDomain.register(scaledGrid, "scaled dilation", DomainRegistry.empty))
      val parcellation = right(VolumeParcellation.resolve(packedScale.value, "scaled seed", Vector("A"),
        Vector(Some(0), None, None))).value
      val mask = Region.whole(packedScale.value.space)
      def run(radius: Double) = right(VolumeDilation(parcellation, right(DilationRadius.from(radius)),
        DilationMetric.WorldEuclidean, DilationTie.LeaveUnassigned, mask)).toPartialMap.optionalTargetOrdinals
      assertEquals(run(0.0), Vector(Some(0), None, None))
      assertEquals(run(spacing), Vector(Some(0), Some(0), None))
