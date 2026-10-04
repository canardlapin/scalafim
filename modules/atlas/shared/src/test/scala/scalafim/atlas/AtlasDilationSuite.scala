package scalafim.atlas

import locus4s.{Region, FiniteDomain}
import locus4s.data.VectorField
import scalafim.image.*
import scalafim.image.world.WorldSpace

class AtlasDilationSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val world = right(WorldSpace.declare("atlas dilation world"))

  private def atlas(labels: Vector[Int], spacing: Double = 1.0): VolumeAtlas =
    val space = right(SampleSpaces.inWorld(SampleSpaces(Vector(labels.size, 1, 1),
      spacing = Some(Vector(spacing, 1.0, 1.0))), world))
    val regions = RegionIndex(Vector(AtlasRegionMetadata.fromStrings(RegionId(7), "A"),
      AtlasRegionMetadata.fromStrings(RegionId(19), "B")))
    VolumeAtlas.fromLabelVolume(AtlasRef.volume("dilation", "fixture", SpaceId.MNI152, SpaceId.MNI152),
      regions, AtlasTestImages.labelVolume(space, labels.toArray))

  private def labels(r: VolumeAtlasRealization): Vector[Int] =
    r.domain.space.indices.map(v => r.parcelAssignment(v).fold(0)(p => r.metadata(p).id.value)).toVector

  test("nearest seeds, ties, retained exact owners and independent line fixture"):
    val parent = atlas(Vector(7, 0, 0, 0, 19)).realization
    val radius = right(DilationRadius.from(2.0))
    val leave = right(AtlasDilation.volume(parent)(radius, DilationMetric.GridEuclidean,
      DilationTie.LeaveUnassigned, Region.whole(parent.domain.space)))
    assertEquals(labels(leave), Vector(7, 7, 0, 19, 19))
    val lowest = right(AtlasDilation.volume(parent)(radius, DilationMetric.GridEuclidean,
      DilationTie.LowestParcelOrdinal, Region.whole(parent.domain.space)))
    assertEquals(labels(lowest), Vector(7, 7, 7, 19, 19))
    assert(leave.domain eq parent.domain)
    assert(leave.parcelDomain.sameRuntimeOwnerAs(parent.parcelDomain))
    assertEquals(labels(parent), Vector(7, 0, 0, 0, 19))
    assertEquals(leave.provenance.derivation.last.asInstanceOf[DerivationStep.DilatedParcels].parent, parent.identity)
    right(leave.validateNeuropublishProjection(leave.neuropublishProjection))
    val schema = right(ParcelMetricSchema.from("dilation values"))
    val json = right(ParcelMetricJson.encode(leave)(VectorField.tabulate(leave.parcelDomain)(_ => 2.0), schema))
    assertEquals(right(ParcelMetricJson.decode(leave)(json)).origin.provenance, leave.neuropublishProjection.atlasProvenance)

  test("world radius honors anisotropic geometry; zero radius is identity"):
    val p = atlas(Vector(7, 0, 0, 0, 19), spacing = 2.0).realization
    def run(radius: Double, metric: DilationMetric) = right(AtlasDilation.volume(p)(
      right(DilationRadius.from(radius)), metric, DilationTie.LowestParcelOrdinal, Region.whole(p.domain.space)))
    assertEquals(labels(run(1.0, DilationMetric.WorldEuclidean)), Vector(7, 0, 0, 0, 19))
    assertEquals(labels(run(2.0, DilationMetric.WorldEuclidean)), Vector(7, 7, 0, 19, 19))
    assertEquals(labels(run(0.0, DilationMetric.GridEuclidean)), labels(p))
    assertEquals(labels(run(100.0, DilationMetric.WorldEuclidean)), Vector(7, 7, 7, 19, 19))
    Vector(-1.0, Double.NaN, Double.PositiveInfinity).foreach(r => assert(DilationRadius.from(r).isLeft))

  test("mask is an exact destination restriction and cannot silently discard seeds"):
    val p = atlas(Vector(7, 0, 0, 0, 19)).realization
    val mask = right(Region.fromOrdinals(p.domain.space, Vector(0, 1, 4)))
    val out = right(AtlasDilation.volume(p)(right(DilationRadius.from(5.0)), DilationMetric.GridEuclidean,
      DilationTie.LowestParcelOrdinal, mask))
    assertEquals(labels(out), Vector(7, 7, 0, 0, 19))
    val excludes = right(Region.fromOrdinals(p.domain.space, Vector(0, 1)))
    assert(AtlasDilation.volume(p)(right(DilationRadius.from(2.0)), DilationMetric.WorldEuclidean,
      DilationTie.LeaveUnassigned, excludes).isLeft)
    assert(compileErrors("""
      import scalafim.atlas.*
      import locus4s.Region
      import scalafim.image.*
      def wrong(a: VolumeAtlasRealization, b: VolumeAtlasRealization) =
        AtlasDilation.volume(a)(DilationRadius.from(1.0).toOption.get, DilationMetric.GridEuclidean,
          DilationTie.LeaveUnassigned, Region.whole(b.domain.space))
    """).nonEmpty)
