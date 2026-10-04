package scalafim.atlas

import locus4s.data.Field
import locus4s.data.VectorField
import scalafim.atlas.fixtures.ParcelAlignmentFixture
import scalafim.image.SampleSpaces
import scalafim.surface.{Hemisphere as SurfaceHemisphere, LabelInfo, LabeledSurface, SurfaceGeometry, SurfaceKind, TriangleMesh, VertexId}

class ParcelFieldsSuite extends munit.FunSuite:
  private val ids = Vector(1, 180, 181, 360)
  private val volumeKeys = Vector("R_V1_ROI", "R_p24_ROI", "L_V1_ROI", "L_p24_ROI")
  private val surfaceKeys = Vector("L_V1_ROI", "L_p24_ROI", "R_V1_ROI", "R_p24_ROI")

  private def metadata(keys: Vector[String], labels: Vector[Int] = ids): RegionIndex =
    RegionIndex(keys.zip(labels).map: (key, id) =>
      val checked = right(GlasserParcelKey.from(key))
      AtlasRegionMetadata.fromStrings(RegionId(id), checked.area, Some(key), Some(checked.hemisphere))
    )

  private def volume(keys: Vector[String] = volumeKeys, labels: Vector[Int] = ids): VolumeAtlas =
    VolumeAtlas.fromLabelVolume(GlasserHcpMmp1().atlasRef(), metadata(keys, labels),
      AtlasTestImages.labelVolume(SampleSpaces(Vector(4, 1, 1)), labels.toArray))

  private def surface(): SurfaceAtlas =
    val mesh = TriangleMesh.fromRows(Vector(Vector(0.0, 0.0, 0.0), Vector(1.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0)), Vector((0, 1, 2)))
    def side(hemi: SurfaceHemisphere, labels: Vector[Int], keys: Vector[String]): LabeledSurface =
      LabeledSurface.fromIndexed(SurfaceGeometry(mesh, hemi, SurfaceKind.Pial),
        Vector(VertexId(0), VertexId(1), VertexId(2)), labels,
        labels.distinct.zip(keys).map((id, key) => LabelInfo(id, key)))
    SurfaceAtlas.fromLabeledSurfaces(GlasserHcpMmp1Surface().atlasRef(), metadata(surfaceKeys),
      side(SurfaceHemisphere.Left, Vector(1, 180, 180), surfaceKeys.take(2)),
      side(SurfaceHemisphere.Right, Vector(181, 360, 360), surfaceKeys.drop(2)))

  test("checked Glasser imports match independently generated R alignment outcomes"):
    val v = volume()
    val s = surface()
    ParcelAlignmentFixture.cases.foreach: fixture =>
      val target = if fixture.target == "surface" then s.realization else v.realization
      val entries = fixture.keys.map(key => right(GlasserParcelKey.fromSourceLabel(key))).zip(fixture.values)
      val missing = if fixture.partial then MissingParcelPolicy.Fill(Double.NaN) else MissingParcelPolicy.Reject
      val result = ParcelFields.fromGlasserKeys(target)(entries, missing = missing)
      fixture.outcome match
        case "pass" =>
          val records = right(ParcelFields.records(target)(right(result)))
          assertEquals(records.map(_.region.fullLabel.value), fixture.targetKeys)
          records.map(_.value).zip(fixture.expected).foreach: (actual, expected) =>
            if expected.isNaN then assert(actual.isNaN)
            else assertEqualsDouble(actual, expected, 0.0)
        case "neuroatlas_error_duplicate_parcel_key" => assert(result.swap.toOption.get.isInstanceOf[ParcelFieldError.DuplicateGlasserKeys])
        case "neuroatlas_error_unknown_parcel_key" => assert(result.swap.toOption.get.isInstanceOf[ParcelFieldError.UnknownGlasserKeys])
        case "neuroatlas_error_missing_parcel_key" => assert(result.swap.toOption.get.isInstanceOf[ParcelFieldError.MissingKeys])
        case other => fail(s"undeclared oracle outcome $other")

  test("source IDs use the declared encoding, including renumbered canonical domains"):
    val v = volume()
    val s = surface()
    val target = s.realization
    val input = ids.reverse.map(id => RegionId(id) -> id.toDouble)
    val values: Field[target.P, Double] = right(ParcelFields.fromSourceIds(target)(v.realization, input))
    assertEquals(right(ParcelFields.records(target)(values)).map(_.value), ParcelAlignmentFixture.cases.head.expected)
    val renumbered = volume(volumeKeys.reverse, Vector(7, 8, 9, 10))
    assert(renumbered.realization.parcelDomain.samePersistentIdentityAs(target.parcelDomain))
    val renumberedInput = Vector(7 -> 360.0, 8 -> 181.0, 9 -> 180.0, 10 -> 1.0).map((id, value) => RegionId(id) -> value)
    val transferred = right(ParcelFields.fromSourceIds(target)(renumbered.realization, renumberedInput))
    assertEquals(transferred.toVector, values.toVector)
    val wrongSource = right(ParcelFields.fromSourceIds(target)(target, input))
    assertNotEquals(wrongSource.toVector, values.toVector)
    assertEquals(right(ParcelFields.records(target)(wrongSource)).map(_.value), ids.map(_.toDouble))

  test("full key import stores canonical order and derives target display metadata"):
    val a = volume()
    val r = a.realization
    val entries = r.displayOrder.indices.map(p => right(AtlasParcelKey.from(r.parcelKeys(p))) -> r.metadata(p).id.value.toDouble).toVector.reverse
    val values: Field[r.P, Double] = right(ParcelFields.fromKeys(r)(entries))
    assertEquals(values.toVector, Vector(181.0, 360.0, 1.0, 180.0))
    val records = right(ParcelFields.records(r)(values))
    assertEquals(records.map(_.region), a.regions.regions)
    assertEquals(records.map(_.value), ids.map(_.toDouble))
    assert(AtlasParcelKey.from(" ").isLeft)

  test("duplicates precede dropping unknown rows and missing fill is explicit"):
    val r = volume().realization
    val keys = ParcelFields.keys(r).toVector
    val unknown = right(AtlasParcelKey.from("foreign-key"))
    val duplicate = ParcelFields.fromKeys(r)(Vector(unknown -> 1, unknown -> 2), UnknownParcelPolicy.Drop, MissingParcelPolicy.Fill(-1))
    assertEquals(duplicate, Left(ParcelFieldError.DuplicateKeys(Vector(unknown))))
    assert(ParcelFields.fromKeys(r)(Vector(unknown -> 1)).swap.toOption.get.isInstanceOf[ParcelFieldError.UnknownKeys])
    assert(ParcelFields.fromKeys(r)(Vector(keys.head -> 1)).swap.toOption.get.isInstanceOf[ParcelFieldError.MissingKeys])
    val filled = right(ParcelFields.fromKeys(r)(Vector(keys.head -> 1, unknown -> 99), UnknownParcelPolicy.Drop, MissingParcelPolicy.Fill(-1)))
    assertEquals(filled.toVector, Vector(1, -1, -1, -1))
    assertEquals(ParcelFields.fromSourceIds(r)(r, Vector(RegionId(999) -> 1, RegionId(999) -> 2), UnknownParcelPolicy.Drop, MissingParcelPolicy.Fill(-1)), Left(ParcelFieldError.DuplicateSourceIds(Vector(RegionId(999)))))
    assertEquals(ParcelFields.fromSourceIds(r)(r, Vector(RegionId(999) -> 1)), Left(ParcelFieldError.UnknownSourceIds(Vector(RegionId(999)))))
    val sourceFilled = right(ParcelFields.fromSourceIds(r)(r, Vector(RegionId(999) -> 1), UnknownParcelPolicy.Drop, MissingParcelPolicy.Fill(-1)))
    assertEquals(sourceFilled.toVector, Vector.fill(4)(-1))

  test("aliases normalize before duplicate admission even when unknown keys are dropped"):
    val r = volume().realization
    val alias = right(GlasserParcelKey.fromSourceLabel("Left_V1"))
    val full = right(GlasserParcelKey.from("L_V1_ROI"))
    assertEquals(ParcelFields.fromGlasserKeys(r)(Vector(alias -> 1, full -> 2), UnknownParcelPolicy.Drop, MissingParcelPolicy.Fill(-1)), Left(ParcelFieldError.DuplicateGlasserKeys(Vector(full))))
    val unknown = right(GlasserParcelKey.from("L_unknown_ROI"))
    assertEquals(ParcelFields.fromGlasserKeys(r)(Vector(unknown -> 1, unknown -> 2), UnknownParcelPolicy.Drop, MissingParcelPolicy.Fill(-1)), Left(ParcelFieldError.DuplicateGlasserKeys(Vector(unknown))))

  test("persistent field transport is explicit and rejects unrelated or reordered domains"):
    val v = volume()
    val s = surface()
    val vr = v.realization
    val sr = s.realization
    assert(!vr.parcelDomain.sameRuntimeOwnerAs(sr.parcelDomain))
    val input = VectorField.tabulate(vr.parcelDomain)(p => vr.metadata(p).id.value)
    val transferred: Field[sr.P, Int] = right(ParcelFields.alignTo(sr)(input))
    assert(sr.parcelDomain.sameRuntimeOwnerAs(transferred.space))
    assertEquals(right(ParcelFields.records(sr)(transferred)).map(_.value), Vector(181, 360, 1, 180))
    val localRef = v.ref.withDetails(_.copy(parcelIdentity = ParcelIdentity.SourceLabels))
    val regions = metadata(volumeKeys)
    val first = VolumeAtlas.fromLabelVolume(localRef, regions, v.labelVolume).realization
    val reordered = VolumeAtlas.fromLabelVolume(localRef, RegionIndex(regions.regions.reverse), v.labelVolume).realization
    val localValues = VectorField.tabulate(first.parcelDomain)(_.ordinal)
    assert(ParcelFields.alignTo(reordered)(localValues).isLeft)
    assert(ParcelFields.alignTo(first)(input).isLeft)
    assert(ParcelFields.fromGlasserKeys(first)(Vector.empty[(GlasserParcelKey, Int)]).swap.toOption.get.isInstanceOf[ParcelFieldError.ExpectedGlasserIdentity])

  test("record presentation statically requires the same parcel owner"):
    assert(compileErrors("""
      import scalafim.atlas.*
      import locus4s.data.Field
      def foreign(a: AtlasRealization, b: AtlasRealization)(values: Field[a.P, Double]) =
        ParcelFields.records(b)(values)
    """).nonEmpty)
    assert(compileErrors("""
      import scalafim.atlas.*
      import locus4s.data.Field
      def foreign(a: AtlasRealization, b: AtlasRealization)(values: Field[a.X, Double]) =
        AtlasReduce.reduceField(b)(values)
    """).nonEmpty)

  test("portable field reduction also retains the surface realization parcel owner"):
    val s = surface()
    val r = s.realization
    val input = VectorField.tabulate(r.parcelAssignment.from)(p => p.ordinal.toDouble)
    val reduced: Field[r.P, Double] = right(AtlasReduce.reduceField(r)(input))
    assertEquals(right(ParcelFields.records(r)(reduced)).map(_.value), Vector(0.0, 1.5, 3.0, 4.5))

  private def right[A](result: Either[?, A]): A = result.fold(error => fail(error.toString), identity)
