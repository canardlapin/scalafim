package scalafim.atlas

import locus4s.data.{Field, VectorField}
import scalafim.image.SampleSpaces
import scalafim.surface.{Hemisphere as SurfaceHemisphere, LabelInfo, LabeledSurface, SurfaceGeometry, SurfaceKind, TriangleMesh, VertexId}

class ParcelMetricSuite extends munit.FunSuite:
  private val keys = Vector("R_V1_ROI", "R_p24_ROI", "L_V1_ROI", "L_p24_ROI")
  private def metadata(labels: Vector[String], ids: Vector[Int]): RegionIndex =
    RegionIndex(labels.zip(ids).map: (label, id) =>
      val key = right(GlasserParcelKey.from(label))
      AtlasRegionMetadata.fromStrings(RegionId(id), key.area, Some(label), Some(key.hemisphere)))
  private def volume(labels: Vector[String] = keys, ids: Vector[Int] = Vector(1, 180, 181, 360)): VolumeAtlas =
    VolumeAtlas.fromLabelVolume(GlasserHcpMmp1().atlasRef(), metadata(labels, ids),
      AtlasTestImages.labelVolume(SampleSpaces(Vector(4, 1, 1)), ids.toArray))
  private def surface(): SurfaceAtlas =
    val labels = keys.drop(2) ++ keys.take(2)
    val mesh = TriangleMesh.fromRows(Vector(Vector(0.0, 0.0, 0.0), Vector(1.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0)), Vector((0, 1, 2)))
    def side(hemi: SurfaceHemisphere, ids: Vector[Int], names: Vector[String]): LabeledSurface =
      LabeledSurface.fromIndexed(SurfaceGeometry(mesh, hemi, SurfaceKind.Pial),
        Vector(VertexId(0), VertexId(1), VertexId(2)), ids,
        ids.distinct.zip(names).map((id, name) => LabelInfo(id, name)))
    SurfaceAtlas.fromLabeledSurfaces(GlasserHcpMmp1Surface().atlasRef(), metadata(labels, Vector(7, 8, 9, 10)),
      side(SurfaceHemisphere.Left, Vector(7, 8, 8), labels.take(2)),
      side(SurfaceHemisphere.Right, Vector(9, 10, 10), labels.drop(2)))
  private val schema = right(ParcelMetricSchema.from("mean beta", Some("percent signal"), Map("contrast" -> "faces > shapes")))

  private def document(): String =
    val r = volume().realization
    right(ParcelMetricJson.encode(r)(VectorField.tabulate(r.parcelDomain)(_.ordinal + 0.25), schema))

  private def rewrite(document: String)(f: ujson.Value => Unit): String =
    val envelope = ujson.read(document)
    val payload = ujson.read(envelope("payload").str)
    f(payload)
    val body = ujson.write(payload)
    envelope("payload") = body
    envelope("sha256") = ParcelMetricJson.digest(body)
    ujson.write(envelope)

  test("volume-to-surface restoration preserves origin and admits precise target fields"):
    val v = volume()
    val s = surface()
    val vr = v.realization
    val sr = s.realization
    val values = VectorField.tabulate(vr.parcelDomain)(p => vr.metadata(p).id.value.toDouble)
    val encoded = right(ParcelMetricJson.encode(vr)(values, schema))
    val restored = right(ParcelMetricJson.decode(sr)(encoded))
    val owned: Field[sr.P, Double] = restored.values
    assert(owned.space.sameRuntimeOwnerAs(sr.parcelDomain))
    assertEquals(owned.toVector, Vector(181.0, 360.0, 1.0, 180.0))
    assertEquals(restored.origin.provenance, vr.neuropublishProjection.atlasProvenance)
    assertEquals(restored.origin.parcelMetadata, vr.neuropublishProjection.parcelMetadata)
    assertEquals(restored.schema.name, schema.name)
    assertEquals(restored.schema.unit, schema.unit)
    assertEquals(restored.schema.attributes.toMap, schema.attributes.toMap)
    assertNotEquals(restored.origin.provenance, sr.neuropublishProjection.atlasProvenance)
    val back = right(ParcelMetricJson.decode(vr)(encoded))
    assertEquals(back.values.toVector, values.toVector)
    val resaved = right(ParcelMetricJson.encode(sr)(restored))
    val again = right(ParcelMetricJson.decode(vr)(resaved))
    assertEquals(again.origin.provenance, restored.origin.provenance)
    assertEquals(again.origin.parcelMetadata, restored.origin.parcelMetadata)
    assertEquals(again.origin.displayOrder, restored.origin.displayOrder)
    assertEquals(again.origin.support, restored.origin.support)
    assertEquals(again.origin.assignments, restored.origin.assignments)
    assertEquals(again.values.toVector, values.toVector)
    assert(!encoded.contains("faceIndices"))

  test("origin namespace, ordering, schema and assignment evidence fail closed even with recomputed digest"):
    val r = volume().realization
    val original = document()
    val mutations: Vector[ujson.Value => Unit] = Vector(
      p => p("origin")("provenance")("identity")("family") = "foreign",
      p => p("origin")("provenance")("identity")("release")("version") = "different",
      p => p("origin")("identityPolicy") = "SharedRegionIds",
      p => p("parcelDomain")("elementKeys") = ujson.Arr.from(p("parcelDomain")("elementKeys").arr.reverse),
      p => p("origin")("parcelMetadata")(0)("fullLabel") = "L_unknown_ROI",
      p => p("origin")("provenance")("sourceArtifacts") = ujson.Arr(),
      p => p("origin")("assignments")(0)("target")("size") = 99,
      p => p("origin")("support")(0)("shape") = ujson.Arr(99, 1, 1),
      p => p("origin")("support")(0)("shape") = ujson.Arr(Int.MaxValue, Int.MaxValue, Int.MaxValue),
      p => p("values") = ujson.Arr("3ff0000000000000"),
      p => p("version") = 2,
      p => p("valueEncoding") = "json-number",
      p => p("schema")("name") = " "
    )
    mutations.zipWithIndex.foreach: (change, index) =>
      assert(ParcelMetricJson.decode(r)(rewrite(original)(change)).isLeft, s"mutation $index was admitted")

  test("whole-payload integrity and independently retained digest detect value alteration"):
    val r = volume().realization
    val original = document()
    val envelope = ujson.read(original)
    envelope("payload") = envelope("payload").str + " "
    assertEquals(ParcelMetricJson.decode(r)(ujson.write(envelope)), Left(ParcelMetricError.IntegrityMismatch))
    val changed = rewrite(original)(p => p("values")(0) = "3ff0000000000000")
    assert(ParcelMetricJson.decode(r)(changed).isRight)
    assertEquals(ParcelMetricJson.decode(r)(changed, Some(ujson.read(original)("sha256").str)), Left(ParcelMetricError.IntegrityMismatch))

  test("decoded origins obey classified spaces and hard-label background admission"):
    val r = volume().realization
    val original = document()
    val mutations: Vector[ujson.Value => Unit] = Vector(
      p => p("origin")("provenance")("declaredSupport")("templateSpaceId") = "fsLR_32k",
      p => p("origin")("provenance")("labels")("backgroundSourceLabel") = 1,
      p => p("origin")("provenance")("labels")("backgroundSourceLabel") = ujson.Null
    )
    val rejected = mutations.map(change => ParcelMetricJson.decode(r)(rewrite(original)(change)).isLeft)
    assertEquals(rejected, Vector(true, true, true))
    val s = surface().realization
    val surfaceDocument = right(ParcelMetricJson.encode(s)(VectorField.tabulate(s.parcelDomain)(_ => 1.0), schema))
    val changedSurface = rewrite(surfaceDocument)(p => p("origin")("provenance")("declaredSupport")("templateSpaceId") = "MNI305")
    assert(ParcelMetricJson.decode(s)(changedSurface).swap.toOption.get.isInstanceOf[ParcelMetricError.InvalidOrigin])

  test("unsorted source IDs preserve display order and canonical restoration"):
    val source = volume(keys.reverse, Vector(360, 181, 180, 1)).realization
    val target = volume().realization
    val values = VectorField.tabulate(source.parcelDomain)(p => source.metadata(p).id.value.toDouble)
    val encoded = right(ParcelMetricJson.encode(source)(values, schema))
    val restored = right(ParcelMetricJson.decode(target)(encoded))
    assertEquals(restored.values.toVector, values.toVector)
    assertEquals(restored.origin.displayOrder, source.neuropublishProjection.displayOrder)

  test("foreign target domain, forged live owner and callback failures are explicit"):
    val a = volume()
    val r = a.realization
    val other = volume(keys.map(_.replace("V1", "V2"))).realization
    assertEquals(ParcelMetricJson.decode(other)(document()), Left(ParcelMetricError.DomainMismatch))
    val foreign = locus4s.FiniteDomain.ephemeral("foreign metric", 4).toOption.get
    val wrong = VectorField.tabulate(foreign.value)(_ => 1.0).asInstanceOf[Field[r.P, Double]]
    assert(ParcelMetricJson.encode(r)(wrong, schema).swap.toOption.get.isInstanceOf[ParcelMetricError.WrongOwner])
    var offset = 0.0
    val lazyValues = Field.view(r.parcelDomain)(_.ordinal.toDouble + offset)
    val encoded = right(ParcelMetricJson.encode(r)(lazyValues, schema))
    offset = 999.0
    assertEquals(right(ParcelMetricJson.decode(r)(encoded)).values.toVector, Vector(0.0, 1.0, 2.0, 3.0))
    val broken = Field.view[r.P, Double](r.parcelDomain)(_ => throw new IllegalStateException("fixture callback"))
    assert(ParcelMetricJson.encode(r)(broken, schema).swap.toOption.get.isInstanceOf[ParcelMetricError.EvaluationFailed])
    assert(ParcelMetricSchema.from(" ").isLeft)
    assert(ParcelMetricSchema.from("x", Some(" ")).isLeft)
    assert(ParcelMetricJson.decode(r)("{malformed").isLeft)

  test("Float64 bit contract preserves signed zero, infinities, subnormals and finite extremes"):
    val bitStrings = Vector("0000000000000000", "8000000000000000", "7ff0000000000000", "fff0000000000000",
      "0000000000000001", "7fefffffffffffff", "7ff8000000000000", "bff4000000000000")
    bitStrings.zipWithIndex.foreach: (bits, index) =>
      assertEquals(ParcelMetricJson.Float64.encode(right(ParcelMetricJson.Float64.decode(bits, index))), bits)
    Vector("7ff0000000000001", "fff8000000000000", "7FF8000000000000", "0", "zzzzzzzzzzzzzzzz").foreach: bits =>
      assert(ParcelMetricJson.Float64.decode(bits, 0).isLeft)
    val r = volume().realization
    val wrongNaN = rewrite(document())(p => p("values")(0) = "7ff0000000000001")
    assert(ParcelMetricJson.decode(r)(wrongNaN).swap.toOption.get.isInstanceOf[ParcelMetricError.InvalidValue])

  test("metric encoding rejects foreign field ownership at compilation"):
    val errors = compileErrors("""
      import scalafim.atlas.*
      import locus4s.data.Field
      def invalid(a: AtlasRealization, b: AtlasRealization)(values: Field[b.P, Double], schema: ParcelMetricSchema) =
        ParcelMetricJson.encode(a)(values, schema)
    """)
    assert(errors.nonEmpty)

  private def right[A](result: Either[?, A]): A = result.fold(e => fail(e.toString), identity)
