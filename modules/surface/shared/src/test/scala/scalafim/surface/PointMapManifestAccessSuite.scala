package scalafim.surface

/** Outside `scalafim.surface.reference`, no API pairs manifest bytes with a
  * caller-supplied parser or builds a manifest from caller-supplied fields.
  */
class PointMapManifestAccessSuite extends munit.FunSuite:
  test("portable public byte admission binds semantics as well as the digest"):
    import scalafim.surface.reference.*
    val text = s"""{"schema":"templateflow4s.point-map/1",
      "coordinates":"RAS+ millimetres (ITK LPS converted with F = diag(-1,-1,1))",
      "applicationOrder":"stages[0] is applied first: y = stages[n-1](...(stages[0](x)))",
      "semantics":"point map: maps point coordinates of the input frame to the output frame. This is the opposite direction of image resampling with the same transform.",
      "source":{"archivePath":"tpl-SynthIn/tpl-SynthIn_from-SynthOut_mode-image_xfm.h5","sha256":"${"0" * 64}","bytes":1},
      "frames":{"input":"SynthIn","output":"SynthOut","derivation":"synthetic affine control"},
      "stages":[{"index":0,"kind":"affine","mapping":"y = matrix * [x, 1] (row-major 4x4, RAS mm)","matrix":[[1,0,0,5],[0,1,0,-2],[0,0,1,0.5],[0,0,0,1]]}]}"""
    def parse(value: String) =
      val bytes = value.getBytes("UTF-8")
      PointMapManifest.fromBytes(bytes, AssetSha256.of(bytes).value)
    val manifest = parse(text).fold(e => fail(e.message), identity)
    val map = DeclaredPointMap.fromManifest(manifest, "0" * 64, _ => None).fold(e => fail(e.message), identity)
    val point = map.map.forward(scalafim.image.WorldPoint(1, 2, 3)).placed.get
    assertEqualsDouble(point.x, 6.0, 1e-12)
    assertEqualsDouble(point.y, 0.0, 1e-12)
    assertEqualsDouble(point.z, 3.5, 1e-12)
    assert(parse(text.replace("RAS+ millimetres", "LPS millimetres")).isLeft)
    assert(parse(text.replace("\"index\":0", "\"index\":1")).isLeft)
    assert(PointMapManifest.fromBytes((text + " ").getBytes("UTF-8"), manifest.sha256.value).isLeft)

  test("matching digests cannot admit contradictory displacement semantics"):
    import scalafim.surface.reference.*
    val field = SyntheticItkOracle.manifest.fields.stages.head.asInstanceOf[ManifestStage.DisplacementEntry]
    val stageBytes = java.util.Base64.getDecoder.decode(SyntheticItkOracle.stageFile)
    def rows(values: Vector[Double]) = ujson.Arr.from(values.grouped(4).map(row => ujson.Arr.from(row)))
    def canonical = ujson.Obj(
      "schema" -> PointMapManifest.Schema,
      "coordinates" -> "RAS+ millimetres (ITK LPS converted with F = diag(-1,-1,1))",
      "applicationOrder" -> "stages[0] is applied first: y = stages[n-1](...(stages[0](x)))",
      "semantics" -> "point map: maps point coordinates of the input frame to the output frame. This is the opposite direction of image resampling with the same transform.",
      "source" -> ujson.Obj("archivePath" -> "tpl-SynthIn/tpl-SynthIn_from-SynthOut_mode-image_xfm.h5", "sha256" -> ("0" * 64), "bytes" -> 1),
      "frames" -> ujson.Obj("input" -> "SynthIn", "output" -> "SynthOut"),
      "stages" -> ujson.Arr(ujson.Obj("index" -> 0, "kind" -> "displacement", "mapping" -> "y = x + d(x)",
        "file" -> field.file, "sha256" -> field.sha256, "bytes" -> ujson.Num(field.bytes.toDouble),
        "dims" -> ujson.Arr.from(field.dims), "voxelToRas" -> rows(field.voxelToRasRowMajor),
        "dtype" -> "float64", "byteOrder" -> "little-endian", "vectorFrame" -> "RAS+ millimetres",
        "layout" -> "NIfTI-1 single file, vox_offset 352, dim [5,nx,ny,nz,1,3,1,1], intent VECTOR (1007); component c of voxel (i,j,k) is element i + nx*(j + ny*(k + nz*c))",
        "interpolation" -> "ITK DisplacementFieldTransform: d(x) = 0 unless -0.5 <= ci < n-0.5 on every axis (ci = continuous voxel index); inside that box, trilinear between voxel centres with neighbour indices clamped to [0, n-1]")))
    def admit(json: ujson.Value) =
      val bytes = ujson.write(json).getBytes("UTF-8")
      PointMapManifest.fromBytes(bytes, AssetSha256.of(bytes).value)
        .flatMap(DeclaredPointMap.fromManifest(_, "0" * 64, _ => Some(stageBytes)))
    val valid = admit(canonical)
    assert(valid.isRight, valid.toString)
    for (key, value) <- Vector("vectorFrame" -> "LPS millimetres", "mapping" -> "y = d(x)",
        "interpolation" -> "nearest", "layout" -> "interleaved vectors") do
      val json = canonical
      json("stages")(0)(key) = value
      assert(admit(json).isLeft, key)
    val reversed = canonical
    reversed("semantics") = "image push transform"
    assert(admit(reversed).isLeft)

  test("manifests cannot be forged from outside the reference package"):
    val viaParser = compileErrors(
      """scalafim.surface.reference.PointMapManifest.verified(Array.emptyByteArray, "0" * 64, _ => Left("forged"))""")
    assert(viaParser.contains("verified") && viaParser.contains("cannot be accessed"), viaParser)
    val viaApply = compileErrors("""scalafim.surface.reference.PointMapManifest(null, null)""")
    assert(viaApply.contains("does not take parameters"), viaApply)
    val viaNew = compileErrors("""new scalafim.surface.reference.PointMapManifest(null, null)""")
    assert(viaNew.contains("cannot be accessed"), viaNew)
    val viaCopy = compileErrors(
      """(??? : scalafim.surface.reference.PointMapManifest).copy(fields = (??? : scalafim.surface.reference.ManifestFields))""")
    assert(viaCopy.contains("cannot be accessed") || viaCopy.contains("copy"), viaCopy)
    assert(viaCopy.nonEmpty, "copy must not re-pair a verified digest with other fields")
