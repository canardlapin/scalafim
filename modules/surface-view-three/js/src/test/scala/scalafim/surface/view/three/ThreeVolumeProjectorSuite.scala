package scalafim.surface.view.three

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scalafim.image.*
import scalafim.image.SampleSpaces.*
import scalafim.surface.*
import scalafim.surface.view.*

class ThreeVolumeProjectorSuite extends munit.FunSuite:
  private val space = SampleSpaces(Vector(3, 2, 4))
  private val volume = SomeScalarVolume.unsafeCopyFromCanonicalArray(
    PrimitiveBuffers.tabulate[Double](24): ordinal =>
      val grid = space.indexToGrid3D(ordinal)
      grid(0).toDouble + 10.0 * grid(1) + 100.0 * grid(2),
    space,
    "asymmetric-ramp"
  )

  private def morphism(points: Vector[Vector[Double]]): VolToSurfMorphism =
    def geometry(kind: SurfaceKind): SurfaceGeometry =
      SurfaceGeometry(
        TriangleMesh.fromRows(points, Vector.tabulate(points.length - 2)(i => (0, i + 1, i + 2))),
        Hemisphere.Left,
        kind
      )
    VolToSurfMorphism(
      SurfaceDomainId("volume"),
      SurfaceDomainId("surface"),
      VolumeSurfaceSamplingPlan(SurfaceGeometryPair(geometry(SurfaceKind.White), geometry(SurfaceKind.Pial)))
    )

  // This is a texture-layout and float32 transport oracle, not a WebGL driver.
  // Texture texels use x + width * (y + height * z), independently of ScalaFIM's canonical ordinals.
  private def textureRuntime(): js.Dynamic =
    js.Dynamic.newInstance(js.Dynamic.global.Function)(
      """const three = {};
        |const noOp = function() {};
        |three.WebGLRenderer = function() {
        |  this.capabilities = { isWebGL2: true, maxTextureSize: 64 };
        |  this.extensions = { has: function() { return true; } };
        |  this.setRenderTarget = noOp;
        |  this.render = noOp;
        |  this.dispose = noOp;
        |  this.readRenderTargetPixels = function(target, x, y, w, h, output) {
        |    const c = three.coordinates;
        |    const v = three.volume;
        |    const fractional = three.fragmentShader.includes('floor(');
        |    for (let i = 0; i < w * h; i++) {
        |      if (c[i * 4 + 3] < 0.5) continue;
        |      const index = axis => fractional
        |        ? Math.floor(Math.fround(Math.fround(c[i * 4 + axis]) + Math.fround(0.5)))
        |        : Math.trunc(c[i * 4 + axis]);
        |      const vx = index(0), vy = index(1), vz = index(2);
        |      output[i * 4] = vx < 0 || vx >= v.width || vy < 0 || vy >= v.height || vz < 0 || vz >= v.depth
        |        ? 0 : v.data[vx + v.width * (vy + v.height * vz)];
        |      output[i * 4 + 3] = 1;
        |    }
        |  };
        |};
        |three.DataTexture = function(data) { three.coordinates = data; this.dispose = noOp; };
        |three.Data3DTexture = function(data, width, height, depth) {
        |  three.volume = { data, width, height, depth }; this.dispose = noOp;
        |};
        |three.WebGLRenderTarget = function() { this.dispose = noOp; };
        |three.ShaderMaterial = function(options) { three.fragmentShader = options.fragmentShader; this.dispose = noOp; };
        |three.PlaneGeometry = function() { this.dispose = noOp; };
        |three.Mesh = function(geometry) { this.geometry = geometry; };
        |three.Scene = function() { this.add = noOp; };
        |three.OrthographicCamera = function() { this.position = {}; this.updateMatrixWorld = noOp; };
        |return three;
        |""".stripMargin
    ).applyDynamic("call")(null).asInstanceOf[js.Dynamic]

  private def project(points: Vector[Vector[Double]], input: SomeScalarVolume[Double] = volume): ThreeVolumeProjectionResult =
    ThreeVolumeProjector.project(textureRuntime(), js.Dynamic.literal(), morphism(points), input)
      .fold(error => fail(error.message), identity)

  test("3D texture transport preserves asymmetric axes and non-cubic dimensions"):
    val result = project(Vector(Vector(2.0, 0.0, 1.0), Vector(0.0, 1.0, 3.0), Vector(1.0, 1.0, 0.0)))
    val expected = Vector(102.0, 310.0, 11.0)
    expected.indices.foreach: vertex =>
      assertEqualsDouble(result.projection.values.valueAt(VertexId(vertex)).get, expected(vertex), 0.0)
    assertEquals(result.projection.receipt.tally, SurfaceSampleTally(3, 0, 0, 0, 3))

  test("nearest decisions survive float32 half-voxel ties and the upper boundary"):
    val points = Vector(
      Vector(0.5 - 1e-8, 1.0, 0.0),
      Vector(0.5, 1.0, 0.0),
      Vector(0.5 + 1e-8, 1.0, 0.0),
      Vector(2.5 - 1e-8, 1.0, 0.0),
      Vector(2.5, 1.0, 0.0),
      Vector(-0.5, 1.0, 0.0),
      Vector(-0.5 - 1e-8, 1.0, 0.0)
    )
    val result = project(points)
    val expected = Vector(10.0, 11.0, 11.0, 12.0, Double.NaN, 10.0, Double.NaN)
    val cpu = SurfaceVolumeProjection.materialize(morphism(points), volume)
    expected.indices.foreach: vertex =>
      val id = VertexId(vertex)
      val actual = result.projection.values.valueAt(id).get
      if expected(vertex).isNaN then assert(actual.isNaN)
      else assertEqualsDouble(actual, expected(vertex), 0.0)
      assertEquals(result.projection.sampleCounts.valueAt(id), cpu.sampleCounts.valueAt(id))
      assertEquals(result.projection.quality.valueAt(id), cpu.quality.valueAt(id))
    assertEquals(result.projection.receipt.tally, SurfaceSampleTally(7, 2, 0, 0, 5))

  test("valid zero and non-finite texels keep the shared sample-accounting policy"):
    val data = PrimitiveBuffers.tabulate[Double](24): ordinal =>
      if ordinal == space.gridToIndex3D(1, 0, 0) then Double.NaN else 0.0
    val input = SomeScalarVolume.unsafeCopyFromCanonicalArray(data, space, "zero-and-nan")
    val result = project(Vector(Vector(0.0, 0.0, 0.0), Vector(1.0, 0.0, 0.0), Vector(3.0, 0.0, 0.0)), input)
    assertEqualsDouble(result.projection.values.valueAt(VertexId(0)).get, 0.0, 0.0)
    assert(result.projection.values.valueAt(VertexId(1)).get.isNaN)
    assert(result.projection.values.valueAt(VertexId(2)).get.isNaN)
    assertEquals(result.projection.receipt.tally, SurfaceSampleTally(3, 1, 0, 1, 1))
    // A non-finite texel is not an observation, so vertex 1 does not qualify.
    assertEquals((0 until 3).map(i => result.projection.quality.valueAt(VertexId(i)).get).toVector, Vector(true, false, false))
    assertEquals((0 until 3).map(i => result.projection.sampleCounts.valueAt(VertexId(i)).get).toVector, Vector(1, 0, 0))
    assertEquals((0 until 3).map(i => result.projection.nonFiniteCounts.valueAt(VertexId(i)).get).toVector, Vector(0, 1, 0))
    assertEquals(result.projection.receipt.vertexTally, SurfaceVertexTally(vertices = 3, qualified = 1, nonFiniteOnly = 1, insufficient = 1))
    val cpu = SurfaceVolumeProjection.materialize(
      morphism(Vector(Vector(0.0, 0.0, 0.0), Vector(1.0, 0.0, 0.0), Vector(3.0, 0.0, 0.0))), input
    )
    (0 until 3).foreach: i =>
      val id = VertexId(i)
      assertEquals(result.projection.quality.valueAt(id), cpu.quality.valueAt(id))
      assertEquals(result.projection.sampleCounts.valueAt(id), cpu.sampleCounts.valueAt(id))
      assertEquals(result.projection.nonFiniteCounts.valueAt(id), cpu.nonFiniteCounts.valueAt(id))
    assertEquals(result.projection.receipt.vertexTally, cpu.receipt.vertexTally)

  test("GPU refuses finite volume values that float32 cannot represent instead of dropping them"):
    // 1e300 is a finite observation on the CPU path but would upload as an infinite texel.
    val data = PrimitiveBuffers.tabulate[Double](24): ordinal =>
      if ordinal == space.gridToIndex3D(1, 0, 0) then 1e300 else 0.0
    val input = SomeScalarVolume.unsafeCopyFromCanonicalArray(data, space, "float32-overflow")
    val points = Vector(Vector(0.0, 0.0, 0.0), Vector(1.0, 0.0, 0.0), Vector(2.0, 0.0, 0.0))
    val cpu = SurfaceVolumeProjection.materialize(morphism(points), input)
    assertEquals(cpu.quality.valueAt(VertexId(1)), Some(true))
    ThreeVolumeProjector.project(textureRuntime(), js.Dynamic.literal(), morphism(points), input) match
      case Left(ThreeSurfaceError.InvalidPlan(reason)) => assert(reason.contains("float32"), reason)
      case other => fail(s"expected a float32 range refusal; got $other")

  test("GPU and CPU fill a NaN-only vertex identically"):
    val data = PrimitiveBuffers.tabulate[Double](24): ordinal =>
      if ordinal == space.gridToIndex3D(1, 0, 0) then Double.NaN else 7.0
    val input = SomeScalarVolume.unsafeCopyFromCanonicalArray(data, space, "nan")
    val points = Vector(Vector(0.0, 0.0, 0.0), Vector(1.0, 0.0, 0.0), Vector(2.0, 0.0, 0.0))
    val policy = SurfaceProjectionPolicy(fill = SurfaceProjectionFill.constant(-1.0).toOption.get)
    val gpu = ThreeVolumeProjector.project(textureRuntime(), js.Dynamic.literal(), morphism(points), input, policy)
      .fold(error => fail(error.message), identity)
    val cpu = SurfaceVolumeProjection.materialize(morphism(points), input, policy)
    (0 until 3).foreach: i =>
      val id = VertexId(i)
      assertEquals(gpu.projection.values.valueAt(id), cpu.values.valueAt(id))
      assertEquals(gpu.projection.quality.valueAt(id), cpu.quality.valueAt(id))
    assertEquals(gpu.projection.values.valueAt(VertexId(1)), Some(-1.0))

  test("GPU nearest transport rejects far-outside coordinates before narrowing indices"):
    val result = project(Vector(Vector(4294967296.0, 0.0, 0.0), Vector(-4294967296.0, 1.0, 0.0), Vector(2.0, 0.0, 1.0)))
    assert(result.projection.values.valueAt(VertexId(0)).get.isNaN)
    assert(result.projection.values.valueAt(VertexId(1)).get.isNaN)
    assertEqualsDouble(result.projection.values.valueAt(VertexId(2)).get, 102.0, 0.0)
    assertEquals(result.projection.receipt.tally, SurfaceSampleTally(3, 2, 0, 0, 1))

  /** Test-only export: runs the production projector with a real injected Three.js/WebGL runtime. */
  def browserParity(three: js.Dynamic): js.Dynamic =
    val cases = Vector(
      (Vector(Vector(2.0, 0.0, 1.0), Vector(0.0, 1.0, 3.0), Vector(1.0, 1.0, 0.0)), Vector(102.0, 310.0, 11.0)),
      (Vector(
        Vector(0.5 - 1e-8, 1.0, 0.0), Vector(0.5, 1.0, 0.0), Vector(0.5 + 1e-8, 1.0, 0.0),
        Vector(2.5 - 1e-8, 1.0, 0.0), Vector(2.5, 1.0, 0.0), Vector(-0.5, 1.0, 0.0), Vector(-0.5 - 1e-8, 1.0, 0.0)
      ), Vector(10.0, 11.0, 11.0, 12.0, Double.NaN, 10.0, Double.NaN)),
      (Vector(Vector(4294967296.0, 0.0, 0.0), Vector(-4294967296.0, 1.0, 0.0), Vector(2.0, 0.0, 1.0)), Vector(Double.NaN, Double.NaN, 102.0))
    )
    var mismatches = 0
    var observations = 0
    cases.foreach: (points, expected) =>
      val canvas = js.Dynamic.global.document.applyDynamic("createElement")("canvas")
      val result = ThreeVolumeProjector.project(three, canvas, morphism(points), volume)
        .fold(error => throw new IllegalStateException(error.message), identity)
      expected.indices.foreach: vertex =>
        val id = VertexId(vertex)
        val actual = result.projection.values.valueAt(id).get
        val valid = !expected(vertex).isNaN
        if (valid && (!actual.isFinite || math.abs(actual - expected(vertex)) > 0.0)) || (!valid && !actual.isNaN) then mismatches += 1
        if result.projection.sampleCounts.valueAt(id).get != (if valid then 1 else 0) then mismatches += 1
        if result.projection.quality.valueAt(id).get != valid then mismatches += 1
        observations += 1
    js.Dynamic.literal(status = (if mismatches == 0 then "pass" else "fail"), observations = observations, mismatches = mismatches)

object ThreeVolumeProjectionBrowserParity:
  @JSExportTopLevel("runScalafimVolumeProjectionParity")
  def run(three: js.Dynamic): js.Dynamic =
    new ThreeVolumeProjectorSuite().browserParity(three)
