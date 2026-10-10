package scalafim.surface.view.three

import scalafim.image.SampleSpaces.*

import scala.scalajs.js
import scala.scalajs.js.typedarray.Float32Array

import scalafim.image.*
import scalafim.surface.*
import scalafim.surface.view.*

final case class ThreeVolumeProjectionReceipt(
  width: Int,
  height: Int,
  vertices: Int,
  renderedPixels: Int,
  uploadedBytes: Long,
  elapsedNanos: Long
)

final case class ThreeVolumeProjectionResult(
  projection: SurfaceProjectionResult,
  receipt: ThreeVolumeProjectionReceipt
)

object ThreeVolumeProjector:
  def project(
    three: js.Dynamic,
    canvas: js.Dynamic,
    morphism: VolToSurfMorphism,
    volume: SomeScalarVolume[Double],
    policy: SurfaceProjectionPolicy = SurfaceProjectionPolicy()
  ): Either[ThreeSurfaceError, ThreeVolumeProjectionResult] =
    if morphism.plan.path != SurfaceSamplingPath.Midpoint ||
      morphism.plan.aggregation != SurfaceSampleAggregation.Nearest
    then
      Left(ThreeSurfaceError.InvalidPlan(
        "GPU volume projection currently admits midpoint sampling with nearest reduction only"
      ))
    else if policy.minimumSamples.value != 1 then
      Left(ThreeSurfaceError.InvalidPlan("GPU midpoint projection requires minimumSamples=1"))
    else
      attempt("GPU volume projection"):
        val started = nowNanos()
        val renderer = js.Dynamic.newInstance(three.selectDynamic("WebGLRenderer"))(
          js.Dynamic.literal(canvas = canvas, antialias = false, alpha = false)
        )
        try
          val webgl2 = renderer.selectDynamic("capabilities").selectDynamic("isWebGL2").asInstanceOf[Boolean]
          val floatTargets = renderer.selectDynamic("extensions")
            .applyDynamic("has")("EXT_color_buffer_float").asInstanceOf[Boolean]
          if !webgl2 || !floatTargets then
            throw new IllegalStateException("WebGL2 with EXT_color_buffer_float is required")
          val vertexCount = morphism.plan.surfaces.white.vertexCount
          val maximumWidth = renderer.selectDynamic("capabilities").selectDynamic("maxTextureSize").asInstanceOf[Int]
          val width = math.min(vertexCount, maximumWidth)
          val height = (vertexCount + width - 1) / width
          val coordinates = new Float32Array(width * height * 4)
          var accepted = 0
          var vertex = 0
          while vertex < vertexCount do
            val id = VertexId.unsafe(vertex)
            val white = worldPoint(morphism.plan.surfaces.white, id)
            val pial = worldPoint(morphism.plan.surfaces.pial, id)
            val world = Vector(
              white.x + 0.5 * (pial.x - white.x),
              white.y + 0.5 * (pial.y - white.y),
              white.z + 0.5 * (pial.z - white.z)
            )
            // Resolve nearest ties in double precision, as the shared CPU sampler does.
            // Uploading fractional float32 coordinates can cross a half-voxel boundary.
            val continuous = volume.space.coordToIndex(world)
            val voxel = continuous.map(v => math.round(v))
            val valid = voxel.indices.forall: axis =>
              continuous(axis).isFinite && voxel(axis) >= 0L && voxel(axis) < volume.space.spatialDims(axis).toLong
            val offset = vertex * 4
            coordinates(offset) = voxel(0).toFloat
            coordinates(offset + 1) = voxel(1).toFloat
            coordinates(offset + 2) = voxel(2).toFloat
            coordinates(offset + 3) = if valid then 1.0f else 0.0f
            if valid then accepted += 1
            vertex += 1

          val dimensions = volume.space.spatialDims
          val volumeValues = dimensions.product
          val volumeData = new Float32Array(volumeValues)
          // Canonical ScalaFIM ordinals are Z-fast; WebGL texels are X-fast.
          val canonicalXStride = dimensions(1) * dimensions(2)
          var valueIndex = 0
          var z = 0
          while z < dimensions(2) do
            var y = 0
            while y < dimensions(1) do
              var x = 0
              while x < dimensions(0) do
                val canonicalOrdinal = x * canonicalXStride + y * dimensions(2) + z
                volumeData(valueIndex) = volume.valueAtCanonicalOrdinal(canonicalOrdinal).toFloat
                valueIndex += 1
                x += 1
              y += 1
            z += 1

          val coordinateTexture = js.Dynamic.newInstance(three.selectDynamic("DataTexture"))(
            coordinates,
            width,
            height,
            three.selectDynamic("RGBAFormat"),
            three.selectDynamic("FloatType")
          )
          coordinateTexture.updateDynamic("minFilter")(three.selectDynamic("NearestFilter"))
          coordinateTexture.updateDynamic("magFilter")(three.selectDynamic("NearestFilter"))
          coordinateTexture.updateDynamic("generateMipmaps")(false)
          coordinateTexture.updateDynamic("needsUpdate")(true)

          val volumeTexture = js.Dynamic.newInstance(three.selectDynamic("Data3DTexture"))(
            volumeData,
            dimensions(0),
            dimensions(1),
            dimensions(2)
          )
          volumeTexture.updateDynamic("format")(three.selectDynamic("RedFormat"))
          volumeTexture.updateDynamic("type")(three.selectDynamic("FloatType"))
          volumeTexture.updateDynamic("minFilter")(three.selectDynamic("NearestFilter"))
          volumeTexture.updateDynamic("magFilter")(three.selectDynamic("NearestFilter"))
          volumeTexture.updateDynamic("unpackAlignment")(1)
          volumeTexture.updateDynamic("generateMipmaps")(false)
          volumeTexture.updateDynamic("needsUpdate")(true)

          val target = js.Dynamic.newInstance(three.selectDynamic("WebGLRenderTarget"))(
            width,
            height,
            js.Dynamic.literal(
              format = three.selectDynamic("RGBAFormat"),
              `type` = three.selectDynamic("FloatType"),
              depthBuffer = false,
              stencilBuffer = false,
              minFilter = three.selectDynamic("NearestFilter"),
              magFilter = three.selectDynamic("NearestFilter")
            )
          )
          val uniforms = js.Dynamic.literal(
            uCoordinates = js.Dynamic.literal(value = coordinateTexture),
            uVolume = js.Dynamic.literal(value = volumeTexture)
          )
          val material = js.Dynamic.newInstance(three.selectDynamic("ShaderMaterial"))(
            js.Dynamic.literal(
              uniforms = uniforms,
              glslVersion = three.selectDynamic("GLSL3"),
              vertexShader = VertexShader,
              fragmentShader = FragmentShader,
              depthTest = false,
              depthWrite = false
            )
          )
          val quad = js.Dynamic.newInstance(three.selectDynamic("Mesh"))(
            js.Dynamic.newInstance(three.selectDynamic("PlaneGeometry"))(2, 2),
            material
          )
          quad.updateDynamic("frustumCulled")(false)
          val scene = js.Dynamic.newInstance(three.selectDynamic("Scene"))()
          scene.applyDynamic("add")(quad)
          val camera = js.Dynamic.newInstance(three.selectDynamic("OrthographicCamera"))(-1, 1, 1, -1, 0.1, 10)
          camera.selectDynamic("position").updateDynamic("z")(1)
          camera.applyDynamic("updateMatrixWorld")()
          renderer.applyDynamic("setRenderTarget")(target)
          renderer.applyDynamic("render")(scene, camera)
          val output = new Float32Array(width * height * 4)
          renderer.applyDynamic("readRenderTargetPixels")(target, 0, 0, width, height, output)
          renderer.applyDynamic("setRenderTarget")(null)

          val values = new Array[Double](vertexCount)
          val counts = new Array[Int](vertexCount)
          val nonFiniteCounts = new Array[Int](vertexCount)
          val quality = new Array[Boolean](vertexCount)
          var renderedPixels = 0
          var nonFinite = 0L
          vertex = 0
          while vertex < vertexCount do
            val valid = coordinates(vertex * 4 + 3) > 0.5f
            // A non-finite texel is not an observation: with minimumSamples=1 the
            // vertex does not qualify, as in the shared CPU projection.
            val finite = valid && output(vertex * 4).toDouble.isFinite
            if valid && !finite then nonFinite += 1L
            counts(vertex) = if finite then 1 else 0
            nonFiniteCounts(vertex) = if valid && !finite then 1 else 0
            quality(vertex) = finite
            values(vertex) =
              if finite then output(vertex * 4).toDouble
              else policy.fill match
                case SurfaceProjectionFill.NaN => Double.NaN
                case SurfaceProjectionFill.Constant(value) => value
            if output(vertex * 4 + 3) > 0.5f then renderedPixels += 1
            vertex += 1
          val geometry = morphism.plan.surfaces.white
          val projection = SurfaceProjectionResult(
            SurfaceField.full(geometry, values.toIndexedSeq, "gpu-surface-sample"),
            SurfaceField.full(geometry, counts.toIndexedSeq, "gpu-surface-sample-count"),
            SurfaceField.full(geometry, nonFiniteCounts.toIndexedSeq, "gpu-surface-non-finite-sample-count"),
            SurfaceField.full(geometry, quality.toIndexedSeq, "gpu-surface-projection-quality"),
            SurfaceProjectionReceipt(
              // One sample per vertex, so a non-finite vertex is non-finite only.
              SurfaceVertexTally(
                vertices = vertexCount,
                qualified = accepted - nonFinite.toInt,
                nonFiniteOnly = nonFinite.toInt,
                insufficient = vertexCount - accepted
              ),
              // One midpoint sample per vertex; the GPU path applies no mask.
              SurfaceSampleTally(
                requested = vertexCount.toLong,
                outsideVolume = (vertexCount - accepted).toLong,
                masked = 0L,
                nonFinite = nonFinite,
                accepted = accepted.toLong - nonFinite
              ),
              volumeValues.toLong,
              volumeValues.toLong * 8L,
              vertexCount.toLong * 17L,
              nowNanos() - started,
              morphism.plan.path,
              morphism.plan.aggregation
            )
          )
          quad.selectDynamic("geometry").applyDynamic("dispose")()
          material.applyDynamic("dispose")()
          coordinateTexture.applyDynamic("dispose")()
          volumeTexture.applyDynamic("dispose")()
          target.applyDynamic("dispose")()
          ThreeVolumeProjectionResult(
            projection,
            ThreeVolumeProjectionReceipt(
              width,
              height,
              vertexCount,
              renderedPixels,
              coordinates.byteLength.toLong + volumeData.byteLength.toLong,
              nowNanos() - started
            )
          )
        finally renderer.applyDynamic("dispose")()

  private def worldPoint(geometry: SurfaceGeometry, vertex: VertexId): WorldPoint =
    SurfaceWorldLink.worldPoint(geometry, vertex)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private def attempt[A](operation: String)(body: => A): Either[ThreeSurfaceError, A] =
    try Right(body)
    catch case error: Throwable => Left(ThreeSurfaceError.RuntimeFailure(operation, Option(error.getMessage).getOrElse(error.toString)))

  private def nowNanos(): Long =
    (js.Dynamic.global.performance.applyDynamic("now")().asInstanceOf[Double] * 1e6).toLong

  private val VertexShader =
    """void main() {
      |  gl_Position = vec4(position, 1.0);
      |}
      |""".stripMargin

  private val FragmentShader =
    """precision highp float;
      |precision highp sampler3D;
      |uniform sampler2D uCoordinates;
      |uniform sampler3D uVolume;
      |layout(location = 0) out vec4 outColor;
      |void main() {
      |  ivec2 pixel = ivec2(gl_FragCoord.xy);
      |  vec4 coordinate = texelFetch(uCoordinates, pixel, 0);
      |  if (coordinate.a < 0.5) {
      |    outColor = vec4(0.0);
      |  } else {
      |    ivec3 voxel = ivec3(coordinate.xyz);
      |    float value = texelFetch(uVolume, voxel, 0).r;
      |    outColor = vec4(value, 1.0, 0.0, 1.0);
      |  }
      |}
      |""".stripMargin
