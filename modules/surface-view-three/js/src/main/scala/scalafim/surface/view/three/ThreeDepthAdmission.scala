package scalafim.surface.view.three

import scala.scalajs.js
import scala.scalajs.js.typedarray.{Float32Array, Uint8Array}

/** Small native capability check, independent of surface compilation. The red
  * triangle covers the complete tested pixel and every red vertex is nearer
  * than every green vertex. Multisampling must not expose the green triangle.
  * These clip coordinates reduce an observed cortical depth-ordering failure.
  */
private[three] object ThreeDepthAdmission:
  val Positions: Vector[Double] = Vector(
    0.2984880387875073, 0.9149595922332878, -0.47781183593737453,
    -0.573554461565223, -0.021142485787464693, -0.4766295261365914,
    0.20505161282218154, -0.32764376184935884, -0.4781022651073559,
    0.15369809971540604, 0.7115875281530464, -0.4630327327636765,
    0.34888187531136694, 1.2537314724282993, -0.4611825913627885,
    -0.1243654886001444, -0.2099440321898811, -0.4610836589246222)

  def admit(samples: Int, red: Int, green: Int, blue: Int): Either[ThreeSurfaceError, Unit] =
    if red >= 254 && green <= 1 && blue <= 1 then Right(())
    else Left(ThreeSurfaceError.NativeDepthMismatch(samples, red, green, blue))

  def check(three: js.Dynamic, renderer: js.Dynamic): Either[ThreeSurfaceError, Unit] =
    def construct(name: String)(args: js.Any*): js.Dynamic = js.Dynamic.newInstance(three.selectDynamic(name))(args*)
    def floats(values: Vector[Double]): Float32Array =
      val array = new Float32Array(values.size)
      values.indices.foreach(i => array(i) = values(i).toFloat)
      array
    val gl = renderer.applyDynamic("getContext")()
    val samples = gl.applyDynamic("getParameter")(gl.SAMPLES).asInstanceOf[Int]
    val previousTarget = renderer.applyDynamic("getRenderTarget")()
    val target = construct("WebGLRenderTarget")(8, 8, js.Dynamic.literal(samples = samples,
      depthBuffer = true, stencilBuffer = false))
    val geometry = construct("BufferGeometry")()
    val material = construct("ShaderMaterial")(js.Dynamic.literal(
      vertexShader = "varying vec3 vColor; void main() { gl_Position = vec4(position, 1.0); vColor = color; }",
      fragmentShader = "varying vec3 vColor; void main() { gl_FragColor = vec4(vColor, 1.0); }",
      vertexColors = true, precision = "highp", toneMapped = false, side = three.selectDynamic("DoubleSide")))
    try
      geometry.applyDynamic("setAttribute")("position", construct("BufferAttribute")(floats(Positions), 3))
      val colors = Vector.fill(3)(Vector(1.0, 0.0, 0.0)).flatten ++ Vector.fill(3)(Vector(0.0, 1.0, 0.0)).flatten
      geometry.applyDynamic("setAttribute")("color", construct("BufferAttribute")(floats(colors), 3))
      val mesh = construct("Mesh")(geometry, material)
      mesh.updateDynamic("frustumCulled")(false)
      val scene = construct("Scene")()
      scene.applyDynamic("add")(mesh)
      val camera = construct("Camera")()
      val pixel = new Uint8Array(4)
      var outcome: Either[ThreeSurfaceError, Unit] = Right(())
      for reverse <- Vector(false, true) if outcome.isRight do
        geometry.applyDynamic("setIndex")(if reverse then js.Array(3, 4, 5, 0, 1, 2) else js.Array(0, 1, 2, 3, 4, 5))
        renderer.applyDynamic("setRenderTarget")(target)
        renderer.applyDynamic("setClearColor")(0xffffff, 1.0)
        renderer.applyDynamic("clear")(true, true, true)
        renderer.applyDynamic("render")(scene, camera)
        renderer.applyDynamic("readRenderTargetPixels")(target, 3, 4, 1, 1, pixel)
        outcome = admit(samples, pixel(0).toInt, pixel(1).toInt, pixel(2).toInt)
      outcome
    finally
      renderer.applyDynamic("setRenderTarget")(previousTarget)
      geometry.applyDynamic("dispose")()
      material.applyDynamic("dispose")()
      target.applyDynamic("dispose")()
