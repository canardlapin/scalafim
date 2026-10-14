package scalafim.surface.view.three

import intaglio.*
import scalafim.surface.view.*

class ThreeFragmentShaderPipelineSuite extends munit.FunSuite:
  test("linear-light packet carries source sampling contract and clip/opacity shader uniforms"):
    val plan = SurfaceScalarFixture.plan()
    val packet = ThreeFragmentShader.compile(plan.meshes.head, plan.layers,
      SurfaceColorPipeline.DisplaySrgbLinearLight, Rgba32.unsafe(0, 0, 0, 0)).toOption.get
    assertEquals(packet.sourceMesh, Some(plan.meshes.head))
    assertEquals(packet.sourceLayers, plan.layers.filter(_.surface == plan.meshes.head.surface))
    assertEquals(packet.colorPipeline, SurfaceColorPipeline.DisplaySrgbLinearLight)
    assertEquals(packet.surfaceBase, Rgba32.unsafe(0, 0, 0, 0))
    assert(packet.vertexShader.contains("vWorldPosition = (modelMatrix * vec4(position, 1.0)).xyz"))
    assert(packet.fragmentShader.contains("uniform float uSurfaceOpacity"))
    assert(packet.fragmentShader.contains("uniform int uClipCount"))
    assert(packet.fragmentShader.contains("uniform vec4 uClipPlanes[8]"))
    assert(packet.fragmentShader.contains("if (alpha <= 0.0) discard"))

  test("legacy packets keep encoded-byte lighting while linear packets decode and encode once"):
    val plan = SurfaceScalarFixture.plan()
    val legacy = ThreeFragmentShader.compile(plan.meshes.head, plan.layers).toOption.get
    val linear = ThreeFragmentShader.compile(plan.meshes.head, plan.layers,
      SurfaceColorPipeline.DisplaySrgbLinearLight).toOption.get
    assert(!legacy.fragmentShader.contains("linearToSrgb(srgbToLinear"))
    assert(linear.fragmentShader.contains("linearToSrgb(srgbToLinear"))
    assert(linear.fragmentShader.contains("srgbToLinear"))
    assert(linear.fragmentShader.contains("outAlpha"))
