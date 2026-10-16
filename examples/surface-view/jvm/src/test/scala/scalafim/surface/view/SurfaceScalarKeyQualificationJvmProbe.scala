package scalafim.surface.view

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import intaglio.*
import intaglio.java2d.*
import scalafim.surface.view.raster.*

/** Actual JVM publication export for the supplementary scalar-key evidence. */
object SurfaceScalarKeyQualificationJvmProbe:
  private def ok[A](value: Either[?, A]): A = value.fold(error => throw new IllegalStateException(String.valueOf(error)), identity)

  def main(args: Array[String]): Unit =
    require(args.length == 1, "usage: output-directory")
    val output = Path.of(args(0)); Files.createDirectories(output)
    val rows = for
      variant <- SurfaceScalarKeyQualificationFixture.Variants
      preset <- SurfaceScalarKeyQualificationFixture.Presets
    yield write(output, variant, preset)
    Files.writeString(output.resolve("candidate.json"), s"{\"schema\":\"surface-scalar-key-qualification.v1\",\"route\":\"supplementary-supported-public-export\",\"c10Qualification\":\"incomplete\",\"rows\":[${rows.mkString(",")}] }\n", StandardCharsets.UTF_8)

  private def write(root: Path, variant: SurfaceScalarKeyQualificationFixture.Variant,
      preset: SurfacePublicationPreset): String =
    val prepared = ok(SurfaceScalarKeyQualificationFixture.prepare(variant))
    val publication = ok(SurfaceScalarKeyQualificationFixture.publication(variant, preset))
    val artifact = ok(publication.renderWith[String]: input =>
      SurfaceRasterizer.render(input.plan, input.dimensions, SurfaceRasterStyle(background = input.background)).left.map(_.message).map(_.image))
    val png = ok(Java2DRenderer.renderPng(artifact.scene,
      ok(Java2DOptions(publication.receipt.publication.width, publication.receipt.publication.height, 96))))
    val stem = s"${variant.id}-${preset.toString.toLowerCase}"
    Files.write(root.resolve(s"$stem.png"), png)
    val endpoints = prepared.endpoints.map(endpointJson).mkString("[", ",", "]")
    val threshold = prepared.threshold.map(values => values.map(endpointJson).mkString("[", ",", "]")).getOrElse("null")
    s"{\"variant\":\"${variant.id}\",\"preset\":\"$preset\",\"image\":\"$stem.png\",\"width\":${publication.receipt.publication.width},\"height\":${publication.receipt.publication.height},\"legendKey\":${quote(prepared.legend.canonicalKey)},\"endpoints\":$endpoints,\"threshold\":$threshold,\"route\":\"actual SurfaceLegendPublication plus Java2DRenderer\"}"

  private def endpointJson(value: SurfaceScalarKeyQualificationFixture.Endpoint): String =
    s"{\"value\":${value.value},\"position\":${value.position},\"color\":${value.color.toPackedInt}}"
  private def quote(value: String): String = SceneJson.Str(value).render
