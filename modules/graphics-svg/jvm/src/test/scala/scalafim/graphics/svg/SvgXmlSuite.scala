package scalafim.graphics.svg

import java.io.StringReader
import javax.xml.parsers.DocumentBuilderFactory
import org.xml.sax.InputSource
import scalafim.graphics.*

class SvgXmlSuite extends munit.FunSuite:

  test("every renderer conformance document parses as XML") {
    val options = SvgOptions.unsafe(width = 240, height = 160, title = Some("scalafim & SVG"))
    val cases = RendererConformance.cases.fold(error => fail(error.message), identity)

    cases.foreach { conformanceCase =>
      val document = SvgRenderer
        .render(conformanceCase.scene, options)
        .fold(error => fail(error.message), identity)
      val parsed = parse(document.value)
      assertEquals(parsed.getDocumentElement.getLocalName, "svg")
    }
  }

  test("escaped text, attributes, and supplementary Unicode parse losslessly") {
    val name = GraphicsName.unsafe("name & <group> \"quoted\"")
    val text = Grob
      .text(
        "activation & <signal> \ud83e\udde0",
        Point.npcUnsafe(0.5, 0.5),
        gp = GraphicParams.unsafe(fontFamily = Some("A&B \"Sans\"")),
        name = Some(name)
      )
      .toOption
      .get
    val parsed = parse(
      SvgRenderer.render(Scene(Vector(text))).fold(error => fail(error.message), identity).value
    )
    val element = parsed.getElementsByTagName("text").item(0)

    assertEquals(element.getTextContent, "activation & <signal> \ud83e\udde0")
    assertEquals(element.getAttributes.getNamedItem("data-name").getNodeValue, name.value)
    assertEquals(element.getAttributes.getNamedItem("font-family").getNodeValue, "A&B \"Sans\"")
  }

  private def parse(value: String): org.w3c.dom.Document =
    val factory = DocumentBuilderFactory.newInstance()
    factory.setNamespaceAware(true)
    factory.newDocumentBuilder().parse(new InputSource(new StringReader(value)))
