package pl.bka.dtb

import org.scalatest.{FlatSpec, Matchers}
import pl.bka.dtb.model.Diode

class DiagramParserSpec extends FlatSpec with Matchers {
  "DiagramParser" should "parse component type" in {
    DiagramLineEncodingParser.parse(DiagramLineEncodingParser.componentType, "d").get shouldBe Diode("")
  }
}
