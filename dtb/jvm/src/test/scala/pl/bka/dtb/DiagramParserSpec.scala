package pl.bka.dtb

import org.scalatest.{FlatSpec, Matchers}
import pl.bka.dtb.model.Transistor

class DiagramParserSpec extends FlatSpec with Matchers {
  "DiagramParser" should "parse line" in {
    DiagramLineEncodingParser.parseDiagram("t 3") shouldBe Right(Transistor(""), 3)
  }
}
