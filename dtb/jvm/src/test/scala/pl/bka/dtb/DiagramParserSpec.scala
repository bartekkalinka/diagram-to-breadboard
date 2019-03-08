package pl.bka.dtb

import org.scalatest.{FlatSpec, Matchers}
import pl.bka.dtb.model.{Diode, Transistor}

class DiagramParserSpec extends FlatSpec with Matchers {
  "DiagramParser" should "parse line" in {
    DiagramLineEncodingParser.parseDiagram("t 3 5 1") shouldBe Right(Transistor(""), Seq(3, 5, 1))
    DiagramLineEncodingParser.parseDiagram("d 3 5") shouldBe Right(Diode(""), Seq(3, 5))
  }
}
