package pl.bka.dtb

import org.scalatest.{FlatSpec, Matchers}
import pl.bka.dtb.model.{Capacitor, Diode, Resistor, Transistor}

class DiagramParserSpec extends FlatSpec with Matchers {
  "DiagramParser" should "parse line" in {
    DiagramLineEncodingParser.parseDiagram("t 3 5 1") shouldBe Right(Transistor(""), Seq(3, 5, 1))
    DiagramLineEncodingParser.parseDiagram("d 3 5") shouldBe Right(Diode(""), Seq(3, 5))
    DiagramLineEncodingParser.parseDiagram("r 8 5") shouldBe Right(Resistor(""), Seq(8, 5))
    DiagramLineEncodingParser.parseDiagram("c 8 5") shouldBe Right(Capacitor(0d, bipolar = true), Seq(8, 5))
  }
}
