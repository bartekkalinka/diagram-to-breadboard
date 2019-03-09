package pl.bka.dtb

import org.scalatest.{FlatSpec, Matchers}
import pl.bka.dtb.model._

class DiagramParserSpec extends FlatSpec with Matchers {
  "DiagramParser" should "parse line" in {
    DiagramLineEncodingParser.parseDiagram("t.1 3 5 1") shouldBe Right(Component("1", Transistor("")), Seq(3, 5, 1))
    DiagramLineEncodingParser.parseDiagram("d.diode1 3 5") shouldBe Right(Component("diode1", Diode("")), Seq(3, 5))
    DiagramLineEncodingParser.parseDiagram("r.22k1 8 5") shouldBe Right(Component("22k1", Resistor("")), Seq(8, 5))
    DiagramLineEncodingParser.parseDiagram("c.hairy1 8 5") shouldBe Right(Component("hairy1", Capacitor(0d, bipolar = true)), Seq(8, 5))
  }
}
