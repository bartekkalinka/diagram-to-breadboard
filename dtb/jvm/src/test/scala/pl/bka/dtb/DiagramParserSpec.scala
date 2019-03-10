package pl.bka.dtb

import org.scalatest.{FlatSpec, Matchers}
import pl.bka.dtb.model.Power.{GND, Plus}
import pl.bka.dtb.model._

class DiagramParserSpec extends FlatSpec with Matchers {
  "DiagramParser" should "parse line" in {
    DiagramLineEncodingParser.parseDiagram("t.1 3 gnd 1") shouldBe Right(Component("1", Transistor("")), Seq(Left(3), Right(GND), Left(1)))
    DiagramLineEncodingParser.parseDiagram("d.diode1 3 gnd") shouldBe Right(Component("diode1", Diode("")), Seq(Left(3), Right(GND)))
    DiagramLineEncodingParser.parseDiagram("r.22k1 8 5") shouldBe Right(Component("22k1", Resistor("")), Seq(Left(8), Left(5)))
    DiagramLineEncodingParser.parseDiagram("c.hairy1 plus 5") shouldBe Right(Component("hairy1", Capacitor(0d, bipolar = true)), Seq(Right(Plus), Left(5)))
  }
}
