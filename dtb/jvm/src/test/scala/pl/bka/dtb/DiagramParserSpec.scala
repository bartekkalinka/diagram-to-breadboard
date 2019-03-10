package pl.bka.dtb

import org.scalatest.{FlatSpec, Matchers}
import pl.bka.dtb.model.Power.{GND, Plus}
import pl.bka.dtb.model._

class DiagramParserSpec extends FlatSpec with Matchers {
  "DiagramParser" should "parse diagram" in {
    val input =
      """
        |t.1 3 gnd 1
        |d.diode1 3 gnd
        |r.22k1 8 5
        |c.hairy1 plus 5
      """.stripMargin

    DiagramLineEncodingParser.parseDiagram(input) shouldBe Right(Seq(
      (Component("1", Transistor("")), Seq(Left(3), Right(GND), Left(1))),
      (Component("diode1", Diode("")), Seq(Left(3), Right(GND))),
      (Component("22k1", Resistor("")), Seq(Left(8), Left(5))),
      (Component("hairy1", Capacitor(0d, bipolar = true)), Seq(Right(Plus), Left(5)))
    ))
  }
}
