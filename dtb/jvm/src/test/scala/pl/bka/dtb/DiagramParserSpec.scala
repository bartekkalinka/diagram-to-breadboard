package pl.bka.dtb

import org.scalatest.{FlatSpec, Matchers}
import pl.bka.dtb.model.Power.{GND, Plus}
import pl.bka.dtb.model._

class DiagramParserSpec extends FlatSpec with Matchers {
  "DiagramParser" should "parse diagram" in {
    val input =
      """
        |t.tr1 3 gnd 1
        |d.diode1 3 gnd
        |r.22k1 8 5
        |c.hairy1 plus 5
      """.stripMargin

    DiagramLineEncodingParser.parseDiagram(input) shouldBe
      Right(
        Diagram(
          Seq(
            Component("tr1", Transistor("")),
            Component("diode1", Diode("")),
            Component("22k1", Resistor("")),
            Component("hairy1", Capacitor(0d, bipolar = true))
          ),
          Map(
            ("tr1", "0") -> Left(3), ("tr1", "1") -> Right(GND), ("tr1", "2") -> Left(1),
            ("diode1", "0") -> Left(3), ("diode1", "1") -> Right(GND),
            ("22k1", "0") -> Left(8), ("22k1", "1") -> Left(5),
            ("hairy1", "0") -> Right(Plus), ("hairy1", "1") -> Left(5)
          )
        )
      )
  }
}
