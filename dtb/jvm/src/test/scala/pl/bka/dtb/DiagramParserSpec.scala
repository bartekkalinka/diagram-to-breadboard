package pl.bka.dtb

import org.scalatest.{FlatSpec, Matchers}
import pl.bka.dtb.model.Power.{GND, Plus}
import pl.bka.dtb.model._

class DiagramParserSpec extends FlatSpec with Matchers {
  "DiagramParser" should "parse diagram" in {
    val input =
      """
        |t.tr1 3 gnd 1
        |d.diode1 band.3 gnd
        |d.diode2 3 band.gnd
        |r.22k1 8 5
        |c.hairy1 plus 5
        |bc.hairy2 -3 +gnd
        |bc.hairy3 +plus -5
        |i.082-1 plus 1 1 gnd gnd 2 gnd plus
      """.stripMargin

    DiagramParser.parse(input) shouldBe
      Right((Seq(
          Component("tr1", Transistor()),
          Component("diode1", Diode()),
          Component("diode2", Diode()),
          Component("22k1", Resistor()),
          Component("hairy1", Capacitor(bipolar = false)),
          Component("hairy2", Capacitor(bipolar = true)),
          Component("hairy3", Capacitor(bipolar = true)),
          Component("082-1", IC("", 8))
        ),
        Map(
          ("tr1", "0") -> Left(3), ("tr1", "1") -> Right(GND), ("tr1", "2") -> Left(1),
          ("diode1", "0") -> Left(3), ("diode1", "1") -> Right(GND),
          ("diode2", "1") -> Left(3), ("diode2", "0") -> Right(GND),
          ("22k1", "0") -> Left(8), ("22k1", "1") -> Left(5),
          ("hairy1", "0") -> Right(Plus), ("hairy1", "1") -> Left(5),
          ("hairy2", Leg.capMinus) -> Left(3), ("hairy2", Leg.capPlus) -> Right(GND),
          ("hairy3", Leg.capMinus) -> Left(5), ("hairy3", Leg.capPlus) -> Right(Plus),
          ("082-1", "0") -> Right(Plus), ("082-1", "1") -> Left(1),
          ("082-1", "2") -> Left(1), ("082-1", "3") -> Right(GND),
          ("082-1", "4") -> Right(GND), ("082-1", "5") -> Left(2),
          ("082-1", "6") -> Right(GND), ("082-1", "7") -> Right(Plus)
        )
      ))
  }

  it should "parse diagram testInput1" in {
    DiagramParser.parse(
      """
        |t.Tr549B-1 plus 1 gnd
        |t.Tr549B-2 plus 1 gnd
      """.stripMargin
    ) shouldBe
    Right((
      List(
        Component("Tr549B-1", Transistor()),
        Component("Tr549B-2", Transistor())
      ),
      Map(("Tr549B-1", "0") -> Right(Plus), ("Tr549B-1", "1") -> Left(1), ("Tr549B-1", "2") -> Right(GND),
        ("Tr549B-2", "0") -> Right(Plus), ("Tr549B-2", "1") -> Left(1), ("Tr549B-2", "2") -> Right(GND))
    ))
  }
}
