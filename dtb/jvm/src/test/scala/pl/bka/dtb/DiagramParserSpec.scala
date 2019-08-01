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
        |n.node1 4
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
          Component("082-1", IC(8)),
          Component("node1", Node())
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
          ("082-1", "6") -> Right(GND), ("082-1", "7") -> Right(Plus),
          ("node1", "0") -> Left(4)
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

  it should "parse diagram testInput2" in {
    DiagramParser.parse(
      """
        |t.Tr549B-1 plus 0 gnd
        |t.Tr549B-2 plus 0 gnd
        |r.R220-1 gnd 0
        |r.R220-2 plus 0
        |r.R220-3 0 gnd
        |r.R220-4 gnd 0
        |r.R220-5 0 plus
        |r.R220-6 gnd 0
      """.stripMargin
    ) shouldBe
    Right((
      List(
        Component("Tr549B-1", Transistor()),
        Component("Tr549B-2", Transistor()),
        Component("R220-1", Resistor()),
        Component("R220-2", Resistor()),
        Component("R220-3", Resistor()),
        Component("R220-4", Resistor()),
        Component("R220-5", Resistor()),
        Component("R220-6", Resistor())
      ),
      Map(("Tr549B-1", "0") -> Right(Plus), ("Tr549B-1", "1") -> Left(0), ("Tr549B-1", "2") -> Right(GND),
        ("Tr549B-2", "0") -> Right(Plus), ("Tr549B-2", "1") -> Left(0), ("Tr549B-2", "2") -> Right(GND),
        ("R220-1", Leg.firstLeg) -> Right(GND), ("R220-1", Leg.secondLeg) -> Left(0),
        ("R220-2", Leg.firstLeg) -> Right(Plus), ("R220-2", Leg.secondLeg) -> Left(0),
        ("R220-3", Leg.firstLeg) -> Left(0), ("R220-3", Leg.secondLeg) -> Right(GND),
        ("R220-4", Leg.firstLeg) -> Right(GND), ("R220-4", Leg.secondLeg) -> Left(0),
        ("R220-5", Leg.firstLeg) -> Left(0), ("R220-5", Leg.secondLeg) -> Right(Plus),
        ("R220-6", Leg.firstLeg) -> Right(GND), ("R220-6", Leg.secondLeg) -> Left(0))
    ))
  }

  it should "parse diagram testInput3" in {
    DiagramParser.parse(
      """
        |t.Tr549B-1 plus 0 gnd
        |r.R220-1 gnd 1
      """.stripMargin
    ) shouldBe
    Right((
      List(
        Component("Tr549B-1", Transistor()),
        Component("R220-1", Resistor())
      ),
      Map(("Tr549B-1", "0") -> Right(Plus), ("Tr549B-1", "1") -> Left(0), ("Tr549B-1", "2") -> Right(GND),
        ("R220-1", Leg.firstLeg) -> Right(GND), ("R220-1", Leg.secondLeg) -> Left(1))
    ))
  }

  it should "parse diagram testInput4" in {
    DiagramParser.parse(
      """
        |t.549B-1 plus 0 gnd
        |t.549B-2 plus 0 gnd
        |r.R220-1 gnd 0
        |r.R220-2 plus 0
        |i.082-1 plus 1 1 gnd gnd 2 gnd plus
      """.stripMargin
    ) shouldBe
    Right((
      List(
        Component("549B-1", Transistor()),
        Component("549B-2", Transistor()),
        Component("R220-1", Resistor()),
        Component("R220-2", Resistor()),
        Component("082-1", IC(8))
      ),
      Map(("549B-1", "0") -> Right(Plus), ("549B-1", "1") -> Left(0), ("549B-1", "2") -> Right(GND),
        ("549B-2", "0") -> Right(Plus), ("549B-2", "1") -> Left(0), ("549B-2", "2") -> Right(GND),
        ("R220-1", Leg.firstLeg) -> Right(GND), ("R220-1", Leg.secondLeg) -> Left(0),
        ("R220-2", Leg.firstLeg) -> Right(Plus), ("R220-2", Leg.secondLeg) -> Left(0),
        ("082-1", "0") -> Right(Plus), ("082-1", "1") -> Left(1),
        ("082-1", "2") -> Left(1), ("082-1", "3") -> Right(GND),
        ("082-1", "4") -> Right(GND), ("082-1", "5") -> Left(2),
        ("082-1", "6") -> Right(GND), ("082-1", "7") -> Right(Plus)
      )
    ))
  }

  it should "parse 3-roll diagram" in {
    DiagramParser.parse(
      """
        |d.diode band.1 plus
        |r.R470K-1 1 3
        |r.R22K-1 1 2
        |r.R470K-2 1 7
        |r.R22K-2 1 6
        |r.R470K-3 1 5
        |r.R22K-3 1 4
        |t.Tr-1 2 3 gnd
        |t.Tr-2 6 7 gnd
        |t.Tr-3 4 5 gnd
        |bc.hairy-1 -7 +2
        |bc.hairy-2 -3 +4
        |bc.hairy-3 -5 +6
        |bc.cap-4 -gnd +1
      """.stripMargin
    ) shouldBe
    Right((
      List(
        Component("diode", Diode()),
        Component("R470K-1", Resistor()),
        Component("R22K-1", Resistor()),
        Component("R470K-2", Resistor()),
        Component("R22K-2", Resistor()),
        Component("R470K-3", Resistor()),
        Component("R22K-3", Resistor()),
        Component("Tr-1", Transistor()),
        Component("Tr-2", Transistor()),
        Component("Tr-3", Transistor()),
        Component("hairy-1", Capacitor(bipolar = true)),
        Component("hairy-2", Capacitor(bipolar = true)),
        Component("hairy-3", Capacitor(bipolar = true)),
        Component("cap-4", Capacitor(bipolar = true))
      ),
      Map(
        ("diode", Leg.cathode) -> Left(1), ("diode", Leg.anode) -> Right(Plus),
        ("R470K-1", Leg.firstLeg) -> Left(1), ("R470K-1", Leg.secondLeg) -> Left(3),
        ("R22K-1", Leg.firstLeg) -> Left(1), ("R22K-1", Leg.secondLeg) -> Left(2),
        ("R470K-2", Leg.firstLeg) -> Left(1), ("R470K-2", Leg.secondLeg) -> Left(7),
        ("R22K-2", Leg.firstLeg) -> Left(1), ("R22K-2", Leg.secondLeg) -> Left(6),
        ("R470K-3", Leg.firstLeg) -> Left(1), ("R470K-3", Leg.secondLeg) -> Left(5),
        ("R22K-3", Leg.firstLeg) -> Left(1), ("R22K-3", Leg.secondLeg) -> Left(4),
        ("Tr-1", "0") -> Left(2), ("Tr-1", "1") -> Left(3), ("Tr-1", "2") -> Right(GND),
        ("Tr-2", "0") -> Left(6), ("Tr-2", "1") -> Left(7), ("Tr-2", "2") -> Right(GND),
        ("Tr-3", "0") -> Left(4), ("Tr-3", "1") -> Left(5), ("Tr-3", "2") -> Right(GND),
        ("hairy-1", Leg.capMinus) -> Left(7), ("hairy-1", Leg.capPlus) -> Left(2),
        ("hairy-2", Leg.capMinus) -> Left(3), ("hairy-2", Leg.capPlus) -> Left(4),
        ("hairy-3", Leg.capMinus) -> Left(5), ("hairy-3", Leg.capPlus) -> Left(6),
        ("cap-4", Leg.capMinus) -> Right(GND), ("cap-4", Leg.capPlus) -> Left(1)
      )
    ))
  }

  it should "parser 4-roll diagram" in {
    DiagramParser.parse(
      """
        |d.diode band.9 plus
        |r.R470K-1 9 4
        |r.R22K-1 9 7
        |r.R470K-2 9 2
        |r.R22K-2 9 6
        |r.R470K-3 9 3
        |r.R22K-3 9 8
        |r.R470K-4 9 1
        |r.R22K-4 9 5
        |t.Tr-1 5 1 gnd
        |t.Tr-2 8 3 gnd
        |t.Tr-3 6 2 gnd
        |t.Tr-4 7 4 gnd
        |bc.hairy-1 -1 +8
        |bc.hairy-2 -3 +6
        |bc.hairy-3 -2 +7
        |bc.hairy-4 -4 +5
        |bc.cap-5 -gnd +9
      """.stripMargin
    ) shouldBe
    Right((
      List(
        Component("diode", Diode()),
        Component("R470K-1", Resistor()),
        Component("R22K-1", Resistor()),
        Component("R470K-2", Resistor()),
        Component("R22K-2", Resistor()),
        Component("R470K-3", Resistor()),
        Component("R22K-3", Resistor()),
        Component("R470K-4", Resistor()),
        Component("R22K-4", Resistor()),
        Component("Tr-1", Transistor()),
        Component("Tr-2", Transistor()),
        Component("Tr-3", Transistor()),
        Component("Tr-4", Transistor()),
        Component("hairy-1", Capacitor(bipolar = true)),
        Component("hairy-2", Capacitor(bipolar = true)),
        Component("hairy-3", Capacitor(bipolar = true)),
        Component("hairy-4", Capacitor(bipolar = true)),
        Component("cap-5", Capacitor(bipolar = true))
      ),
      Map(
        ("diode", Leg.cathode) -> Left(9), ("diode", Leg.anode) -> Right(Plus),
        ("R470K-1", Leg.firstLeg) -> Left(9), ("R470K-1", Leg.secondLeg) -> Left(4),
        ("R22K-1", Leg.firstLeg) -> Left(9), ("R22K-1", Leg.secondLeg) -> Left(7),
        ("R470K-2", Leg.firstLeg) -> Left(9), ("R470K-2", Leg.secondLeg) -> Left(2),
        ("R22K-2", Leg.firstLeg) -> Left(9), ("R22K-2", Leg.secondLeg) -> Left(6),
        ("R470K-3", Leg.firstLeg) -> Left(9), ("R470K-3", Leg.secondLeg) -> Left(3),
        ("R22K-3", Leg.firstLeg) -> Left(9), ("R22K-3", Leg.secondLeg) -> Left(8),
        ("R470K-4", Leg.firstLeg) -> Left(9), ("R470K-4", Leg.secondLeg) -> Left(1),
        ("R22K-4", Leg.firstLeg) -> Left(9), ("R22K-4", Leg.secondLeg) -> Left(5),
        ("Tr-1", "0") -> Left(5), ("Tr-1", "1") -> Left(1), ("Tr-1", "2") -> Right(GND),
        ("Tr-2", "0") -> Left(8), ("Tr-2", "1") -> Left(3), ("Tr-2", "2") -> Right(GND),
        ("Tr-3", "0") -> Left(6), ("Tr-3", "1") -> Left(2), ("Tr-3", "2") -> Right(GND),
        ("Tr-4", "0") -> Left(7), ("Tr-4", "1") -> Left(4), ("Tr-4", "2") -> Right(GND),
        ("hairy-1", Leg.capMinus) -> Left(1), ("hairy-1", Leg.capPlus) -> Left(8),
        ("hairy-2", Leg.capMinus) -> Left(3), ("hairy-2", Leg.capPlus) -> Left(6),
        ("hairy-3", Leg.capMinus) -> Left(2), ("hairy-3", Leg.capPlus) -> Left(7),
        ("hairy-4", Leg.capMinus) -> Left(4), ("hairy-4", Leg.capPlus) -> Left(5),
        ("cap-5", Leg.capMinus) -> Right(GND), ("cap-5", Leg.capPlus) -> Left(9)
      )
    ))
  }
}
