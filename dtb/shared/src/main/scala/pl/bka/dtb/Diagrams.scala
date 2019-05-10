package pl.bka.dtb

import pl.bka.dtb.model.Power.{GND, Plus}
import pl.bka.dtb.model._

object Diagrams {
  val roll3 = Diagram(
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
  )

  val roll4 = Diagram(
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
  )

  //TODO DSL of describing diagrams like: "Tr-1 5 1 GND"
}
