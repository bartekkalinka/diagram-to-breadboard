package pl.bka

import pl.bka.model._
import pl.bka.model.Power._

object Diagrams {
  val example = Diagram(
    List(
      Component("diode", Diode("???")),
      Component("R470K-1", Resistor("470K")),
      Component("R22K-1", Resistor("22K")),
      Component("R470K-2", Resistor("470K")),
      Component("R22K-2", Resistor("22K")),
      Component("R470K-3", Resistor("470K")),
      Component("R22K-3", Resistor("22K")),
      Component("Tr-1", Transistor("minus")),
      Component("Tr-2", Transistor("minus")),
      Component("Tr-3", Transistor("minus")),
      Component("hairy-1", Capacitor(0d, bipolar = true)),
      Component("hairy-2", Capacitor(0d, bipolar = true)),
      Component("hairy-3", Capacitor(0d, bipolar = true)),
      Component("cap-4", Capacitor(0d, bipolar = true))
    ),
    Map(
      ("diode", Leg.cathode) -> Left(1), ("diode", Leg.anode) -> Right(Plus),
      ("R470K-1", "0") -> Left(1), ("R470K-1", "1") -> Left(3),
      ("R22K-1", "0") -> Left(1), ("R22K-1", "1") -> Left(2),
      ("R470K-2", "0") -> Left(1), ("R470K-2", "1") -> Left(7),
      ("R22K-2", "0") -> Left(1), ("R22K-2", "1") -> Left(6),
      ("R470K-3", "0") -> Left(1), ("R470K-3", "1") -> Left(5),
      ("R22K-3", "0") -> Left(1), ("R22K-3", "1") -> Left(4),
      ("Tr-1", "0") -> Left(2), ("Tr-1", "1") -> Left(3), ("Tr-1", "2") -> Right(GND),
      ("Tr-2", "0") -> Left(6), ("Tr-2", "1") -> Left(7), ("Tr-2", "2") -> Right(GND),
      ("Tr-3", "0") -> Left(4), ("Tr-3", "1") -> Left(5), ("Tr-3", "2") -> Right(GND),
      ("hairy-1", Leg.capMinus) -> Left(7), ("hairy-1", Leg.capPlus) -> Left(2),
      ("hairy-2", Leg.capMinus) -> Left(3), ("hairy-2", Leg.capPlus) -> Left(4),
      ("hairy-3", Leg.capMinus) -> Left(5), ("hairy-3", Leg.capPlus) -> Left(6),
      ("cap-4", Leg.capMinus) -> Right(GND), ("cap-4", Leg.capPlus) -> Left(1)
    )
  )

  val example2 = Diagram(
    List(
      Component("Tr549B.1", Transistor("549B")),
      Component("Tr549B.2", Transistor("549B")),
      Component("R220-1", Resistor("220K")),
      Component("R220-2", Resistor("220K")),
      Component("R220-3", Resistor("220K")),
      Component("R220-4", Resistor("220K")),
      Component("R220-5", Resistor("220K")),
      Component("C100uF-1", Capacitor(capacitance = 100d, bipolar = true)),
      Component("C10nF-1", Capacitor(capacitance = 0.01, bipolar = false)),
      Component("1N4148-1", Diode("1N4148"))
    ),
    Map(
      ("Tr549B.1", "0") -> Right(Plus), ("Tr549B.1", "1") -> Left(0), ("Tr549B.1", "2") -> Right(GND),
      ("Tr549B.2", "0") -> Right(Plus), ("Tr549B.2", "1") -> Left(0), ("Tr549B.2", "2") -> Right(GND),
      ("R220-1", "0") -> Right(GND), ("R220-1", "1") -> Left(0),
      ("R220-2", "0") -> Right(Plus), ("R220-2", "1") -> Left(0),
      ("R220-3", "0") -> Left(0), ("R220-3", "1") -> Right(GND),
      ("R220-4", "0") -> Right(GND), ("R220-4", "1") -> Left(0),
      ("R220-5", "0") -> Left(0), ("R220-5", "1") -> Right(Plus),
      ("C100uF-1", Leg.capMinus) -> Left(0), ("C100uF-1", Leg.capPlus) -> Right(Plus),
      ("C10nF-1", "0") -> Right(GND), ("C10nF-1", "1") -> Left(1),
      ("1N4148-1", Leg.cathode) -> Left(1), ("1N4148-1", Leg.anode) -> Right(Plus)
    )
  )

  val example3 = Diagram(
    List(
      Component("Tr549B.1", Transistor("549B")),
      Component("Tr549B.2", Transistor("549B")),
      Component("R220-1", Resistor("220K")),
      Component("R220-2", Resistor("220K")),
      Component("R220-3", Resistor("220K")),
      Component("R220-4", Resistor("220K")),
      Component("R220-5", Resistor("220K")),
      Component("R220-6", Resistor("220K"))
    ),
    Map(("Tr549B.1", "0") -> Right(Plus), ("Tr549B.1", "1") -> Left(0), ("Tr549B.1", "2") -> Right(GND),
      ("Tr549B.2", "0") -> Right(Plus), ("Tr549B.2", "1") -> Left(0), ("Tr549B.2", "2") -> Right(GND),
      ("R220-1", "0") -> Right(GND), ("R220-1", "1") -> Left(0),
      ("R220-2", "0") -> Right(Plus), ("R220-2", "1") -> Left(0),
      ("R220-3", "0") -> Left(0), ("R220-3", "1") -> Right(GND),
      ("R220-4", "0") -> Right(GND), ("R220-4", "1") -> Left(0),
      ("R220-5", "0") -> Left(0), ("R220-5", "1") -> Right(Plus),
      ("R220-6", "0") -> Right(GND), ("R220-6", "1") -> Left(0))
  )
}