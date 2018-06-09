package pl.bka

import pl.bka.model._
import pl.bka.model.Power._

object Diagrams {
  val example = Diagram(
    List(
      Component("Tr549B.1", Transistor("549B")),
      Component("Tr549B.2", Transistor("549B")),
      Component("R220-1", Resistor("220K")),
      Component("R220-2", Resistor("220K")),
      Component("R220-3", Resistor("220K")),
      Component("R220-4", Resistor("220K")),
      Component("R220-5", Resistor("220K")),
      Component("C100uF-1", Capacitor(capacitance = 100, bipolar = true))
    ),
    Map(
      ("Tr549B.1", "0") -> Right(Plus), ("Tr549B.1", "1") -> Left(0), ("Tr549B.1", "2") -> Right(GND),
      ("Tr549B.2", "0") -> Right(Plus), ("Tr549B.2", "1") -> Left(0), ("Tr549B.2", "2") -> Right(GND),
      ("R220-1", "0") -> Right(GND), ("R220-1", "1") -> Left(0),
      ("R220-2", "0") -> Right(Plus), ("R220-2", "1") -> Left(0),
      ("R220-3", "0") -> Left(0), ("R220-3", "1") -> Right(GND),
      ("R220-4", "0") -> Right(GND), ("R220-4", "1") -> Left(0),
      ("R220-5", "0") -> Left(0), ("R220-5", "1") -> Right(Plus),
      ("C100uF-1", Leg.capMinus) -> Left(0), ("C100uF-1", Leg.capPlus) -> Right(Plus)
    )
  )
}