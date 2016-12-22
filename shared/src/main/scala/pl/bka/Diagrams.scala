package pl.bka

import pl.bka.model._
import pl.bka.model.Power._

object Diagrams {
  val example = Diagram(
    List(
      Component("Tr549B.1", Transistor("549B")),
      Component("Tr549B.2", Transistor("549B")),
      Component("R220-1", Resistor("220K"))
    ),
    Map(("Tr549B.1", "0") -> Right(Plus), ("Tr549B.1", "1") -> Left(1), ("Tr549B.1", "2") -> Right(GND),
      ("Tr549B.2", "0") -> Right(Plus), ("Tr549B.2", "1") -> Left(1), ("Tr549B.2", "2") -> Right(GND),
      ("R220-1", "0") -> Right(GND), ("R220-1", "1") -> Left(1))
  )
}