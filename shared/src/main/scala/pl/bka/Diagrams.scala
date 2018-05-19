package pl.bka

import pl.bka.model._
import pl.bka.model.Power._

object Diagrams {
  val example = Diagram(
    List(
      Component("549B-1", Transistor("549B")),
      Component("549B-2", Transistor("549B")),
      Component("R220-1", Resistor("220K")),
      Component("R220-2", Resistor("220K")),
      Component("082-1", IC("082", 8))
    ),
    Map(("549B-1", "0") -> Right(Plus), ("549B-1", "1") -> Left(0), ("549B-1", "2") -> Right(GND),
      ("549B-2", "0") -> Right(Plus), ("549B-2", "1") -> Left(0), ("549B-2", "2") -> Right(GND),
      ("R220-1", "0") -> Right(GND), ("R220-1", "1") -> Left(0),
      ("R220-2", "0") -> Right(Plus), ("R220-2", "1") -> Left(0),
      ("082-1", "0") -> Right(Plus), ("082-1", "1") -> Left(1),
      ("082-1", "2") -> Left(1), ("082-1", "3") -> Right(GND),
      ("082-1", "4") -> Right(GND), ("082-1", "5") -> Left(2),
      ("082-1", "6") -> Right(GND), ("082-1", "7") -> Right(Plus)
    )
  )
}