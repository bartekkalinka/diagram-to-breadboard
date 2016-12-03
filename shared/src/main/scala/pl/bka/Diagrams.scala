package pl.bka

import pl.bka.model._

object Diagrams {
  val example = Diagram(
    List(
      Component("Tr549B.1", Transistor("549B")),
      Component("Tr549B.2", Transistor("549B")),
      Component("+9V", VoltageSource(9))
    ),
    Map(("Tr549B.1", "0") -> 0, ("Tr549B.1", "1") -> 1, ("Tr549B.1", "2") -> 2,
      ("Tr549B.2", "0") -> 0, ("Tr549B.2", "1") -> 1, ("Tr549B.2", "2") -> 2,
      ("+9V", "0") -> 0, ("+9V", "1") -> 1)
  )
}