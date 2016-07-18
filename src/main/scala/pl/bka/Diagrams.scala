package pl.bka

import pl.bka.model._

object Diagrams {
  val avtDog = Diagram(
    List(
      Component("- 1", Transistor("549B")),
      Component("+9V", VoltageSource(9))
    ),
    Map(("- 1", "0") -> 0, ("- 1", "1") -> 1, ("- 1", "2") -> 2,
      ("+9V", "0") -> 0, ("+9V", "1") -> 1)
  )
}