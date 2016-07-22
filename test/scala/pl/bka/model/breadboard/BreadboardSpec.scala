package pl.bka.model.breadboard

import pl.bka.model._

class BreadboardSpec extends FlatSpec with Matchers {
  val example = Diagram(
    List(
      Component("Tr549B.1", Transistor("549B")),
      Component("+9V", VoltageSource(9))
    ),
    Map(("Tr549B.1", "0") -> 0, ("Tr549B.1", "1") -> 1, ("Tr549B.1", "2") -> 2,
      ("+9V", "0") -> 0, ("+9V", "1") -> 1)
  )

  it should "do something" in {
    val board = Breadboard.fromDiagram(example)
    board.physical.tracks.length should be 3
  }
}

