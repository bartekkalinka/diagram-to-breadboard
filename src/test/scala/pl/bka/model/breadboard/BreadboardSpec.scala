package pl.bka.model.breadboard

import org.scalatest.{FlatSpec, Matchers}
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
  val diagram = example match { case Right(d) => d; case _ => fail()}

  it should "do something" in {
    val board = Breadboard.fromDiagram(diagram)
    board.physical.tracks.length shouldBe 3
  }
}

