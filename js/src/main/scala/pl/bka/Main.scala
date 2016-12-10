package pl.bka

import pl.bka.model.Diagram
import pl.bka.model.breadboard.Breadboard
import scala.scalajs.js.annotation.JSExport

@JSExport
object Main {
  import Drawers._

  @JSExport
  def main(): Unit = {
    DomOutput.println("diagram")
    Diagram.prettyPrint(Diagrams.example).foreach(DomOutput.println)
    Diagrams.example match {
      case Right(diagram) =>
        val exampleBoard = Breadboard(diagram)
        DomOutput.println("board")
        exampleBoard.prettyPrint.foreach(DomOutput.println)
        Drawers.drawPhysical(exampleBoard.physical, diagram)
      case _ => ()
    }
  }
}