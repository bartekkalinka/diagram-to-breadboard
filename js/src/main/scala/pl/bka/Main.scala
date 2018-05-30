package pl.bka

import org.scalajs.dom
import pl.bka.drawing.{BoardDrawing, Size}
import pl.bka.model.Diagram
import pl.bka.model.breadboard.Breadboard

import scala.scalajs.js.annotation.JSExport

@JSExport
object Main {

  @JSExport
  def main(): Unit = {
    DomOutput.println("diagram")
    Diagram.prettyPrint(Diagrams.example).foreach(DomOutput.println)
    Diagrams.example match {
      case Right(diagram) =>
        dom.console.log("calculating...")
        val exampleBoard = Breadboard(diagram)
        DomOutput.println("board")
        exampleBoard.prettyPrint.foreach(DomOutput.println)
        val boardDrawing = new BoardDrawing(new Size(1d))
        boardDrawing.drawPhysical(exampleBoard.physical, diagram)
      case _ => ()
    }
  }
}