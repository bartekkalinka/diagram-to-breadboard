package pl.bka

import org.scalajs.dom
import org.scalajs.dom.{html, document}
import pl.bka.model.{Diagram, Breadboard}
import scala.scalajs.js.annotation.JSExport

@JSExport
object Main {

  @JSExport
  def main(): Unit = {
    DomOutput.canvasTest()
    DomOutput.println("diagram")
    Diagram.prettyPrint(Diagrams.example).foreach(DomOutput.println)
    Diagrams.example match {
      case Right(diagram) =>
        val exampleBoard = Breadboard.fromDiagram(diagram)
        DomOutput.println("board")
        exampleBoard.prettyPrint.foreach(DomOutput.println)
      case _ => ()
    }
  }
}