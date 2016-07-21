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
    DomOutput.domPrintln(s"avtDog: ${Diagram.prettyPrint(Diagrams.example)}")
    Diagrams.example match {
      case Right(diagram) =>
        val exampleBoard = Breadboard.fromDiagram(diagram)
        DomOutput.domPrintln(s"exampleBoard: ${exampleBoard.prettyPrint}")
      case _ => ()
    }
  }
}