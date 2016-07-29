package pl.bka

import org.scalajs.dom
import org.scalajs.dom.{document, html}
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
        val exampleBoard = Breadboard.fromDiagram(diagram)
        DomOutput.println("board")
        exampleBoard.prettyPrint.foreach(DomOutput.println)
        //exampleBoard.physical.draw()
      case _ => ()
    }
  }
}