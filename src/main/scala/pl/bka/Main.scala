package pl.bka

import scala.scalajs.js.JSApp
import org.scalajs.dom
import dom.document
import pl.bka.model.{Diagram, Breadboard}

object Main extends JSApp {

  def domPrintln(text: String) = {
    val textNode = document.createTextNode(text)
    val parNode = document.createElement("p")
    parNode.appendChild(textNode)
    document.body.appendChild(parNode)
  }

  def main(): Unit = {
    domPrintln(s"avtDog: ${Diagram.prettyPrint(Diagrams.example)}")
    Diagrams.example match {
      case Right(diagram) =>
        val exampleBoard = Breadboard.fromDiagram(diagram)
        domPrintln(s"exampleBoard: ${exampleBoard.prettyPrint}")
      case _ => ()
    }
  }
}