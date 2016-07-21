package pl.bka

import org.scalajs.dom
import org.scalajs.dom.{html, document}
import pl.bka.model.{Diagram, Breadboard}
import scala.scalajs.js.annotation.JSExport

@JSExport
object Main {

  def domPrintln(text: String) = {
    val textNode = document.createTextNode(text)
    val parNode = document.createElement("p")
    parNode.appendChild(textNode)
    document.body.appendChild(parNode)
  }

  def canvasTest(canvas: html.Canvas) = {
    val renderer = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    canvas.width = canvas.parentElement.clientWidth
    canvas.height = canvas.parentElement.clientHeight

    renderer.fillStyle = "#111111"
    renderer.fillRect(0, 0, canvas.width, canvas.height)
  }

  @JSExport
  def main(canvas: html.Canvas): Unit = {
    canvasTest(canvas)
    domPrintln(s"avtDog: ${Diagram.prettyPrint(Diagrams.example)}")
    Diagrams.example match {
      case Right(diagram) =>
        val exampleBoard = Breadboard.fromDiagram(diagram)
        domPrintln(s"exampleBoard: ${exampleBoard.prettyPrint}")
      case _ => ()
    }
  }
}