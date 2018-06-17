package pl.bka

import org.scalajs.dom
import org.scalajs.dom.{document, html, window}
import pl.bka.drawing.{BoardDrawing, Size}
import pl.bka.model.Diagram
import pl.bka.model.breadboard.Breadboard

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("Main")
object Main {
  window.onload = { _ =>
    val offsetX = DomOutput.canvas.offsetLeft
    val offsetY = DomOutput.canvas.offsetTop
    DomOutput.println("diagram")
    Diagram.prettyPrint(Diagrams.example).foreach(DomOutput.println)
    Diagrams.example match {
      case Right(diagram) =>
        dom.console.log("calculating...")
        val exampleBoard = Breadboard(diagram)
        DomOutput.println("board")
        exampleBoard.prettyPrint.foreach(DomOutput.println)
        val boardDrawing = new BoardDrawing(new Size(2d))
        boardDrawing.drawPhysical(exampleBoard.physical, diagram)
      case _ => ()
    }
    val coordDiv = document.getElementById("coord").asInstanceOf[html.Div]
    DomOutput.canvas.onmousemove = { e =>
      val coord = s"${e.clientX - offsetX}, ${e.clientY - offsetY}"
      coordDiv.innerHTML = coord
    }
  }
}