package pl.bka

import org.scalajs.dom
import org.scalajs.dom.window
import pl.bka.drawing.{BoardDrawing, DirectDrawing, Size}
import pl.bka.guistate.{BoardSelection, ComponentDragging}
import pl.bka.model.ComponentName
import pl.bka.model.breadboard.Breadboard

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("Main")
object Main {
  case class DraggedComponent(name: ComponentName, startMouseXOffset: Int, startMouseYOffset: Int)

  window.onload = { _ =>
    Diagrams.example match {
      case Right(diagram) =>
        val offsetX = DomOutput.canvas.offsetLeft
        val offsetY = DomOutput.canvas.offsetTop
        val size = new Size(2d)
        val physical = Breadboard(diagram).physical
        val directDrawing = new DirectDrawing(size)
        val boardDrawing = new BoardDrawing(directDrawing, size, physical, diagram)
        val boardSelection = new BoardSelection(boardDrawing, directDrawing, physical)
        boardSelection.draw()
        val componentDragging = new ComponentDragging(boardSelection, size)
        var isMouseDown: Boolean = false
        DomOutput.canvas.onmousemove = { e =>
          val (x, y) = ((e.clientX - offsetX).toInt, (e.clientY - offsetY).toInt)
          componentDragging.onmousemove(x, y, isMouseDown)
        }
        DomOutput.canvas.onmousedown = { _ => isMouseDown = true }
        DomOutput.canvas.onmouseup = { _ => isMouseDown = false}
      case _ => dom.console.log("diagram validation error")
    }
  }
}