package pl.bka

import org.scalajs.dom
import org.scalajs.dom.window
import pl.bka.drawing.{BoardDrawing, DirectDrawing, Size}
import pl.bka.guistate.BoardSelection
import pl.bka.model.ComponentName
import pl.bka.model.breadboard.Breadboard

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("Main")
object Main {
  type CoordWithName = ((Int, Int), ComponentName)
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
        var draggedComponent: Option[DraggedComponent] = None
        var isMouseDown: Boolean = false
        DomOutput.canvas.onmousemove = { e =>
          val (x, y) = ((e.clientX - offsetX).toInt, (e.clientY - offsetY).toInt)
          val closest = boardSelection.findClosestComponent(size)(x, y)
          if(isMouseDown) {
            draggedComponent match {
              case Some(dragged) =>
                val relativeDrag = (x - dragged.startMouseXOffset, y - dragged.startMouseYOffset)
                boardSelection.move(dragged.name, relativeDrag._1, relativeDrag._2)
              case None =>
                draggedComponent = closest.map { case (coord, compName) =>
                  DraggedComponent(compName, x - coord._1, y - coord._2)
                }
            }
          } else {
            draggedComponent = None
            closest match {
              case Some((coord, _)) =>
                boardSelection.select(coord)
              case None =>
                boardSelection.unselect()
            }
          }
        }
        DomOutput.canvas.onmousedown = { _ => isMouseDown = true }
        DomOutput.canvas.onmouseup = { _ => isMouseDown = false}
      case _ => dom.console.log("diagram validation error")
    }
  }
}