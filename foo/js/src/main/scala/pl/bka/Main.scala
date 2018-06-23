package pl.bka

import org.scalajs.dom
import org.scalajs.dom.{document, html, window}
import pl.bka.drawing.{BoardDrawing, Size}
import pl.bka.model.{ComponentName, Diagram}
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
        DomOutput.println("diagram")
        Diagram.prettyPrint(Diagrams.example).foreach(DomOutput.println)
        val size = new Size(2d)
        val boardDrawing = new BoardDrawing(size)
        val physical = Breadboard(diagram).physical
        var componentPositionsMap = boardDrawing.drawPhysical(physical, diagram)
        var draggedComponent: Option[DraggedComponent] = None
        var isMouseDown: Boolean = false
        DomOutput.canvas.onmousemove = { e =>
          val (x, y) = ((e.clientX - offsetX).toInt, (e.clientY - offsetY).toInt)
          val closest = findClosestComponent(componentPositionsMap, size)(x, y)
          if(isMouseDown) {
            draggedComponent match {
              case Some(dragged) =>
                val relativeDrag = (x - dragged.startMouseXOffset, y - dragged.startMouseYOffset)
                componentPositionsMap = boardDrawing.move(dragged.name, relativeDrag._1, relativeDrag._2, physical, diagram)
              case None =>
                draggedComponent = closest.map { case (coord, compName) =>
                  DraggedComponent(compName, x - coord._1, y - coord._2)
                }
            }
          } else {
            draggedComponent = None
            closest match {
              case Some((coord, ComponentName(name))) =>
                boardDrawing.select(coord, physical, diagram)
              case None =>
                boardDrawing.unselect(physical, diagram)
            }
          }
        }
        DomOutput.canvas.onmousedown = { _ => isMouseDown = true }
        DomOutput.canvas.onmouseup = { _ => isMouseDown = false}
      case _ => dom.console.log("diagram validation error")
    }
  }

  private def findClosestComponent(componentPositionsMap: Map[(Int, Int), CoordWithName], size: Size)(x: Int, y: Int): Option[CoordWithName] =
    if(componentPositionsMap.isEmpty) {
      None
    } else {
      val ((closestX, closestY), closestDistance) = componentPositionsMap.keys.map { case (cx, cy) =>
        val distance = Math.sqrt((cx - x) * (cx - x) + (cy - y) * (cy - y))
        ((cx, cy), distance)
      }.minBy(_._2)
      componentPositionsMap.get((closestX, closestY)).filter(_ => closestDistance <= size.selectionDistance)
    }
}