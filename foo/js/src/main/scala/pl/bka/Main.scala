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
  case class DraggedComponent(name: ComponentName, startMouseX: Int, startMouseY: Int)

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
        val componentPositions = boardDrawing.drawPhysical(physical, diagram)
        val componentPositionsMap = componentPositions.map { case (name, x, y) => ((x, y), ((x, y), name)) }.toMap
        val coordDiv = document.getElementById("coord").asInstanceOf[html.Div]
        var draggedComponent: Option[DraggedComponent] = None
        var isMouseDown: Boolean = false
        DomOutput.canvas.onmousemove = { e =>
          val (x, y) = ((e.clientX - offsetX).toInt, (e.clientY - offsetY).toInt)
          val closest = findClosestComponent(componentPositionsMap, size)(x, y)
          if(isMouseDown) {
            draggedComponent match {
              case Some(dragged) =>
                val relativeDrag = (x - dragged.startMouseX, y - dragged.startMouseY)
                coordDiv.innerHTML = s"${dragged.name.value} $relativeDrag"
                //TODO redrawing with drag
              case None =>
                draggedComponent = closest.map(close => DraggedComponent(close._2, x, y))
            }
          } else {
            draggedComponent = None
            closest match {
              case Some((coord, ComponentName(name))) =>
                boardDrawing.select(coord, physical, diagram)
              case None =>
                boardDrawing.unselect(physical, diagram)
            }
            coordDiv.innerHTML = "-"
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