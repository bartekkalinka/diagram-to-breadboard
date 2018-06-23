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

  window.onload = { _ =>
    Diagrams.example match {
      case Right(diagram) =>
        val offsetX = DomOutput.canvas.offsetLeft
        val offsetY = DomOutput.canvas.offsetTop
        DomOutput.println("diagram")
        Diagram.prettyPrint(Diagrams.example).foreach(DomOutput.println)
        val size = new Size(2d)
        val boardDrawing = new BoardDrawing(size)
        dom.console.log("calculating...")
        val physical = Breadboard(diagram).physical
        val componentPositions = boardDrawing.drawPhysical(physical, diagram)
        val componentPositionsMap = componentPositions.map { case (name, x, y) => ((x, y), ((x, y), name)) }.toMap
        val coordDiv = document.getElementById("coord").asInstanceOf[html.Div]
        DomOutput.canvas.onmousemove = { e =>
          val (x, y) = (e.clientX - offsetX, e.clientY - offsetY)
          val closest = findClosestComponent(componentPositionsMap, size)(x.toInt, y.toInt)
          coordDiv.innerHTML = closest.map { case (coord, ComponentName(name)) =>
            boardDrawing.drawSelectionMark(coord, physical, diagram)
            s"$coord, $name"
          }.getOrElse {
            boardDrawing.clearSelectionMark(physical, diagram)
            "-"
          }
        }
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