package pl.bka

import org.scalajs.dom
import org.scalajs.dom.{document, html, window}
import pl.bka.drawing.{BoardDrawing, Size}
import pl.bka.model.{ComponentName, Diagram}
import pl.bka.model.breadboard.Breadboard

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("Main")
object Main {
  window.onload = { _ =>
    val offsetX = DomOutput.canvas.offsetLeft
    val offsetY = DomOutput.canvas.offsetTop
    DomOutput.println("diagram")
    Diagram.prettyPrint(Diagrams.example).foreach(DomOutput.println)
    val size = new Size(2d)
    val componentPositions =
      Diagrams.example match {
        case Right(diagram) =>
          dom.console.log("calculating...")
          val exampleBoard = Breadboard(diagram)
          DomOutput.println("board")
          exampleBoard.prettyPrint.foreach(DomOutput.println)
          val boardDrawing = new BoardDrawing(size)
          boardDrawing.drawPhysical(exampleBoard.physical, diagram)
        case _ => Seq.empty[(ComponentName, Int, Int)]
      }
    val componentPositionsMap = componentPositions.map { case (name, x, y) => ((x, y), name) }.toMap
    val coordDiv = document.getElementById("coord").asInstanceOf[html.Div]
    DomOutput.canvas.onmousemove = { e =>
      val (x, y) = (e.clientX - offsetX, e.clientY - offsetY)
      val closest = findClosestComponent(componentPositionsMap, size)(x.toInt, y.toInt)
      coordDiv.innerHTML = closest.map(_.value).getOrElse("-")
    }
  }

  private def findClosestComponent(componentPositionsMap: Map[(Int, Int), ComponentName], size: Size)(x: Int, y: Int): Option[ComponentName] =
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