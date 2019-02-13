package pl.bka.dtb.guistate

import pl.bka.dtb.Types.CoordWithName
import pl.bka.dtb.drawing.{BoardDrawing, DirectDrawing, Size}
import pl.bka.dtb.model.ComponentName
import pl.bka.dtb.model.breadboard.Physical

import scala.collection.mutable

class BoardSelection(boardDrawing: BoardDrawing, directDrawing: DirectDrawing, physical: Physical) {
  private var selectionOn: Boolean = false

  private val movedComponents: mutable.Map[ComponentName, (Int, Int)] = mutable.Map.empty

  private var componentPositionsMap: Map[(Int, Int), CoordWithName] = Map.empty

  def unselect(): Unit =
    if(selectionOn) {
      directDrawing.clear()
      draw()
      selectionOn = false
    }

  def select(coord: (Int, Int)): Unit = {
    unselect()
    directDrawing.drawSelectionMark(coord)
    selectionOn = true
  }

  def move(componentName: ComponentName, x: Int, y: Int): Unit = {
    movedComponents.put(componentName, (x, y))
    directDrawing.clear()
    draw()
  }

  def draw(): Unit = {
    componentPositionsMap = boardDrawing.drawPhysical(movedComponents.toMap)
  }

  def findClosestComponent(size: Size)(x: Int, y: Int): Option[CoordWithName] =
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
