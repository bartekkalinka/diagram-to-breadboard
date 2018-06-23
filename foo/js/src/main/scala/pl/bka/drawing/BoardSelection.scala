package pl.bka.drawing

import pl.bka.model.ComponentName
import pl.bka.model.breadboard.Physical

import scala.collection.mutable

class BoardSelection(boardDrawing: BoardDrawing, directDrawing: DirectDrawing, physical: Physical) {
  private var selectionOn: Boolean = false

  private val movedComponents: mutable.Map[ComponentName, (Int, Int)] = mutable.Map.empty

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

  def move(componentName: ComponentName, x: Int, y: Int): Map[(Int, Int), ((Int, Int), ComponentName)] = {
    movedComponents.put(componentName, (x, y))
    directDrawing.clear()
    draw()
  }

  def draw(): Map[(Int, Int), ((Int, Int), ComponentName)] = {
    boardDrawing.drawPhysical(movedComponents.toMap)
  }
}
