package pl.bka.dtb.guistate

import pl.bka.dtb.Main.DraggedComponent
import pl.bka.dtb.drawing.Size

class ComponentDragging(boardSelection: BoardSelection, size: Size) {
  private var draggedComponent: Option[DraggedComponent] = None

  def onmousemove(x: Int, y: Int, isMouseDown: Boolean): Unit = {
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
}
