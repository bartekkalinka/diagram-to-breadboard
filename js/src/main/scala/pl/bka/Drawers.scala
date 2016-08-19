package pl.bka

import pl.bka.model.Diagram
import pl.bka.model.breadboard.{Physical, Track, Vertical}

trait Drawer {
  def draw(position: Int): Unit
}

object Drawers {
  val verticalTracksStep = 15
  val verticalTrackLength = 125
  val verticalTracksHorizontalOffset = 2 * verticalTracksStep
  val verticalTracksVerticalOffset = 10

  implicit def verticalDrawer(vertical: Vertical): Drawer = new Drawer {
    def draw(offset: Int): Unit = {
      val ctx = DomOutput.renderer
      ctx.strokeStyle = "#000000"
      ctx.lineWidth = 2
      ctx.beginPath()
      ctx.moveTo(vertical.index.index * verticalTracksStep + verticalTracksHorizontalOffset, offset)
      ctx.lineTo(vertical.index.index * verticalTracksStep + verticalTracksHorizontalOffset, offset + verticalTrackLength)
      ctx.stroke()
    }
  }

  implicit def trackDrawer(track: Track): Drawer = new Drawer {
    def draw(offset: Int): Unit = track match {
      case v: Vertical => v.draw(offset)
      case _ => ()
    }
  }

  def physicalWithDiagramDrawer(physical: Physical, diagram: Diagram): Drawer = new Drawer {
    def draw(offset: Int): Unit = {
      physical.tracks.foreach(_.draw(verticalTracksVerticalOffset))
    }
  }
}
