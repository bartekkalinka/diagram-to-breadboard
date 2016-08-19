package pl.bka

import pl.bka.model.{Component, Diagram, LegId}
import pl.bka.model.breadboard.{Hole, Physical, Track, Vertical}

object Drawers {
  val verticalTracksStep = 15
  val verticalTrackLength = 125
  val verticalTracksHorizontalOffset = 2 * verticalTracksStep
  val verticalTracksVerticalOffset = 10

  def drawVerticalTrack(vertical: Vertical, offset: Int): Unit = {
    val ctx = DomOutput.renderer
    ctx.strokeStyle = "#000000"
    ctx.lineWidth = 2
    ctx.beginPath()
    ctx.moveTo(vertical.index.index * verticalTracksStep + verticalTracksHorizontalOffset, offset)
    ctx.lineTo(vertical.index.index * verticalTracksStep + verticalTracksHorizontalOffset, offset + verticalTrackLength)
    ctx.stroke()
  }

  def drawPhysical(physical: Physical, diagram: Diagram, offset: Int): Unit = {
    def drawTrack(track: Track, offset: Int): Unit = track match {
      case v: Vertical => drawVerticalTrack(v, offset)
      case _ => ()
    }
    def drawComponent(component: Component, offset: Int): Unit = {
      val holes: Seq[Hole] = component.legs.map { leg =>
        physical.connections(LegId(component.name, leg))
      }
      val trackIndices = holes.map(_.trackIndex.index)
      val middle = trackIndices.sum / trackIndices.length
    }
    physical.tracks.foreach(drawTrack(_, verticalTracksVerticalOffset))
    diagram.components.foreach(drawComponent(_, offset))
  }
}
