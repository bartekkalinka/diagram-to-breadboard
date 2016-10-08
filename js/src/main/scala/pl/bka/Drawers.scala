package pl.bka

import org.scalajs.dom
import pl.bka.model.{Component, Diagram, LegId}
import pl.bka.model.breadboard._

object Drawers {
  val verticalTracksStep = 15
  val verticalTrackLength = 25 * Tracks.verticalTrackLength
  val verticalTracksHorizontalOffset = 2 * verticalTracksStep
  val verticalTracksVerticalOffset = 10
  val holeRadius = 5

  val ctx = DomOutput.canvas.getContext("2d")
    .asInstanceOf[dom.CanvasRenderingContext2D]

  def drawPhysical(physical: Physical, diagram: Diagram): Unit = {
    def drawComponent(component: Component): Unit = {
      val holes: Seq[Hole] = component.legs.map { leg =>
        physical.connections(LegId(component.name, leg))
      }
      val trackIndices = holes.map(_.trackIndex.index)
      val middle = trackIndices.sum / trackIndices.length
    }
    physical.tracks.foreach(drawTrack(_, verticalTracksVerticalOffset))
    diagram.components.foreach(drawComponent)
  }

  private def drawHole(x: Int, y: Int): Unit = {
    ctx.strokeStyle = "#000000"
    ctx.lineWidth = 1
    ctx.beginPath()
    ctx.arc(x, y, holeRadius, 0, 2*Math.PI)
    ctx.stroke()
  }

  private def drawVerticalTrack(vertical: Vertical): Unit = {
    ctx.strokeStyle = "#000000"
    ctx.lineWidth = 2
    ctx.beginPath()
    ctx.moveTo(vertical.index.index * verticalTracksStep + verticalTracksHorizontalOffset, verticalTracksVerticalOffset)
    ctx.lineTo(vertical.index.index * verticalTracksStep + verticalTracksHorizontalOffset, verticalTracksVerticalOffset + verticalTrackLength)
    ctx.stroke()
    //TODO
    val holeStep = verticalTrackLength / Tracks.verticalTrackLength
    for(h <- 1 to Tracks.verticalTrackLength) {
      drawHole(vertical.index.index * verticalTracksStep + verticalTracksHorizontalOffset, verticalTracksVerticalOffset + ((h - 0.5) * holeStep).toInt)
    }
  }

  private def drawTrack(track: Track, offset: Int): Unit = track match {
    case v: Vertical => drawVerticalTrack(v)
    case _ => ()
  }
}
