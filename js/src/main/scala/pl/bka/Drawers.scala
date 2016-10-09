package pl.bka

import org.scalajs.dom
import pl.bka.model.{Component, Diagram, LegId, Transistor}
import pl.bka.model.breadboard._

object Drawers {
  val verticalTracksStep = 30
  val verticalTrackLength = 25 * Tracks.verticalTrackLength
  val verticalTracksHorizontalOffset = 2 * verticalTracksStep
  val verticalTracksVerticalOffset = 10
  val holeRadius = 5

  val ctx = DomOutput.canvas.getContext("2d")
    .asInstanceOf[dom.CanvasRenderingContext2D]

  def drawPhysical(physical: Physical, diagram: Diagram): Unit = {
    def drawComponent(component: Component): Unit = {
      component.cType match {
        case Transistor(symbol, _) =>
          val holes: Seq[Hole] = component.legs.map { leg =>
            physical.connections(LegId(component.name, leg))
          }
          val centerHole = holePosition(holes(1))
          val center = (centerHole._1, centerHole._2 - (0.5 * holeStep).toInt)
          drawHole(center)
          drawLine(holePosition(holes.head), center, 1)
          drawLine(holePosition(holes(2)), center, 1)
          //TODO
        case _ => ()
      }
    }
    physical.tracks.foreach(drawTrack(_, verticalTracksVerticalOffset))
    diagram.components.foreach(drawComponent)
  }

  val holeStep = verticalTrackLength / Tracks.verticalTrackLength

  private def holePosition(hole: Hole): (Int, Int) =
    (hole.trackIndex.index * verticalTracksStep + verticalTracksHorizontalOffset, verticalTracksVerticalOffset + ((hole.holeIndex.position - 0.5) * holeStep).toInt)

  private def drawHole(pos: (Int, Int)): Unit = {
    ctx.fillStyle = "#FFFFFF"
    ctx.strokeStyle = "#000000"
    ctx.lineWidth = 1
    ctx.beginPath()
    ctx.arc(pos._1, pos._2, holeRadius, 0, 2*Math.PI)
    ctx.stroke()
    ctx.fill()
  }

  private def drawLine(from: (Int, Int), to: (Int, Int), lineWidth: Int): Unit = {
    ctx.strokeStyle = "#000000"
    ctx.lineWidth = lineWidth
    ctx.beginPath()
    ctx.moveTo(from._1, from._2)
    ctx.lineTo(to._1, to._2)
    ctx.stroke()
  }

  private def drawVerticalTrack(vertical: Vertical): Unit = {
    val from = (vertical.index.index * verticalTracksStep + verticalTracksHorizontalOffset, verticalTracksVerticalOffset)
    val to = (vertical.index.index * verticalTracksStep + verticalTracksHorizontalOffset, verticalTracksVerticalOffset + verticalTrackLength)
    drawLine(from, to, 2)

    for(h <- 1 to Tracks.verticalTrackLength) {
      drawHole(holePosition(Hole(vertical.index, VerticalPosition(h))))
    }
  }

  private def drawTrack(track: Track, offset: Int): Unit = track match {
    case v: Vertical => drawVerticalTrack(v)
    case _ => ()
  }
}
