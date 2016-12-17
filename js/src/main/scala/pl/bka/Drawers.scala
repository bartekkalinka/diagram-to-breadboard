package pl.bka

import org.scalajs.dom
import pl.bka.model._
import pl.bka.model.breadboard._

object Drawers {
  val verticalTracksStep = 30
  val verticalTrackLength = 40 * Tracks.verticalTrackLength
  val verticalTracksHorizontalOffset = 2 * verticalTracksStep
  val verticalTracksVerticalOffset = 20
  val holeRadius = 5
  val transistorBodyRadius = 12
  val transistorLegsSpread = 3
  val fontSize = 8
  val font = s"${fontSize}px Arial"
  val cableArcRadiusFactor = 0.75

  val ctx = DomOutput.canvas.getContext("2d")
    .asInstanceOf[dom.CanvasRenderingContext2D]

  def drawPhysical(physical: Physical, diagram: Diagram): Unit = {
    def drawComponent(component: Component, compIndex: Int): Unit = {
      val holes: Seq[Hole] = component.legs.map { leg =>
        physical.connections(LegId(component.name, leg))
      }
      val color: String = Seq("#000000", "#FF0000", "#0000FF", "#00FF00")(compIndex % 4)
      component.cType match {
        case Transistor(symbol, _) =>
          val centerHole = holePosition(holes(1))
          val (centerX, centerY) = (centerHole._1, centerHole._2 - (0.5 * holeStep).toInt)
          drawLine(holePosition(holes.head), (centerX - transistorLegsSpread, centerY), 2)
          drawLine(holePosition(holes(1)), (centerX, centerY), 2)
          drawLine(holePosition(holes(2)), (centerX + transistorLegsSpread, centerY), 2)
          drawTransistorBody(symbol, (centerX, centerY))
        case Cable(_, _) =>
          drawCable(holePosition(holes.head), holePosition(holes(1)), color)
        case _ => ()
      }
    }
    dom.console.log("drawing...")
    physical.tracks.foreach(drawTrack(_, verticalTracksVerticalOffset))
    physical.components.reverse.zipWithIndex.foreach((drawComponent _).tupled)
  }

  val holeStep = verticalTrackLength / Tracks.verticalTrackLength

  private def holePosition(hole: Hole): (Int, Int) =
    (hole.trackIndex.index * verticalTracksStep + verticalTracksHorizontalOffset, verticalTracksVerticalOffset + ((hole.holeIndex.position + 0.5) * holeStep).toInt)

  private def drawCable(from: (Int, Int), to: (Int, Int), color: String): Unit = {
    ctx.strokeStyle = color
    ctx.lineWidth = 2
    ctx.beginPath()
    val xDelta = (to._1 - from._1) / 2
    val radius = (to._1 - from._1) * cableArcRadiusFactor
    val yDelta = Math.sqrt(radius * radius - xDelta * xDelta).toInt
    val angle = Math.atan2(xDelta, yDelta)
    val (centerX, centerY) = (from._1 + xDelta, from._2 + yDelta)
    ctx.arc(centerX, centerY, radius, - Math.PI / 2 - angle, - Math.PI / 2 + angle)
    ctx.stroke()
  }

  private def drawTransistorBody(symbol: String, pos: (Int, Int)): Unit = {
    ctx.fillStyle = "#FFFFFF"
    ctx.strokeStyle = "#000000"
    ctx.lineWidth = 1
    ctx.beginPath()
    ctx.arc(pos._1, pos._2, transistorBodyRadius, - Math.PI, 0)
    ctx.moveTo(pos._1 - transistorBodyRadius, pos._2)
    ctx.lineTo(pos._1 + transistorBodyRadius, pos._2)
    ctx.stroke()
    ctx.fill()
    ctx.font = font
    ctx.fillStyle = "#000000"
    ctx.fillText(symbol, pos._1 - transistorBodyRadius + 2, pos._2 - 2)
  }

  private def drawHole(pos: (Int, Int)): Unit = {
    ctx.fillStyle = "#FFFFFF"
    ctx.strokeStyle = "#000000"
    ctx.lineWidth = 1
    ctx.beginPath()
    ctx.arc(pos._1, pos._2, holeRadius, 0, 2*Math.PI)
    ctx.stroke()
    ctx.fill()
  }

  private def drawLine(from: (Int, Int), to: (Int, Int), lineWidth: Int, color: String = "#000000"): Unit = {
    ctx.strokeStyle = color
    ctx.lineWidth = lineWidth
    ctx.beginPath()
    ctx.moveTo(from._1, from._2)
    ctx.lineTo(to._1, to._2)
    ctx.stroke()
  }

  private def drawVerticalTrack(vertical: Vertical): Unit = {
    val from = (vertical.index.index * verticalTracksStep + verticalTracksHorizontalOffset, verticalTracksVerticalOffset)
    val to = (vertical.index.index * verticalTracksStep + verticalTracksHorizontalOffset, verticalTracksVerticalOffset + verticalTrackLength)
    drawLine(from, to, 1)

    for(h <- 0 until Tracks.verticalTrackLength) {
      drawHole(holePosition(Hole(vertical.index, TrackPosition(h))))
    }
  }

  private def drawTrack(track: Track, offset: Int): Unit = track match {
    case v: Vertical => drawVerticalTrack(v)
    case _ => ()
  }
}
