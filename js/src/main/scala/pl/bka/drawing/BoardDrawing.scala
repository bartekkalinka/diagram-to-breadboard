package pl.bka.drawing

import org.scalajs.dom
import pl.bka.DomOutput
import pl.bka.model.Power.{GND, Plus}
import pl.bka.model._
import pl.bka.model.breadboard._

object BoardDrawing extends Const {
  def drawPhysical(physical: Physical, diagram: Diagram): Unit = {
    def drawComponent(component: Component, compIndex: Int): Unit = {
      val holes: Seq[Hole] = component.legs.map { leg =>
        physical.connections(LegId(component.name, leg))
      }
      val color: String = Seq("#000000", "#FF0000", "#0000FF", "#00FF00")(compIndex % 4)
      component.cType match {
        case Transistor(symbol, _) =>
          val centerHole = verticalHolePosition(holes(1))
          val (centerX, centerY) = (centerHole._1, centerHole._2 - (0.3 * holeStep).toInt)
          DirectDrawing.drawLine(verticalHolePosition(holes.head), (centerX - transistorLegsSpread, centerY), 2)
          DirectDrawing.drawLine(verticalHolePosition(holes(1)), (centerX, centerY), 2)
          DirectDrawing.drawLine(verticalHolePosition(holes(2)), (centerX + transistorLegsSpread, centerY), 2)
          DirectDrawing.drawTransistorBody(symbol, (centerX, centerY))
        case Cable(_, _) =>
          DirectDrawing.drawCable(verticalHolePosition(holes.head), verticalHolePosition(holes(1)), color)
        case _ => ()
      }
    }
    dom.console.log("drawing...")
    physical.tracks.foreach(drawTrack(_, verticalTracksVerticalOffset))
    physical.components.reverse.zipWithIndex.foreach((drawComponent _).tupled)
  }

  private def verticalHolePosition(hole: Hole): (Int, Int) =
    (hole.trackIndex.index * tracksStep + tracksHorizontalOffset, verticalTracksVerticalOffset + hole.holeIndex.position * holeStep)

  private def horizontalHolePosition(hole: Hole): (Int, Int) =
    (tracksHorizontalOffset + hole.holeIndex.position * holeStep, hole.trackIndex.index * tracksStep + horizontalTracksVerticalOffset)

  private def drawVerticalTrack(vertical: Vertical): Unit = {
    val from = (vertical.index.index * tracksStep + tracksHorizontalOffset, verticalTracksVerticalOffset)
    val to = (vertical.index.index * tracksStep + tracksHorizontalOffset, verticalTracksVerticalOffset + verticalTrackLength)
    DirectDrawing.drawLine(from, to, 1)

    for(h <- 0 until Tracks.verticalTrackLength) {
      DirectDrawing.drawHole(verticalHolePosition(Hole(vertical.index, TrackPosition(h))))
    }
  }

  private def drawHorizontalTrack(horizontal: Horizontal): Unit = {
    val from = (tracksHorizontalOffset, horizontal.index.index * tracksStep + horizontalTracksVerticalOffset)
    val to = (tracksHorizontalOffset + horizontalTrackLength, horizontal.index.index * tracksStep + horizontalTracksVerticalOffset)
    DirectDrawing.drawLine(from, to, 1)

    for(h <- 0 until Tracks.horizontalTrackLength) {
      DirectDrawing.drawHole(horizontalHolePosition(Hole(horizontal.index, TrackPosition(h))))
    }
    drawPowerSign(horizontal.power, (from._1 - 12, from._2))
  }

  private def drawTrack(track: Track, offset: Int): Unit = track match {
    case v: Vertical => drawVerticalTrack(v)
    case h: Horizontal => drawHorizontalTrack(h)
  }

  private def drawPowerSign(power: Power.PowerConnection, pos: (Int, Int)) = power match {
    case Plus =>
      DirectDrawing.drawLine(from = (pos._1 - 5, pos._2), to = (pos._1 + 5, pos._2), 1)
      DirectDrawing.drawLine(from = (pos._1, pos._2 - 5), to = (pos._1, pos._2 + 5), 1)
    case GND =>
      DirectDrawing.drawLine(from = (pos._1 - 5, pos._2), to = (pos._1 + 5, pos._2), 1)
      DirectDrawing.drawLine(from = (pos._1 - 3, pos._2 + 3), to = (pos._1 + 3, pos._2 + 3), 1)
      DirectDrawing.drawLine(from = (pos._1 - 1, pos._2 + 6), to = (pos._1 + 1, pos._2 + 6), 1)
  }
}
