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
      val color: String = Seq("#FFBB00", "#FF0000", "#0000FF", "#00FF00")(compIndex % 4)
      component.cType match {
        case Transistor(symbol, _) =>
          val centerHole = holePosition(holes(1))
          val (centerX, centerY) = (centerHole._1, centerHole._2 - (0.3 * holeStep).toInt)
          DirectDrawing.drawLine(holePosition(holes.head), (centerX - transistorLegsSpread, centerY), 2)
          DirectDrawing.drawLine(holePosition(holes(1)), (centerX, centerY), 2)
          DirectDrawing.drawLine(holePosition(holes(2)), (centerX + transistorLegsSpread, centerY), 2)
          DirectDrawing.drawTransistorBody(symbol, (centerX, centerY))
        case Cable(_, _) =>
          val (from, to) = (holePosition(holes.head), holePosition(holes(1)))
          if(from._2 == to._2) {
            DirectDrawing.drawArcCable(from, to, color)
          } else {
            DirectDrawing.drawStraightCable(from, to, color)
          }
        case Resistor(ohms, _) =>
          val Seq(holePos1, holePos2) = holes.map(holePosition).sortBy(_._1)
          val (centerX, centerY) = ((holePos1._1 + holePos2._1) / 2, Seq(holePos1._2, holePos2._2).min - (0.3 * holeStep).toInt)
          DirectDrawing.drawLine(holePos1, (centerX - resistorBodySize._1 / 2, centerY - resistorBodySize._2 / 2), 2)
          DirectDrawing.drawLine(holePos2, (centerX + resistorBodySize._1 / 2, centerY - resistorBodySize._2 / 2), 2)
          DirectDrawing.drawResistorBody(ohms, (centerX, centerY))
        case _ => ()
      }
    }
    dom.console.log("drawing...")
    physical.tracks.foreach(drawTrack(_, verticalTracksVerticalOffset))
    physical.components.reverse.zipWithIndex.foreach((drawComponent _).tupled)
  }

  private def holePosition(hole: Hole): (Int, Int) =
    if(hole.trackIndex.horizontal) {
      (tracksHorizontalOffset + hole.holeIndex.position * holeStep, hole.trackIndex.index * tracksStep + horizontalTracksVerticalOffset)
    } else {
      (hole.trackIndex.index * tracksStep + tracksHorizontalOffset, verticalTracksVerticalOffset + hole.holeIndex.position * holeStep)
    }

  private def drawVerticalTrack(vertical: Vertical): Unit = {
    val from = (vertical.index.index * tracksStep + tracksHorizontalOffset, verticalTracksVerticalOffset)
    val to = (vertical.index.index * tracksStep + tracksHorizontalOffset, verticalTracksVerticalOffset + verticalTrackLength)
    DirectDrawing.drawLine(from, to, 1)

    for(h <- 0 until Tracks.verticalTrackLength) {
      DirectDrawing.drawHole(holePosition(Hole(vertical.index, TrackPosition(h))))
    }
  }

  private def drawHorizontalTrack(horizontal: Horizontal): Unit = {
    val from = (tracksHorizontalOffset, horizontal.index.index * tracksStep + horizontalTracksVerticalOffset)
    val to = (tracksHorizontalOffset + horizontalTrackLength, horizontal.index.index * tracksStep + horizontalTracksVerticalOffset)
    DirectDrawing.drawLine(from, to, 1)

    for(h <- 0 until Tracks.horizontalTrackLength) {
      DirectDrawing.drawHole(holePosition(Hole(horizontal.index, TrackPosition(h))))
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
