package pl.bka.drawing

import org.scalajs.dom
import pl.bka.DomOutput
import pl.bka.model.Power.{GND, Plus}
import pl.bka.model._
import pl.bka.model.breadboard._

class BoardDrawing(size: Size) {
  val directDrawing = new DirectDrawing(size)
  def drawPhysical(physical: Physical, diagram: Diagram): Unit = {
    def drawComponent(component: Component, compIndex: Int): Unit = {
      val holes: Seq[Hole] = component.legs.map { leg =>
        physical.connections(LegId(component.name, leg))
      }
      val color: String = Seq("#FFBB00", "#FF0000", "#0000FF", "#00FF00")(compIndex % 4)
      component.cType match {
        case IC(_, _) =>
          val (xs, ys) = holes.map(holePosition).unzip
          val (centerX, centerY) = (xs.sum / xs.length, ys.sum / ys.length)
          holes.zipWithIndex.foreach { case (hole, i) =>
            val (x, y) = holePosition(hole)
            val bodyOffset = if(i < holes.length / 2) size.tracksStep / 2 else -size.tracksStep / 2
            directDrawing.drawLine((x, y), (x, y + bodyOffset), 4)
          }
          directDrawing.drawICBody(component.name.value, (centerX, centerY), xs(holes.length / 2 - 1) - xs.head, size.tracksStep)
        case Transistor(_, _) =>
          val centerHole = holePosition(holes(1))
          val (centerX, centerY) = (centerHole._1, centerHole._2 - (0.3 * size.holeStep).toInt)
          directDrawing.drawLine(holePosition(holes.head), (centerX - size.transistorLegsSpread, centerY), 2)
          directDrawing.drawLine(holePosition(holes(1)), (centerX, centerY), 2)
          directDrawing.drawLine(holePosition(holes(2)), (centerX + size.transistorLegsSpread, centerY), 2)
          directDrawing.drawTransistorBody(component.name.value, (centerX, centerY))
        case Cable(_, _) =>
          val (from, to) = (holePosition(holes.head), holePosition(holes(1)))
          if(from._2 == to._2) {
            directDrawing.drawArcCable(from, to, color)
          } else {
            directDrawing.drawStraightCable(from, to, color)
          }
        case Resistor(_, _) =>
          val Seq(holePos1, holePos2) = holes.map(holePosition).sortBy(_._1)
          val (centerX, centerY) = ((holePos1._1 + holePos2._1) / 2, Seq(holePos1._2, holePos2._2).min - (0.3 * size.holeStep).toInt)
          directDrawing.drawLine(holePos1, (centerX - size.resistorBodySize._1 / 2, centerY - size.resistorBodySize._2 / 2), 2)
          directDrawing.drawLine(holePos2, (centerX + size.resistorBodySize._1 / 2, centerY - size.resistorBodySize._2 / 2), 2)
          directDrawing.drawResistorBody(component.name.value, (centerX, centerY))
        case Capacitor(_, bipolar, _) =>
          val Seq(holePos1, holePos2) = holes.map(holePosition).sortBy(_._1)
          val (centerX, centerY) = ((holePos1._1 + holePos2._1) / 2, Seq(holePos1._2, holePos2._2).min - (0.3 * size.holeStep).toInt)
          directDrawing.drawLine(holePos1, (centerX - size.capacitorSize._1 / 2, centerY), 2)
          directDrawing.drawLine(holePos2, (centerX + size.capacitorSize._1 / 2, centerY), 2)
          val minusOnLeft = Some(holes.head.trackIndex.index < holes(1).trackIndex.index).filter(_ => bipolar)
          directDrawing.drawCapacitorBody(component.name.value, (centerX, centerY), minusOnLeft)
        case Diode(_, _) =>
          val Seq(holePos1, holePos2) = holes.map(holePosition).sortBy(_._1)
          val (centerX, centerY) = ((holePos1._1 + holePos2._1) / 2, Seq(holePos1._2, holePos2._2).min - (0.3 * size.holeStep).toInt)
          directDrawing.drawLine(holePos1, (centerX - size.resistorBodySize._1 / 2, centerY), 2)
          directDrawing.drawLine(holePos2, (centerX + size.resistorBodySize._1 / 2, centerY), 2)
          val cathodeOnLeft = holes.head.trackIndex.index < holes(1).trackIndex.index
          directDrawing.drawDiodeBody(component.name.value, (centerX, centerY), cathodeOnLeft)
        case _ => ()
      }
    }
    dom.console.log("drawing...")
    physical.tracks.foreach(drawTrack)
    physical.components.reverse.zipWithIndex.foreach((drawComponent _).tupled)
  }

  private def holePosition(hole: Hole): (Int, Int) =
    if(hole.trackIndex.horizontal) {
      (size.tracksHorizontalOffset + hole.holeIndex.position * size.holeStep, horizontalTrackVerticalOffset(hole.trackIndex))
    } else {
      val verticalOffset = verticalTrackVerticalOffset(hole.trackIndex)
      val locationIndex = hole.trackIndex.verticalLocationIndex
      if(hole.trackIndex.upper) {
        (locationIndex * size.tracksStep + size.tracksHorizontalOffset, verticalOffset + hole.holeIndex.position * size.holeStep)
      } else {
        (locationIndex * size.tracksStep + size.tracksHorizontalOffset, verticalOffset - hole.holeIndex.position * size.holeStep)
      }
    }

  private def drawVerticalTrack(vertical: Vertical): Unit = {
    val verticalOffset = verticalTrackVerticalOffset(vertical.index)
    val locationIndex = vertical.index.verticalLocationIndex
    val from = (locationIndex * size.tracksStep + size.tracksHorizontalOffset, verticalOffset)
    val to = (
      locationIndex * size.tracksStep + size.tracksHorizontalOffset,
      verticalOffset + (if(vertical.upper) size.verticalTrackLength else -size.verticalTrackLength)
    )
    directDrawing.drawLine(from, to, 1)
    for(h <- 0 until Tracks.verticalTrackLength) {
      directDrawing.drawHole(holePosition(Hole(vertical.index, TrackPosition(h))))
    }
  }

  private def verticalTrackVerticalOffset(index: TrackIndex) =
    if(index.upper) size.upperVerticalTracksVerticalOffset else size.bottomVerticalTracksVerticalOffset

  private def horizontalTrackVerticalOffset(index: TrackIndex) =
    if(index.upper) {
      index.index * size.tracksStep + size.upperHorizontalTracksVerticalOffset
    }  else {
      (index.index + 1) * size.tracksStep + size.bottomHorizontalTracksVerticalOffset
    }

  private def drawHorizontalTrack(horizontal: Horizontal): Unit = {
    val trackY = horizontalTrackVerticalOffset(horizontal.index)
    val from = (size.tracksHorizontalOffset, trackY)
    val to = (size.tracksHorizontalOffset + size.horizontalTrackLength, trackY)
    directDrawing.drawLine(from, to, 1)

    for(h <- 0 until Tracks.horizontalTrackLength) {
      directDrawing.drawHole(holePosition(Hole(horizontal.index, TrackPosition(h))))
    }
    drawPowerSign(horizontal.power, (from._1 - 12, from._2))
  }

  private def drawTrack(track: Track): Unit = track match {
    case v: Vertical => drawVerticalTrack(v)
    case h: Horizontal => drawHorizontalTrack(h)
  }

  private def drawPowerSign(power: Power.PowerConnection, pos: (Int, Int)) = power match {
    case Plus =>
      directDrawing.drawLine(from = (pos._1 - 5, pos._2), to = (pos._1 + 5, pos._2), 1)
      directDrawing.drawLine(from = (pos._1, pos._2 - 5), to = (pos._1, pos._2 + 5), 1)
    case GND =>
      directDrawing.drawLine(from = (pos._1 - 5, pos._2), to = (pos._1 + 5, pos._2), 1)
      directDrawing.drawLine(from = (pos._1 - 3, pos._2 + 3), to = (pos._1 + 3, pos._2 + 3), 1)
      directDrawing.drawLine(from = (pos._1 - 1, pos._2 + 6), to = (pos._1 + 1, pos._2 + 6), 1)
  }
}
