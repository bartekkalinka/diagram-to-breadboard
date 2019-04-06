package pl.bka.dtb.drawing

import pl.bka.dtb.Types.CoordWithName
import pl.bka.dtb.model.Power.{GND, Plus}
import pl.bka.dtb.model._
import pl.bka.dtb.model.breadboard._

class BoardDrawing(directDrawing: DirectDrawing, size: Size, physical: Physical, diagram: Diagram) {
  def drawPhysical(movedComponents: Map[ComponentName, (Int, Int)]): Map[(Int, Int), CoordWithName] = {
    physical.tracksWithFillIns.foreach(drawTrack)
    val componentPositions = physical.components.reverse.zipWithIndex.flatMap { case (component, index) =>
      val positionOverride = movedComponents.get(component.name)
      drawComponent(physical, component, index, positionOverride)
    }
    componentPositions.map { case (name, x, y) => ((x, y), ((x, y), name)) }.toMap
  }

  def drawComponent(physical: Physical, component: Component, compIndex: Int, positionOverride: Option[(Int, Int)]): Option[(ComponentName, Int, Int)] = {
    val holes: Seq[Hole] = component.legs.map { leg =>
      physical.connections(LegId(component.name, leg))

    }
    val color: String = Seq("#FFBB00", "#FF0000", "#0000FF", "#00FF00")(compIndex % 4)
    component.cType match { //TODO get rid of code blocks in match branches - move them to type class instances for components
      case IC(_, _) =>
        val (xs, ys) = holes.map(holePosition).unzip
        val (centerX, centerY) = positionOverride.getOrElse((xs.sum / xs.length, ys.sum / ys.length))
        holes.zipWithIndex.foreach { case (hole, i) =>
          val (x, y) = holePosition(hole)
          val bodyOffset = if(i < holes.length / 2) size.tracksStep / 2 else -size.tracksStep / 2
          directDrawing.drawLine((x, y), (x, y + bodyOffset), 4)
        }
        directDrawing.drawICBody(component.name.value, (centerX, centerY), xs(holes.length / 2 - 1) - xs.head, size.tracksStep)
        Some((component.name, centerX, centerY))
      case Transistor(_, _) =>
        val centerHole = holePosition(holes(1))
        val (centerX, centerY) = positionOverride.getOrElse((centerHole._1, centerHole._2 - (0.3 * size.holeStep).toInt))
        directDrawing.drawLine(holePosition(holes.head), (centerX - size.transistorLegsSpread, centerY), 2)
        directDrawing.drawLine(holePosition(holes(1)), (centerX, centerY), 2)
        directDrawing.drawLine(holePosition(holes(2)), (centerX + size.transistorLegsSpread, centerY), 2)
        directDrawing.drawTransistorBody(component.name.value, (centerX, centerY))
        Some((component.name, centerX, centerY))
      case Cable(_, tpe, _) =>
        val Seq((from, fromTrackIndex), (to, toTrackIndex)) = Seq((holePosition(holes.head), holes.head.trackIndex), (holePosition(holes(1)), holes(1).trackIndex)).sortBy(_._2.index)
        if(tpe == CableType.ConnCable) {
          val (dirFrom, dirTo) = cableArrowDirection(fromTrackIndex, toTrackIndex)
          directDrawing.drawArrowsRepresentingCable(from, to, dirFrom, dirTo, fromTrackIndex, toTrackIndex, color)
        } else if(tpe == CableType.UnionCable) {
          directDrawing.drawArcCable(from, to, color)
        } else {
          directDrawing.drawStraightCable(from, to, color)
        }
        None
      case Resistor(_, _) =>
        val Seq(holePos1, holePos2) = holes.map(holePosition).sortBy(_._1)
        val (centerX, centerY) = positionOverride.getOrElse(
          ((holePos1._1 + holePos2._1) / 2, Seq(holePos1._2, holePos2._2).min - (0.3 * size.holeStep).toInt)
        )
        directDrawing.drawLine(holePos1, (centerX - size.resistorBodySize._1 / 2, centerY - size.resistorBodySize._2 / 2), 2)
        directDrawing.drawLine(holePos2, (centerX + size.resistorBodySize._1 / 2, centerY - size.resistorBodySize._2 / 2), 2)
        directDrawing.drawResistorBody(component.name.value, (centerX, centerY))
        Some((component.name, centerX, centerY))
      case Capacitor(_, bipolar, _) =>
        val Seq(holePos1, holePos2) = holes.map(holePosition).sortBy(_._1)
        val (centerX, centerY) = positionOverride.getOrElse(
          ((holePos1._1 + holePos2._1) / 2, Seq(holePos1._2, holePos2._2).min - (0.3 * size.holeStep).toInt)
        )
        directDrawing.drawLine(holePos1, (centerX - size.capacitorSize._1 / 2, centerY), 2)
        directDrawing.drawLine(holePos2, (centerX + size.capacitorSize._1 / 2, centerY), 2)
        val minusOnLeft = Some(holes(Leg.capMinus.toInt).trackIndex.index < holes(Leg.capPlus.toInt).trackIndex.index).filter(_ => bipolar)
        directDrawing.drawCapacitorBody(component.name.value, (centerX, centerY), minusOnLeft)
        Some((component.name, centerX, centerY))
      case Diode(_, _) =>
        val Seq(holePos1, holePos2) = holes.map(holePosition).sortBy(_._1)
        val (centerX, centerY) = positionOverride.getOrElse(
          ((holePos1._1 + holePos2._1) / 2, Seq(holePos1._2, holePos2._2).min - (0.3 * size.holeStep).toInt)
        )
        directDrawing.drawLine(holePos1, (centerX - size.diodeBodySize._1 / 2, centerY), 2)
        directDrawing.drawLine(holePos2, (centerX + size.diodeBodySize._1 / 2, centerY), 2)
        val cathodeOnLeft = holes.head.trackIndex.index < holes(1).trackIndex.index
        directDrawing.drawDiodeBody(component.name.value, (centerX, centerY), cathodeOnLeft)
        Some((component.name, centerX, centerY))
      case _ => None
    }
  }

  private def cableArrowDirection(fromTrackIndex: TrackIndex, toTrackIndex: TrackIndex): ((Int, Int), (Int, Int)) = {
    def trackXY(index: TrackIndex): (Int, Int) = {
      val upper = index.upper
      (index.index - Breadboard.sideStartIndex(upper), if(upper) 0 else 1)
    }
    def sgn(a: Int, b: Int): Int =
      if(a > b) {
        1
      } else if(a < b) {
        -1
      } else if(fromTrackIndex.upper) {
        -1
      } else {
        1
      }
    val (fromX, fromY) = trackXY(fromTrackIndex)
    val (toX, toY) = trackXY(toTrackIndex)
    ((sgn(toX, fromX), sgn(toY, fromY)), (sgn(fromX, toX), sgn(fromY, toY)))
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
    val verticalShift = if(vertical.upper) -6 * size.holeRadius else 6 * size.holeRadius
    directDrawing.drawTrackIndex((from._1, from._2 + verticalShift), vertical.index)
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
    val horizontalLength = physical.horizontalTrackLength(horizontal.upper)
    val trackY = horizontalTrackVerticalOffset(horizontal.index)
    val from = (size.tracksHorizontalOffset, trackY)
    val to = (size.tracksHorizontalOffset + size.horizontalTrackLength(horizontalLength), trackY)
    directDrawing.drawLine(from, to, 1)

    for(h <- 0 until horizontalLength) {
      directDrawing.drawHole(holePosition(Hole(horizontal.index, TrackPosition(h))))
    }
    drawPowerSign(horizontal.power, (from._1 - 12, from._2))
  }

  private def drawTrack(track: Track): Unit = track match {
    case v: Vertical => drawVerticalTrack(v)
    case h: Horizontal => drawHorizontalTrack(h)
  }

  private def drawPowerSign(power: Power.PowerConnection, pos: (Int, Int)): Unit = power match {
    case Plus =>
      directDrawing.drawLine(from = (pos._1 - 5, pos._2), to = (pos._1 + 5, pos._2), 1)
      directDrawing.drawLine(from = (pos._1, pos._2 - 5), to = (pos._1, pos._2 + 5), 1)
    case GND =>
      directDrawing.drawLine(from = (pos._1 - 5, pos._2), to = (pos._1 + 5, pos._2), 1)
      directDrawing.drawLine(from = (pos._1 - 3, pos._2 + 3), to = (pos._1 + 3, pos._2 + 3), 1)
      directDrawing.drawLine(from = (pos._1 - 1, pos._2 + 6), to = (pos._1 + 1, pos._2 + 6), 1)
  }
}