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
    component.cType match { //TODO get rid of code blocks in match branches
      case IC(_) =>
        val (xs, ys) = holes.map(holePosition).unzip
        val (centerX, centerY) = positionOverride.getOrElse((xs.sum / xs.length, ys.sum / ys.length))
        holes.zipWithIndex.foreach { case (hole, i) =>
          val (x, y) = holePosition(hole)
          val bodyOffset = if(i < holes.length / 2) size.tracksStep / 2 else -size.tracksStep / 2
          directDrawing.drawLine((x, y), (x, y + bodyOffset), 4)
        }
        directDrawing.drawICBody(component.name.value, (centerX, centerY), xs(holes.length / 2 - 1) - xs.head, size.tracksStep)
        Some((component.name, centerX, centerY))
      case Transistor(_) =>
        val centerHole = holePosition(holes(1))
        val (centerX, centerY) = positionOverride.getOrElse((centerHole._1, centerHole._2 - (0.3 * size.holeStep).toInt))
        directDrawing.drawLine(holePosition(holes.head), (centerX - size.transistorLegsSpread, centerY), 2)
        directDrawing.drawLine(holePosition(holes(1)), (centerX, centerY), 2)
        directDrawing.drawLine(holePosition(holes(2)), (centerX + size.transistorLegsSpread, centerY), 2)
        directDrawing.drawTransistorBody(component.name.value, (centerX, centerY))
        Some((component.name, centerX, centerY))
      case Cable(tpe, _) =>
        if(tpe == CableType.ConnCable) {
          val List((from, fromTrackIndex), (to, toTrackIndex)) = List((holePosition(holes.head), holes.head.trackIndex), (holePosition(holes(1)), holes(1).trackIndex)).sortBy(_._2.index)
          val (dirFrom, dirTo) = cableArrowDirection(fromTrackIndex, toTrackIndex)
          directDrawing.drawTwoCableArrows(from, to, dirFrom, dirTo, fromTrackIndex, toTrackIndex, color)
        } else if(tpe == CableType.UnionCable) {
          val List((from, fromTrackIndex), (to, toTrackIndex)) = List((holePosition(holes.head), holes.head.trackIndex), (holePosition(holes(1)), holes(1).trackIndex)).sortBy(_._2.index)
          directDrawing.drawArcCable(from, to, color)
        } else { //CableType.PowerCable
          val List((from, fromTrackIndex), (to, toTrackIndex)) = List((holePosition(holes.head), holes.head.trackIndex), (holePosition(holes(1)), holes(1).trackIndex))
          if(fromTrackIndex.tpe == OutOfBoardType) {
            val (_, dirTo) = cableArrowDirection(fromTrackIndex, toTrackIndex)
            directDrawing.drawOneCableArrow(to, dirTo, toTrackIndex, fromTrackIndex, color)
          } else {
            directDrawing.drawStraightCable(from, to, color)
          }
        }
        None
      case Resistor(_) =>
        val Seq(holePos1, holePos2) = holes.map(holePosition).sortBy(_._1)
        val (centerX, centerY) = positionOverride.getOrElse(
          ((holePos1._1 + holePos2._1) / 2, Seq(holePos1._2, holePos2._2).min - (0.3 * size.holeStep).toInt)
        )
        directDrawing.drawLine(holePos1, (centerX - size.resistorBodySize._1 / 2, centerY - size.resistorBodySize._2 / 2), 2)
        directDrawing.drawLine(holePos2, (centerX + size.resistorBodySize._1 / 2, centerY - size.resistorBodySize._2 / 2), 2)
        directDrawing.drawResistorBody(component.name.value, (centerX, centerY))
        Some((component.name, centerX, centerY))
      case Capacitor(bipolar, _) =>
        val Seq(holePos1, holePos2) = holes.map(holePosition).sortBy(_._1)
        val (centerX, centerY) = positionOverride.getOrElse(
          ((holePos1._1 + holePos2._1) / 2, Seq(holePos1._2, holePos2._2).min - (0.3 * size.holeStep).toInt)
        )
        directDrawing.drawLine(holePos1, (centerX - size.capacitorSize._1 / 2, centerY), 2)
        directDrawing.drawLine(holePos2, (centerX + size.capacitorSize._1 / 2, centerY), 2)
        val minusOnLeft = Some(holes(Leg.capMinus.toInt).trackIndex.index < holes(Leg.capPlus.toInt).trackIndex.index).filter(_ => bipolar)
        directDrawing.drawCapacitorBody(component.name.value, (centerX, centerY), minusOnLeft)
        Some((component.name, centerX, centerY))
      case Diode(_) =>
        val Seq(holePos1, holePos2) = holes.map(holePosition).sortBy(_._1)
        val (centerX, centerY) = positionOverride.getOrElse(
          ((holePos1._1 + holePos2._1) / 2, Seq(holePos1._2, holePos2._2).min - (0.3 * size.holeStep).toInt)
        )
        directDrawing.drawLine(holePos1, (centerX - size.diodeBodySize._1 / 2, centerY), 2)
        directDrawing.drawLine(holePos2, (centerX + size.diodeBodySize._1 / 2, centerY), 2)
        val cathodeOnLeft = holes.head.trackIndex.index < holes(1).trackIndex.index
        directDrawing.drawDiodeBody(component.name.value, (centerX, centerY), cathodeOnLeft)
        Some((component.name, centerX, centerY))
      case Node(_) =>
        val holePos = outOfBoardHolePosition(holes.head)
        directDrawing.drawLine((holePos._1, holePos._2 - size.tracksStep / 3), (holePos._1, holePos._2 + size.tracksStep / 3), 2)
        directDrawing.drawLine((holePos._1 - size.tracksStep / 3, holePos._2), (holePos._1 + size.tracksStep / 3, holePos._2), 2)
        directDrawing.drawText((holePos._1 - size.tracksStep / 3, holePos._2 - size.tracksStep / 3 - 1), component.name.value)
        directDrawing.drawTrackIndex((holePos._1 - size.tracksStep / 3, holePos._2 + size.tracksStep / 3 + size.trackIndexFontSize), holes.head.trackIndex)
        None
      case Pot(_) =>
        val holePositions = holes.map(outOfBoardHolePosition).sortBy(_._1)
        holePositions.foreach(directDrawing.drawHole)
        val margin = size.tracksStep / 2
        val floorY = holePositions.head._2 + size.holeRadius + 4
        val lowerRoofY = holePositions.head._2 - size.holeRadius - 4
        val higherRoofY = holePositions.head._2 - size.holeRadius - margin
        directDrawing.drawLine((holePositions.head._1 - margin, lowerRoofY), (holePositions.head._1 - margin, floorY), 2)
        directDrawing.drawLine((holePositions.head._1 - margin, floorY), (holePositions(2)._1 + margin, floorY), 2)
        directDrawing.drawLine((holePositions(2)._1 + margin, floorY), (holePositions(2)._1 + margin, lowerRoofY), 2)
        directDrawing.drawLine((holePositions(2)._1 + margin, lowerRoofY), (holePositions(2)._1, higherRoofY), 2)
        directDrawing.drawLine((holePositions(2)._1, higherRoofY), ((holePositions(2)._1 + holePositions(1)._1) / 2, lowerRoofY), 2)
        directDrawing.drawLine(((holePositions(2)._1 + holePositions(1)._1) / 2, lowerRoofY), (holePositions(1)._1, higherRoofY), 2)
        directDrawing.drawLine((holePositions(1)._1, higherRoofY), ((holePositions(1)._1 + holePositions.head._1) / 2, lowerRoofY), 2)
        directDrawing.drawLine(((holePositions(1)._1 + holePositions.head._1) / 2, lowerRoofY), (holePositions.head._1, higherRoofY), 2)
        directDrawing.drawLine((holePositions.head._1, higherRoofY), (holePositions.head._1 - margin, lowerRoofY), 2)
        directDrawing.drawText((holePositions(1)._1 - margin, higherRoofY - size.tracksStep / 8), component.name.value)
        holePositions.zip(holes).foreach { case (holePos, hole) =>
          directDrawing.drawTrackIndex((holePos._1 - size.tracksStep / 3, holePos._2 + size.tracksStep / 3 + size.trackIndexFontSize), hole.trackIndex)
        }
        None
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
    if(hole.trackIndex.tpe == HorizontalType) {
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

  private def outOfBoardHolePosition(hole: Hole): (Int, Int) =
    (hole.trackIndex.index * size.tracksStep + size.tracksHorizontalOffset, size.bottomHorizontalTracksVerticalOffset + size.tracksStep)

  private def drawVerticalTrack(vertical: DiagramConnectionTrack): Unit = {
    val verticalOffset = verticalTrackVerticalOffset(vertical.trackIndex)
    val locationIndex = vertical.trackIndex.verticalLocationIndex
    val from = (locationIndex * size.tracksStep + size.tracksHorizontalOffset, verticalOffset)
    val to = (
      locationIndex * size.tracksStep + size.tracksHorizontalOffset,
      verticalOffset + (if(vertical.upper) size.verticalTrackLength else -size.verticalTrackLength)
    )
    directDrawing.drawLine(from, to, 1)
    for(h <- 0 until Tracks.verticalTrackLength) {
      directDrawing.drawHole(holePosition(Hole(vertical.trackIndex, TrackPosition(h))))
    }
    val verticalShift = if(vertical.upper) -6 * size.holeRadius else 6 * size.holeRadius
    directDrawing.drawTrackIndex((from._1, from._2 + verticalShift), vertical.trackIndex)
  }

  private def verticalTrackVerticalOffset(index: TrackIndex): Int =
    if(index.upper) size.upperVerticalTracksVerticalOffset else size.bottomVerticalTracksVerticalOffset

  private def horizontalTrackVerticalOffset(trackIndex: TrackIndex): Int =
    if(trackIndex.upper) {
      trackIndex.index * size.tracksStep + size.upperHorizontalTracksVerticalOffset
    }  else {
      (trackIndex.index + 1) * size.tracksStep + size.bottomHorizontalTracksVerticalOffset
    }

  private def drawHorizontalTrack(horizontal: Horizontal): Unit = {
    val horizontalLength = physical.horizontalTrackLength(horizontal.upper)
    val trackY = horizontalTrackVerticalOffset(horizontal.trackIndex)
    val from = (size.tracksHorizontalOffset, trackY)
    val to = (size.tracksHorizontalOffset + size.horizontalTrackLength(horizontalLength), trackY)
    directDrawing.drawLine(from, to, 1)

    for(h <- 0 until horizontalLength) {
      directDrawing.drawHole(holePosition(Hole(horizontal.trackIndex, TrackPosition(h))))
    }
    drawPowerSign(horizontal.power, (from._1 - 12, from._2))
  }

  private def drawTrack(track: Track): Unit = track match {
    case v: DiagramConnectionTrack if !v.isOutOfBoard => drawVerticalTrack(v)
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