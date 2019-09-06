package pl.bka.dtb.model.breadboard

import pl.bka.dtb.model.Connection
import pl.bka.dtb.model.Power._

sealed trait TrackType
case object HorizontalType extends TrackType
case object VerticalType extends TrackType
case object OutOfBoardType extends TrackType

object TrackType {
  implicit val trackTypeOrdering: Ordering[TrackType] = implicitly[Ordering[Int]].on {
    case HorizontalType => 0
    case VerticalType => 1
    case OutOfBoardType => 2
  }
}

case class TrackIndex(tpe: TrackType, index: Int) {
  def upper: Boolean = index >= 0
  def verticalLocationIndex: Int = if(upper) index else index + Breadboard.maxVerticalTracks
}

sealed trait Track {
  val index: Int
  val length: Int
  def trackIndex: TrackIndex
  def upper: Boolean = trackIndex.upper
}

sealed trait DiagramConnectionTrack extends Track {
  val diagramConnection: Connection
  val freeSpace: Int
  def setFreeSpace(newFreeSpace: Int): DiagramConnectionTrack
}

case class Vertical(index: Int, diagramConnection: Connection,
                    length: Int = Tracks.verticalTrackLength, freeSpace: Int = Tracks.verticalTrackLength) extends DiagramConnectionTrack {
  def trackIndex: TrackIndex = TrackIndex(VerticalType, index)
  def setFreeSpace(newFreeSpace: Int): DiagramConnectionTrack = copy(freeSpace = newFreeSpace)
}

case class OutOfBoard(index: Int, diagramConnection: Connection, length: Int = 2, freeSpace: Int = 2) extends DiagramConnectionTrack {
  def trackIndex: TrackIndex = TrackIndex(OutOfBoardType, index)
  def setFreeSpace(newFreeSpace: Int): DiagramConnectionTrack = copy(freeSpace = newFreeSpace)
}

case class Horizontal(index: Int,
                      power: PowerConnection, length: Int = Tracks.horizontalTrackLength) extends Track {
  def trackIndex: TrackIndex = TrackIndex(HorizontalType, index)
}

object Tracks {
  val verticalTrackLength = 5
  val verticalTrackLegsNumber = 1
  val horizontalTrackLength = 25
}

