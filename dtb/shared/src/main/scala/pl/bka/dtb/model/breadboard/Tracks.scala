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
  val index: TrackIndex
  val length: Int
  def upper: Boolean = index.upper
}

case class Vertical(index: TrackIndex, diagramConnection: Connection,
                    length: Int = Tracks.verticalTrackLength, freeSpace: Int = Tracks.verticalTrackLength, freeSpaceForLegs: Int = Tracks.verticalTrackLegsNumber) extends Track

case class Horizontal(index: TrackIndex,
                      power: PowerConnection, length: Int = Tracks.horizontalTrackLength) extends Track

object Tracks {
  val verticalTrackLength = 5
  val verticalTrackLegsNumber = 1
  val horizontalTrackLength = 25
}

