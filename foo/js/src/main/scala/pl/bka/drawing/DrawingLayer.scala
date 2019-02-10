package pl.bka.drawing

import pl.bka.model.breadboard.{Breadboard, TrackIndex}

object DrawingLayer {
  val isLowerBoardNumberingReversed = false
  val startIndex = 1

  implicit class TrackIndexLabeling(index: TrackIndex) {
    def label: String =
      if(index.upper) {
        "U" + "%02d".format(index.index + startIndex)
      } else {
        val lowerIndex =
          if(isLowerBoardNumberingReversed) {
            -index.index
          } else {
            Breadboard.maxVerticalTracks + index.index + startIndex
          }
        "D" + "%02d".format(lowerIndex)
      }
  }
}