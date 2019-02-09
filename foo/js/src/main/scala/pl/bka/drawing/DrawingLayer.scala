package pl.bka.drawing

import pl.bka.model.breadboard.{Breadboard, TrackIndex}

object DrawingLayer {
  val isLowerBoardNumberingReversed = false

  implicit class TrackIndexLabeling(index: TrackIndex) {
    def label: String =
      if(index.upper) {
        "U" + "%02d".format(index.index)
      } else {
        val lowerIndex =
          if(isLowerBoardNumberingReversed) {
            -index.index
          } else {
            Breadboard.maxVerticalTracks + index.index + 1
          }
        "D" + "%02d".format(lowerIndex)
      }
  }
}