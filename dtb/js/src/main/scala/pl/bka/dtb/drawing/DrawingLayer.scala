package pl.bka.dtb.drawing

import pl.bka.dtb.model.breadboard.{Breadboard, OutOfBoardType, TrackIndex}

object DrawingLayer {
  val isLowerBoardNumberingReversed = false
  val startIndex = 1

  implicit class TrackIndexLabeling(index: TrackIndex) {
    def label: String =
      if(index.tpe == OutOfBoardType) {
        "O" + "%02d".format(index.index)
      } else if(index.upper) {
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