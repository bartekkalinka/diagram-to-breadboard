package pl.bka.model.breadboard

import pl.bka.DomOutput

case class TrackIndex(index: Int)

sealed trait Track {
  val upper: Boolean
  val index: TrackIndex
  val length: Int
  def draw(position: Int): Unit
}

case class Vertical(upper: Boolean, index: TrackIndex, length: Int = 5) extends Track {
  def draw(position: Int): Unit = {
    val ctx = DomOutput.renderer
    ctx.strokeStyle = "#000000"
    ctx.lineWidth = 2
    ctx.beginPath()
    ctx.moveTo(index.index * 15 + 30, position)
    ctx.lineTo(index.index * 15 + 30, position + 125)
    ctx.stroke()
  }
}

case class Horizontal(upper: Boolean, left: Boolean, index: TrackIndex, length: Int = 25) extends Track {
  def draw(position: Int): Unit = () //TODO
}


