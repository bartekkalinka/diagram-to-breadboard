package pl.bka.drawing

import org.scalajs.dom
import pl.bka.DomOutput

object DirectDrawing extends Const {
  val ctx = DomOutput.canvas.getContext("2d")
    .asInstanceOf[dom.CanvasRenderingContext2D]

  def drawCable(from: (Int, Int), to: (Int, Int), color: String): Unit = {
    ctx.strokeStyle = color
    ctx.lineWidth = 2
    ctx.beginPath()
    val xDelta = (to._1 - from._1) / 2
    val radius = (to._1 - from._1) * cableArcRadiusFactor
    val yDelta = Math.sqrt(radius * radius - xDelta * xDelta).toInt
    val angle = Math.atan2(xDelta, yDelta)
    val (centerX, centerY) = (from._1 + xDelta, from._2 + yDelta)
    ctx.arc(centerX, centerY, radius, - Math.PI / 2 - angle, - Math.PI / 2 + angle)
    ctx.stroke()
  }

  def drawTransistorBody(symbol: String, pos: (Int, Int)): Unit = {
    ctx.fillStyle = "#FFFFFF"
    ctx.strokeStyle = "#000000"
    ctx.lineWidth = 1
    ctx.beginPath()
    ctx.arc(pos._1, pos._2, transistorBodyRadius, - Math.PI, 0)
    ctx.moveTo(pos._1 - transistorBodyRadius, pos._2)
    ctx.lineTo(pos._1 + transistorBodyRadius, pos._2)
    ctx.stroke()
    ctx.fill()
    ctx.font = font
    ctx.fillStyle = "#000000"
    ctx.fillText(symbol, pos._1 - transistorBodyRadius + 2, pos._2 - 2)
  }

  def drawHole(pos: (Int, Int)): Unit = {
    ctx.fillStyle = "#FFFFFF"
    ctx.strokeStyle = "#000000"
    ctx.lineWidth = 1
    ctx.beginPath()
    ctx.arc(pos._1, pos._2, holeRadius, 0, 2*Math.PI)
    ctx.stroke()
    ctx.fill()
  }

  def drawLine(from: (Int, Int), to: (Int, Int), lineWidth: Int, color: String = "#000000"): Unit = {
    ctx.strokeStyle = color
    ctx.lineWidth = lineWidth
    ctx.beginPath()
    ctx.moveTo(from._1, from._2)
    ctx.lineTo(to._1, to._2)
    ctx.stroke()
  }
}

