package pl.bka.dtb.drawing

import org.scalajs.dom
import pl.bka.dtb.model.breadboard.{OutOfBoardType, TrackIndex}
import DrawingLayer._
import pl.bka.dtb.DomOutput

class DirectDrawing(size: Size) {
  val ctx = DomOutput.canvas.getContext("2d")
    .asInstanceOf[dom.CanvasRenderingContext2D]

  def clear(): Unit = {
    ctx.clearRect(0, 0, DomOutput.canvas.width, DomOutput.canvas.height)
  }

  def drawSelectionMark(pos: (Int, Int)): Unit = {
    ctx.fillStyle = "#FFFFFF"
    ctx.strokeStyle = "#000000"
    ctx.lineWidth = 1
    ctx.beginPath()
    ctx.arc(pos._1, pos._2, size.selectionMarkSize, - 2 * Math.PI, 0)
    ctx.stroke()
  }

  def drawArcCable(from: (Int, Int), to: (Int, Int), color: String): Unit = {
    ctx.strokeStyle = color
    ctx.lineWidth = 2
    ctx.beginPath()
    if(from._2 == to._2) {
      val xDelta = (to._1 - from._1) / 2
      val radius = (to._1 - from._1) * size.shortCableArcRadiusFactor
      val yDelta = Math.sqrt(radius * radius - xDelta * xDelta).toInt
      val angle = Math.atan2(xDelta, yDelta)
      val (centerX, centerY) = (from._1 + xDelta, from._2 + yDelta)
      val startAngle = -Math.PI / 2 - angle
      val endAngle = -Math.PI / 2 + angle
      ctx.arc(centerX, centerY, radius, startAngle, endAngle)
    } else {
      val yDelta = (from._2 - to._2) / 2
      val radius = (from._2 - to._2) * size.longCableArcRadiusFactor
      val xDelta = Math.sqrt(radius * radius - yDelta * yDelta).toInt
      val angle = Math.atan2(yDelta, xDelta)
      val (centerY, centerX) = (from._2 - yDelta, from._1 + xDelta)
      val startAngle = -Math.PI  - angle
      val endAngle = -Math.PI  + angle
      ctx.arc(centerX, centerY, radius, startAngle, endAngle)
    }
    ctx.stroke()
  }

  def drawStraightCable(from: (Int, Int), to: (Int, Int), color: String): Unit =
    drawLine(from, to, 2, color)

  def drawOneCableArrow(pos: (Int, Int), arrowDir: (Int, Int), thisTrackIndex: TrackIndex, otherTrackIndex: TrackIndex, color: String): Unit = {
    ctx.font = size.arrowHeadFont
    ctx.fillStyle = color
    if (thisTrackIndex.tpe != OutOfBoardType) {
      val arrowHead = (pos._1 + arrowDir._1 * size.arrowLength, pos._2 + arrowDir._2 * size.arrowLength)
      drawLine(pos, arrowHead, 2, color)
      drawLine(arrowHead, (arrowHead._1 - arrowDir._1 * size.arrowHeadWidth, arrowHead._2), 2, color)
      drawLine(arrowHead, (arrowHead._1, arrowHead._2 - arrowDir._2 * size.arrowHeadWidth), 2, color)
      ctx.fillText(otherTrackIndex.label, arrowHead._1 + (1.5 * arrowDir._1 - 1) * size.arrowHeadWidth, arrowHead._2 + (0.5 * arrowDir._2 + 0.5) * size.arrowHeadWidth)
    }
  }

  def drawTwoCableArrows(from: (Int, Int), to: (Int, Int), fromArrowDir: (Int, Int), toArrowDir: (Int, Int), fromTrackIndex: TrackIndex, toTrackIndex: TrackIndex, color: String): Unit = {
    drawOneCableArrow(from, fromArrowDir, fromTrackIndex, toTrackIndex, color)
    drawOneCableArrow(to, toArrowDir, toTrackIndex, fromTrackIndex, color)
  }

  def drawICBody(name: String, pos: (Int, Int), width: Int, height: Int): Unit = {
    ctx.fillStyle = "#FFFFFF"
    ctx.strokeStyle = "#000000"
    ctx.lineWidth = 1
    val leftX = pos._1 - width / 2 - 2
    ctx.strokeRect(leftX, pos._2 - height / 2, width + 4, height)
    ctx.strokeRect(leftX, pos._2 - height / 4, 8, height / 2)
    ctx.font = size.icFont
    ctx.fillStyle = "#000000"
    val textSize = name.length * size.icFontSize / 2
    ctx.fillText(name, pos._1 - textSize / 2, pos._2)
  }

  def drawTransistorBody(name: String, pos: (Int, Int)): Unit = {
    ctx.fillStyle = "#FFFFFF"
    ctx.strokeStyle = "#000000"
    ctx.lineWidth = 1
    ctx.beginPath()
    ctx.arc(pos._1, pos._2, size.transistorBodyRadius, - Math.PI, 0)
    ctx.moveTo(pos._1 - size.transistorBodyRadius, pos._2)
    ctx.lineTo(pos._1 + size.transistorBodyRadius, pos._2)
    ctx.stroke()
    ctx.fill()
    ctx.font = size.font
    ctx.fillStyle = "#000000"
    ctx.fillText(name, pos._1 - size.transistorBodyRadius + 2, pos._2 - 2)
  }

  def drawResistorBody(cname: String, pos: (Int, Int)): Unit = {
    ctx.fillStyle = "#FFFFFF"
    ctx.strokeStyle = "#000000"
    ctx.lineWidth = 1
    val bodyVerticalSize = Math.max(size.resistorBodySize._1, cname.length * (size.fontSize- 2))
    ctx.beginPath()
    ctx.moveTo(pos._1 - bodyVerticalSize / 2, pos._2 - size.resistorBodySize._2)
    ctx.lineTo(pos._1 + bodyVerticalSize / 2, pos._2 - size.resistorBodySize._2)
    ctx.stroke()
    ctx.lineTo(pos._1 + bodyVerticalSize / 2, pos._2)
    ctx.stroke()
    ctx.lineTo(pos._1 - bodyVerticalSize / 2, pos._2)
    ctx.stroke()
    ctx.lineTo(pos._1 - bodyVerticalSize / 2, pos._2 - size.resistorBodySize._2)
    ctx.stroke()
    ctx.fill()
    ctx.font = size.font
    ctx.fillStyle = "#000000"
    ctx.fillText(cname, pos._1 - bodyVerticalSize / 2 + 4, pos._2 - 2)
  }

  def drawCapacitorBody(cname: String, pos: (Int, Int), minusOnLeft: Option[Boolean]): Unit = {
    ctx.fillStyle = "#FFFFFF"
    ctx.strokeStyle = "#000000"
    ctx.lineWidth = 1
    //body
    ctx.beginPath()
    ctx.moveTo(pos._1 - size.capacitorSize._1 / 2, pos._2 - size.capacitorSize._2 / 2)
    ctx.lineTo(pos._1 - size.capacitorSize._1 / 2, pos._2 + size.capacitorSize._2 / 2)
    ctx.stroke()
    ctx.moveTo(pos._1 + size.capacitorSize._1 / 2, pos._2 - size.capacitorSize._2 / 2)
    ctx.lineTo(pos._1 + size.capacitorSize._1 / 2, pos._2 + size.capacitorSize._2 / 2)
    ctx.stroke()
    minusOnLeft.foreach { minOnLeft =>
      //polarity
      val polarity = if (minOnLeft) -1 else 1
      //plus
      ctx.moveTo(pos._1 - 2 * size.capacitorSize._1 * polarity / 2 - 10 * polarity, pos._2 - size.capacitorSize._2 / 2 + 5)
      ctx.lineTo(pos._1 - 2 * size.capacitorSize._1 * polarity / 2 - 5 * polarity, pos._2 - size.capacitorSize._2 / 2 + 5)
      ctx.stroke()
      ctx.moveTo(pos._1 - 2 * size.capacitorSize._1 * polarity / 2 - 7.5 * polarity, pos._2 - size.capacitorSize._2 / 2 + 2.5)
      ctx.lineTo(pos._1 - 2 * size.capacitorSize._1 * polarity / 2 - 7.5 * polarity, pos._2 - size.capacitorSize._2 / 2 + 7.5)
      ctx.stroke()
      //minus
      ctx.moveTo(pos._1 + 2 * size.capacitorSize._1 * polarity / 2 + 5 * polarity, pos._2 - size.capacitorSize._2 / 2 + 5)
      ctx.lineTo(pos._1 + 2 * size.capacitorSize._1 * polarity / 2 + 10 * polarity, pos._2 - size.capacitorSize._2 / 2 + 5)
      ctx.stroke()
    }
    //name
    ctx.font = size.font
    ctx.fillStyle = "#000000"
    ctx.fillText(cname, pos._1 - 12, pos._2 - size.capacitorSize._2 / 2 - 2)
  }

  def drawDiodeBody(cname: String, pos: (Int, Int), cathodeOnLeft: Boolean): Unit = {
    ctx.fillStyle = "#FFFFFF"
    ctx.strokeStyle = "#000000"
    ctx.lineWidth = 1
    ctx.beginPath()
    ctx.moveTo(pos._1 - size.diodeBodySize._1 / 2, pos._2 - size.diodeBodySize._2)
    ctx.lineTo(pos._1 + size.diodeBodySize._1 / 2, pos._2 - size.diodeBodySize._2)
    ctx.stroke()
    ctx.lineTo(pos._1 + size.diodeBodySize._1 / 2, pos._2)
    ctx.stroke()
    ctx.lineTo(pos._1 - size.diodeBodySize._1 / 2, pos._2)
    ctx.stroke()
    ctx.lineTo(pos._1 - size.diodeBodySize._1 / 2, pos._2 - size.diodeBodySize._2)
    ctx.stroke()
    ctx.fill()
    val polarity = if(cathodeOnLeft) -1 else 1
    ctx.beginPath()
    ctx.moveTo(pos._1 + polarity * (size.diodeBodySize._1 / 2 - 4), pos._2 - size.diodeBodySize._2)
    ctx.lineTo(pos._1 + polarity * (size.diodeBodySize._1 / 2 - 4), pos._2)
    ctx.stroke()
    ctx.fill()
    ctx.font = size.font
    ctx.fillStyle = "#000000"
    ctx.fillText(cname, pos._1 - size.diodeBodySize._1 / 2, pos._2 - 2)
  }

  def drawHole(pos: (Int, Int)): Unit = {
    ctx.fillStyle = "#FFFFFF"
    ctx.strokeStyle = "#000000"
    ctx.lineWidth = 1
    ctx.beginPath()
    ctx.arc(pos._1, pos._2, size.holeRadius, 0, 2*Math.PI)
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

  def drawTrackIndex(pos: (Int, Int), trackIndex: TrackIndex): Unit = {
    ctx.font = size.trackIndexFont
    ctx.fillStyle = "#000000"
    ctx.fillText(trackIndex.label, pos._1, pos._2)
  }

  def drawText(pos: (Int, Int), text: String, color: String = "#000000"): Unit = {
    ctx.font = size.font
    ctx.fillStyle = color
    ctx.fillText(text, pos._1, pos._2)
  }
}

