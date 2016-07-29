package pl.bka

import org.scalajs.dom
import org.scalajs.dom.{html, _}
import pl.bka.model.breadboard.{Physical, Track, Vertical}

object DomOutput {
  val canvas = document.getElementById("canvas").asInstanceOf[html.Canvas]
  val renderer = canvas.getContext("2d")
    .asInstanceOf[dom.CanvasRenderingContext2D]

  def println(text: String) = {
    val textNode = document.createTextNode(text)
    val parNode = document.createElement("p")
    parNode.appendChild(textNode)
    document.body.appendChild(parNode)
  }
}

trait Drawer {
  def draw(position: Int): Unit
}

object Drawers {
  implicit def verticalDrawer(vertical: Vertical): Drawer = new Drawer {
    def draw(position: Int): Unit = {
      val ctx = DomOutput.renderer
      ctx.strokeStyle = "#000000"
      ctx.lineWidth = 2
      ctx.beginPath()
      ctx.moveTo(vertical.index.index * 15 + 30, position)
      ctx.lineTo(vertical.index.index * 15 + 30, position + 125)
      ctx.stroke()
    }
  }

  implicit def trackDrawer(track: Track): Drawer = new Drawer {
    def draw(position: Int): Unit = track match {
      case v: Vertical => v.draw(position)
      case _ => ()
    }
  }

  implicit def physicalDrawer(physical: Physical): Drawer = new Drawer {
    def draw(position: Int): Unit = physical.tracks.foreach(_.draw(10))
  }
}
