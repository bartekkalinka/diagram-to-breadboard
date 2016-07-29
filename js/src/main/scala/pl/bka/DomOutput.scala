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

