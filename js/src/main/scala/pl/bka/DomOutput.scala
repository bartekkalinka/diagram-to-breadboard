package pl.bka

import org.scalajs.dom
import org.scalajs.dom.{html, _}

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

  //TODO remove after using as an example
  def canvasTest() = {
    canvas.width = canvas.parentElement.clientWidth
    canvas.height = canvas.parentElement.clientHeight

    renderer.fillStyle = "#111111"
    renderer.fillRect(0, 0, canvas.width, canvas.height)
  }
}

