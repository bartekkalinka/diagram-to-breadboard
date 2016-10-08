package pl.bka

import org.scalajs.dom.{html, _}

object DomOutput {
  val canvas = document.getElementById("canvas").asInstanceOf[html.Canvas]

  def println(text: String) = {
    val textNode = document.createTextNode(text)
    val parNode = document.createElement("p")
    parNode.appendChild(textNode)
    document.body.appendChild(parNode)
  }
}

