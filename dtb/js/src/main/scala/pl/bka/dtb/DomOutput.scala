package pl.bka.dtb

import org.scalajs.dom.{document, html}

object DomOutput {
  val canvas = document.getElementById("canvas").asInstanceOf[html.Canvas]
  val messages = document.getElementById("messages").asInstanceOf[html.Div]

  def println(text: String) = {
    val textNode = document.createTextNode(text)
    val parNode = document.createElement("p")
    parNode.appendChild(textNode)
    document.body.appendChild(parNode)
  }
}
