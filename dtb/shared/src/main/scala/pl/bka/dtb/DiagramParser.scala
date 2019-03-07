package pl.bka.dtb

import pl.bka.dtb.model._

import scala.util.parsing.combinator._

case class DiagramLineEncoding(
  component: Component,
  simpleLegConnection: ((String, String), Either[Int, Power.PowerConnection])
)

object DiagramLineEncodingParser extends RegexParsers {
  def componentType: Parser[ComponentType]   = """d|r|t|c""".r       ^^ {
    case "d" => Diode("")
    case "r" => Resistor("")
    case "t" => Transistor("")
    case "c" => Capacitor(0d, bipolar = true)
  }
}
