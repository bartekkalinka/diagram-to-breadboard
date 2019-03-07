package pl.bka.dtb

import pl.bka.dtb.model._

import scala.util.parsing.combinator._

case class DiagramLineEncoding(
  component: Component,
  simpleLegConnection: ((String, String), Either[Int, Power.PowerConnection])
)

object DiagramLineEncodingParser extends RegexParsers {
  type Result = (ComponentType, Int)

  private def componentType: Parser[ComponentType]   = """d|r|t|c""".r       ^^ {
    case "d" => Diode("") //TODO solve ComponentType params better
    case "r" => Resistor("")
    case "t" => Transistor("")
    case "c" => Capacitor(0d, bipolar = true) //TODO solve bipolar better
  }

  private def legId: Parser[Int] = """[1-9]|[1-9]\\d*""".r ^^ { _.toInt }

  private def line: Parser[(ComponentType, Int)] = componentType ~ legId ^^ { case ct ~ lid => (ct, lid)}

  def parseDiagram(diagramStr: String): Either[String, Result] =
    parse(line, diagramStr) match {
      case Success(result, _) => Right(result)
      case Failure(msg, _) => Left(msg)
      case Error(msg, _) => Left(msg)
    }
}
