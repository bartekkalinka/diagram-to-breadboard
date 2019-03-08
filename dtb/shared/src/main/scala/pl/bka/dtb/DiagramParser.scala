package pl.bka.dtb

import pl.bka.dtb.model._

import scala.util.parsing.combinator._

case class DiagramLineEncoding(
  component: Component,
  simpleLegConnection: ((String, String), Either[Int, Power.PowerConnection])
)

object DiagramLineEncodingParser extends RegexParsers {
  type Result = (ComponentType, Seq[Int])

  private def diode: Parser[Diode] = "d" ^^ { _ => Diode("") }
  private def resistor: Parser[Resistor] = "r" ^^ { _ => Resistor("") }
  private def capacitor: Parser[Capacitor] = "c" ^^ { _ => Capacitor(0d, bipolar = true) } //TODO better params
  private def transistor: Parser[Transistor] = "t" ^^ { _ => Transistor("") }

  private def legId: Parser[Int] = """[1-9]|[1-9]\\d*""".r ^^ { _.toInt }

  private def twoLegsLine: Parser[Result] =
    (diode | resistor | capacitor) ~ legId ~ legId ^^ { case ct ~ lid1 ~ lid2 => (ct, List(lid1, lid2)) }

  private def transistorLine: Parser[Result] =
    transistor ~ legId ~ legId ~ legId ^^ { case t ~ lid1 ~ lid2 ~ lid3 => (t, List(lid1, lid2, lid3)) }

  private def line: Parser[Result] = twoLegsLine | transistorLine

  def parseDiagram(diagramStr: String): Either[String, Result] =
    parse(line, diagramStr) match {
      case Success(result, _) => Right(result)
      case Failure(msg, _) => Left(msg)
      case Error(msg, _) => Left(msg)
    }
}
