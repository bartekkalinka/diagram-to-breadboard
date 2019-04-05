package pl.bka.dtb

import pl.bka.dtb.model.Power.{GND, Plus}
import pl.bka.dtb.model._

import scala.util.parsing.combinator._
import scala.languageFeature.postfixOps

case class DiagramLineEncoding(
  component: Component,
  simpleLegConnection: ((String, String), Either[Int, Power.PowerConnection])
)

object DiagramLineEncodingParser extends RegexParsers {
  type Line = (Component, Seq[Either[Int, Power.PowerConnection]])
  type Result = Either[Fail, Diagram]

  private def name(str: String) = str.split("\\.")(1)
  private def cName = "[a-zA-Z0-9\\-]+"
  private def diode: Parser[Component] = s"""d\\.$cName""".r ^^ { str => Component(name(str), Diode("")) }
  private def resistor: Parser[Component] = s"""r\\.$cName""".r ^^ { str => Component(name(str), Resistor("")) }
  private def capacitor: Parser[Component] = s"""c\\.$cName""".r ^^ { str => Component(name(str), Capacitor(0d, bipolar = true)) } //TODO better params
  private def transistor: Parser[Component] = s"""t\\.$cName""".r ^^ { str => Component(name(str), Transistor("")) }
  private def ic: Parser[Component] = s"""i\\.$cName""".r ^^ { str => Component(name(str), IC("", 0)) }

  private def legId: Parser[Either[Int, Power.PowerConnection]] =
    """[1-9]|[1-9]\\d*""".r ^^ { n => Left(n.toInt) } | "plus" ^^ { _ => Right(Plus) } |  "gnd" ^^ { _ => Right(GND) }

  private def twoLegsLine: Parser[Line] =
    (diode | resistor | capacitor) ~ legId ~ legId ^^ { case ct ~ lid1 ~ lid2 => (ct, List(lid1, lid2)) }

  private def transistorLine: Parser[Line] =
    transistor ~ legId ~ legId ~ legId ^^ { case t ~ lid1 ~ lid2 ~ lid3 => (t, List(lid1, lid2, lid3)) }

  private def icLine: Parser[Line] = ic ~ (legId+) ^^ {
    case Component(ComponentName(icName), _, _) ~ legIds => (Component(icName, IC("", legIds.length)), legIds)
  }

  private def line: Parser[Line] = twoLegsLine | transistorLine | icLine

  private def diagram: Parser[Result] = (line+) ^^ { lines =>
    val (components, connections) = lines.map { case (component, legsConnections) =>
      (
        component,
        legsConnections.zipWithIndex.map { case (legId, i) =>
          (component.name.value, i.toString) -> legId
        }
      )
    }.unzip
    Diagram(components, connections.flatten.toMap)
  }

  def parseDiagram(diagramStr: String): Either[String, Result] =
    parse(diagram, diagramStr) match {
      case Success(result, _) => Right(result)
      case Failure(msg, _) => Left(msg)
      case Error(msg, _) => Left(msg)
    }
}
