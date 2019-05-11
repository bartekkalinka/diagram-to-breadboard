package pl.bka.dtb

import pl.bka.dtb.model.Power.{GND, Plus}
import pl.bka.dtb.model._

import scala.util.parsing.combinator._
import scala.languageFeature.postfixOps

object DiagramParser extends RegexParsers {
  sealed trait CommonConnection { def id: Connection }
  case class RegularConnection(id: Connection) extends CommonConnection
  case class BipolarConnection(plus: Boolean, id: Connection) extends CommonConnection
  case class BandConnection(id: Connection) extends CommonConnection
  type Line = (Component, Seq[CommonConnection])
  type Result = (Seq[Component], Map[(String, String), Either[Int, Power.PowerConnection]])

  private def name(str: String) = str.split("\\.")(1)
  private def cName = "[a-zA-Z0-9\\-]+"
  private def diode: Parser[Component] = s"""d\\.$cName""".r ^^ { str => Component(name(str), Diode()) }
  private def resistor: Parser[Component] = s"""r\\.$cName""".r ^^ { str => Component(name(str), Resistor()) }
  private def capacitor: Parser[Component] = s"""c\\.$cName""".r ^^ { str => Component(name(str), Capacitor(bipolar = false)) }
  private def bipolarCapacitor: Parser[Component] = s"""bc\\.$cName""".r ^^ { str => Component(name(str), Capacitor(bipolar = true)) }
  private def transistor: Parser[Component] = s"""t\\.$cName""".r ^^ { str => Component(name(str), Transistor()) }
  private def ic: Parser[Component] = s"""i\\.$cName""".r ^^ { str => Component(name(str), IC("", 0)) }

  private def connection: Parser[Connection] =
    """\d+""".r ^^ { n => Connection(Left(n.toInt)) } |
      "plus" ^^ { _ => Connection(Right(Plus)) } |
      "gnd" ^^ { _ => Connection(Right(GND)) }

  private def regularConnection: Parser[RegularConnection] = connection ^^ RegularConnection

  private def bipolarConnection: Parser[BipolarConnection] =
    """\+|\-""".r ~ connection ^^ { case pm ~ conn => BipolarConnection(pm == "+", conn) }

  private def bandConnection: Parser[BandConnection] =
    "band\\.".r ~ connection ^^ { case _ ~ conn => BandConnection(conn) }

  private def twoLegsLine: Parser[Line] =
    (resistor | capacitor) ~ regularConnection ~ regularConnection ^^
      { case ct ~ conn1 ~ conn2 => (ct, List(conn1, conn2)) }

  private def transistorLine: Parser[Line] =
    transistor ~ regularConnection ~ regularConnection ~ regularConnection ^^ { case t ~ conn1 ~ conn2 ~ conn3 => (t, List(conn1, conn2, conn3)) }

  private def icLine: Parser[Line] = ic ~ (regularConnection+) ^^ {
    case Component(ComponentName(icName), _, _) ~ connections => (Component(icName, IC("", connections.length)), connections)
  }

  private def bipolarCapLine: Parser[Line] =
    bipolarCapacitor ~ bipolarConnection ~ bipolarConnection ^^ { case ct ~ conn1 ~ conn2 =>
      (ct, if(conn1.plus) List(conn2, conn1) else List(conn1, conn2))
    }

  private def diodeLine: Parser[Line] =
    (diode ~ bandConnection ~ regularConnection) ^^ { case ct ~ bconn ~ rconn => (ct, List(bconn, rconn)) } |
      (diode ~ regularConnection ~ bandConnection) ^^ { case ct ~ rconn ~ bconn => (ct, List(bconn, rconn)) }

  private def line: Parser[Line] = twoLegsLine | diodeLine | transistorLine | icLine | bipolarCapLine

  private def diagramInput: Parser[Result] = (line+) ^^ { lines =>
    val (components, connections) = lines.map {
      case (component, legsConnections) =>
        (
          component,
          legsConnections.zipWithIndex.map { case (connection, i) =>
            (component.name.value, i.toString) -> connection.id.id
          }
        )
    }.unzip
    (components, connections.flatten.toMap)
  }

  def parse(diagramStr: String): Either[String, Result] =
    parse(diagramInput, diagramStr) match {
      case Success(result, _) => Right(result)
      case Failure(msg, _) => Left(msg)
      case Error(msg, _) => Left(msg)
    }
}
