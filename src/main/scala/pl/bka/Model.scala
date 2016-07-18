package pl.bka

sealed trait ComponentType {
  val legsCount: Int
  def initLegs: Seq[Leg] = List.tabulate(legsCount)(_.toString).map(Leg)
}
case class VoltageSource(voltage: Int, legsCount: Int = 2) extends ComponentType
case class IC(symbol: String, legsCount: Int) extends ComponentType
case class Resistor(ohms: Int, legsCount: Int = 2) extends ComponentType
case class Capacitor(capacitance: Int, electrolytic: Boolean, legsCount: Int = 2) extends ComponentType
case class Diode(symbol: String, legsCount: Int = 2) extends ComponentType
case class Transistor(symbol: String, legsCount: Int = 3) extends ComponentType


case class Leg(name: String)
case class ComponentName(value: String)
case class Component(name: ComponentName, cType: ComponentType, legs: Seq[Leg])

object Component {
  def apply(name: String, cType: ComponentType): Component = Component(ComponentName(name), cType, cType.initLegs)
}

case class Connection(id: Int)
case class Fail(reason: String)
case class Diagram(
                    components: Seq[Component],
                    legsConnections: Map[(ComponentName, Leg), Connection]
                  ) {
  lazy val connections: Seq[Connection] = legsConnections.values.toSeq.distinct
  lazy val connectionsComponents: Map[Connection, ComponentName] = legsConnections.toSeq.map(lc => (lc._2, lc._1._1)).toMap

  def validate: Either[Fail, Diagram] = {
    val allLegs = components.flatMap(c => c.legs.map((c.name, _))).toSet
    val allMappedLegs = legsConnections.keys.toSet
    if(allLegs == allMappedLegs) Right(this) else Left(Fail("Not all legs mapped"))
  }
}

object Diagram {
  def apply(components: Seq[Component], legsConnections: Map[(String, String), Int]): Either[Fail, Diagram] = {
    val diagram = Diagram(components, legsConnections.toSeq.map { case ((c, l), conn) => ((ComponentName(c), Leg(l)), Connection(conn)) }.toMap)
    diagram.validate
  }
}