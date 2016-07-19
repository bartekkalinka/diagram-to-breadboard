package pl.bka.model

case class Connection(id: Int)
case class Fail(reason: String)
case class LegId(cName: ComponentName, leg: Leg)
case class Diagram(
                    components: Seq[Component],
                    legsConnections: Map[LegId, Connection]
                  ) {
  lazy val connections: Seq[Connection] = legsConnections.values.toSeq.distinct
  lazy val connectionsComponents: Map[Connection, ComponentName] = legsConnections.toSeq.map(lc => (lc._2, lc._1.cName)).toMap

  def validate: Either[Fail, Diagram] = {
    val allLegs = components.flatMap(c => c.legs.map(LegId(c.name, _))).toSet
    val allMappedLegs = legsConnections.keys.toSet
    if(allLegs == allMappedLegs) Right(this) else Left(Fail("Not all legs mapped"))
  }
}

object Diagram {
  def apply(components: Seq[Component], legsConnectionsSimple: Map[(String, String), Int]): Either[Fail, Diagram] = {
    val diagram = Diagram(components, legsConnectionsSimple.toSeq.map {
      case ((c, l), conn) => (LegId(ComponentName(c), Leg(l)), Connection(conn))
    }.toMap)
    diagram.validate
  }
}

