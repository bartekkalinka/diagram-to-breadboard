package pl.bka.model

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

