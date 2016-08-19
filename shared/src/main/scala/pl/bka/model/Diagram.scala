package pl.bka.model

case class Connection(id: Int)
case class Fail(reason: String)
case class LegId(cName: ComponentName, leg: Leg) {
  def prettyPrint = s"${cName.value} leg${leg.name}"
}
case class Diagram(
                    components: Seq[Component],
                    legsConnections: Map[LegId, Connection]
                  ) {
  //TODO which lazy vals are really needed?
  lazy val connections: Seq[Connection] = legsConnections.values.toSeq.distinct
  lazy val connectionsComponents: Map[Connection, ComponentName] = legsConnections.toSeq.map(lc => (lc._2, lc._1.cName)).toMap
  lazy val connectionsLegs: Map[Connection, Seq[LegId]] = legsConnections.toSeq.groupBy(_._2).mapValues(_.map(_._1))
  lazy val allLegs: Set[LegId] = components.flatMap(c => c.legs.map(LegId(c.name, _))).toSet
  lazy val componentsLegs: Map[ComponentName, Seq[LegId]] = components.map(c => (c.name, c.legs.map(LegId(c.name, _)))).toMap

  def validate: Either[Fail, Diagram] = {
    val allMappedLegs = legsConnections.keys.toSet
    if(allLegs == allMappedLegs) Right(this) else Left(Fail("Not all legs mapped"))
  }

  def prettyPrint: Seq[String] = Seq(
    "   components: " + components.map(_.prettyPrint).reduce(_ + " | " + _),
    "   connections: " + legsConnections.toSeq.map { case (legId, conn) => s"${legId.prettyPrint} conn${conn.id}" }.reduce(_ + " : " + _)
  )
}

object Diagram {
  def apply(components: Seq[Component], legsConnectionsSimple: Map[(String, String), Int]): Either[Fail, Diagram] = {
    val diagram = Diagram(components, legsConnectionsSimple.toSeq.map {
      case ((c, l), conn) => (LegId(ComponentName(c), Leg(l)), Connection(conn))
    }.toMap)
    diagram.validate
  }

  def prettyPrint(applyResult: Either[Fail, Diagram]): Seq[String] = applyResult match {
    case Right(diagram) => diagram.prettyPrint
    case Left(fail) => Seq(fail.toString)
  }
}
