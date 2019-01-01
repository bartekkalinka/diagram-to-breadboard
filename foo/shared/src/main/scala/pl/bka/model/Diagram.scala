package pl.bka.model

object Power {
  sealed trait PowerConnection
  case object Plus extends PowerConnection
  case object GND extends PowerConnection
}
case class Connection(id: Either[Int, Power.PowerConnection]) {
  def initialTrackIndex: Int = id match {
    case Right(Power.Plus) => 0
    case Right(Power.GND) => 1
    case Left(index) => index + 2
  }
}
case class Fail(reason: String)
case class LegId(cName: ComponentName, leg: Leg) {
  def prettyPrintStr = s"${cName.value} leg${leg.name}"
}
case class Diagram(
                    components: Seq[Component],
                    legsConnections: Map[LegId, Connection]
                  ) extends Container {
  def validate: Either[Fail, Diagram] = for {
    _ <- {
      val allMappedLegs = legsConnections.keys.toSet
      if (allLegs == allMappedLegs) Right(this) else Left(Fail("Not all legs mapped"))
    }
    _ <- {
      val componentsDuplicatedNames = components.groupBy(_.name).toSeq.filter { case (name, comps) => comps.length > 1 }.map(_._1)
      if (componentsDuplicatedNames.nonEmpty) Left(Fail(s"Components with duplicated name: $componentsDuplicatedNames")) else Right(this)
    }
  } yield this

  //TODO move to PrettyPrint type class
  def prettyPrint: Seq[String] = Seq(
    "   components: " + components.map(_.prettyPrint).reduce(_ + " | " + _),
    "   connections: " + legsConnections.toSeq.map { case (legId, conn) => s"${legId.prettyPrintStr} conn${conn.id}" }.reduce(_ + " : " + _)
  )

  private def equivalenceSets: (Set[Component], Set[Set[LegId]]) = (
      components.toSet,
      legsConnections.toSeq.groupBy(_._2).mapValues(v => v.map(_._1)).values.map(_.toSet).toSet
    )

  def isEquivalentTo(that: Diagram): Boolean =
    this.equivalenceSets == that.equivalenceSets
}

object Diagram {
  def apply(components: Seq[Component], legsConnectionsSimple: Map[(String, String),
            Either[Int, Power.PowerConnection]]): Either[Fail, Diagram] = {
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

