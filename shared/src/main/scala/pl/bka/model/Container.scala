package pl.bka.model

trait Container {
  val components: Seq[Component]

  def noCables: Seq[Component] =
    components.filter(c => !c.cType.isInstanceOf[Cable])

  def componentsLegs: Map[ComponentName, Seq[LegId]] = components.map(c => (c.name, c.legs.map(LegId(c.name, _)))).toMap
  def allLegs: Set[LegId] = components.flatMap(c => c.legs.map(LegId(c.name, _))).toSet
}
