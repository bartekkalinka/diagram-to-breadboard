package pl.bka.dtb.model

trait Container {
  val components: Seq[Component]

  def noCables: Seq[Component] = components.filterNot(_.cType.isInstanceOf[Cable])
  def componentsByName: Map[ComponentName, Component] = components.groupBy(_.name).mapValues(_.head)
  def componentsLegs: Map[ComponentName, Seq[LegId]] = components.map(c => (c.name, c.legs.map(LegId(c.name, _)))).toMap
  def allLegs: Set[LegId] = components.flatMap(c => c.legs.map(LegId(c.name, _))).toSet
  def cablesNumber: Int = components.count(_.cType.isInstanceOf[Cable])
}

