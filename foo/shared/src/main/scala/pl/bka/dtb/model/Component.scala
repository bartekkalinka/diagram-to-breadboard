package pl.bka.dtb.model

case class Leg(name: String)

object Leg {
  val firstLeg = "0"
  val secondLeg = "1"
  val capMinus = firstLeg
  val capPlus = secondLeg
  val cathode = firstLeg //minus / ring
  val anode = secondLeg //plus / no ring
}

case class ComponentName(value: String)
case class Component(name: ComponentName, cType: ComponentType, legs: Seq[Leg]) {
  def prettyPrint = s"${name.value} $cType"
}

object Component {
  def apply(name: String, cType: ComponentType): Component = Component(ComponentName(name), cType, cType.initLegs)
}


