package pl.bka.model

case class Leg(name: String)

object Leg {
  val capMinus = "0"
  val capPlus = "1"
  val cathode = "0" //minus / ring
  val anode = "1" //plus / no ring
}

case class ComponentName(value: String)
case class Component(name: ComponentName, cType: ComponentType, legs: Seq[Leg]) {
  def prettyPrint = s"${name.value} $cType"
}

object Component {
  def apply(name: String, cType: ComponentType): Component = Component(ComponentName(name), cType, cType.initLegs)
}


