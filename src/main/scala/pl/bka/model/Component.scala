package pl.bka.model

case class Leg(name: String)
case class ComponentName(value: String)
case class Component(name: ComponentName, cType: ComponentType, legs: Seq[Leg])

object Component {
  def apply(name: String, cType: ComponentType): Component = Component(ComponentName(name), cType, cType.initLegs)
}


