# diagram-to-breadboard
Converting electronic diagram to breadboard

This project loosely aims at being a converter of electronic diagram encoded in scala to another diagram describing placement of electronic parts on solderless breadboard.  

For me, the motivation to start this project came from:
  * my long obsession with ciat-lonbarde designs, for example [rollz5 paper circuit](http://www.ciat-lonbarde.net/rollz5/index.html)
  * my poor soldering skills

## Example

First diagram from rollz5 is called 3-Roll.  Here it is with connections marked with numbers (and ground as blue "G", +9V marked as red "P"):

<img src=./static/3roll.png width="300" height="300">

3-Roll diagram can be encoded in Scala in `Diagrams.scala` file as:

    val example = Diagram(
      List(
        Component("diode", Diode("???")),
        Component("R470K-1", Resistor("470K")),
        Component("R22K-1", Resistor("22K")),
        Component("R470K-2", Resistor("470K")),
        Component("R22K-2", Resistor("22K")),
        Component("R470K-3", Resistor("470K")),
        Component("R22K-3", Resistor("22K")),
        Component("Tr-1", Transistor("minus")),
        Component("Tr-2", Transistor("minus")),
        Component("Tr-3", Transistor("minus")),
        Component("hairy-1", Capacitor(0d, bipolar = true)),
        Component("hairy-2", Capacitor(0d, bipolar = true)),
        Component("hairy-3", Capacitor(0d, bipolar = true)),
        Component("cap-4", Capacitor(0d, bipolar = true))
      ),
      Map(
        ("diode", Leg.cathode) -> Left(1), ("diode", Leg.anode) -> Right(Plus),
        ("R470K-1", "0") -> Left(1), ("R470K-1", "1") -> Left(3),
        ("R22K-1", "0") -> Left(1), ("R22K-1", "1") -> Left(2),
        ("R470K-2", "0") -> Left(1), ("R470K-2", "1") -> Left(7),
        ("R22K-2", "0") -> Left(1), ("R22K-2", "1") -> Left(6),
        ("R470K-3", "0") -> Left(1), ("R470K-3", "1") -> Left(5),
        ("R22K-3", "0") -> Left(1), ("R22K-3", "1") -> Left(4),
        ("Tr-1", "0") -> Left(2), ("Tr-1", "1") -> Left(3), ("Tr-1", "2") -> Right(GND),
        ("Tr-2", "0") -> Left(6), ("Tr-2", "1") -> Left(7), ("Tr-2", "2") -> Right(GND),
        ("Tr-3", "0") -> Left(4), ("Tr-3", "1") -> Left(5), ("Tr-3", "2") -> Right(GND),
        ("hairy-1", Leg.capMinus) -> Left(7), ("hairy-1", Leg.capPlus) -> Left(2),
        ("hairy-2", Leg.capMinus) -> Left(3), ("hairy-2", Leg.capPlus) -> Left(4),
        ("hairy-3", Leg.capMinus) -> Left(5), ("hairy-3", Leg.capPlus) -> Left(6),
        ("cap-4", Leg.capMinus) -> Right(GND), ("cap-4", Leg.capPlus) -> Left(1)
      )
      
Compiling to JS:

    sbt fastOptJS
    
Then, after opening `index-dev.html` it should show resulting breadboard diagram.  It's possible to drag components to make whole thing more readable:

<img src=./static/3roll-breadboard.png width="1000" height="500">
