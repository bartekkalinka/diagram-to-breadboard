name := "diagram-to-breadboard"

version := "1.0"

scalaVersion := "2.11.8"

scalaJSUseRhino in Global := false

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.0",
  "org.scalatest"     %% "scalatest" % "2.2.6" % "test"
)

enablePlugins(ScalaJSPlugin)