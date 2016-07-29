import sbt.Keys._

name := "diagram-to-breadboard root"

lazy val root = project.in(file(".")).
  aggregate(dtbJS, dtbJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val diagramToBreadboard = crossProject.in(file(".")).
  settings(
    name := "diagram-to-breadboard",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.8"
  ).
  jvmSettings(
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.0.0-RC4" % "test"
    )
  ).
  jsSettings(
    scalaJSUseRhino in Global := false,
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.9.0"
    )
  )


lazy val dtbJVM = diagramToBreadboard.jvm
lazy val dtbJS = diagramToBreadboard.js