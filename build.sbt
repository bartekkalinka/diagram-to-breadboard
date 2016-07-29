name := "diagram-to-breadboard root"

scalaVersion in ThisBuild := "2.11.8"

lazy val root = project.in(file(".")).
  aggregate(fooJS, fooJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val foo = crossProject.in(file(".")).
  settings(
    name := "diagram-to-breadboard",
    version := "0.1-SNAPSHOT"
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

lazy val fooJVM = foo.jvm
lazy val fooJS = foo.js