import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val foo =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full) // [Pure, Full, Dummy], default: CrossType.Full
    .settings(
      name := "diagram-to-breadboard root",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.12.6"
    )
    .jsSettings(
      libraryDependencies ++= Seq(
        "org.scala-js" %%% "scalajs-dom" % "0.9.6"
      )
    )
    .jvmSettings(
      libraryDependencies ++= Seq(
        "org.scalatest" %%% "scalatest" % "3.0.0" % "test"
      )
    )

lazy val fooJS = foo.js
lazy val fooJVM = foo.jvm
