import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val dtb =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full) // [Pure, Full, Dummy], default: CrossType.Full
    .settings(
      name := "diagram-to-breadboard root",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.12.6",
      libraryDependencies ++= Seq(
        "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.1"
      )
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

lazy val dtbJS = dtb.js
lazy val dtbJVM = dtb.jvm
