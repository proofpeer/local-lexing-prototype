lazy val root = project.in(file(".")).aggregate(llJS, llJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val ll = crossProject.in(file(".")).
  settings(
    name := "LocalLexing Prototype",
    organization := "net.proofpeer",
    version := "0.2-SNAPSHOT",
    scalaVersion := "2.12.0",
    scalacOptions += "-deprecation"
  )

lazy val llJS = ll.js
lazy val llJVM = ll.jvm