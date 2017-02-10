lazy val root = project.in(file(".")).aggregate(llJS, llJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val ll = crossProject.in(file(".")).
  settings(
    name := "LocalLexing",
    organization := "net.locallexing",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.12.0",
    scalacOptions += "-deprecation"
  )

lazy val llJS = ll.js
lazy val llJVM = ll.jvm