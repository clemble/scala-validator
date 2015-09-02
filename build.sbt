name := "scala-validator"

organization := "com.clemble"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

offline := true

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.4.2" % "provided",
  "org.specs2" %% "specs2-core" % "3.6.2" % "test",
  "org.specs2" %% "specs2-junit" % "3.6.2" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")

fork in run := false
