name := """aws-stepfunctions"""

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.typesafe.play" % "play-json_2.11" % "2.5.8",
  "org.specs2" % "specs2-core_2.11" % "3.7" % "test"
)
scalacOptions in Test ++= Seq("-Yrangepos")

scalacOptions += "-feature"
scalacOptions += "-deprecation"

fork in run := true
