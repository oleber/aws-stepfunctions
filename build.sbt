name := """aws-stepfunctions"""

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "com.typesafe.play" % "play-json_2.12" % "2.6.0-M1",
  "org.specs2" % "specs2-core_2.12" % "3.8.6" % "test"
)
scalacOptions in Test ++= Seq("-Yrangepos")

fork in run := true
