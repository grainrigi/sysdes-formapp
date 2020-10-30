name := "formapp"
organization := "sysdes"

version := "0.1"

scalaVersion := "2.13.3"

scalafmtOnCompile := true

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "4.6.0" % "test"
)
scalacOptions in Test ++= Seq("-Yrangepos")

fork in run := true