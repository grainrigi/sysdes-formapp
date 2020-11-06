import sbt.Keys.{libraryDependencies, organization}

lazy val commonSettings = Seq(
  name := "formapp",
  organization := "sysdes",
  version := "0.1",
  scalaVersion := "2.13.3",
  scalafmtOnCompile := true,
  scalacOptions ++= Seq("-Ymacro-annotations"),
  fork in run := true,
  libraryDependencies ++= Seq(
    "org.specs2" %% "specs2-core" % "4.6.0" % "test"
  ),
  libraryDependencies += scalaReflect.value,
)

lazy val scalaReflect = Def.setting {"org.scala-lang" % "scala-reflect" % scalaVersion.value}

scalacOptions in Test ++= Seq("-Yrangepos")

lazy val core = (project in file("."))
  .dependsOn(macroSub)
  .settings(
    commonSettings,
  )

lazy val macroSub = (project in file ("macro"))
  .settings(
    commonSettings,
  )
