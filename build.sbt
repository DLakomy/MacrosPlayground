val scala3Version = "3.5.0"

ThisBuild / scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Wunused:all"
)


lazy val root = project
  .in(file("."))
  .settings(
    name         := "macros-playground",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version
  )
