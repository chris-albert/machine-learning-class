name := """scala-ml"""

version := "1.0"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "org.typelevel" %% "cats-core" % "1.0.0-RC1",
  "org.typelevel" %% "cats-effect" % "0.5",
  "org.scalaz" %% "scalaz-core" % "7.2.17",
  "org.sameersingh.scalaplot" % "scalaplot" % "0.0.4"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

