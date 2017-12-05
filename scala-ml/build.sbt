name := """scala-ml"""

version := "1.0"

scalaVersion := "2.12.4"

// Change this to another test framework if you prefer
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "org.typelevel" %% "cats-core" % "1.0.0-RC1",
  "org.typelevel" %% "cats-effect" % "1.0.0-RC1",
  "org.sameersingh.scalaplot" % "scalaplot" % "0.0.4"
)

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

