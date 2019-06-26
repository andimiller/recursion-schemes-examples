
name := "recursion-schemes-examples"

version := "0.1"

scalaVersion := "2.12.8"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= List(
  "io.higherkindness" %% "droste-core" % "0.7.0",
  "io.higherkindness" %% "droste-macros" % "0.7.0",
  "org.tpolecat" %% "atto-core" % "0.6.5",
  "io.circe" %% "circe-core" % "0.11.1",
  compilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
  compilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.patch)
)
