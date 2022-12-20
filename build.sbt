ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

val circeVersion = "0.14.1"

lazy val root = (project in file("."))
  .settings(
    name := "Advent2022" ,
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.14",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % "test",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser",
    ).map(_ % circeVersion),
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
)





