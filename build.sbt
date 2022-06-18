name := "scala-exercises"

version := "0.1"

scalaVersion := "2.13.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test"
libraryDependencies += "joda-time" % "joda-time" % "2.10.14"
libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.30.0"
libraryDependencies += "io.monix" %% "monix" % "3.4.1"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-deprecation"
)

