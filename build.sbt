name := "scala-exercises"

version := "0.1"

scalaVersion := "3.1.3"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.12" % "test",
  "joda-time" % "joda-time" % "2.10.14",
  "org.typelevel" %% "cats-effect" % "3.3.12",
  "com.github.nscala-time" %% "nscala-time" % "2.30.0",
//  "io.monix" %% "monix" % "3.4.1", // uses an old version of cats-effect
)

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.12" withSources() withJavadoc()

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-deprecation"
)

