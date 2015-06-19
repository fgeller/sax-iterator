organization := "geller.io"

name := "sax-iterator"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
// "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % "test"
)

incOptions := incOptions.value.withNameHashing(true)

fork in Test := true

scalacOptions ++= Seq(
  "-feature",
  "-deprecation"
)
