organization := "com.phasmidsoftware"

name := "Comparer"

version := "1.0.1"

scalaVersion := "2.12.8"

val scalaTestVersion = "3.0.5"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)
