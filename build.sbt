organization := "com.phasmidsoftware"

name := "Comparer"

version := "1.0.9"

scalaVersion := "2.13.1"

val scalaTestVersion = "3.1.1"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)
