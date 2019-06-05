organization := "com.com.phasmidsoftware"

name := "Comparer"

version := "1.0.0"

scalaVersion := "2.12.5"

val scalaTestVersion = "3.0.5"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)
