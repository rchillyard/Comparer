organization := "com.phasmidsoftware"

name := "Comparer"

version := "1.0.10"

scalaVersion := "2.13.16"

scalacOptions += "-deprecation"

val scalaTestVersion = "3.2.19"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)
